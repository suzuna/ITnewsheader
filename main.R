library(tidyverse)
library(lubridate)
library(rvest)
library(httr)

USER_AGENT <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36"

# 2021年5月8日 -> "2021-05-08"(date)
# as.Date(format=...)は、leading spaceのないmonth（例：5月）をパースできない
parse_date2 <- function(str){
  y <- str_extract(str,"[0-9]+(?=年)")
  m <- str_pad(str_extract(str,"[0-9]+(?=月)"),width=2,side="left",pad="0")
  d <- str_pad(str_extract(str,"[0-9]+(?=日)"),width=2,side="left",pad="0")
  as.Date(str_c(y,m,d),format="%Y%m%d")
}

# https://www.itmedia.co.jp/news/articles/2105/07/news107.html -> "2021-05-07"
# ITmedia系のサイトはURLに日付が含まれている
parse_date_from_ITmedia_URL <- function(url){
  yymm <- str_extract(url,"(?<=articles/)[0-9]{4}(?=/)")
  y <- str_sub(yymm,1,2)
  m <- str_sub(yymm,3,4)
  d <- str_extract(url,"(?<=articles/[0-9]{4}/)[0-9]+(?=/)")
  as.Date(str_c(y,m,d),format="%y%m%d")
}

# "2020-12-15"と"2021-05-08" -> c("2012","2101",...,"2105")
seq_yymm <- function(start_date,end_date){
  seq(floor_date(start_date,unit="month"),floor_date(end_date,unit="month"),by="1 month") %>% 
    format("%y%m")
}

# ITmediaの一覧ページは共通なので
read_ITmedia_articlelist_html <- function(page,source){
  tmp <- page %>% 
    html_elements("div.colBox.colBoxBacknumber>div.colBoxOuter>div.colBoxInner>div.colBoxIndex>div.colBoxUlist>ul>li")
  
  article_type <- tmp %>% 
    html_elements("span.colBoxArticletype") %>% 
    html_text(trim=TRUE)
  title <- tmp %>% 
    html_elements("a") %>%
    html_text(trim=TRUE)
  url <- tmp %>% 
    html_elements("a") %>% 
    html_attr("href") %>% 
    str_c("https:",.)
  writer <- tmp %>% 
    html_elements("span.colBoxArticlewriter") %>% 
    html_text(trim=TRUE) %>% 
    str_extract("(?<=（).*(?=）)")
  date <- tmp %>% 
    html_elements("span.colBoxUlistDate") %>% 
    html_text(trim=TRUE) %>% 
    str_extract("[0-9]+年[0-9]+月[0-9]+日") %>% 
    parse_date2()
  
  data.frame(
    source=source,
    date=date,
    article_type=article_type,
    title=title,
    url=url,
    writer=writer
  )
}

read_ITmedia_ranking_html <- function(page,source){
  tmp <- page %>% 
    html_elements("div#Ranking") %>% 
    html_elements("div.colBoxIndex")
  
  subtitle <- tmp %>% 
    html_elements("div.colBoxSubTitle") %>% 
    html_text(trim=TRUE) %>% 
    if_else(.=="",NA_character_,.)
  title <- tmp %>% 
    html_elements("div.colBoxTitle") %>% 
    html_elements("a") %>% 
    html_text(trim=TRUE)
  url <- tmp %>% 
    html_elements("div.colBoxTitle") %>% 
    html_elements("a") %>% 
    html_attr("href")
  date <- tmp %>% 
    html_elements("div.colBoxInfo>span.colBoxDate") %>% 
    html_text(trim=TRUE) %>% 
    str_extract("[0-9]+年[0-9]+月[0-9]+日") %>% 
    parse_date2()
  
  data.frame(
    source=source,
    date=date,
    rank=1:length(title),
    subtitle=subtitle,
    title=title,
    url=url
  )
}

read_netlabo_ranking_html <- function(page,source){
  tmp <- page %>% 
    html_elements("div.myBox.myBoxStandard.myBoxRanking.myBoxRankingMain") %>% 
    html_elements("div.myBoxR")
  
  title <- tmp %>% 
    html_elements("h3.myBoxTitle") %>% 
    html_elements("a") %>% 
    html_text(trim=TRUE)
  abstract <- tmp %>% 
    html_elements("p.myBoxAbs") %>% 
    html_text(trim=TRUE) %>% 
    if_else(.=="",NA_character_,.)
  url <- tmp %>% 
    html_elements("h3.myBoxTitle") %>% 
    html_elements("a") %>% 
    html_attr("href")
  date <- url %>% 
    parse_date_from_ITmedia_URL()
  data.frame(
    source=source,
    date=date,
    rank=1:length(title),
    title=title,
    url=url
  )
}
