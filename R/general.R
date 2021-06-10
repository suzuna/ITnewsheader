library(tidyverse)
library(lubridate)
library(rvest)
library(httr)
library(rlist)
library(jsonlite)


# 関数を定義する -----------------------------------------------------------------
#' 2021年5月8日 -> "2021-05-08"(date)
#' as.Date(format=...)は、leading spaceのないmonth（例：5月）をパースできない
#' @param str: character vector. yyyy年m月d日（2021年5月8日）の形式。
#' @return: date vector.
parse_date2 <- function(str){
  y <- str_extract(str,"[0-9]+(?=年)")
  m <- str_pad(str_extract(str,"[0-9]+(?=月)"),width=2,side="left",pad="0")
  d <- str_pad(str_extract(str,"[0-9]+(?=日)"),width=2,side="left",pad="0")
  as.Date(str_c(y,m,d),format="%Y%m%d")
}


#' "https://www.itmedia.co.jp/news/articles/2105/07/news107.html" -> "2021-05-07"(date)
#' ITmedia系のサイトはURLに日付が含まれている
#' @param url: character vector.
#' @return: date vector.
parse_date_from_ITmedia_URL <- function(url){
  yymm <- str_extract(url,"(?<=articles/)[0-9]{4}(?=/)")
  y <- str_sub(yymm,1,2)
  m <- str_sub(yymm,3,4)
  d <- str_extract(url,"(?<=articles/[0-9]{4}/)[0-9]+(?=/)")
  as.Date(str_c(y,m,d),format="%y%m%d")
}


#' "2020-12-15"と"2021-05-08" -> c("2012","2101",...,"2105")やc("202012","202101",...,"202105")
#' @param start_date: date.
#' @param end_date: date.
#' @param format: character. strftimeのformat
#' @return: character vector.
create_seq_yymm <- function(start_date,end_date,format){
  seq(floor_date(start_date,unit="month"),floor_date(end_date,unit="month"),by="1 month") %>% 
    format(format)
}


#' ITmediaの一覧ページは共通なので
#' @param page: xml document.
#' @param source: character.
#' @return data.frame.
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


#' @param page: xml document.
#' @param source: character.
#' @return data.frame.
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


#' @param page: xml document.
#' @param source: character.
#' @return data.frame.
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


#' title列とurl列を持つdata.frameにおいて、urlをリンクにしたtitle列を作る
#' @param data: "title"と"url"を列名に持つdata.frame
#' @param format: "DT" or "kable"
#' @return data.frame. 引数のdataにあるtitle列とurl列が消え、title_with_link列ができる
make_table_for_report <- function(df,format="DT"){
  if (format=="DT"){
    df <- df %>% 
      mutate(title_with_link=str_glue("<a href='{url}' target='_blank' rel='noopener noreferrer'>{title}</a>"))
  } else if (format=="kable"){
    df <- df %>% 
      mutate(title_with_link=str_glue("[{title}]({url})"))
  }
  df %>% 
    mutate(date=as.character(date,format="%Y/%m/%d")) %>% 
    select(date,title_with_link)
}


# 定数 ----------------------------------------------------------------------
SLEEP_TIME <- 1

settings <- fromJSON("settings.json")
USER_AGENT <- settings$general$user_agent

ITMEDIA_MONTH_NUM <- settings$articles$ITmedia$month_num
NIKKEI_XTECH_PAGES <- settings$articles$nikkei_xtech$p_num
ASCII_MONTH_NUM <- settings$articles$ascii$month_num
IMPRESS_MONTH_NUM <- settings$articles$impress$month_num

ITMEDIA_YYMM <- create_seq_yymm(Sys.Date()-days(365),Sys.Date(),format="%y%m") %>% 
  tail(ITMEDIA_MONTH_NUM) %>% 
  rev()
ASCII_YYYYMM <- create_seq_yymm(Sys.Date()-days(365),Sys.Date(),format="%Y%m") %>% 
  tail(ASCII_MONTH_NUM) %>% 
  rev()
IMPRESS_YYYYMM <- create_seq_yymm(Sys.Date()-days(365),Sys.Date(),format="%Y%m") %>% 
  tail(IMPRESS_MONTH_NUM) %>% 
  rev()
