# yymm:その月のトップページなら""（もしくはyymm）、あるいはyymm
get_list_ITmedia_NEWS <- function(yymm,UA){
  if (yymm=="") {
    url <- "https://www.itmedia.co.jp/news/subtop/archive/"
  } else {
    url <- str_c("https://www.itmedia.co.jp/news/subtop/archive/",yymm,".html")
  }
  
  page <- session(url,user_agent(UA)) %>% 
    read_html()
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
    source="ITmedia NEWS",
    date=date,
    article_type=article_type,
    title=title,
    url=url,
    writer=writer
  )
}

get_ranking_ITmedia_NEWS <- function(UA){
  url <- "https://www.itmedia.co.jp/news/subtop/ranking/"
  
  page <- session(url,user_agent(UA)) %>% 
    read_html()
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
    source="ITmedia NEWS",
    date=date,
    rank=1:length(title),
    subtitle=subtitle,
    title=title,
    url=url
  )
}