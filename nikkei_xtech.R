get_nikkei_xtech_articlelist <- function(p,UA,sleep_time){
  url <- str_glue("https://xtech.nikkei.com/top/latest.html?bn=news&M=50&P={p}")
  
  page <- session(url,user_agent(UA)) %>% 
    read_html(encoding="UTF-8")
  tmp <- page %>% 
    html_elements("article.l-main_primary") %>% 
    html_elements("li.p-articleList_item>a.p-articleList_item_link")
  
  title <- tmp %>% 
    html_elements("div.p-articleList_item_text>h3.p-articleList_item_title") %>% 
    html_text(trim=TRUE)
  url <- tmp %>% 
    html_attr("href") %>% 
    str_c("https://xtech.nikkei.com",.)
  subtitle <- tmp %>% 
    html_elements("div.p-articleList_item_text>p.p-articleList_item_subTitle") %>% 
    html_text(trim=TRUE)
  date <- tmp %>% 
    html_elements("div.p-articleList_item_text>time.p-articleList_item_date") %>% 
    html_text(trim=TRUE) %>% 
    as.Date(format="%Y.%m.%d")
  res <- data.frame(
    source="日経クロステック",
    date=date,
    subtitle=subtitle,
    title=title,
    url=url
  )
  Sys.sleep(sleep_time)
  return(res)
}

get_nikkei_xtech_ranking <- function(UA,sleep_time){
  url <- "https://xtech.nikkei.com/ranking/"
  
  page <- session(url,user_agent(UA)) %>% 
    read_html(encoding="UTF-8")
  tmp <- page %>% 
    html_elements("div#categoryTabToday>ul.p-articleRankList>li.p-articleRankList_item")
  
  title <- tmp %>% 
    html_elements("div.p-articleRankList_item_text>h3.p-articleRankList_item_title") %>% 
    html_text(trim=TRUE)
  url <- tmp %>% 
    html_elements("a") %>% 
    html_attr("href") %>% 
    str_c("https://xtech.nikkei.com",.)
  subtitle <- tmp %>% 
    html_elements("div.p-articleRankList_item_text>div.p-articleRankList_item_subTitle") %>% 
    html_text(trim=TRUE)
  date <- NA
  res <- data.frame(
    source="日経クロステック",
    date=date,
    subtitle=subtitle,
    title=title,
    url=url
  )
  Sys.sleep(sleep_time)
  return(res)
}
