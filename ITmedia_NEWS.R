# yymm:その月のトップページなら""（もしくはyymm）、あるいはyymm
get_ITmedia_NEWS_articlelist <- function(yymm,UA){
  if (yymm=="") {
    url <- "https://www.itmedia.co.jp/news/subtop/archive/"
  } else {
    url <- str_c("https://www.itmedia.co.jp/news/subtop/archive/",yymm,".html")
  }
  
  page <- session(url,user_agent(UA)) %>% 
    read_html()
  res <- read_ITmedia_list_html(page,"ITmedia NEWS")
  return(res)
}

get_ITmedia_NEWS_ranking <- function(UA){
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