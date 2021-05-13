# yymm:その月のトップページなら""（もしくはyymm）、あるいはyymm
get_ITmedia_articlelist <- function(yymm,UA){
  url <- "https://www.itmedia.co.jp/"
  
  page <- session(url,user_agent(UA)) %>% 
    read_html()
  tmp <- page %>% 
    html_elements("div.colBoxTab2#colBoxTopStoriesBacknum")
  
  title <- tmp %>% 
    html_elements("h3>a") %>%
    html_text(trim=TRUE)
  url <- tmp %>% 
    html_elements("h3>a") %>% 
    html_attr("href")
  date <- url %>% 
    parse_date_from_ITmedia_URL()
  
  res <- data.frame(
    source="ITmedia",
    date=date,
    title=title,
    url=url
  )
  return(res)
}

get_ITmedia_ranking <- function(UA){
  url <- "https://www.itmedia.co.jp/ranking/"
  
  page <- session(url,user_agent(UA)) %>% 
    read_html()
  
  # 1位-10位
  tmp1 <- page %>% 
    html_elements("div#rank-all")
  title1 <- tmp1 %>% 
    html_elements("h3>a") %>% 
    html_text(trim=TRUE)
  url1 <- tmp1 %>% 
    html_elements("h3>a") %>% 
    html_attr("href")
  date1 <- tmp1 %>% 
    html_elements("div.art") %>% 
    html_text(trim=TRUE) %>% 
    str_extract("(?<=（)[0-9]{4}/[0-9]{2}/[0-9]{2}(?=）)") %>% 
    # 日付は30位まで全部取れてしまうので
    head(10) %>% 
    as.Date()
  
  # 11位-30位
  tmp2 <- page %>% 
    html_elements("div#rank-all>div.rank-btm")
  title2 <- tmp2 %>% 
    html_elements("a>h3") %>% 
    html_text(trim=TRUE)
  url2 <- tmp2 %>% 
    html_elements("a") %>% 
    html_attr("href")
  date2 <- tmp2 %>% 
    html_elements("div.art") %>% 
    html_text(trim=TRUE) %>% 
    str_extract("(?<=（)[0-9]{4}/[0-9]{2}/[0-9]{2}(?=）)") %>% 
    as.Date()
  
  title <- c(title1,title2)
  url <- c(url1,url2)
  date <- c(date1,date2)
  
  res <- data.frame(
    source="ITmedia",
    date=date,
    rank=1:length(title),
    title=title,
    url=url
  )
  return(res)
}
