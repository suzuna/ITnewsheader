# yymm:その月のトップページなら""（もしくはyymm）、あるいはyymm
get_ITmedia_enterprise_articlelist <- function(yymm,UA){
  if (yymm=="") {
    url <- "https://www.itmedia.co.jp/enterprise/subtop/archive/"
  } else {
    url <- str_c("https://www.itmedia.co.jp/enterprise/subtop/archive/",yymm,".html")
  }
  
  page <- session(url,user_agent(UA)) %>% 
    read_html()
  res <- read_ITmedia_articlelist_html(page,"ITmedia エンタープライズ")
  return(res)
}

get_ITmedia_enterprise_ranking <- function(UA){
  url <- "https://www.itmedia.co.jp/enterprise/subtop/ranking/index.html"
  
  page <- session(url,user_agent(UA)) %>% 
    read_html()
  res <- read_ITmedia_ranking_html(page,"ITmedia エンタープライズ")
  return(res)
}
