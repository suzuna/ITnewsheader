# yymm:その月のトップページなら""（もしくはyymm）、あるいはyymm
get_ITmedia_PCUSER_articlelist <- function(yymm,UA){
  if (yymm=="") {
    url <- "https://www.itmedia.co.jp/pcuser/subtop/archive/"
  } else {
    url <- str_c("https://www.itmedia.co.jp/pcuser/subtop/archive/",yymm,".html")
  }
  
  page <- session(url,user_agent(UA)) %>% 
    read_html(encoding="Shift-JIS")
  res <- read_ITmedia_articlelist_html(page,"ITmedia PCUSER")
  return(res)
}

get_ITmedia_PCUSER_ranking <- function(UA){
  url <- "https://www.itmedia.co.jp/pcuser/subtop/ranking/"
  
  page <- session(url,user_agent(UA)) %>% 
    read_html(encoding="Shift-JIS")
  res <- read_ITmedia_ranking_html(page,"ITmedia PCUSER")
  return(res)
}
