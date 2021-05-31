# yymm:その月のトップページなら""（もしくはyymm）、あるいはyymm
get_ITmedia_mobile_articlelist <- function(yymm,UA,sleep_time){
  if (yymm=="") {
    url <- "https://www.itmedia.co.jp/mobile/subtop/archive/"
  } else {
    url <- str_c("https://www.itmedia.co.jp/mobile/subtop/archive/",yymm,".html")
  }
  
  page <- session(url,user_agent(UA)) %>% 
    read_html(encoding="Shift-JIS")
  res <- read_ITmedia_articlelist_html(page,"ITmedia Mobile")
  Sys.sleep(sleep_time)
  return(res)
}

get_ITmedia_mobile_ranking <- function(UA,sleep_time){
  url <- "https://www.itmedia.co.jp/mobile/subtop/ranking/"
  
  page <- session(url,user_agent(UA)) %>% 
    read_html(encoding="Shift-JIS")
  res <- read_ITmedia_ranking_html(page,"ITmedia Mobile")
  Sys.sleep(sleep_time)
  return(res)
}
