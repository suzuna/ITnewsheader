# yymm:その月のトップページなら""（もしくはyymm）、あるいはyymm
get_ITmedia_Mobile_articlelist <- function(yymm,UA){
  if (yymm=="") {
    url <- "https://www.itmedia.co.jp/mobile/subtop/archive/"
  } else {
    url <- str_c("https://www.itmedia.co.jp/mobile/subtop/archive/",yymm,".html")
  }
  
  page <- session(url,user_agent(UA)) %>% 
    read_html()
  res <- read_ITmedia_articlelist_html(page,"ITmedia Mobile")
  return(res)
}

get_ITmedia_Mobile_ranking <- function(UA){
  url <- "https://www.itmedia.co.jp/mobile/subtop/ranking/"
  
  page <- session(url,user_agent(UA)) %>% 
    read_html()
  res <- read_ITmedia_ranking_html(page,"ITmedia Mobile")
  return(res)
}
