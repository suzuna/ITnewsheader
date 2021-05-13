# yymm:その月のトップページなら""（もしくはyymm）、あるいはyymm
get_netlabo_articlelist <- function(yymm,UA){
  if (yymm=="") {
    url <- "https://nlab.itmedia.co.jp/nl/subtop/archive/"
  } else {
    url <- str_c("https://nlab.itmedia.co.jp/nl/subtop/archive/",yymm,".html")
  }
  
  page <- session(url,user_agent(UA)) %>% 
    read_html()
  res <- read_ITmedia_articlelist_html(page,"ねとらぼ")
  return(res)
}

get_netlabo_ranking <- function(UA){
  url <- "https://nlab.itmedia.co.jp/nl/subtop/ranking/"
  
  page <- session(url,user_agent(UA)) %>% 
    read_html()
  res <- read_netlabo_ranking_html(page,"ねとらぼ")
  return(res)
}
