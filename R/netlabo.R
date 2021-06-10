#' @param yymm: character. 取得したい記事のyymm
#' @param UA: character. user_agent
#' @param sleep_time: numeric.
#' @return data.frame.
get_netlabo_articlelist <- function(yymm,UA,sleep_time){
  if (yymm=="") {
    url <- "https://nlab.itmedia.co.jp/nl/subtop/archive/"
  } else {
    url <- str_c("https://nlab.itmedia.co.jp/nl/subtop/archive/",yymm,".html")
  }
  
  page <- session(url,user_agent(UA)) %>% 
    read_html(encoding="Shift-JIS")
  res <- read_ITmedia_articlelist_html(page,"ねとらぼ")
  Sys.sleep(sleep_time)
  return(res)
}


#' @param UA: character. user_agent
#' @param sleep_time: numeric.
#' @return data.frame.
get_netlabo_ranking <- function(UA,sleep_time){
  url <- "https://nlab.itmedia.co.jp/nl/subtop/ranking/"
  
  page <- session(url,user_agent(UA)) %>% 
    read_html(encoding="Shift-JIS")
  res <- read_netlabo_ranking_html(page,"ねとらぼ")
  Sys.sleep(sleep_time)
  return(res)
}
