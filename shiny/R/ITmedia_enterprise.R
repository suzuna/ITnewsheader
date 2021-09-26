#' @param yymm: character. 取得したい記事のyymm
#' @param UA: character. user_agent
#' @param sleep_time: numeric.
#' @return data.frame.
get_ITmedia_enterprise_articlelist <- function(yymm,UA,sleep_time){
  if (yymm=="") {
    url <- "https://www.itmedia.co.jp/enterprise/subtop/archive/"
  } else {
    url <- str_c("https://www.itmedia.co.jp/enterprise/subtop/archive/",yymm,".html")
  }
  
  page <- session(url,user_agent(UA)) %>% 
    read_html(encoding="Shift-JIS")
  res <- read_ITmedia_articlelist_html(page,"ITmedia エンタープライズ")
  Sys.sleep(sleep_time)
  return(res)
}


#' @param UA: character. user_agent
#' @param sleep_time: numeric.
#' @return data.frame.
get_ITmedia_enterprise_ranking <- function(UA,sleep_time){
  url <- "https://www.itmedia.co.jp/enterprise/subtop/ranking/index.html"
  
  page <- session(url,user_agent(UA)) %>% 
    read_html(encoding="Shift-JIS")
  res <- read_ITmedia_ranking_html(page,"ITmedia エンタープライズ")
  Sys.sleep(sleep_time)
  return(res)
}
