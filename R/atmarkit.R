#' @param yymm: character. 取得したい記事のyymm
#' @param UA: character. user_agent
#' @param sleep_time: numeric.
#' @return data.frame.
get_atmarkit_articlelist <- function(yymm,UA,sleep_time){
  if (yymm=="") {
    url <- "https://www.atmarkit.co.jp/ait/subtop/archive/"
  } else {
    url <- str_c("https://www.atmarkit.co.jp/ait/subtop/archive/",yymm,".html")
  }
  
  page <- session(url,user_agent(UA)) %>% 
    read_html(encoding="Shift-JIS")
  res <- read_ITmedia_articlelist_html(page,"atmarkit")
  Sys.sleep(sleep_time)
  return(res)
}


#' @param category: character. "https://www.atmarkit.co.jp/json/ait/rss_rankindex_{category}_day.json"のcategory
#' @param UA: character. user_agent
#' @param sleep_time: numeric.
#' @return data.frame.
get_atmarkit_ranking <- function(category,UA,sleep_time){
  url <- str_glue("https://www.atmarkit.co.jp/json/ait/rss_rankindex_{category}_day.json")
  tmp <- GET(url,user_agent(UA)) %>% 
    content("text",encoding="Shift-JIS")
  title <- tmp %>% 
    str_subset("'title':") %>%
    str_extract_all("(?<='title':').*(?=',)") %>%
    .[[1]] %>% 
    str_trim(side="both")
  url <- tmp %>% 
    str_subset("'link':") %>%
    str_extract_all("(?<='link':').*(?=',)") %>% 
    .[[1]]
  date <- url %>% 
    parse_date_from_ITmedia_URL()
  res <- data.frame(
    source="atmarkit",
    date=date,
    rank=1:length(title),
    title=title,
    url=url,
    writer=NA_character_
  )
  Sys.sleep(sleep_time)
  return(res)
}
