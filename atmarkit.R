# yymm:その月のトップページなら""（もしくはyymm）、あるいはyymm
get_atmarkit_articlelist <- function(yymm,UA){
  if (yymm=="") {
    url <- "https://www.atmarkit.co.jp/ait/subtop/archive/"
  } else {
    url <- str_c("https://www.atmarkit.co.jp/ait/subtop/archive/",yymm,".html")
  }
  
  page <- session(url,user_agent(UA)) %>% 
    read_html()
  res <- read_ITmedia_articlelist_html(page,"atmarkit")
  return(res)
}

# read_linesを使う以上UAの指定はできない
get_atmarkit_ranking <- function(){
  url <- "https://www.atmarkit.co.jp/json/ait/rss_rankindex_all_day.json"
  tmp <- read_lines(url,locale=locale(encoding="Shift-JIS"))
  title <- tmp %>% 
    str_subset("'title':") %>%
    str_extract("(?<='title':').*(?=',)") %>%
    str_trim(side="both")
  url <- tmp %>% 
    str_subset("'link':") %>%
    str_extract("(?<='link':').*(?=',)")
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
  return(res)
}
