# yymm:その月のトップページなら""（もしくはyymm）、あるいはyymm
get_atmarkit_articlelist <- function(yymm,UA){
  if (yymm=="") {
    url <- "https://www.atmarkit.co.jp/ait/subtop/archive/"
  } else {
    url <- str_c("https://www.atmarkit.co.jp/ait/subtop/archive/",yymm,".html")
  }
  
  page <- session(url,user_agent(UA)) %>% 
    read_html(encoding="Shift-JIS")
  res <- read_ITmedia_articlelist_html(page,"atmarkit")
  return(res)
}

get_atmarkit_ranking <- function(category,UA){
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
  return(res)
}
