# category:"https://ascii.jp/archive/top/202105/"の"top"
get_ascii_articlelist <- function(category="top",yyyymm=format(Sys.Date(),"%Y%m"),UA,sleep_time){
  url <- str_glue("https://ascii.jp/archive/{category}/{yyyymm}/")
  
  page <- session(url,user_agent(UA)) %>% 
    read_html(encoding="UTF-8")
  tmp <- page %>% 
    html_elements("div.archives")
  
  title <- tmp %>% 
    html_elements("ul") %>% 
    map(~html_elements(.x,"li>a") %>% 
          html_text(trim=TRUE))
  url <- tmp %>% 
    html_elements("ul") %>% 
    map(~html_elements(.x,"li>a") %>% 
          html_attr("href") %>% 
          str_c("https://ascii.jp",.))
  date <- tmp %>% 
    html_elements("h4") %>% 
    html_text(trim=TRUE) %>% 
    as.Date()
  res <- tibble(
    source=str_glue("ascii_{category}"),
    date=date,
    title=title,
    url=url
  ) %>% 
    unnest(c(title,url))
  Sys.sleep(sleep_time)
  return(res)
}
