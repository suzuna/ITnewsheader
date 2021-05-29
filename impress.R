get_impress_articlelist <- function(category,yyyymm=format(Sys.Date(),"%Y%m"),UA){
  if (category=="top"){
    url <- str_glue("https://watch.impress.co.jp/backno/top/index{yyyymm}.html")
  } else {
    url <- str_glue("https://{category}.watch.impress.co.jp/backno/top/index{yyyymm}.html") 
  }
  
  page <- session(url,user_agent(UA)) %>% 
    read_html()
  tmp <- page %>% 
    html_elements("div#main") %>% 
    html_elements("section.list") %>% 
    html_elements("ul.list-02")
  
  if (category=="top") {
    date <- tmp %>% 
      html_elements("p.date") %>% 
      map(~{
        .x %>% 
          html_text(trim=TRUE) %>% 
          str_extract("(?<=\\().*(?=\\))") %>% 
          as.Date()
      }) %>% 
      reduce(c)
    
    title <- tmp %>% 
      html_elements("p.title") %>% 
      html_text(trim=TRUE)
    url <- tmp %>% 
      html_elements("p.title>a") %>% 
      html_attr("href")
    res <- tibble(
      source=str_glue("impress_{category}"),
      date=date,
      title=title,
      url=url
    )
  } else {
    date <- page %>% 
      html_elements("div#main") %>% 
      html_elements("section.list") %>% 
      html_elements("p.block") %>% 
      html_text() %>% 
      map(~{
        m <- str_extract(.x,"[0-9]+(?=月)")
        d <- str_extract(.x,"[0-9]+(?=日)")
        y <- str_sub(yyyymm,1,4)
        str_glue("{y}/{m}/{d}") %>% 
          as.Date()
      }) %>% 
      reduce(c)
    
    title <- tmp %>% 
      map(~{
        .x %>% 
          html_elements("p.title") %>% 
          html_text(trim=TRUE)
      })
    url <- tmp %>% 
      map(~{
        .x %>% 
          html_elements("p.title>a") %>% 
          html_attr("href")
      })
    res <- tibble(
      source=str_glue("impress_{category}"),
      date=date,
      title=title,
      url=url
    ) %>% 
      unnest(c(title,url))
  }
  return(res)
}

get_impress_ranking <- function(category,UA){
  category2 <- case_when(
    # クラウドはランキングがない
    category=="top" ~ "watch",
    category=="internet" ~ "iw",
    category=="pc" ~ "pcw",
    category=="dc" ~ "dcw",
    category=="akiba-pc" ~ "ah",
    category=="av" ~ "avw",
    category=="kaden" ~ "kdw",
    category=="k-tai" ~ "ktw"
  )
  
  if (category=="top"){
    url <- str_glue("https://watch.impress.co.jp/include/auto/{category2}/ranking/access_24_30.json")
  } else {
    url <- str_glue("https://{category}.watch.impress.co.jp/include/auto/{category2}/ranking/access_24_30.json")
  }
  
  res <- GET(url,user_agent(UA)) %>% 
    content() %>% 
    .$articles %>% 
    list.stack() %>% 
    select(title,url) %>% 
    mutate(title=str_remove_all(title," *<br> *")) %>% 
    mutate(source=str_glue("impress_{category}"),date=NA) %>% 
    relocate(source,date,title,url)
  return(res)
}
