library(tidyverse)
library(lubridate)
library(rvest)
library(httr)
library(rlist)
library(jsonlite)
library(DT)
library(Hmisc)


walk(list.files("./R",pattern=".R$",recursive=TRUE,full.names=TRUE),source,encoding="UTF-8")

ITmedia <- get_ITmedia_articlelist(USER_AGENT,SLEEP_TIME)
ITmedia_news <- map_dfr(ITMEDIA_YYMM,~possibly(get_ITmedia_news_articlelist,NULL)(.x,USER_AGENT,SLEEP_TIME))
ITmedia_pcuser <- map_dfr(ITMEDIA_YYMM,~possibly(get_ITmedia_pcuser_articlelist,NULL)(.x,USER_AGENT,SLEEP_TIME))
ITmedia_enterprise <- map_dfr(ITMEDIA_YYMM,~possibly(get_ITmedia_enterprise_articlelist,NULL)(.x,USER_AGENT,SLEEP_TIME))
ITmedia_mobile <- map_dfr(ITMEDIA_YYMM,~possibly(get_ITmedia_mobile_articlelist,NULL)(.x,USER_AGENT,SLEEP_TIME))
ITmedia_business <- map_dfr(ITMEDIA_YYMM,~possibly(get_ITmedia_business_articlelist,NULL)(.x,USER_AGENT,SLEEP_TIME))
atmarkit <- map_dfr(ITMEDIA_YYMM,~possibly(get_atmarkit_articlelist,NULL)(.x,USER_AGENT,SLEEP_TIME))
netlabo <- map_dfr(ITMEDIA_YYMM,~possibly(get_netlabo_articlelist,NULL)(.x,USER_AGENT,SLEEP_TIME))

nikkei_xtech <- map_dfr(1:NIKKEI_XTECH_PAGES,~get_nikkei_xtech_articlelist(.x,USER_AGENT,SLEEP_TIME))

ascii <- map_dfr(ASCII_YYYYMM,~possibly(get_ascii_articlelist,NULL)("top",.x,USER_AGENT,SLEEP_TIME))
ascii_ai <- map_dfr(ASCII_YYYYMM,~possibly(get_ascii_articlelist,NULL)("ai",.x,USER_AGENT,SLEEP_TIME))
ascii_biz <- map_dfr(ASCII_YYYYMM,~possibly(get_ascii_articlelist,NULL)("biz",.x,USER_AGENT,SLEEP_TIME))
ascii_tech <- map_dfr(ASCII_YYYYMM,~possibly(get_ascii_articlelist,NULL)("tech",.x,USER_AGENT,SLEEP_TIME))
ascii_akiba <- map_dfr(ASCII_YYYYMM,~possibly(get_ascii_articlelist,NULL)("akiba",.x,USER_AGENT,SLEEP_TIME))

impress <- map_dfr(IMPRESS_YYYYMM,~possibly(get_impress_articlelist,NULL)("top",.x,USER_AGENT,SLEEP_TIME))
impress_internet <- map_dfr(IMPRESS_YYYYMM,~possibly(get_impress_articlelist,NULL)("internet",.x,USER_AGENT,SLEEP_TIME))
impress_pc <- map_dfr(IMPRESS_YYYYMM,~possibly(get_impress_articlelist,NULL)("pc",.x,USER_AGENT,SLEEP_TIME))
impress_akiba <- map_dfr(IMPRESS_YYYYMM,~possibly(get_impress_articlelist,NULL)("akiba-pc",.x,USER_AGENT,SLEEP_TIME))
impress_ktai <- map_dfr(IMPRESS_YYYYMM,~possibly(get_impress_articlelist,NULL)("k-tai",.x,USER_AGENT,SLEEP_TIME))
impress_cloud <- map_dfr(IMPRESS_YYYYMM,~possibly(get_impress_articlelist,NULL)("cloud",.x,USER_AGENT,SLEEP_TIME))
impress_hobby <- map_dfr(IMPRESS_YYYYMM,~possibly(get_impress_articlelist,NULL)("hobby",.x,USER_AGENT,SLEEP_TIME))

data <- Hmisc::llist(
  ITmedia,ITmedia_news,ITmedia_pcuser,ITmedia_enterprise,ITmedia_mobile,ITmedia_business,atmarkit,netlabo,
  nikkei_xtech,
  ascii,ascii_ai,ascii_biz,ascii_tech,ascii_akiba,
  impress,impress_internet,impress_pc,impress_akiba,impress_ktai,impress_cloud,impress_hobby
) %>% 
  map(~{
    .x %>% 
      mutate(date=as.character(date,format="%m/%d"))
  }) %>% 
  map(~make_table_for_report(.x,"DT"))
