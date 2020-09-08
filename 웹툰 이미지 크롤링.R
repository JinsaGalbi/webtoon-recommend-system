library(rvest)
library(tidyverse) ; library(magrittr)

# 요일별 웹툰 링크 크롤링
url <- 'https://comic.naver.com/webtoon/weekdayList.nhn?week='
day <- c('mon','tue','wed','thu','fri','sat','sun')
comic_url <- c() ; comic_title <- c()
for(i in day){
  webtoon_url <- paste0(url,i)
  html <- read_html(webtoon_url)
  temp <- html_nodes(html,'#wrap')%>%
    html_nodes('#container')%>%
    html_nodes('#content')%>%
    html_nodes(css='.list_area.daily_img')%>%
    html_nodes(css='.img_list')%>%
    html_nodes('a')%>%
    html_attr('href')%>%unique()
  comic_url <- c(comic_url,temp)
  temp1 <- html_nodes(html,'#wrap')%>%
    html_nodes('#container')%>%
    html_nodes('#content')%>%
    html_nodes(css='.list_area.daily_img')%>%
    html_nodes(css='.img_list')%>%
    html_nodes('a')%>%
    html_attr('title') %>% unique()
  comic_title <- c(comic_title,temp1)
}
comic <- data.frame(url=comic_url,title=comic_title)
comic %<>% na.omit
comic %<>% distinct(title)

# 웹툰별 첫페이지의 10개 링크 크롤링
comic_url2 <- data.frame(title=NULL, url2 = NULL)
for(j in 1: length(comic$url)){
  html2 <- read_html(paste0('http://comic.naver.com',comic$url[j]))
  temp2 <- html_nodes(html2,'#wrap')%>%
    html_nodes('#container')%>%
    html_nodes('#content.webtoon')%>%
    html_nodes(css='.viewList')%>%
    html_nodes(css = '.title')%>%
    html_nodes('a')%>%
    html_attr('href')%>%unique()
  a <- data.frame(title = rep(comic$title[j], length(temp2)), url2 = temp2)
  comic_url2 <- rbind(comic_url2, a)
}

# 웹툰 이미지 링크 크롤링
comic2 <- data.frame(title = NULL, url3 = NULL)
for(k in 1: length(comic_url2[,2])){
  html3 <- read_html(paste0('http://comic.naver.com',comic_url2$url2[k]))
  temp3 <- html_nodes(html3, '#wrap') %>% 
    html_nodes('#container')%>%
    html_nodes('#content.webtoon')%>%
    html_nodes('.section_cont.wide') %>% 
    html_nodes('.view_area') %>% 
    html_nodes('.wt_viewer') %>% 
    html_nodes('img') %>% 
    html_attr('src') %>% unique()
  temp3 <- temp3[-1];temp3 <- temp3[-length(temp3)]
  b <- data.frame(title = rep(comic_url2$title[k], length(temp3)), url3 = temp3)
  comic2 <- rbind(comic2,b)
}
comic2 %<>% group_by(title) %>%
  distinct(url3)%>% 
  mutate(number = 1:n()) %>%
  ungroup() %>% select(title, url3, number)
comic2 %<>% mutate(title = gsub(":"," ",title))
comic2 %<>% mutate(title = recode(title, '오늘, 걸을까?'='오늘 걸을까'))
comic2 %<>% mutate(title = str_replace_all(title , '\\?|\\!|\\^|\\@|\\#|\\$|\\%|\\*|<|>|\\+|\|'))


# 웹툰 이미지 크롤링
library(httr)
setwd('c:/Users/USER/Desktop/이미지3')
ref <- 'http://comic.naver.com'
for(l in 36098:length(comic2$url3)){
  GET(as.character(comic2$url3[l]), add_headers(Referer = ref),
      write_disk(paste0(as.character(comic2$title[l]),
                        comic2$number[l],'.png')))
}


# 웹툰 섬네일 이미지 크롤링
## 섬네일 링크 크롤링
t <- c()
for(i in 1:nrow(comic)){
  html <- read_html(paste0('http://comic.naver.com',comic$url[i]))
  temp <- html_nodes(html, css='.thumb') %>% 
    html_nodes('img') %>% 
    html_attr('src') %>% unique()
  t <- c(t, temp)
}
a <- data.frame(title = comic$title, url = t)

# 섬네일 이미지 다운로드
library(httr)
setwd('c:/Users/USER/Desktop/썸네일2')
ref <- 'http://comic.naver.com'
a %<>% mutate(title = str_replace_all(title, '\\?',''))
for(l in 1:length(a$url)){
  GET(as.character(a$url[l]), add_headers(Referer = ref),
      write_disk(paste0(as.character(a$title[l]),'.png'),overwrite = T))
}
