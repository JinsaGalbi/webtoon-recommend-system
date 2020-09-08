setwd('c:/Users/USER/Desktop/주제분석/웹툰크롤링')
library(rvest);library(tidyverse);library(magrittr)
url <- 'https://comic.naver.com/webtoon/weekdayList.nhn?week='
day <- c('mon','tue','wed','thu','fri','sat','sun')

# 제목, 저자, 평점, 요일 크롤링
title <- c(); author <- c();rating <- c();url2 <- c();weekday <- c()
for(i in 1:7){
  html1 <- read_html(paste0(url, day[i]))
  temp1 <- html_nodes(html1, '#wrap') %>% 
    html_nodes(css='.img_list') %>% 
    html_nodes('a') %>% 
    html_attr('title') %>%
    unique() %>% na.omit()
  temp2 <- html_nodes(html1, '#wrap') %>% 
    html_nodes(css='.img_list') %>% 
    html_nodes(css='.desc') %>% 
    html_text() %>% unique() %>% 
    str_replace_all('\n|\t|\r| ','')
  temp3 <- html_nodes(html1, '#wrap') %>% 
    html_nodes(css='.rating_type') %>% 
    html_nodes('strong') %>% 
    html_text()
  temp4 <- html_nodes(html1, '#wrap') %>% 
    html_nodes(css='.img_list') %>% 
    html_nodes('a') %>% 
    html_attr('href') %>%
    unique()
  title <- c(title, temp1)
  author <- c(author, temp2)
  rating <- c(rating, temp3)
  url2 <- c(url2,temp4[temp4!='#'])
  weekday <- c(weekday, rep(day[i], length(temp1)))
}
url2 %<>% str_replace_all('&weekday=|mon|tue|wed|thu|fri|sat|sun','')

# 장르,스토리 크롤링
genre <- c() ; story <- c()
for(j in 1:length(url2)){
  html2 <- read_html(paste0('http://comic.naver.com',url2[j]))
  temp5 <- html_nodes(html2, '#wrap') %>% 
    html_nodes(css='.genre') %>% 
    html_text() %>% unique
  temp6 <- html_nodes(html2, '#wrap') %>% 
    html_nodes(css='.detail') %>% 
    html_nodes('p') %>% 
    html_text() %>% first()
  genre <- c(genre, temp5)
  story <- c(story, temp6)
}

# 데이터 저장
webtoon <- data.frame(title=title, author=author,rating=rating, genre=genre, story = story, url=url2) %>% 
  distinct()
# 요일 어떻게 합칠지 생각
# write.csv(webtoon, 'webtoon.csv', row.names = F)
webtoon <- read.csv('webtoon.csv')
webtoon %>% ggplot(aes(x=rating,y=..count..))+geom_histogram(binwidth=0.1)
webtoon %>% group_by(title,author,rating,genre,story,url) %>% summarise(weekday=paste0(weekday))



# 댓글 크롤링 재료 준비
comment_url <- data.frame(title=NULL, url = NULL)
for(k in 1:length(webtoon$url)){
  html3 <- read_html(paste0('http://comic.naver.com',webtoon$url[k]))
  temp7 <- html_nodes(html3,'#wrap') %>%
    html_nodes(css = '.title') %>%
    html_nodes('a') %>%
    html_attr('href') %>% 
    str_replace_all('webtoon/detail','comment/comment') %>% 
    str_replace_all('&weekday=|mon|tue|wed|thu|fri|sat|sun','') %>% unique() 
  a <- data.frame(title = rep(webtoon$title[k], length(temp7)), url = temp7)
  comment_url <- rbind(comment_url, a)
}
comment_url %>% group_by(title) %>% summarise(N=n()) %>% View() # 회차 몇 개씩 크롤링 되었는지 확인 -> 10개 미만인 것 39개;;
# write.csv(comment_url, 'comment_url.csv', row.names = F)