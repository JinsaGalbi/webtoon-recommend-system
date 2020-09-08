## 댓글 크롤링
setwd('c:/Users/USER/Desktop/주제분석/웹툰크롤링')
comment_url <- read.csv('comment_url.csv')
library(rvest);library(tidyverse);library(magrittr)
library(RSelenium)
rD <- rsDriver(port = 4445L,browser = 'chrome',chromever = '78.0.3904.70')
remDr <- rD[["client"]]

# comment_final <- data.frame(nickname=NULL,id=NULL,recommend=NULL,unrecommend=NULL,text=NULL,i=NULL)
for(i in 127:length(comment_url$url)){
  nickname <- c();id <- c();recommend <- c();unrecommend <- c();date <- c();text <- c()
  remDr$navigate(paste0('http://comic.naver.com',comment_url$url[i]))
  webElem <- remDr$findElements(using = 'xpath',value = '//*[@id="cbox_module"]/div/div[4]/div[1]/div/ul/li[2]/a') #전체댓글 표시하기  
  remDr$mouseMoveToLocation(webElement = webElem[[1]]) #마우스 이동
  remDr$click() #버튼 클릭
  Sys.sleep(0.2) 
  
  frontPage <- remDr$getPageSource() #페이지 전체 소스 가져오기
  nickname_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_nick') %>%  html_text() %>% trimws()
  id_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_id') %>%  html_text() %>% trimws()
  recommend_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_cnt_recomm') %>%  html_text() %>% trimws()
  unrecommend_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_cnt_unrecomm') %>%  html_text() %>% trimws()
  date_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_date') %>%  html_text() %>% trimws()
  text_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_text_wrap') %>% html_text() %>% trimws()
  
  nickname <- c(nickname, nickname_temp)
  id <- c(id, id_temp)
  recommend <- c(recommend, recommend_temp)  
  unrecommend <- c(unrecommend, unrecommend_temp)
  date <- c(date, date_temp)
  text <- c(text, text_temp)  # 여기까지 첫페이지
  
  page_flag <- read_html(frontPage[[1]]) %>%
    html_nodes(css='.u_cbox_next') %>%
    html_attr('href') %>% first()
  
  if(is.na(page_flag)){ # 댓글에 다음페이지가 없는 경우 = 페이지가 1~10까지만 있는 경우
    for(j in c(1,3:10)){
      webElem <- remDr$findElements(using = 'xpath',value = paste0('//*[@id="cbox_module"]/div/div[7]/div/a[',j,']'))
      remDr$mouseMoveToLocation(webElement = webElem[[1]])
      remDr$click() #버튼 클릭
      Sys.sleep(0.2) 
      
      frontPage <- remDr$getPageSource() #페이지 전체 소스 가져오기
      nickname_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_nick') %>%  html_text() %>% trimws()
      id_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_id') %>%  html_text() %>% trimws()
      recommend_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_cnt_recomm') %>%  html_text() %>% trimws()
      unrecommend_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_cnt_unrecomm') %>%  html_text() %>% trimws()
      date_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_date') %>%  html_text() %>% trimws()
      text_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_text_wrap') %>% html_text() %>% trimws()
      
      nickname <- c(nickname, nickname_temp)
      id <- c(id, id_temp)
      recommend <- c(recommend, recommend_temp)  
      unrecommend <- c(unrecommend, unrecommend_temp)
      date <- c(date, date_temp)
      text <- c(text, text_temp)  # 여기까지 2~11페이지
    }
  }else{ # 댓글에 다음페이지가 있는 경우 = 페이지가 11장 이상
    for(k in c(1,3:11)){ 
      webElem <- remDr$findElements(using = 'xpath',value = paste0('//*[@id="cbox_module"]/div/div[7]/div/a[',k,']'))
      remDr$mouseMoveToLocation(webElement = webElem[[1]])
      remDr$click() #버튼 클릭
      Sys.sleep(0.2) 
      
      frontPage <- remDr$getPageSource() #페이지 전체 소스 가져오기
      nickname_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_nick') %>%  html_text() %>% trimws()
      id_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_id') %>%  html_text() %>% trimws()
      recommend_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_cnt_recomm') %>%  html_text() %>% trimws()
      unrecommend_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_cnt_unrecomm') %>%  html_text() %>% trimws()
      date_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_date') %>%  html_text() %>% trimws()
      text_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_text_wrap') %>% html_text() %>% trimws()
      
      nickname <- c(nickname, nickname_temp)
      id <- c(id, id_temp)
      recommend <- c(recommend, recommend_temp)  
      unrecommend <- c(unrecommend, unrecommend_temp)
      date <- c(date, date_temp) 
      text <- c(text, text_temp) # 여기까지 2~11페이지
    }
    for(l in rep(3:12,10000)){
      webElem <- remDr$findElements(using = 'xpath',value = paste0('//*[@id="cbox_module"]/div/div[7]/div/a[',l,']'))
      if(length(webElem)>=1){
        remDr$mouseMoveToLocation(webElement = webElem[[1]])
        remDr$click() #버튼 클릭
        Sys.sleep(0.2) 
        
        frontPage <- remDr$getPageSource() #페이지 전체 소스 가져오기
        nickname_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_nick') %>%  html_text() %>% trimws()
        id_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_id') %>%  html_text() %>% trimws()
        recommend_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_cnt_recomm') %>%  html_text() %>% trimws()
        unrecommend_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_cnt_unrecomm') %>%  html_text() %>% trimws()
        date_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_date') %>%  html_text() %>% trimws()
        text_temp <- read_html(frontPage[[1]]) %>% html_nodes('.u_cbox_text_wrap') %>% html_text() %>% trimws()
        
        nickname <- c(nickname, nickname_temp)
        id <- c(id, id_temp)
        recommend <- c(recommend, recommend_temp)  
        unrecommend <- c(unrecommend, unrecommend_temp)
        date <- c(date, date_temp)
        text <- c(text, text_temp)
      }else{break}}
  }
  instant <- data.frame(i=i,title=rep(comment_url$title[i],length(id)),nickname=nickname,id=id,text=text) %>% filter(text!='클린봇이 이용자 보호를 위해 숨긴 댓글입니다.') %>% bind_cols(data.frame(recommend=recommend,unrecommend=unrecommend))
  comment_final %<>% bind_rows(instant)
}
# write.csv(comment_final,'comment1.csv', row.names = F)
# write.csv(comment_final,'comment2.csv', row.names = F)
# write.csv(comment_final,'comment3.csv', row.names = F)
# write.csv(comment_final,'comment4.csv', row.names = F)
# write.csv(comment_final,'comment5.csv', row.names = F)
# write.csv(comment_final,'comment6.csv', row.names = F)
# write.csv(comment_final,'comment7.csv', row.names = F)
# write.csv(comment_final,'comment8.csv', row.names = F)
# write.csv(comment_final,'comment9.csv', row.names = F)
