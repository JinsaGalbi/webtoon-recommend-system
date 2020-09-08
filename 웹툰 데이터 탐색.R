# 웹툰 데이터 탐색 및 전처리
## 인사이트 : 기술된 장르만ㅇ르
setwd('c:/Users/USER/Desktop/final')
library(tidyverse);library(magrittr)
library(KoNLP)
webtoon <- read.csv('webtoon.csv')
webtoon_weekday <- read.csv('webtoon_weekday.csv')
webtoon %<>% select(-url) # url 삭제
webtoon %<>% separate(genre, sep=', ', into = paste0('genre',1:7)) # 한 웹툰에 장르 최대 7개 까지 있음 -> 분리!
webtoon %<>% mutate(num_genre = rowSums(ifelse(is.na(webtoon %>% select(genre1:genre7)), 0,1))) # 장르 갯수 변수 만들기
webtoon %>% ggplot(aes(num_genre))+geom_bar(fill = '#58FAAC')+theme_classic() # 장르 갯수 분포 시각화
long_webtoon <- webtoon %>% gather(key='a',value='genre',genre1:genre7,na.rm = T) %>% select(-a)
long_webtoon %>% ggplot(aes(genre, fill=genre))+geom_bar()+theme_classic()
long_webtoon %<>% group_by(title) %>% arrange(title)

webtoon_weekday %<>% mutate(weekday = recode(weekday, 'mon'='월','tue'='화','wed'='수','thu'='목','fri'='금','sat'='토','sun'='일'))
webtoon_weekday %>% ggplot(aes(weekday,fill=weekday))+
  geom_bar()+scale_x_discrete(label = c('월','화','수','목','금','토','일'))+
  theme(legend.position = 'none')+theme_classic() # 요일별 웹툰 분포


## 장르 대분류 소분류 나누기
webtoon <- read.csv('webtoon.csv')
genre_list<- long_webtoon %>% ungroup %>%  distinct(genre) %>% as.matrix() %>% as.vector()
for(i in 1:length(genre_list)){
  assign(paste0(genre_list[i], ''), ifelse(str_detect(webtoon$genre, genre_list[i]), genre_list[i], NA))
}
webtoon %<>% cbind(스토리,스릴러,액션,드라마,에피소드,일상,개그,판타지,로맨스,감성,시대극,옴니버스,스포츠)
webtoon %<>% mutate_if(is.factor, as.character)
webtoon %<>% unite(col = 'genre', c(옴니버스,에피소드,스토리),sep = '',na.rm = T)
webtoon %>% ggplot(aes(genre, fill=genre))+geom_bar()+theme_classic() # 큰 장르 그리기

long_webtoon <- webtoon %>% gather(key='a',value='sub_genre',스릴러,액션:스포츠,na.rm = T) %>% select(-a)
long_webtoon %>% ggplot(aes(sub_genre, fill=sub_genre))+geom_bar()+theme_classic() # 작은 장르그리기
##



##모든 데이터 한 번에 합치기
# real1 <- fread('real_final.csv')
# real2 <- fread('final12.csv')
# real3 <- fread('final22.csv')
# real<- real1 %>% bind_rows(real2,real3)
# real %<>% distinct()
# real %<>% mutate(text = iconv(text, to ='UTF-8'))
# write.csv(real, 'real.csv', row.names = F)

webtoon <- data.table::fread('real.csv')
webtoon %<>%
  mutate(text = iconv(text, to = 'UTF-8')) %>%  # 텍스트 인코딩
  unite(col=user_id,nickname,id,sep = '') %>%  # 유저 식별변수 만들기
  distinct() %>%   # 중복되는 것 없애기
  mutate(text = str_replace_all(text,' ','')) %>% # 띄어쓰기 모두 없애기
  mutate(score = recommend-unrecommend) %>% # 공강-비공감 점수 만들기
  filter(!str_detect(text, '<U+')) %>%  # 문자표 들어간 것 없애기
  filter(Encoding(unlist(text))=='UTF-8') # 인코딩이 utf8인 것만 남기기
# write.csv(real, 'real.csv', row.names = F)
# 댓글 데이터 탐색
## 1인당 댓글 수의 분포
s1<- webtoon %>% group_by(user_id) %>%  summarise(N=n()) %>% ungroup()
quantile(s1$N) 
s1 %>% ggplot(aes(N))+geom_histogram(fill='#A9F5D0')+theme_classic()
table(s1$N)
## 1인당 본 웹툰 수의 분포
s2<- webtoon %>% group_by(user_id) %>%  distinct(title) %>% summarise(n = n())
s2 %>% ggplot(aes(N))+geom_histogram(fill='#33CC00',color='white')+theme_classic()
## 웹툰 하나당 댓글 수 분포
s3 <- webtoon %>%
  group_by(title, i) %>% 
  summarise(n = n())
s3 %<>%
  summarise(average = mean(n)) 
s3 %>% ggplot(aes(average))+geom_histogram(fill='#33CC00',color='white')+theme_classic()+coord_cartesian(xlim = seq(0,5000,1000))
  

##
library(KoSpacing)
set_env()
sp <- webtoon %>% filter(title == '신의 탑'|title == '백수세끼'|title == '닥터 프로스트 시즌 3~4')
sp %<>% mutate(text = spacing(text))
a <- sp %>% mutate(text = unlist(text,use.names = F)) %>%  mutate(text = iconv(text, to = 'UTF-8'))
data.table::fwrite(a,)
# write.csv(a, 'spacing.csv', row.names = F)

sp <- data.table::fread('spacing.csv')
q <- sp %>% select(i,title,text) %>% 
  group_by(i, title) %>% 
  summarise(text= paste(text, collapse = ' '))
# write.csv(q, 'tf-idf.csv',row.names = F)
library(tm)
library(tidytext)
library(RcppMeCab)
library(KoNLP)
pos1 <- q %>% unnest_tokens(input=text,output=mor,token = SimplePos09)
k <- pos1 %>% group_by(title, mor) %>% 
  summarise(n =n())
a <- separate_rows(data = pos1,mor, sep = "\\+")
b<- a %>% 
  filter(str_detect(mor,'/n|/p')) %>% 
  mutate(mor = str_replace_all(mor, '[ㄱ-ㅋ]|[ㅏ-ㅣ]',''))
c <- b %>% mutate(mor= str_replace_all(mor,'\\^|\\?|\\!|\\.|/n|/p',''))
c %>% filter(mor!='') -> d
d %>% group_by(title, mor,i) %>% summarise(n = n()) -> m
m %>% arrange(desc(n)) %>% View()
d %<>% filter(nchar(mor)>=2)
d %<>% group_by(i, title) %>% summarise(text = paste(mor, collapse = ' '))

write.csv(d,'신의탑 tf-idf.csv',row.names = F)
k %>%
  filter(str_detect(mor, '')) %>% 
  arrange(desc(n)) %>% View()


pos1 %>% filter(str_replace_all(mor, ''))
vq <- as.VCorpus(VectorSource(q$text))
