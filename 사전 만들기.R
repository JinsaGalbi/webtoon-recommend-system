setwd("C:/Users/USER/Desktop/final")
library(tidyverse) ; library(magrittr)
library(tidytext)
library(data.table)
library(RcppMeCab)
library(fastrtext)
library(wordVectors)

# 각자 파트 나누기
webtoon <- fread('real5.csv')
pos1 <- fread('final_pos.csv')
pos1 %>% select(title) %>% distinct() -> webtoon_title
webtoon_title %<>%  unlist(use.names = F)
pos1 %>% filter(title %in% webtoon_title[1:90])-> set1
pos1 %>% filter(title %in% webtoon_title[91:180])-> set2
pos1 %>% filter(title %in% webtoon_title[181:280])-> set3

# fwrite(set1, 'set1.csv')
# fwrite(set2, 'set2.csv')
# fwrite(set3, 'set3.csv')

set1 <- fread('set1.csv')

full_sentence <- set1 %>% 
  group_by(i,title,user_id,date) %>% 
  summarise(text = paste(mor, collapse = ' '))
full_sentence %<>% arrange(title)

set1 %>%
  group_by(title, mor) %>% 
  summarise(n = n()) %>% 
  arrange(title,desc(n)) -> k1

k1 %>%
  filter(str_detect(mor, '/nnp|/nng|/xr'))-> k2

k2 %>% top_n(100, wt=n) -> k3

# load('11.26.RData')

## 사전 합치기
dic <- read.table('raw_userdic.txt',header = T, encoding = 'UTF-8')
colnames(dic) <- 'word'
dic %<>% separate_rows(word, sep = '\n') %>% mutate(word=trimws(word))
dic %<>% distinct()
write.table(dic, 'userdic_incomplete.txt',row.names = F,col.names = F,quote = F)

dic %>%slice(1:385) ->dic1
dic %>%slice(386:770) ->dic2

## 상강사전 만들기
# ,,,, 붙이기
di <- read.table('상강사전.txt',header = F)
di %<>% mutate(V1 = paste0(V1, ',,,,'))
write.table(di, '상강사전.txt',row.names = F,col.names = F,quote = F)

## 대체텍스트!
replace <- fread('with_spacing.csv',encoding = 'UTF-8')
replace %<>%
  mutate(text = str_replace_all(text,'ㆆ|ㆍ',''))
a <- c('사뢍', '마이쪙', '마이쪙', '따블', '따불',
       '견찰','뽀르쉐','레츠게릿', '레츠기릿', '렛츠게릿',
       '배댓','댕댕이','재밋','꾸르잼','쓰뤠기','비뻡',
       '쵝오','됌','봣','줫','쁘랙딱',
       '쁘랙닥','쁘렉닥','쓰래기','배지터',
       '소오름','콘댄싱','켑처','미춋','미춌',
       '쫀듸기','진쨔','짲응','즹말','즥이네',
       '줴엔장','줫다','죻','졋','힣',
       '갘','걌는데','걌엌','긍데','뀰잼',
       '넘모','꾾으면','꺆','텤','넵퓨',
       '넿','베뎃','배뎃','배댓','커여워',
       '기여워','킫밀리','줴발','쌋다',
       '쎅씨','쎅쒸','진짴','왤캐','쩰',
       '껒여','지이인짜','빅피쳐','놧다','아닡',
       '아닣','됑','됏','됫','됴음',
       '어딧','뚀륵','어뜩','미뗫','그랰',
       '믓찌','믓쪄','뽜이뜅','왭툰','젛아',
       '껍니다','훝|훔냐|훔냐링|훔냐릥|훔냐륑|훟|햫|퓻탕|큥|킄|컄|캵|캌|킼|')
b <- c('사랑', '맛있어', '맛있어', '더블', '더블',
       '경찰','포르쉐','렛츠기릿', '렛츠기릿', '렛츠기릿',
       '베댓','멍멍이','재밌','꿀잼','쓰레기','비법',
       '최고','됨','봤','줬','쁘렉딱',
       '쁘렉딱','쁘렉딱','쓰레기','베지터',
       '소름','콘덴싱','캡처','미쳤','미쳤',
       '쫀드기','진짜','짜증','정말','죽이네',
       '젠장','줬다','좋','졌','핳',
       '가','갔는데','갔엌','근데','꿀잼',
       '너무','끊으면','꺅','테','넵',
       '네','베댓','베댓','베댓','귀여워',
       '귀여워','키드밀리','제발','쌌다',
       '섹시','섹시','진짜','왤케','제일',
       '꺼져','진자','빅픽쳐','놨다','아니',
       '아니','됨','돼','됐','좋음',
       '어딨','또륵','어떡','미쳤','그래',
       '멋지','멋져','파이팅','웹툰','좋아',
       '겁니다','')
for(i in 1:85){
  replace$text <- str_replace_all(string = replace$text,pattern = a[86],replacement = b[86])
}
# fwrite(replace,'final_of_final.csv')

## 사용자 사전 추가 완료! 형태소 분석
replace <- fread('final_of_final.csv', encoding = 'UTF-8')
pos1 <- replace[600001:1200000,] %>%
  unnest_tokens(input=text, output=mor, token = pos)
# fwrite(pos1,'pos_sanggang.csv')

# hierarchical clustering
library(fastcluster)
library(parallelDist)
library(factoextra)
wv <- fread('wordvector.csv')
wv %<>% filter(str_detect(word, '/nnp|/nng|/va|/xr'))

## 거리행렬 크기가 너무 크다
# d <- parDist(u %>% as.matrix(), method = 'euclidean')
# h <- hclust(d,method="ward.D2") #클러스터링
# plot(h) #덴드로그램 확인
# plot(silhouette(cutree(h,k=7),dist=d,col=1:7)) # 실루엣값 확인
# fviz_nbclust(wv %>% select(-word), kmeans, method='silhouette')

# 실루엣값 구하기
library(cluster)
set.seed(101)
kmodel <- kmeans(wv[,-101],centers = 10, iter.max = 100)
ind_ss <- sample(1:nrow(wv), 10000)
sil <- silhouette(kmodel$cluster[ind_ss], dist(wv[,-101][ind_ss,])) 
fviz_silhouette(sil) -> a

# 실루엣값 비교 위해 할당
set.seed(101)
s <- c()
for(i in 3:30){
  kmodel <- kmeans(wv[,-101],centers = i, iter.max = 30)
  sil <- silhouette(kmodel$cluster[ind_ss], dist(wv[,-101][ind_ss,])) 
  fviz_silhouette(sil) -> a
  s[i-1] <- mean(a$data$sil_width)
}
## 덴드로그램 예쁘게
library(factoextra)
fviz_dend(h, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco",
          rect_fill = TRUE,show_labels=F)

# 
library('umap')
umapping <- function(data){
  data.umap <- data %>% umap
  umapdata <- data.umap$layout
  colnames(umapdata) <- c('a','b')
  umapdata %<>% as.data.frame()
  return(umapdata)
}
u <- umapping(wv[,-101])

# umap 클러스터링
u <- read.csv('umap.csv')
library(cluster)
set.seed(101)
ind_ss <- sample(1:nrow(u), 10000)

# 실루엣값 비교 위해 할당
s <- c()
for(i in 2:30){
  set.seed(101)
  kmodel <- kmeans(u,centers = i, iter.max = 30)
  sil <- silhouette(kmodel$cluster[ind_ss], dist(u[ind_ss,])) 
  fviz_silhouette(sil) -> a
  s[i-1] <- mean(a$data$sil_width)
}
c <- data.frame(x=1:30,y=c(0,s))
# write.csv(c,'silhouette.csv',row.names = F)
c %>% ggplot(aes(x=x, y=y))+
  geom_line(col = "#00C63B", size = 2)+
  geom_point(size = 4, col = "#00C63B")+
  theme_bw() # 클러스터 갯수는 8개

# 클러스터 k=6
set.seed(101)
kmodel <- kmeans(u[,-3], centers = 6, iter.max = 100) # k=6
u %<>% mutate(cluster = as.factor(kmodel$cluster))

# 클러스터 플롯
u %<>% mutate(word = wv$word)
u %>%
  filter(str_detect(word,'/nnp|/nng|/xr|/va')) %>%
  ggplot(aes(a,b,col=cluster))+geom_point()+theme_bw()

# 클러스터링된 워드 파일 저장
# write.csv(u,'cluster_umap.csv',row.names = F)

# 댓글에 클러스터 부여
u <- read.csv('cluster_umap.csv')
u %<>% select(-a,-b)
u %<>% rename('mor'=word)
# pos1 <- fread('pos_sanggang.csv', encoding = 'UTF-8')
# pos2 <- fread('pos_junghoon.csv', encoding = 'UTF-8')
# pos3 <- fread('pos_yunhye.csv', encoding = 'UTF-8')
fpos <- bind_rows(pos1,pos2,pos3)
fpos %<>% mutate(mor = as.factor(mor))
# fwrite(fpos,'pos_final.csv')

fpos %<>% 
  left_join(u, by='mor') # 클러스터 할당

fpos %<>% select(title,mor,cluster)

fpos %<>% filter(!str_detect(title,'이엑스피|돼지만화|
                            \\?|순정망화|킬더바디|위장불륜|지옥캠프')) #
fpos %>% distinct(title) %>% nrow() # 오늘 걸을까 삭제 안 됨
fpos %<>% filter(title!='오늘, 걸을까?')
# fwrite(fpos,'pos_final.csv') #다시 저장

fpos <- fread('pos_final.csv', encoding = 'UTF-8')
fpos %<>% mutate(cluster = as.factor(cluster))
g <- fpos %>%
  group_by(title, cluster) %>% 
  summarise(N = n())
g %<>% ungroup()
g %<>% na.omit()

g2 <- g %>% spread(key='cluster',value='N')
colnames(g2) <- c('title',paste0('c',colnames(g2)[2:7]))
write.csv(g2, 'cluster_score.csv', row.names = F)

g2 %<>% mutate(total = c1+c2+c3+c4+c5+c6)
# write.csv(g2, 'score.csv',row.names = F)
g2 <- read.csv('score.csv')
g3 <- g2 %>%
  group_by(title) %>% 
  summarise(c1 = c1/total, c2 = c2/total, c3 = c3/total, c4 = c4/total, c5 = c5/total, c6 = c6/total)
g3 %<>% as.matrix()
g3 <- g3[,-1]
rownames(g3) <- unlist(g2[,1],use.names = F)

# 유사행렬 구하기
library(proxy)
euclid <- as.matrix(dist(g3, method = "euclidean"))
cosine <- as.matrix(dist(g3, method = "cosine"))
minkowski <- as.matrix(dist(g3, method = "minkowski"))
manhattan <- as.matrix(dist(g3, method = "manhattan"))
gower <- as.matrix(dist(g3, method = "gower"))
rm(g2,g3)
save.image()

# write.csv(euclid, 'euclid.csv')
# write.csv(cosine, 'cosine.csv')
# write.csv(minkowski, 'minkowski.csv')
# write.csv(manhattan, 'manhattan.csv')
# write.csv(gower, 'gower.csv')


# 클러스터 점수 분포 시각화
u %<>% select(-a,-b)
u %<>% filter(str_detect(word,'/nnp|/nng|/va|/xr'))
u1 <- u %>% filter(cluster ==1)
u2 <- u %>% filter(cluster ==2)
u3 <- u %>% filter(cluster ==3)
u4 <- u %>% filter(cluster ==4)
u5 <- u %>% filter(cluster ==5)
u6 <- u %>% filter(cluster ==6)

# 색깔
rgb(245,100,227,maxColorValue = '255') # 분홍색
rgb(0,186,56,maxColorValue = '255') # 초록색
rgb(183,159,0,maxColorValue = '255') # 갈색
rgb(97,156,255,maxColorValue = '255') # 파랑색
rgb(248,118,109,maxColorValue = '255') # 주황색
rgb(0,191,196,maxColorValue = '255') # 옥색

g3 %>% ggplot(aes(x=c1))+geom_histogram(binwidth = 0.01,fill="#F8766D",col='white')+theme_classic()
g3 %>% ggplot(aes(x=c2))+geom_histogram(binwidth = 0.01,fill="#B79F00",col='white')+theme_classic()
g3 %>% ggplot(aes(x=c3))+geom_histogram(binwidth = 0.01,fill="#00BA38",col='white')+theme_classic()
g3 %>% ggplot(aes(x=c4))+geom_histogram(binwidth = 0.003,fill="#00BFC4",col='white')+theme_classic()
g3 %>% ggplot(aes(x=c5))+geom_histogram(binwidth = 0.01,fill="#619CFF",col='white')+theme_classic()
g3 %>% ggplot(aes(x=c6))+geom_histogram(binwidth = 0.01,fill="#F564E3",col='white')+theme_classic()
