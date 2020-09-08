library(umap)
library(tidyverse)
library(data.table)
library(magrittr)
library(philentropy)
library(cluster)
library(doBy)
library(KoNLP)
library(lsa)
library(proxy)
level <- list.files('C:/Users/USER/Desktop/이미지')
level <- str_replace_all(level,'.png','')
title <- fread('title.csv',header = T)
##function

mat_ord2 <- function(mx) mx[sort(rownames(mx)), c(sort(rownames(mx)), sort(setdiff(colnames(mx), rownames(mx))))]

#file 불러와서 하나로 만들기
filemerge <- function(filename,n=10){
  data3<-NULL
  for(i in 1:n){
    X <- fread(paste(filename,' (',i,').csv',sep=''),encoding='UTF-8')
    y <- fread(paste(filename,'label (',i,').csv',sep=''),encoding='UTF-8')
    y %<>% mutate(name=V2) %>% select(name)
    y %<>% filter(name!=0)
    X %<>% select(-V1) 
    X %<>% filter(V2!=0)
    data <- X %>% cbind(y)
    data <- unique(data)
    data3<- rbind(data3,data)
    data3 <- unique(data3)
  }
  
  data3 %<>% mutate(name = str_replace_all(name,'/gdrive/My Drive/왜죠/2/|/gdrive/My Drive/왜죠/1/|.png',''))
  data3 %<>% mutate(name=as.factor(name))
  levels(data3$name) <- str_replace_all(level,'.png','')
  data3 %<>% filter(!(name %in% setdiff(level,title$title)))
  rownames(data3) <- data3$name
  data3 %<>% select(-name)
  return(data3)
}

#umap data 반환 함수
umapping <- function(data){
  data.umap <- data %>% umap
  umapdata <- data.umap$layout
  colnames(umapdata) <- c('a','b')
  umapdata %<>% as.data.frame()
  return(umapdata)
}

#유사도 행렬 반환 함수
simmatrix <- function(data, method){
  if(method %in% c('euclidean','manhattan','gower')){
    dmatrix <- daisy(data, metric = method, stand = FALSE)}
  if(method == 'cosine'){
    dmatrix <- cosine(t(data))
  }
  if(method == 'minkowski'){
    dmatrix <- distance(data,method='minkowski',p=3)
  }
  dmatrix <- as.matrix(dmatrix)
  return(dmatrix)
}

#recommendation 함수 dmatrix : 유사도행렬, name : 웹툰이름

recommendation <- function(data,dmatrix,name,method){
  num <- which(rownames(data) %in% name)
  distance <- dmatrix[num,]
  if(length(name)>1){
    totaldist <- apply(distance,2,mean)
    totaldist <- totaldist[-num]
  }
  else{totaldist <- distance}
  
  #change min or max by method
  if(method %in% c('euclidean','manhattan','gower','minkowski')){
    similarnum <- which.minn(totaldist,n=11)
  }
  if(method == 'cosine'){
    similarnum <- which.maxn(totaldist,n=11)
  }
  webtoon <- rownames(data)[similarnum[-1]]
  similarity <- totaldist[similarnum[-1]]
  print(paste('당신이 입력한 웹툰과의 유사도'))
  for(i in 1:length(webtoon)){
    print(paste(webtoon[i],':',similarity[i]))
  }
}

#recommendation total

Webtoonrecommend <- function(data,imgmatrix,name,method,rate=0.5){
  imgmatrix <- mat_ord2(imgmatrix)
  texmatrix <- fread(paste(method,'.csv',sep=''))
  rownames(texmatrix) <- texmatrix$V1
  texmatrix %<>% select(-V1)
  if(method %in% c('euclidean','manhattan','gower','minkowski')){
    imgmat <- 1/(1+imgmatrix)
    texmat <- 1/(1+texmatrix)
  }
  else{
    imgmat <- imgmatrix
    texmat <- texmatrix
  }
  dmatrix <- rate*imgmat + (1-rate)*texmat
  num <- which(colnames(dmatrix) %in% name)
  distance <- dmatrix[num,]
  if(length(name)>1){
    totaldist <- as.data.frame(t(apply(distance,2,mean)))
    a <- colnames(totaldist)[num]
    for(k in a){
      totaldist %<>% select(-k)
    }
  }
  else{
    totaldist <- distance
    a <- colnames(totaldist)[num]
    totaldist %<>% select(-a)
  }
  totaldist %<>% as.data.frame()
  similarnum <- which.maxn(totaldist,n=11)
  webtoon <- colnames(totaldist)[similarnum]
  similarity <- totaldist[,similarnum]
  print(paste('당신이 입력한 웹툰과의 유사도'))
  for(i in 1:length(webtoon)){
    print(paste(webtoon[i],':',similarity[,i]))
  }
}

wow <- fread('wow (10).csv',encoding='UTF-8')
label <- fread('label (11).csv',encoding='UTF-8')

label %<>% mutate(name=V2) %>% select(name)
label %<>% filter(name!=0)
wow %<>% select(-V1) 
wow %<>% filter(V2!=0)
data <- label %>% cbind(wow)
data <- unique(data)

data2 <- rbind(data2,data)
data2 <- unique(data2)
data.umap <- data2[,-1] %>% umap

umapdata <- data.umap$layout
colnames(umapdata) <- c('a','b')
umapdata %<>% as.data.frame() %>% cbind(data2$name)

data2 %<>% mutate(name = str_replace_all(names,'/gdrive/My Drive/왜죠/2/|/gdrive/My Drive/왜죠/1/|.png',''))

kmean <- umapdata %>% select(a,b) %>% kmeans(centers=10)

umapdata %<>% mutate(kmean= kmean$cluster)
umapdata$kmean %<>% as.factor()

umapdata %>% ggplot(aes(a,b,col=kmean)) +geom_point()

umapdata %>% filter(kmean==10)

rownames(data2) <- data2$name
data2 %<>% select(-name)

#vgg model

vggdata <- filemerge('vgg',n=8)

vggumap <- umapping(vggdata)

vggumap %>% ggplot(aes(a,b))+geom_point()

cluster<-vggumap %>% select(a,b) %>% kmeans(centers=10)

vggumap %>% mutate(cluster=cluster$cluster) %>% ggplot(aes(a,b,col=as.factor(cluster)))+geom_point()+theme_classic()+theme(legend.position = 'None',axis.title.x = element_blank(),axis.title.y=element_blank())

vggumap[cluster$cluster==3,]

vggsim <- simmatrix(vggdata,'euclidean')
vggsim2 <- simmatrix(vggdata,'manhattan')
vggsim3 <- simmatrix(vggumap,'cosine')
vggsim4 <- simmatrix(vggdata,'gower')
vggsim5 <- simmatrix(vggdata,'minkowski')

recommendation(vggdata,vggsim,'개장수','euclidean')
recommendation(vggdata,vggsim2,'개장수','manhattan')
recommendation(vggdata,vggsim3,'개장수','cosine')
recommendation(vggdata,vggsim4,'개장수','gower')
recommendation(vggdata,vggsim5,'개장수','minkowski')

vggsim4 <- simmatrix(vggdata,'gower')

#resnet model

resnetdata <- filemerge('resnet',n=8)

resumap <- umapping(resnetdata)
resumap %>% ggplot(aes(a,b))+geom_point()

cluster<-resumap %>% select(a,b) %>% kmeans(centers=10)

resumap %>% mutate(cluster=cluster$cluster) %>% ggplot(aes(a,b,col=as.factor(cluster)))+geom_point()+theme_classic()+theme(legend.position = 'None',axis.title.x = element_blank(),axis.title.y=element_blank())

resumap[which(resumap$a<=-2.5),]
ressim <- simmatrix(resnetdata,'euclidean')
ressim2 <- simmatrix(resnetdata,'manhattan')
ressim3 <- simmatrix(resnetdata,'cosine')
vggsim4 <- simmatrix(vggdata,'gower')
vggsim5 <- simmatrix(vggdata,'minkowski')
recommendation(resnetdata,ressim,'개장수','euclidean')
recommendation(vggdata,vggsim5,'개장수','cosine')

resumap %>% mutate(cluster=cluster$cluster) %>% ggplot(aes(a,b,col=as.factor(cluster)))+geom_point()

a <- distance(data2,method='minkowski',p=3)

#cnn
cnndata <- filemerge('wow',n=10)
cnnumap <- umapping(cnndata)
cluster <- cnnumap %>% select(a,b) %>% kmeans(centers=10)
cnnumap %>% mutate(cluster=cluster$cluster) %>% ggplot(aes(a,b,col=as.factor(cluster)))+geom_point()+theme_classic()+theme(legend.position = 'None',axis.title.x = element_blank(),axis.title.y=element_blank())
cnnsim <- simmatrix(cnndata,'euclidean')
recommendation(cnndata,cnnsim,'개장수','euclidean')
Webtoonrecommend(vggdata,vggsim,'개장수','euclidean')
cnnumap[which(cluster$cluster==2),]
cnnumap[which(cnnumap$a>2),]

#inception
inceptiondata <- fread('inception.csv')
inceptiondata %<>% filter(!(V1 %in% setdiff(level,title$title)))
rownames(inceptiondata) <- inceptiondata$V1
inceptiondata %<>% select(-V1)

inceptionsim <- simmatrix(inceptiondata,'euclidean')
recommendation(inceptiondata,inceptionsim,'개장수','euclidean')

#googlenet
googlenetdata <- fread('googlenet.csv')
googlenetdata %<>% filter(!(V1 %in% setdiff(level,title$title)))
rownames(googlenetdata) <- googlenetdata$V1
googlenetdata %<>% select(-V1)
googlenetsim <- simmatrix(inceptiondata,'euclidean')
recommendation(googlenetdata,googlenetsim,'개장수','euclidean')


#최종 추천 알고리즘
setwd('c:/Users/USER/Desktop')
vggdata <- fread('vggdata.csv')
rownames(vggdata) <- vggdata$V1
vggdata %<>% select(-V1) %>% as.data.frame()

vggsim <- fread('vggsim.csv')
vggsim %<>% select(-V1) %>% as.matrix() 
rownames(vggsim) <- colnames(vggsim)

#input : 3번째 인자에 c(웹툰 목록), 5번째 인자에 그림체에 얼마나 비중 줄건지
Webtoonrecommend(vggdata,vggsim,'개장수','euclidean',rate=0.5) %>% DT::datatable()
