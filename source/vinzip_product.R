product <- read.csv('C:\\Users\\baek\\Desktop\\vinzip\\T_PRODUCT.csv',sep=';')


#product <- read.csv('C:\\Users\\pc\\Desktop\\vipzip\\t_product.csv',sep = ';')
colnames(product)#[1] "상품코드" "상품명"   "대분류"   "판매가"   "T_CNT"    "T_SAL"   

library(wordcloud)
pal <- brewer.pal(8,"Dark2")    # Dark2라는 색깔을 추가하는 작업 #참고 display.brewer.all()
pal <- pal[-(1)]

#대분류별 매출 점유율
#install.packages("plotrix")
library(plotrix)

x<-tapply(product$판매가, product$대분류,sum)
label<-round(x/sum(x)*100,1)
label2<-paste(sort(unique(product$대분류)),label)
pie(x,col=pal,label=paste(label2,"%"),main='대분류별 매출 점유율')
pie3D(x,explode = 0.1, labels=paste(label2,'%'),main='대분류별 매출 점유율')
ggplot(product,aes(x=대분류,y=T_SAL))+geom_bar(stat="identity", width=1)+coord_polar(theta="y")


#대분류별 상품 건수 점유율
#library(plotrix)

x<-tapply(product$T_CNT, product$대분류,sum)
label<-round(x/sum(x)*100,1)
label2<-paste(sort(unique(product$대분류)),label)
pie(x,col=pal,label=paste(label2,"%",'(',x,'건)'),main='대분류별 등록 상품건수 점유율')
#pie3D(x,explode = 0.1, labels=paste(label2,'%','(',x,'건)'),main='대분류별 상품 건수 점유율')


# #가격대별 매출 점유율

product <- within( product, {
  가격대 = character(0) 
  가격대[ product$판매가/2 <= 20000 ] = "~20000" 
  가격대[ product$판매가/2 >20000 & product$판매가/2 <= 30000 ] = "20000-30000" 
  가격대[ product$판매가/2 >30000 & product$판매가/2 <= 40000 ] = "30000-40000" 
  가격대[ product$판매가/2 >40000 & product$판매가/2 <= 50000 ] = "40000-50000" 
  가격대[ product$판매가/2 >50000 & product$판매가/2 <= 60000 ] = "50000-60000" 
  가격대[ product$판매가/2 >60000] = "60000~" 
  가격대 = factor(가격대, level = c("~20000", "20000-30000", "30000-40000", "40000-50000",'50000-60000','60000~'))
})

#가격대별 '매출' 점유율

x<-tapply(product$판매가, product$가격대,sum) 
x
label<-round(x/sum(x)*100,1)
label2<-paste(sort(unique(product$가격대)),label)
pie(x,col=pal,cex=0.8,label=paste(label2,"%"),main='가격대별 매출 점유율(50%세일 적용)')
#pie3D(x,explode = 0.1, labels=paste(label2,'%'),main='가격대별 매출 점유율(50%세일 적용)')

#가격대별 '판매건수' 점유율 및 건수

x<-tapply(product$T_CNT, product$가격대,sum) 
x
label<-round(x/sum(x)*100,1)
label2<-paste(unique(product$가격대),label)
#pie(x,col=rainbow(nrow(x)),label=paste(label2,"%"))
pie(x, labels=paste(label2,'%','(',x,'건',')'),col=pal,cex=0.8,main='가격대별 판매건수 및 점유율(50%세일 적용)')



#### 워드클라우드 
#install.packages("wordcloud")   
#install.packages("KoNLP")      
library(KoNLP)
library(plyr)

graphics.off()
p<-paste(product$상품명,collspace="")
class(nouns)
nouns <- extractNoun(p)    #명사만 출력 
b <- unlist(nouns)
df<- data.frame(단어=b)
#nouns <- unlist(nouns)

nrow(df)
df <- df[nchar(df$단어)>=2,]      #두글자 이상인 명사만 추출 
df <- df[grepl('아디다스',df$단어),]
product2[!(grepl('아디다스',product2$상품명),]
rmv <- c('아디다스','나이키','라코스테','엄브로','폴로','칼하트','버버리','빈폴','휠라','캉골','챔피언','퓨마','카파','타미 힐피거','리복','리바이스','뉴발란스','데상트','디키즈','엘레쎄','입생로랑','르꼬끄','USA','JPN','MLB','아식스','노티카','유니클로','스투시','갭')
rmv<-c(rmv,'man','MAN','빈티지')

length(nouns)
for (i in 1:length(rmv)){
  print(i)
  nouns<-nouns[nouns!=rmv[i]]
}
nouns <- unlist(nouns)

nouns <- nouns[nouns!='MAN'] 
#cnouns <- count(unlist(nouns))   #단어와 건수 출력

head(cnouns[order(cnouns$freq, decreasing = T),],10)


windowsFonts(malgun=windowsFont("a발레리나"))     #폰트 추가
wordcloud(words=cnouns$x, freq=cnouns$freq, colors=pal, min.freq=50, scale=c(5,.2), 
          random.order=F, family="malgun")   


#카테고리별 브랜드별 평균 가격 & 팔린 상품 평균 가격 이런식으로 분석하면
#어떤 브랜드의 어떤 상품은 가격을 좀 상향으로 잡아도 된다 이런건 알려줄 수 있을 것 같아


product
tapply(product$판매가, product$가격대,sum) 
a<-tapply(product$T_CNT, list(product$가격대,product$대분류),sum)



### 브랜드 

product3<-product
brand <- data.frame()
str(product3)



평균가격<-c()
팔린개수<-c()
브랜드명 <- c('아디다스','나이키','라코스테','엄브로','폴로','칼하트','버버리','빈폴','휠라','캉골','챔피언','퓨마','카파','타미 힐피거','리복','리바이스','뉴발란스','데상트','디키즈','엘레쎄','입생로랑','르꼬끄','USA','JPN','MLB','아식스','노티카','유니클로','스투시','갭')
for (i in 1:length(브랜드명)){
  print(브랜드명[i])
  평균가격<-c(평균가격,round(mean(product3[grepl(브랜드명[i],product3$상품명),'판매가'])/2))
  팔린개수<-c(팔린개수,as.numeric(sum(product3[grepl(브랜드명[i],product3$상품명),'T_CNT'])))
}
제품수<-c(1786,1877,608,130,1041,102,125,282,450,50,378,307,104,306,220,151,113,55,83,59,114,80,421,701,75,50,103,124,78,81)
brand<-data.frame(브랜드명,평균가격,제품수,팔린개수)

brand<-brand[c(order(brand$제품수,decreasing=T)),]

library(ggplot2)
str(brand)
dev.off()

#브랜드별 평균가격
ggplot(brand, aes(x=브랜드명,y=평균가격, fill=브랜드명))+ ggtitle("브랜드별 평균 가격")+geom_bar(stat = "identity")+ 
  geom_text(aes(y = 평균가격, label = 평균가격, vjust = 0 ,hjust=0.5),color='black',size=3)+coord_flip()
#브랜드별 제품수(종류가 몇개인지)

ggplot(brand, aes(x=브랜드명,y=제품수, fill=브랜드명))+ ggtitle("브랜드별 제품수(종류)")+geom_bar(stat = "identity")+
  geom_text(aes(y = 제품수, label = 제품수 , vjust = 0 ,hjust=0.5),color='black',size=3)+coord_flip()

#브랜드별 제품수 - 팔린개수 (제품수는 등록된 제품의 종류 (최소 제품수 이상 재고가 있음)/판매한 수 (높을수록 안좋음))
# 어떤 브랜드가 제고가 많을까 ? (=어떤 브랜드가 많이 선호 될지? 건수가 아닌 비율로..) 
ggplot(brand, aes(브랜드명,제품수/팔린개수, fill=브랜드명))+
  geom_bar(stat='identity',position="dodge")+
  geom_text(aes(y = 제품수/팔린개수, label = round(제품수/팔린개수,2) , vjust = 0 ,hjust=0),color='black',size=3)+
  coord_flip()
