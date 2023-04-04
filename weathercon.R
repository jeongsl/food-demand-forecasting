####필요한 라이브러리
library(corrplot) 
library(rpart) 
library(rpart.plot) 
library(ggsci) 
library(reshape2)
library(dplyr)
library(magrittr)
library(ggplot2)

####필요한 함수 생성 - 변수의 상대적 중요도 시각화 함수
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)}

plotRelWeights=function(fit){
  data<-relweights(fit)
  data$Predictors<-rownames(data)
  p<-ggplot(data=data,aes(x=reorder(Predictors,Weights),y=Weights,fill=Predictors))+ 
    geom_bar(stat="identity",width=0.5)+
    ggtitle("Relative Importance of Predictor Variables")+
    ylab(paste0("% of R-square \n(Total R-Square=",attr(data,"R-square"),")"))+
    geom_text(aes(y=Weights-0.1,label=paste(round(Weights,1),"%")),hjust=1)+
    guides(fill=FALSE)+
    coord_flip()
  p
}

####온라인 구매이력 데이터
buy181=read.csv('buy2018_1.csv',header=T,fileEncoding='euc-kr')
buy182=read.csv('buy2018_2.csv',header=T,fileEncoding='euc-kr')
buy191=read.csv('buy2019_1.csv',header=T,fileEncoding='euc-kr')
buy192=read.csv('buy2019_2.csv',header=T,fileEncoding='euc-kr')

##식품만 고르고, 변수 이름 바꾸기
buy181_food=subset(buy181,buy2018_1.big_cat=='식품')
names(buy181_food)=c("obs","date","sex","age","b_cat","s_cat","q")

buy182_food=subset(buy182,buy2018_2.big_cat=='식품')
names(buy182_food)=c("obs","date","sex","age","b_cat","s_cat","q")

buy191_food=subset(buy191,buy2019_1.big_cat=='식품')
names(buy191_food)=c("obs","date","sex","age","b_cat","s_cat","q")

buy192_food=subset(buy192,buy2019_2.big_cat=='식품')
names(buy192_food)=c("obs","date","sex","age","b_cat","s_cat","q")

##18,19년 자료 합치기
buy2018=rbind(buy181_food,buy182_food)
buy2019=rbind(buy191_food,buy192_food)
buy=rbind(buy2018, buy2019)

####결측치 확인
sum(is.na(buy))

####데이터 전처리-월별, 상품군별 수요량 나타내기
##김치/반찬
buy_kimchi=subset(buy,s_cat=='갓김치'|s_cat=='김치류'|s_cat=='무김치'|s_cat=='물김치'
                  |s_cat=='배추김치'|s_cat=='백김치'|s_cat=='파김치'|s_cat=='젓갈류'
                  |s_cat=='절임배추/김치속'|s_cat=='반찬류')
buy_m_kc18=matrix(c(201801,201802,201803,201804,201805,201806,
                    201807,201808,201809,201810,201811,201812,
                    sum(subset(buy_kimchi,date<=20180131)$q),
                    sum(subset(buy_kimchi,date>=20180201&date<=20180228)$q),
                    sum(subset(buy_kimchi,date>=20180301&date<=20180331)$q),
                    sum(subset(buy_kimchi,date>=20180401&date<=20180430)$q),
                    sum(subset(buy_kimchi,date>=20180501&date<=20180531)$q),
                    sum(subset(buy_kimchi,date>=20180601&date<=20180630)$q),
                    sum(subset(buy_kimchi,date>=20180701&date<=20180731)$q),
                    sum(subset(buy_kimchi,date>=20180801&date<=20180831)$q),
                    sum(subset(buy_kimchi,date>=20180901&date<=20180930)$q),
                    sum(subset(buy_kimchi,date>=20181001&date<=20181031)$q),
                    sum(subset(buy_kimchi,date>=20181101&date<=20181130)$q),
                    sum(subset(buy_kimchi,date>=20181201&date<=20181231)$q)),
                  ncol=2)
buy_m_kc18=as.data.frame(buy_m_kc18)
colnames(buy_m_kc18)=c('month','kimchiq')

buy_m_kc19=matrix(c(201901,201902,201903,201904,201905,201906,
                    201907,201908,201909,201910,201911,201912,
                    sum(subset(buy_kimchi,date>=20190101&date<=20190131)$q),
                    sum(subset(buy_kimchi,date>=20190201&date<=20190228)$q),
                    sum(subset(buy_kimchi,date>=20190301&date<=20190331)$q),
                    sum(subset(buy_kimchi,date>=20190401&date<=20190430)$q),
                    sum(subset(buy_kimchi,date>=20190501&date<=20190531)$q),
                    sum(subset(buy_kimchi,date>=20190601&date<=20190630)$q),
                    sum(subset(buy_kimchi,date>=20190701&date<=20190731)$q),
                    sum(subset(buy_kimchi,date>=20190801&date<=20190831)$q),
                    sum(subset(buy_kimchi,date>=20190901&date<=20190930)$q),
                    sum(subset(buy_kimchi,date>=20191001&date<=20191031)$q),
                    sum(subset(buy_kimchi,date>=20191101&date<=20191130)$q),
                    sum(subset(buy_kimchi,date>=20191201&date<=20191231)$q)),
                  ncol=2)
buy_m_kc19=as.data.frame(buy_m_kc19)
colnames(buy_m_kc19)=c('month','kimchiq')

buy_m_kc=rbind(buy_m_kc18,buy_m_kc19)

##음료,유제품
buy_drink=subset(buy,s_cat=='옥수수차'|s_cat=='프라페/버블티 파우더'|s_cat=='원두커피'
                 |s_cat=='탄산수'|s_cat=='이온음료'|s_cat=='인스턴트 커피'|s_cat=='선식'
                 |s_cat=='복분자/석류/과실즙'|s_cat=='차 선물세트'|s_cat=='더치 커피'
                 |s_cat=='유자차'|s_cat=='기타 주스류'|s_cat=='코코아/핫초코'
                 |s_cat=='홍차'|s_cat=='과실차'|s_cat=='아이스티'|s_cat=='캡슐/POD커피'
                 |s_cat=='홍삼 음료'|s_cat=='바나나우유'|s_cat=='둥글레차'|s_cat=='생수'
                 |s_cat=='차/곡물 음료'|s_cat=='전통차'|s_cat=='과채 음료/주스'
                 |s_cat=='율무차'|s_cat=='식혜/수정과'|s_cat=='어린이 음료'
                 |s_cat=='저/무지방우유'|s_cat=='에이드'|s_cat=='숙취/에너지/건강 음료'
                 |s_cat=='곡물차'|s_cat=='녹차'|s_cat=='두유'|s_cat=='밀크티/티라떼'
                 |s_cat=='허브차'|s_cat=='유제품 음료'|s_cat=='탄산음료'|s_cat=='건강즙'
                 |s_cat=='아몬드유/코코넛밀크'|s_cat=='딸기우유'|s_cat=='초코우유'
                 |s_cat=='보리차'|s_cat=='전통주'|s_cat=='비타민/화이바 음료'
                 |s_cat=='요거트/발효유'|s_cat=='커피음료')
buy_m_d18=matrix(c(201801,201802,201803,201804,201805,201806,
                   201807,201808,201809,201810,201811,201812,
                   sum(subset(buy_drink,date<=20180131)$q),
                   sum(subset(buy_drink,date>=20180201&date<=20180228)$q),
                   sum(subset(buy_drink,date>=20180301&date<=20180331)$q),
                   sum(subset(buy_drink,date>=20180401&date<=20180430)$q),
                   sum(subset(buy_drink,date>=20180501&date<=20180531)$q),
                   sum(subset(buy_drink,date>=20180601&date<=20180630)$q),
                   sum(subset(buy_drink,date>=20180701&date<=20180731)$q),
                   sum(subset(buy_drink,date>=20180801&date<=20180831)$q),
                   sum(subset(buy_drink,date>=20180901&date<=20180930)$q),
                   sum(subset(buy_drink,date>=20181001&date<=20181031)$q),
                   sum(subset(buy_drink,date>=20181101&date<=20181130)$q),
                   sum(subset(buy_drink,date>=20181201&date<=20181231)$q)),
                 ncol=2)
buy_m_d18=as.data.frame(buy_m_d18)
colnames(buy_m_d18)=c('month','drinkq')

buy_m_d19=matrix(c(201901,201902,201903,201904,201905,201906,
                   201907,201908,201909,201910,201911,201912,
                   sum(subset(buy_drink,date>=20190101&date<=20190131)$q),
                   sum(subset(buy_drink,date>=20190201&date<=20190228)$q),
                   sum(subset(buy_drink,date>=20190301&date<=20190331)$q),
                   sum(subset(buy_drink,date>=20190401&date<=20190430)$q),
                   sum(subset(buy_drink,date>=20190501&date<=20190531)$q),
                   sum(subset(buy_drink,date>=20190601&date<=20190630)$q),
                   sum(subset(buy_drink,date>=20190701&date<=20190731)$q),
                   sum(subset(buy_drink,date>=20190801&date<=20190831)$q),
                   sum(subset(buy_drink,date>=20190901&date<=20190930)$q),
                   sum(subset(buy_drink,date>=20191001&date<=20191031)$q),
                   sum(subset(buy_drink,date>=20191101&date<=20191130)$q),
                   sum(subset(buy_drink,date>=20191201&date<=20191231)$q)),
                 ncol=2)
buy_m_d19=as.data.frame(buy_m_d19)
colnames(buy_m_d19)=c('month','drinkq')

buy_m_d=rbind(buy_m_d18,buy_m_d19)

##쌀/잡곡
buy_rice=subset(buy,s_cat=='혼합곡'|s_cat=='잡곡 씨드류'
                |s_cat=='쌀'|s_cat=='잡곡')
buy_m_r18=matrix(c(201801,201802,201803,201804,201805,201806,
                   201807,201808,201809,201810,201811,201812,
                   sum(subset(buy_rice,date<=20180131)$q),
                   sum(subset(buy_rice,date>=20180201&date<=20180228)$q),
                   sum(subset(buy_rice,date>=20180301&date<=20180331)$q),
                   sum(subset(buy_rice,date>=20180401&date<=20180430)$q),
                   sum(subset(buy_rice,date>=20180501&date<=20180531)$q),
                   sum(subset(buy_rice,date>=20180601&date<=20180630)$q),
                   sum(subset(buy_rice,date>=20180701&date<=20180731)$q),
                   sum(subset(buy_rice,date>=20180801&date<=20180831)$q),
                   sum(subset(buy_rice,date>=20180901&date<=20180930)$q),
                   sum(subset(buy_rice,date>=20181001&date<=20181031)$q),
                   sum(subset(buy_rice,date>=20181101&date<=20181130)$q),
                   sum(subset(buy_rice,date>=20181201&date<=20181231)$q)),
                 ncol=2)
buy_m_r18=as.data.frame(buy_m_r18)
colnames(buy_m_r18)=c('month','riceq')

buy_m_r19=matrix(c(201901,201902,201903,201904,201905,201906,
                   201907,201908,201909,201910,201911,201912,
                   sum(subset(buy_rice,date>=20190101&date<=20190131)$q),
                   sum(subset(buy_rice,date>=20190201&date<=20190228)$q),
                   sum(subset(buy_rice,date>=20190301&date<=20190331)$q),
                   sum(subset(buy_rice,date>=20190401&date<=20190430)$q),
                   sum(subset(buy_rice,date>=20190501&date<=20190531)$q),
                   sum(subset(buy_rice,date>=20190601&date<=20190630)$q),
                   sum(subset(buy_rice,date>=20190701&date<=20190731)$q),
                   sum(subset(buy_rice,date>=20190801&date<=20190831)$q),
                   sum(subset(buy_rice,date>=20190901&date<=20190930)$q),
                   sum(subset(buy_rice,date>=20191001&date<=20191031)$q),
                   sum(subset(buy_rice,date>=20191101&date<=20191130)$q),
                   sum(subset(buy_rice,date>=20191201&date<=20191231)$q)),
                 ncol=2)
buy_m_r19=as.data.frame(buy_m_r19)
colnames(buy_m_r19)=c('month','riceq')

buy_m_r=rbind(buy_m_r18,buy_m_r19)

##장/양념/소스류
buy_sauce=subset(buy,s_cat=='음용 식초')
buy_m_sa18=matrix(c(201801,201802,201803,201804,201805,201806,
                    201807,201808,201809,201810,201811,201812,
                    sum(subset(buy_sauce,date<=20180131)$q),
                    sum(subset(buy_sauce,date>=20180201&date<=20180228)$q),
                    sum(subset(buy_sauce,date>=20180301&date<=20180331)$q),
                    sum(subset(buy_sauce,date>=20180401&date<=20180430)$q),
                    sum(subset(buy_sauce,date>=20180501&date<=20180531)$q),
                    sum(subset(buy_sauce,date>=20180601&date<=20180630)$q),
                    sum(subset(buy_sauce,date>=20180701&date<=20180731)$q),
                    sum(subset(buy_sauce,date>=20180801&date<=20180831)$q),
                    sum(subset(buy_sauce,date>=20180901&date<=20180930)$q),
                    sum(subset(buy_sauce,date>=20181001&date<=20181031)$q),
                    sum(subset(buy_sauce,date>=20181101&date<=20181130)$q),
                    sum(subset(buy_sauce,date>=20181201&date<=20181231)$q)),
                  ncol=2)
buy_m_sa18=as.data.frame(buy_m_sa18)
colnames(buy_m_sa18)=c('month','sauceq')

buy_m_sa19=matrix(c(201901,201902,201903,201904,201905,201906,
                    201907,201908,201909,201910,201911,201912,
                    sum(subset(buy_sauce,date>=20190101&date<=20190131)$q),
                    sum(subset(buy_sauce,date>=20190201&date<=20190228)$q),
                    sum(subset(buy_sauce,date>=20190301&date<=20190331)$q),
                    sum(subset(buy_sauce,date>=20190401&date<=20190430)$q),
                    sum(subset(buy_sauce,date>=20190501&date<=20190531)$q),
                    sum(subset(buy_sauce,date>=20190601&date<=20190630)$q),
                    sum(subset(buy_sauce,date>=20190701&date<=20190731)$q),
                    sum(subset(buy_sauce,date>=20190801&date<=20190831)$q),
                    sum(subset(buy_sauce,date>=20190901&date<=20190930)$q),
                    sum(subset(buy_sauce,date>=20191001&date<=20191031)$q),
                    sum(subset(buy_sauce,date>=20191101&date<=20191130)$q),
                    sum(subset(buy_sauce,date>=20191201&date<=20191231)$q)),
                  ncol=2)
buy_m_sa19=as.data.frame(buy_m_sa19)
colnames(buy_m_sa19)=c('month','sauceq')

buy_m_sa=rbind(buy_m_sa18,buy_m_sa19)

##베이커리
buy_bakery=subset(buy,s_cat=='카페용 초콜릿시럽'|s_cat=='카페 푸드'
                  |s_cat=='커피용 프림')
buy_m_bk18=matrix(c(201801,201802,201803,201804,201805,201806,
                    201807,201808,201809,201810,201811,201812,
                    sum(subset(buy_bakery,date<=20180131)$q),
                    sum(subset(buy_bakery,date>=20180201&date<=20180228)$q),
                    sum(subset(buy_bakery,date>=20180301&date<=20180331)$q),
                    sum(subset(buy_bakery,date>=20180401&date<=20180430)$q),
                    sum(subset(buy_bakery,date>=20180501&date<=20180531)$q),
                    sum(subset(buy_bakery,date>=20180601&date<=20180630)$q),
                    sum(subset(buy_bakery,date>=20180701&date<=20180731)$q),
                    sum(subset(buy_bakery,date>=20180801&date<=20180831)$q),
                    sum(subset(buy_bakery,date>=20180901&date<=20180930)$q),
                    sum(subset(buy_bakery,date>=20181001&date<=20181031)$q),
                    sum(subset(buy_bakery,date>=20181101&date<=20181130)$q),
                    sum(subset(buy_bakery,date>=20181201&date<=20181231)$q)),
                  ncol=2)
buy_m_bk18=as.data.frame(buy_m_bk18)
colnames(buy_m_bk18)=c('month','bakeryq')

buy_m_bk19=matrix(c(201901,201902,201903,201904,201905,201906,
                    201907,201908,201909,201910,201911,201912,
                    sum(subset(buy_bakery,date>=20190101&date<=20190131)$q),
                    sum(subset(buy_bakery,date>=20190201&date<=20190228)$q),
                    sum(subset(buy_bakery,date>=20190301&date<=20190331)$q),
                    sum(subset(buy_bakery,date>=20190401&date<=20190430)$q),
                    sum(subset(buy_bakery,date>=20190501&date<=20190531)$q),
                    sum(subset(buy_bakery,date>=20190601&date<=20190630)$q),
                    sum(subset(buy_bakery,date>=20190701&date<=20190731)$q),
                    sum(subset(buy_bakery,date>=20190801&date<=20190831)$q),
                    sum(subset(buy_bakery,date>=20190901&date<=20190930)$q),
                    sum(subset(buy_bakery,date>=20191001&date<=20191031)$q),
                    sum(subset(buy_bakery,date>=20191101&date<=20191130)$q),
                    sum(subset(buy_bakery,date>=20191201&date<=20191231)$q)),
                  ncol=2)
buy_m_bk19=as.data.frame(buy_m_bk19)
colnames(buy_m_bk19)=c('month','bakeryq')

buy_m_bk=rbind(buy_m_bk18,buy_m_bk19)

##정육/계란
buy_meategg=subset(buy,s_cat=='소고기 육회'|s_cat=='윙봉/닭다리/날개'
                   |s_cat=='메추리알'|s_cat=='국내산 돈육'
                   |s_cat=='한우육'|s_cat=='닭가슴살'
                   |s_cat=='구이/수육용 돈육'|s_cat=='소고기 등심/안심'
                   |s_cat=='계란'|s_cat=='오리고기/훈제오리'|s_cat=='가공란'
                   |s_cat=='생닭/닭부분육'|s_cat=='갈비용 우육'
                   |s_cat=='양념우육'|s_cat=='수입우육'
                   |s_cat=='장조림/카레용 돈육'|s_cat=='갈비/찜/바비큐용 돈육'
                   |s_cat=='양념 돈육'|s_cat=='닭 양념육'|s_cat=='돼지 곱창')
buy_m_me18=matrix(c(201801,201802,201803,201804,201805,201806,
                    201807,201808,201809,201810,201811,201812,
                    sum(subset(buy_meategg,date<=20180131)$q),
                    sum(subset(buy_meategg,date>=20180201&date<=20180228)$q),
                    sum(subset(buy_meategg,date>=20180301&date<=20180331)$q),
                    sum(subset(buy_meategg,date>=20180401&date<=20180430)$q),
                    sum(subset(buy_meategg,date>=20180501&date<=20180531)$q),
                    sum(subset(buy_meategg,date>=20180601&date<=20180630)$q),
                    sum(subset(buy_meategg,date>=20180701&date<=20180731)$q),
                    sum(subset(buy_meategg,date>=20180801&date<=20180831)$q),
                    sum(subset(buy_meategg,date>=20180901&date<=20180930)$q),
                    sum(subset(buy_meategg,date>=20181001&date<=20181031)$q),
                    sum(subset(buy_meategg,date>=20181101&date<=20181130)$q),
                    sum(subset(buy_meategg,date>=20181201&date<=20181231)$q)),
                  ncol=2)
buy_m_me18=as.data.frame(buy_m_me18)
colnames(buy_m_me18)=c('month','meatq')

buy_m_me19=matrix(c(201901,201902,201903,201904,201905,201906,
                    201907,201908,201909,201910,201911,201912,
                    sum(subset(buy_meategg,date>=20190101&date<=20190131)$q),
                    sum(subset(buy_meategg,date>=20190201&date<=20190228)$q),
                    sum(subset(buy_meategg,date>=20190301&date<=20190331)$q),
                    sum(subset(buy_meategg,date>=20190401&date<=20190430)$q),
                    sum(subset(buy_meategg,date>=20190501&date<=20190531)$q),
                    sum(subset(buy_meategg,date>=20190601&date<=20190630)$q),
                    sum(subset(buy_meategg,date>=20190701&date<=20190731)$q),
                    sum(subset(buy_meategg,date>=20190801&date<=20190831)$q),
                    sum(subset(buy_meategg,date>=20190901&date<=20190930)$q),
                    sum(subset(buy_meategg,date>=20191001&date<=20191031)$q),
                    sum(subset(buy_meategg,date>=20191101&date<=20191130)$q),
                    sum(subset(buy_meategg,date>=20191201&date<=20191231)$q)),
                  ncol=2)
buy_m_me19=as.data.frame(buy_m_me19)
colnames(buy_m_me19)=c('month','meatq')

buy_m_me=rbind(buy_m_me18,buy_m_me19)


##수산/건어물
buy_fish=subset(buy,s_cat=='전복 생물'|s_cat=='건어물 쥐포'|s_cat=='연어/훈제연어'|s_cat=='해조류 다시마'|s_cat=='오징어'|s_cat=='해조류 미역'|s_cat=='건어물 황태'|s_cat=='고등어'|s_cat=='굴비/조기'|s_cat=='건어물 마른오징어'|s_cat=='건어물 멸치'|s_cat=='주꾸미'|s_cat=='수산 생물'|s_cat=='조개'|s_cat=='건어물 진미채'|s_cat=='생선류'|s_cat=='건어물 건새우'|s_cat=='랍스타'|s_cat=='새우/대하'|s_cat=='옥돔'|s_cat=='명태/동태'|s_cat=='낙지'|s_cat=='게장류'|s_cat=='가자미'|s_cat=='문어'|s_cat=='건어물 노가리'|s_cat=='갈치'|s_cat=='홍어'|s_cat=='생식/선식 분말'|s_cat=='어란(생선알)'|s_cat=='굴 생물'|s_cat=='장어'|s_cat=='삼치'|s_cat=='꽃게'|s_cat=='대게/킹크랩'|s_cat=='회')
buy_m_f18=matrix(c(201801,201802,201803,201804,201805,201806,
                   201807,201808,201809,201810,201811,201812,
                   sum(subset(buy_fish,date<=20180131)$q),
                   sum(subset(buy_fish,date>=20180201&date<=20180228)$q),
                   sum(subset(buy_fish,date>=20180301&date<=20180331)$q),
                   sum(subset(buy_fish,date>=20180401&date<=20180430)$q),
                   sum(subset(buy_fish,date>=20180501&date<=20180531)$q),
                   sum(subset(buy_fish,date>=20180601&date<=20180630)$q),
                   sum(subset(buy_fish,date>=20180701&date<=20180731)$q),
                   sum(subset(buy_fish,date>=20180801&date<=20180831)$q),
                   sum(subset(buy_fish,date>=20180901&date<=20180930)$q),
                   sum(subset(buy_fish,date>=20181001&date<=20181031)$q),
                   sum(subset(buy_fish,date>=20181101&date<=20181130)$q),
                   sum(subset(buy_fish,date>=20181201&date<=20181231)$q)),
                 ncol=2)
buy_m_f18=as.data.frame(buy_m_f18)
colnames(buy_m_f18)=c('month','fishq')

buy_m_f19=matrix(c(201901,201902,201903,201904,201905,201906,
                   201907,201908,201909,201910,201911,201912,
                   sum(subset(buy_fish,date>=20190101&date<=20190131)$q),
                   sum(subset(buy_fish,date>=20190201&date<=20190228)$q),
                   sum(subset(buy_fish,date>=20190301&date<=20190331)$q),
                   sum(subset(buy_fish,date>=20190401&date<=20190430)$q),
                   sum(subset(buy_fish,date>=20190501&date<=20190531)$q),
                   sum(subset(buy_fish,date>=20190601&date<=20190630)$q),
                   sum(subset(buy_fish,date>=20190701&date<=20190731)$q),
                   sum(subset(buy_fish,date>=20190801&date<=20190831)$q),
                   sum(subset(buy_fish,date>=20190901&date<=20190930)$q),
                   sum(subset(buy_fish,date>=20191001&date<=20191031)$q),
                   sum(subset(buy_fish,date>=20191101&date<=20191130)$q),
                   sum(subset(buy_fish,date>=20191201&date<=20191231)$q)),
                 ncol=2)
buy_m_f19=as.data.frame(buy_m_f19)
colnames(buy_m_f19)=c('month','fishq')

buy_m_f=rbind(buy_m_f18,buy_m_f19)

##과일
buy_fruit=subset(buy,s_cat=='건바나나'|s_cat=='배/포도/과일즙'
                 |s_cat=='해초류'|s_cat=='감귤/한라봉/오렌지'
                 |s_cat=='건포도'|s_cat=='바나나/파일애플/망고'
                 |s_cat=='레몬/자몽'|s_cat=='딸기/복분자/블루베리'
                 |s_cat=='건자두'|s_cat=='건망고'|s_cat=='포도/거봉/체리'|s_cat=='곶감/반건시'
                 |s_cat=='참외/메론/수박'|s_cat=='키위/참다래'
                 |s_cat=='감/홍시'|s_cat=='감말랭이'
                 |s_cat=='건자두')
buy_m_fru18=matrix(c(201801,201802,201803,201804,201805,201806,
                     201807,201808,201809,201810,201811,201812,
                     sum(subset(buy_fruit,date<=20180131)$q),
                     sum(subset(buy_fruit,date>=20180201&date<=20180228)$q),
                     sum(subset(buy_fruit,date>=20180301&date<=20180331)$q),
                     sum(subset(buy_fruit,date>=20180401&date<=20180430)$q),
                     sum(subset(buy_fruit,date>=20180501&date<=20180531)$q),
                     sum(subset(buy_fruit,date>=20180601&date<=20180630)$q),
                     sum(subset(buy_fruit,date>=20180701&date<=20180731)$q),
                     sum(subset(buy_fruit,date>=20180801&date<=20180831)$q),
                     sum(subset(buy_fruit,date>=20180901&date<=20180930)$q),
                     sum(subset(buy_fruit,date>=20181001&date<=20181031)$q),
                     sum(subset(buy_fruit,date>=20181101&date<=20181130)$q),
                     sum(subset(buy_fruit,date>=20181201&date<=20181231)$q)),
                   ncol=2)
buy_m_fru18=as.data.frame(buy_m_fru18)
colnames(buy_m_fru18)=c('month','fruitq')

buy_m_fru19=matrix(c(201901,201902,201903,201904,201905,201906,
                     201907,201908,201909,201910,201911,201912,
                     sum(subset(buy_fruit,date>=20190101&date<=20190131)$q),
                     sum(subset(buy_fruit,date>=20190201&date<=20190228)$q),
                     sum(subset(buy_fruit,date>=20190301&date<=20190331)$q),
                     sum(subset(buy_fruit,date>=20190401&date<=20190430)$q),
                     sum(subset(buy_fruit,date>=20190501&date<=20190531)$q),
                     sum(subset(buy_fruit,date>=20190601&date<=20190630)$q),
                     sum(subset(buy_fruit,date>=20190701&date<=20190731)$q),
                     sum(subset(buy_fruit,date>=20190801&date<=20190831)$q),
                     sum(subset(buy_fruit,date>=20190901&date<=20190930)$q),
                     sum(subset(buy_fruit,date>=20191001&date<=20191031)$q),
                     sum(subset(buy_fruit,date>=20191101&date<=20191130)$q),
                     sum(subset(buy_fruit,date>=20191201&date<=20191231)$q)),
                   ncol=2)
buy_m_fru19=as.data.frame(buy_m_fru19)
colnames(buy_m_fru19)=c('month','fruitq')

buy_m_fru=rbind(buy_m_fru18,buy_m_fru19)

##채소
buy_veget=subset(buy,s_cat=='부추'|s_cat=='무/배추'
                 |s_cat=='느타리버섯'|s_cat=='콩나물/숙주'
                 |s_cat=='표고버섯'|s_cat=='토마토'
                 |s_cat=='쌈채소'|s_cat=='마늘/생강'
                 |s_cat=='나물'|s_cat=='야채/호박즙'|s_cat=='감자'|s_cat=='도라지/더덕'
                 |s_cat=='인삼/수삼/산삼'|s_cat=='파/양파'
                 |s_cat=='상황버섯'|s_cat=='고추/피망/파프리카'
                 |s_cat=='기타 농산물'|s_cat=='우엉/연근'|s_cat=='시금치'
                 |s_cat=='과일채소 분말/분태'|s_cat=='오이/가지'
                 |s_cat=='마/야콘'|s_cat=='어린잎/새싹채소'
                 |s_cat=='브로콜리/셀러리'|s_cat=='새송이버섯'
                 |s_cat=='호박'|s_cat=='믹스 채소'|s_cat=='영지버섯'|s_cat=='옥수수'
                 |s_cat=='미나리'|s_cat=='양배추/양상추')
buy_m_vt18=matrix(c(201801,201802,201803,201804,201805,201806,
                    201807,201808,201809,201810,201811,201812,
                    sum(subset(buy_veget,date<=20180131)$q),
                    sum(subset(buy_veget,date>=20180201&date<=20180228)$q),
                    sum(subset(buy_veget,date>=20180301&date<=20180331)$q),
                    sum(subset(buy_veget,date>=20180401&date<=20180430)$q),
                    sum(subset(buy_veget,date>=20180501&date<=20180531)$q),
                    sum(subset(buy_veget,date>=20180601&date<=20180630)$q),
                    sum(subset(buy_veget,date>=20180701&date<=20180731)$q),
                    sum(subset(buy_veget,date>=20180801&date<=20180831)$q),
                    sum(subset(buy_veget,date>=20180901&date<=20180930)$q),
                    sum(subset(buy_veget,date>=20181001&date<=20181031)$q),
                    sum(subset(buy_veget,date>=20181101&date<=20181130)$q),
                    sum(subset(buy_veget,date>=20181201&date<=20181231)$q)),
                  ncol=2)
buy_m_vt18=as.data.frame(buy_m_vt18)
colnames(buy_m_vt18)=c('month','vegetq')

buy_m_vt19=matrix(c(201901,201902,201903,201904,201905,201906,
                    201907,201908,201909,201910,201911,201912,
                    sum(subset(buy_veget,date>=20190101&date<=20190131)$q),
                    sum(subset(buy_veget,date>=20190201&date<=20190228)$q),
                    sum(subset(buy_veget,date>=20190301&date<=20190331)$q),
                    sum(subset(buy_veget,date>=20190401&date<=20190430)$q),
                    sum(subset(buy_veget,date>=20190501&date<=20190531)$q),
                    sum(subset(buy_veget,date>=20190601&date<=20190630)$q),
                    sum(subset(buy_veget,date>=20190701&date<=20190731)$q),
                    sum(subset(buy_veget,date>=20190801&date<=20190831)$q),
                    sum(subset(buy_veget,date>=20190901&date<=20190930)$q),
                    sum(subset(buy_veget,date>=20191001&date<=20191031)$q),
                    sum(subset(buy_veget,date>=20191101&date<=20191130)$q),
                    sum(subset(buy_veget,date>=20191201&date<=20191231)$q)),
                  ncol=2)
buy_m_vt19=as.data.frame(buy_m_vt19)
colnames(buy_m_vt19)=c('month','vegetq')

buy_m_vt=rbind(buy_m_vt18,buy_m_vt19)

##가공/건강식품
buy_manufacture=subset(buy,s_cat=='홍삼 분말/환'|s_cat=='헛개/가시오가피'|s_cat=='한방 분말/환제품'
                       |s_cat=='칼슘/철분 영양제'|s_cat=='견과류'|s_cat=='비타민'
                       |s_cat=='스피루리나 영양제'|s_cat=='프로바이오틱스'|s_cat=='감마리놀렌산 영양제'
                       |s_cat=='견과류 땅콩'|s_cat=='견과류 캐슈넛'|s_cat=='기타 한방/환제품'
                       |s_cat=='프로폴리스/로얄젤리'|s_cat=='오메가3/스쿠알렌 영양제'|s_cat=='루테인/눈 영양제'
                       |s_cat=='홍삼액/홍삼정'|s_cat=='건대추/혼합견과/ 건강즙/녹용'|s_cat=='다이어트용 헬스보충식품'
                       |s_cat=='글루코사민/키토산 영양제'|s_cat=='견과류 피스타치오'|s_cat=='견과류 잣/은행'
                       |s_cat=='옻/칡/쑥즙'|s_cat=='콜라겐/코큐텐 영양제'|s_cat=='양파/마늘즙'
                       |s_cat=='어린이영양제'|s_cat=='견과류 카카오닙스'|s_cat=='한방재료'
                       |s_cat=='견과류 밤'|s_cat=='반건조고구마'|s_cat=='홍삼절편/홍삼정과'                  
                       |s_cat=='클로렐라 영양제'|s_cat=='견과류 마카다미아'|s_cat=='다이어트보조식'|s_cat=='환자식'|s_cat=='초유 영양제')
buy_m_m18=matrix(c(201801,201802,201803,201804,201805,201806,
                   201807,201808,201809,201810,201811,201812,
                   sum(subset(buy_manufacture,date<=20180131)$q),
                   sum(subset(buy_manufacture,date>=20180201&date<=20180228)$q),
                   sum(subset(buy_manufacture,date>=20180301&date<=20180331)$q),
                   sum(subset(buy_manufacture,date>=20180401&date<=20180430)$q),
                   sum(subset(buy_manufacture,date>=20180501&date<=20180531)$q),
                   sum(subset(buy_manufacture,date>=20180601&date<=20180630)$q),
                   sum(subset(buy_manufacture,date>=20180701&date<=20180731)$q),
                   sum(subset(buy_manufacture,date>=20180801&date<=20180831)$q),
                   sum(subset(buy_manufacture,date>=20180901&date<=20180930)$q),
                   sum(subset(buy_manufacture,date>=20181001&date<=20181031)$q),
                   sum(subset(buy_manufacture,date>=20181101&date<=20181130)$q),
                   sum(subset(buy_manufacture,date>=20181201&date<=20181231)$q)),
                 ncol=2)
buy_m_m18=as.data.frame(buy_m_m18)
colnames(buy_m_m18)=c('month','manuq')

buy_m_m19=matrix(c(201901,201902,201903,201904,201905,201906,
                   201907,201908,201909,201910,201911,201912,
                   sum(subset(buy_manufacture,date>=20190101&date<=20190131)$q),
                   sum(subset(buy_manufacture,date>=20190201&date<=20190228)$q),
                   sum(subset(buy_manufacture,date>=20190301&date<=20190331)$q),
                   sum(subset(buy_manufacture,date>=20190401&date<=20190430)$q),
                   sum(subset(buy_manufacture,date>=20190501&date<=20190531)$q),
                   sum(subset(buy_manufacture,date>=20190601&date<=20190630)$q),
                   sum(subset(buy_manufacture,date>=20190701&date<=20190731)$q),
                   sum(subset(buy_manufacture,date>=20190801&date<=20190831)$q),
                   sum(subset(buy_manufacture,date>=20190901&date<=20190930)$q),
                   sum(subset(buy_manufacture,date>=20191001&date<=20191031)$q),
                   sum(subset(buy_manufacture,date>=20191101&date<=20191130)$q),
                   sum(subset(buy_manufacture,date>=20191201&date<=20191231)$q)),
                 ncol=2)
buy_m_m19=as.data.frame(buy_m_m19)
colnames(buy_m_m19)=c('month','manuq')

buy_m_m=rbind(buy_m_m18,buy_m_m19)

####전국 날씨 데이터(온도,강수,폭설,황사)
weather=read.csv('weather.csv',header=T)
sum(is.na(weather)) #결측치 확인->없음

####적합할 데이터(날씨,판매량)
weather2=cbind(weather,buy_m_bk$bakeryq,buy_m_d$drinkq,buy_m_f$fishq,
               buy_m_kc$kimchiq,buy_m_m$manuq,buy_m_me$meatq,
               buy_m_r$riceq,buy_m_sa$sauceq,buy_m_vt$vegetq,
               buy_m_fru$fruitq)
weather2$newdust=as.integer(weather2$dust<=30) #미세먼지 좋음 여부
weather2$newsnow=as.integer(weather2$snow>=5) #폭설 여부
colnames(weather2)=c('month','snow','temp','rain','dust','bakeryQ',
                     'drinkQ','fishQ','kimchiQ','manuQ','meatQ',
                     'riceQ','sauceQ','vegetQ','fruitQ','newdust','newsnow')

######소비 패턴 트렌드 분석
####꺾은선 그래프를 이용한 수요 추이
month1=rep(c(1:12),2)
month1

##김치류
#꺾은선 그래프 형태를 위한 데이터 전처리
buy_kimchi_sum=rbind(buy_m_kc18,buy_m_kc19)
buy_kimchi_sum=cbind(buy_kimchi_sum,month1)
buy_kimchi_sum$month[1:12]=2018
buy_kimchi_sum$month[13:24]=2019

#꺾은선 그래프 나타내기
kimchi_p=ggplot(data=buy_kimchi_sum, aes(x=month1,y=kimchiq,group=month,color=month))+geom_line()
kimchi_p=kimchi_p+scale_x_continuous(breaks = seq(1,12,1))
plot(kimchi_p)

##베이커리
#꺾은선 그래프를 형태를 위한 데이터 전처리
buy_bakery_sum=rbind(buy_m_bk18,buy_m_bk19)
buy_bakery_sum=cbind(buy_bakery_sum,month1)
buy_bakery_sum$month[1:12]=2018
buy_bakery_sum$month[13:24]=2019

#꺾은선 그래프 나타내기
bakery_p=ggplot(data=buy_bakery_sum, aes(x=month1,y=bakeryq,group=month,color=month))+geom_line()
bakery_p=bakery_p+scale_x_continuous(breaks = seq(1,12,1))
plot(bakery_p)

##음료,유제품
#꺾은선 그래프를 형태를 위한 데이터 전처리
buy_drink_sum=rbind(buy_m_d18,buy_m_d19)
buy_drink_sum=cbind(buy_drink_sum,month1)
buy_drink_sum$month[1:12]=2018
buy_drink_sum$month[13:24]=2019

#꺾은선 그래프 나타내기
drink_p=ggplot(data=buy_drink_sum, aes(x=month1,y=drinkq,group=month,color=month))+geom_line()
drink_p=drink_p+scale_x_continuous(breaks = seq(1,12,1))
plot(drink_p)

#2018보다 2019년도의 수요량이 급격히 증가함을 보임에 대한 요인 분석
drink18=subset(buy_drink,substr(buy_drink$date,1,6)==201805)
drink19=subset(buy_drink,substr(buy_drink$date,1,6)==201905)
drink18_cat=drink18%>%group_by(s_cat)%>%summarise(sum_q = sum(q))
drink19_cat=drink19%>%group_by(s_cat)%>%summarise(sum_q = sum(q))
drink=data.frame(drink19_cat$sum_q-drink18_cat$sum_q)

##수산,건어물
#꺾은선 그래프를 형태를 위한 데이터 전처리
buy_fish_sum=rbind(buy_m_f18,buy_m_f19)
buy_fish_sum=cbind(buy_fish_sum,month1)
buy_fish_sum$month[1:12]=2018
buy_fish_sum$month[13:24]=2019

#꺾은선 그래프 나타내기
fish_p=ggplot(data=buy_fish_sum, aes(x=month1,y=fishq,group=month,color=month))+geom_line()
fish_p=fish_p+scale_x_continuous(breaks = seq(1,12,1))
plot(fish_p)

##채소
#꺾은선 그래프를 형태를 위한 데이터 전처리
buy_veget_sum=rbind(buy_m_vt18,buy_m_vt19)
buy_veget_sum=cbind(buy_veget_sum,month1)
buy_veget_sum$month[1:12]=2018
buy_veget_sum$month[13:24]=2019

#꺾은선 그래프 나타내기
veget_p=ggplot(data=buy_veget_sum, aes(x=month1,y=vegetq,group=month,color=month))+geom_line()
veget_p=veget_p+scale_x_continuous(breaks = seq(1,12,1))
plot(veget_p)

##장,양념,소스류
#꺾은선 그래프를 형태를 위한 데이터 전처리
buy_sauce_sum=rbind(buy_m_sa18,buy_m_sa19)
buy_sauce_sum=cbind(buy_sauce_sum,month1)
buy_sauce_sum$month[1:12]=2018
buy_sauce_sum$month[13:24]=2019

#꺾은선 그래프 나타내기
sauce_p=ggplot(data=buy_sauce_sum, aes(x=month1,y=sauceq,group=month,color=month))+geom_line()
sauce_p=sauce_p+scale_x_continuous(breaks = seq(1,12,1))
plot(sauce_p)

##정육,계란
#꺾은선 그래프를 형태를 위한 데이터 전처리
buy_meat_sum=rbind(buy_m_me18,buy_m_me19)
buy_meat_sum=cbind(buy_meat_sum,month1)
buy_meat_sum$month[1:12]=2018
buy_meat_sum$month[13:24]=2019

# 꺾은선 그래프 나타내기
meat_p=ggplot(data=buy_meat_sum, aes(x=month1,y=meatq,group=month,color=month))+geom_line()
meat_p=meat_p+scale_x_continuous(breaks = seq(1,12,1))
plot(meat_p)

##과일
#꺾은선 그래프를 형태를 위한 데이터 전처리
buy_fruit_sum=rbind(buy_m_fru18,buy_m_fru19)
buy_fruit_sum=cbind(buy_fruit_sum,month1)
buy_fruit_sum$month[1:12]=2018
buy_fruit_sum$month[13:24]=2019

#꺾은선 그래프 나타내기
fruit_p=ggplot(data=buy_fruit_sum, aes(x=month1,y=fruitq,group=month,color=month))+geom_line()
fruit_p=fruit_p+scale_x_continuous(breaks = seq(1,12,1))
plot(fruit_p)

#2018년 1월에서 2월 사이 증가 요인 분석
fruit01=subset(buy_fruit,substr(buy_fruit$date,1,6)==201801)
fruit02=subset(buy_fruit,substr(buy_fruit$date,1,6)==201802)
fruit_cat01=fruit01%>%group_by(s_cat)%>%summarise(sum_q = sum(q))
fruit_cat02=fruit02%>%group_by(s_cat)%>%summarise(sum_q = sum(q))
fruit=data.frame(fruit_cat02$sum_q-fruit_cat01$sum_q)

##쌀,잡곡
#꺾은선 그래프를 형태를 위한 데이터 전처리
buy_rice_sum=rbind(buy_m_r18,buy_m_r19)
buy_rice_sum=cbind(buy_rice_sum,month1)
buy_rice_sum$month[1:12]=2018
buy_rice_sum$month[13:24]=2019

#꺾은선 그래프 나타내기
rice_p=ggplot(data=buy_rice_sum, aes(x=month1,y=riceq,group=month,color=month))+geom_line()
rice_p=rice_p+scale_x_continuous(breaks = seq(1,12,1))
plot(rice_p)

##가공,건강식품
#꺾은선 그래프를 형태를 위한 데이터 전처리
buy_manu_sum=rbind(buy_m_m18,buy_m_m19)
buy_manu_sum=cbind(buy_manu_sum,month1)
buy_manu_sum$month[1:12]=2018
buy_manu_sum$month[13:24]=2019

#꺾은선 그래프 나타내기
manu_p=ggplot(data=buy_manu_sum, aes(x=month1,y=manuq,group=month,color=month))+geom_line()
manu_p=manu_p+scale_x_continuous(breaks = seq(1,12,1))
plot(manu_p)

#2019년 2월에서 3월 사이 증가 요인 분석
manu01=subset(buy_manufacture,substr(buy_manufacture$date,1,6)==201902)
manu02=subset(buy_manufacture,substr(buy_manufacture$date,1,6)==201903)
manu_cat01=manu01%>%group_by(s_cat)%>%summarise(sum_q = sum(q))
manu_cat02=manu02%>%group_by(s_cat)%>%summarise(sum_q = sum(q))
manu=data.frame(manu_cat02$sum_q-manu_cat01$sum_q)

#####날씨에 민감한 상품군 분석
####상관계수행렬 시각화
weather_col=cor(weather2)
round(weather_col,2)
corrplot(weather_col,
         method='color',type='lower',
         order='hclust',diag=F)

####판매량에 영향을 끼치는 날씨 변수 알아보기(변수의 상대적 중요도 함수 이용)
plotRelWeights(lm(scale(bakeryQ)~scale(newsnow)+scale(temp)+scale(newdust)+scale(rain),data=weather2))
plotRelWeights(lm(scale(drinkQ)~scale(newsnow)+scale(temp)+scale(newdust)+scale(rain),data=weather2))
plotRelWeights(lm(scale(fishQ)~scale(newsnow)+scale(temp)+scale(newdust)+scale(rain),data=weather2))
plotRelWeights(lm(scale(kimchiQ)~scale(newsnow)+scale(temp)+scale(newdust)+scale(rain),data=weather2))
plotRelWeights(lm(scale(manuQ)~scale(newsnow)+scale(temp)+scale(newdust)+scale(rain),data=weather2))
plotRelWeights(lm(scale(meatQ)~scale(newsnow)+scale(temp)+scale(newdust)+scale(rain),data=weather2))
plotRelWeights(lm(scale(riceQ)~scale(newsnow)+scale(temp)+scale(newdust)+scale(rain),data=weather2))
plotRelWeights(lm(scale(sauceQ)~scale(newsnow)+scale(temp)+scale(newdust)+scale(rain),data=weather2))
plotRelWeights(lm(scale(vegetQ)~scale(newsnow)+scale(temp)+scale(newdust)+scale(rain),data=weather2))
plotRelWeights(lm(scale(fruitQ)~scale(newsnow)+scale(temp)+scale(newdust)+scale(rain),data=weather2))

#####다중선형회귀,의사결정회귀나무-모델 적합 및 예측, 평가
##베이커리
#다중선형회귀
reg_fit_bakery=lm(bakeryQ~newsnow+temp+newdust+rain,data=weather2) #회귀모델 만들기
summary(reg_fit_bakery) #회귀모형 결과
reg_pred_bakery=predict(reg_fit_bakery,weather2) #생성한 모델에 날씨 데이터 넣어 판매량 예측값 알아보기
cor(reg_pred_bakery,weather2$bakeryQ) #0.8395, 예측값과 실제 판매량 비교
mean((weather2$bakeryQ-reg_pred_bakery)^2) #예측값과 실제 판매량 오차

#의사결정회귀나무
tree_fit_bakery=rpart(bakeryQ~newdust+rain+newsnow+temp,data=weather2,method='anova',control=rpart.control(minsplit=3))
rpart.plot(tree_fit_bakery)
tree_pred_bakery=predict(tree_fit_bakery,newdata=weather2)
cor(tree_pred_bakery,weather2$bakeryQ) #0.9592
mean((weather2$bakeryQ-tree_pred_bakery)^2)

##음료
#다중선형회귀
reg_fit_drink=lm(drinkQ~newsnow+temp+newdust+rain,data=weather2)
summary(reg_fit_drink)
reg_pred_drink=predict(reg_fit_drink,weather2)
cor(reg_pred_drink,weather2$drinkQ) #0.6333
mean((weather2$drinkQ-reg_pred_drink)^2)

#의사결정회귀나무
tree_fit_drink=rpart(drinkQ~newdust+rain+newsnow+temp,data=weather2,method='anova',control=rpart.control(minsplit=3))
rpart.plot(tree_fit_drink)
tree_pred_drink=predict(tree_fit_drink,newdata=weather2)
cor(tree_pred_drink,weather2$drinkQ) #0.9815
mean((weather2$drinkQ-tree_pred_drink)^2)

##수산/건어물
#다중선형회귀
reg_fit_fish=lm(fishQ~newsnow+temp+newdust+rain,data=weather2)
summary(reg_fit_fish)
reg_pred_fish=predict(reg_fit_fish,weather2)
cor(reg_pred_fish,weather2$fishQ) #0.5724
mean((weather2$fishQ-reg_pred_fish)^2)

#의사결정회귀나무
tree_fit_fish=rpart(fishQ~newdust+rain+newsnow+temp,data=weather2,method='anova',control=rpart.control(minsplit=3))
rpart.plot(tree_fit_fish)
tree_pred_fish=predict(tree_fit_fish,newdata=weather2)
cor(tree_pred_fish,weather2$fishQ) #0.9645
mean((weather2$fishQ-tree_pred_fish)^2)

##과일
#다중선형회귀
reg_fit_fruit=lm(fruitQ~newsnow+temp+newdust+rain,data=weather2)
summary(reg_fit_fruit)
reg_pred_fruit=predict(reg_fit_fruit,weather2)
cor(reg_pred_fruit,weather2$fruitQ) #0.7829
mean((weather2$fruitQ-reg_pred_fruit)^2)

#의사결정회귀나무
tree_fit_fruit=rpart(fruitQ~newdust+rain+newsnow+temp,data=weather2,method='anova',control=rpart.control(minsplit=3))
rpart.plot(tree_fit_fruit)
tree_pred_fruit=predict(tree_fit_fruit,newdata=weather2)
cor(tree_pred_fruit,weather2$fruitQ) #0.9818
mean((weather2$fruitQ-tree_pred_fruit)^2)

##김치류
#다중선형회귀
reg_fit_kimchi=lm(kimchiQ~newsnow+temp+newdust+rain,data=weather2)
summary(reg_fit_kimchi)
reg_pred_kimchi=predict(reg_fit_kimchi,weather2)
cor(reg_pred_kimchi,weather2$kimchiQ) #0.6392
mean((weather2$kimchiQ-reg_pred_kimchi)^2)

#의사결정회귀나무
tree_fit_kimchi=rpart(kimchiQ~newdust+rain+newsnow+temp,data=weather2,method='anova',control=rpart.control(minsplit=3))
rpart.plot(tree_fit_kimchi)
tree_pred_kimchi=predict(tree_fit_kimchi,newdata=weather2)
cor(tree_pred_kimchi,weather2$kimchiQ) #0.9491
mean((weather2$kimchiQ-tree_pred_kimchi)^2)

##가공/건강식품
#다중회귀
reg_fit_manu=lm(manuQ~newsnow+temp+newdust+rain,data=weather2)
summary(reg_fit_manu)
reg_pred_manu=predict(reg_fit_manu,weather2)
cor(reg_pred_manu,weather2$manuQ) #0.3444
mean((weather2$manuQ-reg_pred_manu)^2)

#의사결정회귀나무
tree_fit_manu=rpart(manuQ~newdust+rain+newsnow+temp,data=weather2,method='anova',control=rpart.control(minsplit=3))
rpart.plot(tree_fit_manu)
tree_pred_manu=predict(tree_fit_manu,newdata=weather2)
cor(tree_pred_manu,weather2$manuQ) #0.9591
mean((weather2$manuQ-tree_pred_manu)^2)

##정육/계란
#다중회귀
reg_fit_meategg=lm(meatQ~newsnow+temp+newdust+rain,data=weather2)
summary(reg_fit_meategg)
reg_pred_meategg=predict(reg_fit_meategg,weather2)
cor(reg_pred_meategg,weather2$meatQ) #0.5782
mean((weather2$meatQ-reg_pred_meategg)^2)

#의사결정회귀나무
tree_fit_meategg=rpart(meatQ~newdust+rain+newsnow+temp,data=weather2,method='anova',control=rpart.control(minsplit=3))
rpart.plot(tree_fit_meategg)
tree_pred_meategg=predict(tree_fit_meategg,newdata=weather2)
cor(tree_pred_meategg,weather2$meatQ) #0.9880
mean((weather2$meatQ-tree_pred_meategg)^2)

##쌀/잡곡
#다중회귀
reg_fit_rice=lm(riceQ~newsnow+temp+newdust+rain,data=weather2)
summary(reg_fit_rice)
reg_pred_rice=predict(reg_fit_rice,weather2)
cor(reg_pred_rice,weather2$riceQ) #0.4658
mean((weather2$riceQ-reg_pred_rice)^2)

#의사결정회귀나무
tree_fit_rice=rpart(riceQ~newdust+rain+newsnow+temp,data=weather2,method='anova',control=rpart.control(minsplit=3))
rpart.plot(tree_fit_rice)
tree_pred_rice=predict(tree_fit_rice,newdata=weather2)
cor(tree_pred_rice,weather2$riceQ) #0.9627
mean((weather2$riceQ-tree_pred_rice)^2)

##장/양념/소스류
#다중회귀
reg_fit_sauce=lm(sauceQ~newsnow+temp+newdust+rain,data=weather2)
summary(reg_fit_sauce)
reg_pred_sauce=predict(reg_fit_sauce,weather2)
cor(reg_pred_sauce,weather2$sauceQ) #0.7789
mean((weather2$sauceQ-reg_pred_sauce)^2)

#의사결정회귀나무
tree_fit_sauce=rpart(sauceQ~newdust+rain+newsnow+temp,data=weather2,method='anova',control=rpart.control(minsplit=3))
rpart.plot(tree_fit_sauce)
tree_pred_sauce=predict(tree_fit_sauce,newdata=weather2)
cor(tree_pred_sauce,weather2$sauceQ) #0.9700
mean((weather2$sauceQ-tree_pred_sauce)^2)

##채소
#다중회귀
reg_fit_veget=lm(vegetQ~newsnow+temp+newdust+rain,data=weather2)
summary(reg_fit_veget)
reg_pred_veget=predict(reg_fit_veget,weather2)
cor(reg_pred_veget,weather2$vegetQ) #0.5446
mean((weather2$vegetQ-reg_pred_veget)^2)

#의사결정회귀나무
tree_fit_veget=rpart(vegetQ~newdust+rain+newsnow+temp,data=weather2,method='anova',control=rpart.control(minsplit=3))
rpart.plot(tree_fit_veget)
tree_pred_veget=predict(tree_fit_veget,newdata=weather2)
cor(tree_pred_veget,weather2$vegetQ) #0.9701
mean((weather2$vegetQ-tree_pred_veget)^2)

#####날씨에 따른 수요 예측
####예측할 20년 데이터
weather20=read.csv('weather20.csv',header=T)
weather20$newdust=as.integer(weather20$dust<=30)
weather20$newsnow=as.integer(weather20$snow>=5)
sum(is.na(weather20)) #결측치 없음

##2020년 판매량 예측
pred20=matrix(c(1:12),byrow=T)
pred20=as.data.frame(pred20)
colnames(pred20)='month'
pred20$bakeryQ=predict(tree_fit_bakery,newdata=weather20) #베이커리
pred20$drinkQ=predict(tree_fit_drink,newdata=weather20) #음료,유제품
pred20$fishQ=predict(tree_fit_fish,newdata=weather20) #수산,건어물
pred20$fruitQ=predict(tree_fit_fruit,newdata=weather20) #과일
pred20$kimchiQ=predict(tree_fit_kimchi,newdata=weather20) #김치치
pred20$manuQ=predict(tree_fit_manu,newdata=weather20) #가공,건강식품
pred20$meateggQ=predict(tree_fit_meategg,newdata=weather20) #정육,계란
pred20$riceQ=predict(tree_fit_rice,newdata=weather20) #쌀,잡곡곡
pred20$sauceQ=predict(tree_fit_sauce,newdata=weather20) #장,양념,소스류
pred20$vegetQ=predict(tree_fit_veget,newdata=weather20) #야채

ggplot(pred20,aes(x=month,y=bakeryQ))+geom_line()
ggplot(pred20,aes(x=month,y=drinkQ))+geom_line()
ggplot(pred20,aes(x=month,y=fishQ))+geom_line()
ggplot(pred20,aes(x=month,y=fruitQ))+geom_line()
ggplot(pred20,aes(x=month,y=kimchiQ))+geom_line()
ggplot(pred20,aes(x=month,y=manuQ))+geom_line()
ggplot(pred20,aes(x=month,y=meateggQ))+geom_line()
ggplot(pred20,aes(x=month,y=riceQ))+geom_line()
ggplot(pred20,aes(x=month,y=sauceQ))+geom_line()
ggplot(pred20,aes(x=month,y=vegetQ))+geom_line()