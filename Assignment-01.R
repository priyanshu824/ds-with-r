library("tidyverse")
library("rvest")
#a
html <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
main_table = html%>%html_table()
data=data.frame(main_table[1])
data=data[,-1]
#problem c1
tennis <- function(p)
{
  a=rbinom(n=5,size=1,prob=p)
  b=0
  c=0
  d=0
  for(i in a){
    if(c>=3) {return(b)}
    if(d>=3) {return(b)}
    if(d==1){
      b=b+1
    }
    else{
      d=d+1
    }
    b=b+1
    
  }
  return(b)
}
#problem c2
matches = {}
for(i in 1:1000) 
{
  matches[i] <- tennis(0.7)
}
ans <-mean(matches)
#problem D
MontyHall = function(){
  c=sample(x=1:3,size = 1)
  if(c ==1) {
    return(1)
  }
  if(c==2) {
    return(1)
  }
  else {
    return(0)
  }
  if(c==3){
    return(1)
  }
  else {
    return(0)
  }
}

results = {}
for(i in 1:1000) 
{
  results[i] <- MontyHall()
}
ans <- sum(results)/1000
#problem E
html3 = read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
ranking=html3%>%html_elements(".countdown-index")%>%html_text()
name=html3%>%html_elements(".article_movie_title a")%>%html_text()
t=html3%>%html_elements(".tMeterScore")%>%html_text()
y=html3%>%html_elements(".start-year")%>%html_text()

data3=data.frame(Ranking=ranking, Movie=name,Tomato_Score=t,Year=y)
#problem b
html201=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/telecom/telecommunication-service-provider/bharti-airtel/company-info")
a=html201%>%html_table()
a1=data.frame(a[1])  
a1<-a1[-c(1,2,3,4,5),]
a1<-a1[,-c(12,13,14)]
a2=data.frame(a[3])
a2<-a2[,-c(12,13)]
a1 <- a1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
a2 <- a2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
a=rbind(a1,a2)
a<-a[-c(9),]
row.names(a)=c(1:14)


html202=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/power/power-generation-distribution/power-grid-corp/company-info")
b=html202%>%html_table()
b1=data.frame(a[1])  
b1<-b1[-c(1,2,3,4,5),]
b1<-b1[,-c(12,13,14)]
b2=data.frame(a[3])
b2<-b2[,-c(12,13)]
b1 <- b1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
b2 <- b2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
b=rbind(b1,b2)
b<-b[-c(9),]
row.names(b)=c(1:14)


html203=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/automobiles/automobile-two-three-wheelers/hero-motocorp/company-info")
c=html202%>%html_table()
c1=data.frame(a[1])  
c1<-c1[-c(1,2,3,4,5),]
c1<-c1[,-c(12,13,14)]
c2=data.frame(a[3])
c2<-c2[,-c(12,13)]
c1 <- c1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
c2 <- c2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
c=rbind(c1,c2)
c<-b[-c(9),]
row.names(b)=c(1:14)

html204=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/automobiles/automobiles-passenger-cars/maruti-suzuki/company-info")
d=html202%>%html_table()
d1=data.frame(a[1])  
d1<-d1[-c(1,2,3,4,5),]
d1<-d1[,-c(12,13,14)]
d2=data.frame(a[3])
d2<-d2[,-c(12,13)]
d1 <- d1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
d2 <- d2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
d=rbind(d1,d2)
d<-b[-c(9),]
row.names(b)=c(1:14)



