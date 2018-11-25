train<-readRDS("/Users/Erkin/Desktop/McGill/Fall/Decision Analytics/Assignment 3 attached files Nov 18, 2018 1032 PM/amz_train.rds")
attach(train)
table(pid)
table(sid)
library(stringr)
train$date <- as.Date( str_split_fixed(train$epoc,n=Inf,pattern = " ")[,1])
train$time <- str_split_fixed(train$epoc,n=Inf,pattern = " ")[,2]
train2=train[complete.cases(train), ]
detach(train)
attach(train2)
library(dplyr)

library(lubridate)

#### Q.1 ####

# part a
length(unique(unlist(train2[c("pid")]))) #9
length(unique(unlist(train2[c("sid")]))) #184

train2$seller_rating=ifelse(sid_rating %in% c(5),'high',ifelse(sid_rating %in% c(4.5,4),'medium','low'))

# part b
#groupby product
train2 %>% group_by(pid) %>% summarise(mean(price))
train2 %>% group_by(pid) %>% summarise(max(price))
train2 %>% group_by(pid) %>% summarise(min(price))

train2 %>% group_by(pid) %>% summarise(mean(bbox_price))
train2 %>% group_by(pid) %>% summarise(max(bbox_price))
train2 %>% group_by(pid) %>% summarise(min(bbox_price))

#groupby day
train2 %>% group_by(date) %>% summarise(mean(price))
train2 %>% group_by(date) %>% summarise(max(price))
train2 %>% group_by(date) %>% summarise(min(price))

train2 %>% group_by(date) %>% summarise(mean(bbox_price))
train2 %>% group_by(date) %>% summarise(max(bbox_price))
train2 %>% group_by(date) %>% summarise(min(bbox_price))



#groupby product
train3=train2 %>% group_by(pid,date) %>% mutate(avg_product_day=mean(price))
train3=train3 %>% group_by(pid,date) %>% mutate(min_product_day=min(price))
train3=train3 %>% group_by(pid,date) %>% mutate(max_product_day=max(price))
train3$is_free_shipping=ifelse(train3$shipping==0,'free','paid')
train3=train3 %>% group_by(pid,date) %>% mutate(is_lowest=ifelse(price==min_product_day,'yes','no'))
train3$is_amazon=ifelse(train3$bbox_sid=='amazon','amazon','other')
train3$rating_count=ifelse(train3$sid_rating_cnt>50,'high','low')
train3$pos_feedback_group=ifelse(train3$sid_pos_fb>7.5,'high',ifelse(train3$sid_pos_fb>2.5,'medium','low'))
train5=train3
train3$rank=as.numeric(train3$rank)+(as.numeric(train3$page)-1)*12

train4=train3
train3 <- as.data.frame(unclass(train3))

onur=train4 %>% group_by(pid,date,bbox_sid) %>% summarise()

others=train5[train3$sid!='amazon',]
amazon=train5[train3$sid=='amazon',]

otherwinner=train4[train4$bbox_sid!='amazon',]
otherwinner2=otherwinner[otherwinner$sid==otherwinner$bbox_sid,]
detach(train2)
attach(train3)




train2 %>% group_by(pid) %>% summarise(mean(bbox_price))
train2 %>% group_by(pid) %>% summarise(max(bbox_price))
train2 %>% group_by(pid) %>% summarise(min(bbox_price))

#groupby day
# train2=train2 %>% group_by(date) %>% (=mean(price))
train2 %>% group_by(date) %>% summarise(max(price))
train2 %>% group_by(date) %>% summarise(min(price))

train2 %>% group_by(date) %>% summarise(mean(bbox_price))
train2 %>% group_by(date) %>% summarise(max(bbox_price))
train2 %>% group_by(date) %>% summarise(min(bbox_price))






library(ggplot2)

# min max mean price changing per day
  pl = ggplot(train3) + geom_line(aes(x=date, y=avg_product_day, group=pid,color=pid))+
    geom_line(aes(x=date, y=max_product_day, group=pid,color=pid))+
    geom_line(aes(x=date, y=min_product_day, group=pid,color=pid))+theme(legend.position="top")+xlab("Date") +
    ylab("Price") +
    ggtitle("Daily change in max,min and average price of each product")
  
  pl + facet_wrap(~pid,scales = "free")  
  
#bbox price vs min price
  
asd= ggplot(train3) + geom_line(aes(x=date, y=bbox_price, group=pid))+ geom_line(aes(x=date, y=min_product_day, group=pid,color=pid))+theme(legend.position="top")+xlab("Date") +
  ylab("Price") +
 labs(title = "Daily change in min and bbox price of each product", subtitle = "Black line reprents buybox price, colored line represents minimum price")
asd + facet_wrap(~pid,scales = "free")  

  
sel1=count(train2,bars=seller_rating)
train2$week = week(train2$epoc)
amazon=train2%>%filter(sid=="amazon")
amazon_success=amazon%>%filter(bbox=="success")%>%group_by(week,pid)%>%summarise(cnt=n())
total=train2%>%filter(bbox=="success")%>%group_by(week,pid)%>%summarise(cnt=n())
divide_total=left_join(total,amazon_success,c("week","pid"))
divide_total[is.na(divide_total)]<-0
divide_total=divide_total%>%mutate(amazon_success=cnt.y,cnt.x)
divide_total$amazon_percentage=divide_total$cnt.y/divide_total$cnt.x*100

# ggplot(train2, aes(x=date)) + 
  #   geom_line(aes(y= col="psavert")) + 
  #   geom_line(aes(y=uempmed, col="uempmed")) + 
  #   labs(title="Time Series of Returns Percentage", 
  #        subtitle="Drawn From Wide Data format", 
  #        caption="Source: Economics", y="Returns %") +  # title and caption
  #   scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
  #   scale_color_manual(name="", 
  #                      values = c("psavert"="#00ba38", "uempmed"="#f8766d")) +  # line color
  #   theme(panel.grid.minor = element_blank())  # turn off minor grid
  # 






geom_smooth(aes(group=interaction(size, type)))
# splitting the data into two




#pie chart shows affect of fba

#1 winners fba
pie <- ggplot(otherwinner2, aes(x = "", fill = factor(otherwinner2$is_fba))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill=otherwinner2$is_fba, 
       x=NULL, 
       y=NULL, 
       title="Winners' is_fba")

pie + coord_polar(theta = "y", start=0)


#2 all sellers fba
pie <- ggplot(others, aes(x = "", fill = factor(others$is_fba))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill=others$is_fba, 
       x=NULL, 
       y=NULL, 
       title="All sellers' is_fba")

pie + coord_polar(theta = "y", start=0)


#sid_rating_cnt
mean(otherwinner2$sid_rating_cnt)
mean(others$sid_rating_cnt)

g <- ggplot(train3, aes(sid_rating_cnt)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=bbox), bins = 10,
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram of sid_rating_cnt", 
       subtitle="Correlation between sid_rating_cnt and bbox success")  




#sid_pos_fb
g <- ggplot(train3, aes(sid_pos_fb)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=bbox), bins = 10,
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram of sid_pos_fb", 
       subtitle="Correlation between sid_pos_fb and bbox success")  



# both 

g <- ggplot(train3, aes(sid_rating_cnt)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=feedback_group), bins = 10,
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram of sid_pos_fb", 
       subtitle="Correlation between sid_pos_fb and bbox success") 


#pie chart shows affect of prime


#1 winners prime
pie <- ggplot(otherwinner2, aes(x = "", fill = factor(otherwinner2$is_prime))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill=otherwinner2$is_prime, 
       x=NULL, 
       y=NULL, 
       title="Winners' is_Prime")

pie + coord_polar(theta = "y", start=0)


#2 all sellers prime
pie <- ggplot(others, aes(x = "", fill = factor(others$is_prime))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill=others$is_prime, 
       x=NULL, 
       y=NULL, 
       title="All sellers' is_Prime")

pie + coord_polar(theta = "y", start=0)



#pie chart shows affect of frees hipping


#1 winners free shipping
pie <- ggplot(otherwinner2, aes(x = "", fill = factor(otherwinner2$is_free_shipping))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill=otherwinner2$is_free_shipping, 
       x=NULL, 
       y=NULL, 
       title="Winners' Free Shipping")

pie + coord_polar(theta = "y", start=0)


#2 all sellers free shipping
pie <- ggplot(others, aes(x = "", fill = factor(others$is_free_shipping))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill=others$is_free_shipping, 
       x=NULL, 
       y=NULL, 
       title="All sellers' Free Shipping")

pie + coord_polar(theta = "y", start=0)

#1 winners is_lowest
pie <- ggplot(otherwinner2, aes(x = "", fill = factor(otherwinner2$is_lowest))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill=otherwinner2$is_lowest, 
       x=NULL, 
       y=NULL, 
       title="Winners' is_lowest")

pie + coord_polar(theta = "y", start=0)


#2 all sellers is_lowest
pie <- ggplot(others, aes(x = "", fill = factor(others$is_lowest))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill=others$is_lowest, 
       x=NULL, 
       y=NULL, 
       title="All sellers' is_lowest")

pie + coord_polar(theta = "y", start=0)


#1 winners rank
pie <- ggplot(otherwinner2, aes(x = "", fill = factor(otherwinner2$rank))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill=otherwinner2$rank, 
       x=NULL, 
       y=NULL, 
       title="Winners' rank")

pie + coord_polar(theta = "y", start=0)


#2 all sellers rank
pie <- ggplot(others, aes(x = "", fill = factor(others$rank))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill=others$rank, 
       x=NULL, 
       y=NULL, 
       title="All sellers' rank")

pie + coord_polar(theta = "y", start=0)



#seller rating

#1 winners seller rating
pie <- ggplot(otherwinner2, aes(x = "", fill = factor(otherwinner2$seller_rating))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill=otherwinner2$seller_rating, 
       x=NULL, 
       y=NULL, 
       title="Winners' seller rating")

pie + coord_polar(theta = "y", start=0)


#2 all sellers seller rating
pie <- ggplot(others, aes(x = "", fill = factor(others$seller_rating))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill=others$seller_rating, 
       x=NULL, 
       y=NULL, 
       title="All sellers' seller rating")

pie + coord_polar(theta = "y", start=0)



#average raing count










#summary data set which has prdocut date and bbox winner

#pie chart shows bbox winner

pie <- ggplot(onur, aes(x = "", fill = factor(onur$is_amazon))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill=onur$is_amazon, 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Buybox Winner")

pie + coord_polar(theta = "y", start=0)


#bar chart shows bbox winner
g <- ggplot(onur, aes(pid))
g + geom_bar(aes(fill=bbox_sid), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Buybox Winner Bar Chart", 
       subtitle="Sellers at Amazon", 
       caption="Note: Pink one represents amazon")




#### Random Forest ####
library(randomForest)
myforest= randomForest(bbox~sid_pos_fb+sid_rating_cnt+is_fba+is_prime+seller_rating+
                         is_free_shipping+is_lowest+is_amazon+pid_rating+pid_rating_cnt+rank+page
                         ,ntree=100,do.trace=5,data = train3,importance=TRUE)
myforest


importance(myforest)
varImpPlot(myforest)

detach(train3)
detach(train2)
detach(otherwinner)
attach(otherwinner)
#second random forest
myforest2= randomForest(bbox~sid_pos_fb+sid_rating_cnt+is_fba+is_prime+seller_rating+
                         is_free_shipping+is_lowest
                       ,ntree=100,do.trace=5,data = otherwinner,importance=TRUE)
myforest2


importance(myforest2)
varImpPlot(myforest2)




#### QUESTION 3 ####
library(bnlearn)
library(Rgraphviz)
# attach(train3)
# is_amazon, sid_rating_cnt, is_lowest, sid_pos_fb, is_prime, rank, seller_rating, bbox

df = subset(train3,select = c(is_amazon,sid_rating_cnt,is_lowest,sid_pos_fb,is_prime,rank,seller_rating,bbox))
df2 = subset(train3,select = c(is_amazon,sid_rating_cnt,is_lowest,is_prime,rank,seller_rating,bbox))
df3 = subset(train3,select = c(is_amazon,is_lowest,is_prime,rank,seller_rating,bbox))
df4 = subset(train3,select = c(is_amazon,sid_rating_cnt,is_lowest,sid_pos_fb,seller_rating,bbox))
df5 = subset(train3,select = c(is_amazon,is_lowest,is_prime,seller_rating,bbox,pos_feedback_group,rating_count))

#score based
amazon.hc<-hc(df5)
graphviz.plot(amazon.hc, layout="fdp")
score(amazon.hc,df5)
class(rank)

# amazon.tabu<-tabu(df5)
# graphviz.plot(amazon.tabu, layout="fdp")


#constraint based

amazon.mmpc<-mmpc(df5, undirected = FALSE)
graphviz.plot(amazon.mmpc,layout="fdp")

amazon.gs = gs(df5, undirected = FALSE)
graphviz.plot(amazon.gs,layout="fdp")

amazon.iamb = iamb(df5, undirected = FALSE)
graphviz.plot(amazon.iamb,layout="fdp")

amazon.hiton=si.hiton.pc(df5, undirected =  FALSE )
graphviz.plot(amazon.hiton,layout="fdp")



#hybrid
# amazon.mmhc=mmhc(df)
# graphviz.plot(amazon.mmhc,layout="fdp")

amazon.rsmax2 = rsmax2(df5)
graphviz.plot(amazon.rsmax2,layout="fdp")



bn.cv(df5, bn = "hc")
bn.cv(df5, bn = "gs")
bn.cv(df5, bn = "rsmax2")


bn.cv(df5, bn = "hc", method = "hold-out")
bn.cv(df5, bn = "gs", method = "hold-out")
bn.cv(df5, bn = "rsmax2", method = "hold-out")

#### QUESTION 4 ####


#part a
amazon.trained <- bn.fit(amazon.hc, df5, method = "bayes", iss=10)
print(amazon.trained)
bn.fit.barchart(amazon.trained$bbox)

#part b

set.seed(1257)
cpquery(amazon.trained,
        event = (bbox=='success'),evidence = (is_amazon=='amazon'),
       method="ls")

#part c
df6 = subset(df5,select = c(is_amazon,is_lowest,is_prime,seller_rating,pos_feedback_group,rating_count))

asd = predict(amazon.trained, node="bbox", df6, method = "bayes-lw")
mean(df5$bbox == asd)


#part d
test<-readRDS("/Users/Erkin/Desktop/McGill/Fall/Decision Analytics/Assignment 3 attached files Nov 18, 2018 1032 PM/amz_test.rds")
test=test[complete.cases(test), ]
test$date <- as.Date( str_split_fixed(test$epoc,n=Inf,pattern = " ")[,1])
test$time <- str_split_fixed(test$epoc,n=Inf,pattern = " ")[,2]
test=test %>% group_by(pid,date) %>% mutate(avg_product_day=mean(price))
test=test %>% group_by(pid,date) %>% mutate(min_product_day=min(price))
test=test %>% group_by(pid,date) %>% mutate(max_product_day=max(price))
test$is_free_shipping=ifelse(test$shipping==0,'free','paid')
test=test %>% group_by(pid,date) %>% mutate(is_lowest=ifelse(price==min_product_day,'yes','no'))
test$is_amazon=ifelse(test$bbox_sid=='amazon','amazon','other')
test$rating_count=ifelse(test$sid_rating_cnt>50,'high','low')
test$pos_feedback_group=ifelse(test$sid_pos_fb>7.5,'high',ifelse(test$sid_pos_fb>2.5,'medium','low'))
test$seller_rating=ifelse(test$sid_rating %in% c(5),'high',ifelse(test$sid_rating %in% c(4.5,4),'medium','low'))
train5=train3
test$rank=as.numeric(test$rank)+(as.numeric(test$page)-1)*12
test <- as.data.frame(unclass(test))
detach(train3)
attach(test)
test2=subset(test,select = c(is_amazon,is_lowest,is_prime,seller_rating,pos_feedback_group,rating_count))

nrow(test[test$bbox=='failure',])/nrow(test)

predictions = predict(amazon.trained, node="bbox", test2, method = "bayes-lw")
mean(test$bbox == predictions)

