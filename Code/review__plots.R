setwd("~/Desktop/STAT 628/Module3/Data")
review_summary = read.csv("review_summary.csv",header=TRUE)
variable.names(review_summary) 
str(review_summary)

library(stringr)
review_summary[,1] = word(review_summary[,1],1)
review_summary[,1] = as.Date(review_summary[,1])
review_summary[,1] = str_sub(review_summary[,1],start=1,end=-4)


# Number of reviews by year
library(dplyr)
library(ggplot2)
library(wesanderson)
e = review_summary
e[,1] = str_sub(e[,1],start=1,end=-4)
yearly_count = e %>% count(date,stars)
ggplot(data=yearly_count,mapping=aes(x=date,y=n,group=stars,color=stars))+geom_line() +
  scale_x_discrete(breaks=c(seq(2004,2018,2))) + geom_area(aes(fill=stars))+
  ggtitle("Number of Review Submissions by Year")+ylab("Number of Reviews")+xlab("Year")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),panel.grid.major.x = element_blank())


# Proportion of rating by year
total_num_reviews_year = NULL
year = 2004:2018
for(i in 1:15){
  total_num_reviews_year[i] = sum(yearly_count$n[which(yearly_count$date==year[i])])
}
total_year = c(rep(total_num_reviews_year[1],2),rep(total_num_reviews_year[2:15],each=5))


yearly_count = cbind(yearly_count,total_year)
prop = c(rep(0,nrow(yearly_count)))
yearly_count = cbind(yearly_count,prop)

for(i in 1:nrow(yearly_count)){
  yearly_count[i,5] = yearly_count$n[i]/yearly_count[i,4]
}

ggplot(data=yearly_count,mapping=aes(x=date,y=prop,group=stars,color=stars))+geom_line() +
  scale_x_discrete(breaks=c(seq(2004,2018,2))) + geom_area(aes(fill=stars))+
  ggtitle("Proportion of Review Submissions by Year")+ylab("Proportion of Reviews")+xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5),panel.grid.major.x = element_blank())





    