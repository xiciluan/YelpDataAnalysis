setwd("~/Desktop/STAT 628/Module3")
category_count = read.csv("category_count.csv",header = FALSE, sep=",")
colnames(category_count)[1] = "Category"
colnames(category_count)[2] = "Count"

category_count = category_count[order(category_count$Count,decreasing=TRUE),]

library(ggplot2)
category = c("Restaurants","Shopping", "Beauty & Spas","Health & Medical",
          "Automotive", "Hotels & Travel","Other")

count = rep(0,length(category))
for(i in 1:(length(category)-1)){
  count[i]=category_count$Count[which(category_count$Category==category[i])]
}
count[length(count)] = 192609-sum(count)

plot_data = as.data.frame(cbind(category,count),row.names = FALSE)
plot_data$count = as.numeric(as.character(plot_data$count))
plot = ggplot(plot_data,aes(x=category,y=count,fill=category))+geom_bar(stat = "identity",width=1) #+ ggtitle("Number of Counts by Category") 
plot+xlab("Category")+ylab("Count")+theme(text = element_text(size=20),plot.title = element_text(face="bold",hjust = 0.5), axis.text.x=element_blank(),axis.ticks.x=element_blank())+ geom_text(stat="identity",aes(label=count), position = position_stack(vjust = 0.5),size=5)


plot_piechart = plot_data
prop = plot_data$count/192609
plot_piechart = cbind(plot_data,prop)
plot = ggplot(plot_data,aes(x="",y=prop,fill=category))+geom_bar(stat = "identity",width=1) 
plot = plot+geom_text(aes(label=paste0(round(prop*100),"%")),position = position_stack(vjust = 0.5))+coord_polar("y", start=0) 

plot + labs(x = NULL, y = NULL, fill = NULL)+ theme_classic() + theme(text=element_text(size=20),
                                                                             axis.line = element_blank(),
                                                                             axis.text = element_blank(),
                                                                             axis.ticks = element_blank())




# Pie chart for various types of restaurants
Types = c("Bars","Fast Food","Italian","Mexican","Chinese","Coffee","Japanese","Others","Steakhouse")
Number = c(11341,7251,4716,4618,4428,3232,2716,3567,1603)
restaurant_prop = Number/sum(Number)
restaurant_dat = as.data.frame(cbind(Types,Number,restaurant_prop))
restaurant_dat$Number = as.numeric(as.character(restaurant_dat$Number))
restaurant_dat$restaurant_prop = as.numeric(as.character(restaurant_dat$restaurant_prop))
pie_chart = ggplot(restaurant_dat,aes(x="",y=restaurant_prop,fill=Types))+geom_bar(stat = "identity",width=1) 
pie_chart = pie_chart+geom_text(aes(label=paste0(round(restaurant_prop*100),"%")),position = position_stack(vjust = 0.5))+coord_polar("y", start=0) 

pie_chart + labs(x = NULL, y = NULL, fill = NULL)+ theme_classic() + theme(text=element_text(size=20),
                                                                           axis.line = element_blank(),
                                                                           axis.text = element_blank(),
                                                                           axis.ticks = element_blank())
