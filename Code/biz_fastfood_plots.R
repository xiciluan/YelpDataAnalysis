setwd("~/Desktop/STAT 628/Module3/Data")
biz_fastfood = read.csv("biz_fast_food.csv",header=TRUE,na.strings = "")
variable.names(biz_fastfood) 
str(biz_fastfood)
columns = biz_fastfood[,c(3,4)]
biz_fastfood = biz_fastfood[,-c(3,4)]
biz_fastfood = cbind(biz_fastfood,columns)

biz_fastfood[which(biz_fastfood[,4]=="None"),4] <- "False"
biz_fastfood[which(biz_fastfood[,5]=="None"),5] <- "False"
biz_fastfood[which(biz_fastfood[,7]=="None"),7] <- "False"
biz_fastfood[which(biz_fastfood[,9]=="None"),9] <- "no"
biz_fastfood[which(biz_fastfood[,6]=="None"),6] <- NA
biz_fastfood[which(biz_fastfood[,8]=="None"),8] <- NA

# Drop extra levels
biz_fastfood$RestaurantsDelivery = droplevels(biz_fastfood$RestaurantsDelivery)
biz_fastfood$RestaurantsReservations = droplevels(biz_fastfood$RestaurantsReservations)
biz_fastfood$RestaurantsTakeOut = droplevels(biz_fastfood$RestaurantsTakeOut)
biz_fastfood$WiFi = droplevels(biz_fastfood$WiFi)
biz_fastfood$NoiseLevel = droplevels(biz_fastfood$NoiseLevel)
biz_fastfood$RestaurantsPriceRange2 = droplevels(biz_fastfood$RestaurantsPriceRange2)

# Fit anova
fit = lm(stars~.,data=biz_fastfood[,-c(1,10,11)])
summary(fit)
anova(fit)

fit_no_takeout = lm(stars~BusinessParking+RestaurantsDelivery+RestaurantsReservations+NoiseLevel+RestaurantsPriceRange2+WiFi,data = biz_fastfood)
anova(fit_no_takeout)

fit_no_takeout_no_wifi = lm(stars~BusinessParking+RestaurantsDelivery+RestaurantsReservations+NoiseLevel+RestaurantsPriceRange2,data=biz_fastfood)
anova(fit_no_takeout_no_wifi)


library(xtable)
print(xtable(anova(fit)))


# Plotting
biz_fastfood$stars[which(biz_fastfood$stars==1.5)] = floor(biz_fastfood$stars[which(biz_fastfood$stars==1.5)])
biz_fastfood$stars[which(biz_fastfood$stars==2.5)] = floor(biz_fastfood$stars[which(biz_fastfood$stars==2.5)])
biz_fastfood$stars[which(biz_fastfood$stars==3.5)] = ceiling(biz_fastfood$stars[which(biz_fastfood$stars==3.5)])
biz_fastfood$stars[which(biz_fastfood$stars==4.5)] = ceiling(biz_fastfood$stars[which(biz_fastfood$stars==4.5)])


## Restaurant Delivery
# Plot Restaurant Delivery by Stars (Proportion)
library(ggplot2)
library(wesanderson)

plot_RD = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$RestaurantsDelivery)),aes(x=stars,fill=RestaurantsDelivery))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..]),position = position_dodge()) + ggtitle("Restaurants Delivery") 
plot_RD + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot Restaurant Delivery by Stars (Count)
plot_RD = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$RestaurantsDelivery)),aes(x=stars,fill=RestaurantsDelivery))+geom_histogram(breaks=seq(1,5, by =1),col="black",position = position_dodge()) + ggtitle("Restaurants Delivery") 
plot_RD + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(face="bold",hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars")+ylab("Count")
  
# Plot Average Rating by Restaurant Delivery
RD_false_index= which(biz_fastfood$RestaurantsDelivery=="False")
RD_true_index = which(biz_fastfood$RestaurantsDelivery=="True")
avg_rating_RD = c(mean(biz_fastfood$stars[RD_false_index]),mean(biz_fastfood$stars[RD_true_index]))
rd = c("False","True")
avg_rating_RD_dat = as.data.frame(cbind(avg_rating_RD,rd))
avg_rating_RD_dat$avg_rating_RD=as.numeric(as.character(avg_rating_RD_dat$avg_rating_RD))
colnames(avg_rating_RD_dat)[1] = "AverageRating"; colnames(avg_rating_RD_dat)[2] = "RestaurantsDelivery"
plot_avg_rating_RD = ggplot(avg_rating_RD_dat,aes(x=RestaurantsDelivery,y= AverageRating,fill= RestaurantsDelivery))+geom_bar(stat = "identity") 
plot_avg_rating_RD+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Restaurants Delivery") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))

library(car)
# 2.049e-05
fligner.test(biz_fastfood$stars ~ biz_fastfood$RestaurantsDelivery)


## Business Parking
# Plot Business Parking by Stars (Proportion)
plot_BP = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$BusinessParking)),aes(x=stars,fill=BusinessParking))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..]),position = position_dodge()) + ggtitle("Business Parking") 
plot_BP + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot Business Parking by Stars (Count)
plot_BP = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$BusinessParking)),aes(x=stars,fill=BusinessParking))+geom_histogram(breaks=seq(1,5, by =1),col="black",position = position_dodge()) + ggtitle("Business Parking") 
plot_BP + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars") +ylab("Count")

# Plot Average Rating by Business Parking
BP_false_index= which(biz_fastfood$BusinessParking=="False")
BP_true_index = which(biz_fastfood$BusinessParking=="True")
avg_rating_BP = c(mean(biz_fastfood$stars[BP_false_index]),mean(biz_fastfood$stars[BP_true_index]))
bp = c("False","True")
avg_rating_BP_dat = as.data.frame(cbind(avg_rating_BP,bp))
avg_rating_BP_dat$avg_rating_BP=as.numeric(as.character(avg_rating_BP_dat$avg_rating_BP))
colnames(avg_rating_BP_dat)[1] = "AverageRating"; colnames(avg_rating_BP_dat)[2] = "BusinessParking"
plot_avg_rating_BP = ggplot(avg_rating_BP_dat,aes(x=BusinessParking,y= AverageRating,fill= BusinessParking))+geom_bar(stat = "identity") 
plot_avg_rating_BP+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Business Parking") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))

fligner.test(biz_fastfood$stars ~ biz_fastfood$BusinessParking)


## Restaurants Reservations
# Plot Restaurnts Reservations by Stars (Proportion)
plot_RR = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$RestaurantsReservations)),aes(x=stars,fill=RestaurantsReservations))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..]),position = position_dodge()) + ggtitle("Restaurants Reservations") 
plot_RR + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot Restaurnts Reservations by Stars (Count)
plot_RR = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$RestaurantsReservations)),aes(x=stars,fill=RestaurantsReservations))+geom_histogram(breaks=seq(1,5, by =1),col="black",position = position_dodge()) + ggtitle("Restaurants Reservations") 
plot_RR + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars")+ylab("Count")

# Plot Average Rating by Restaurant Reservations
RR_false_index= which(biz_fastfood$RestaurantsReservations=="False")
RR_true_index = which(biz_fastfood$RestaurantsReservations=="True")
avg_rating_RR = c(mean(biz_fastfood$stars[RR_false_index]),mean(biz_fastfood$stars[RR_true_index]))
rr = c("False","True")
avg_rating_RR_dat = as.data.frame(cbind(avg_rating_RR,rr))
avg_rating_RR_dat$avg_rating_RR=as.numeric(as.character(avg_rating_RR_dat$avg_rating_RR))
colnames(avg_rating_RR_dat)[1] = "AverageRating"; colnames(avg_rating_RR_dat)[2] = "RestaurantsReservations"
plot_avg_rating_RR = ggplot(avg_rating_RR_dat,aes(x=RestaurantsReservations,y= AverageRating,fill= RestaurantsReservations))+geom_bar(stat = "identity") 
plot_avg_rating_RR+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Restaurants Reservations") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))

fligner.test(biz_fastfood$stars ~ biz_fastfood$RestaurantsReservations)


## Noise level
# Plot Noise level by Stars (Proportion)
biz_fastfood$NoiseLevel = factor(biz_fastfood$NoiseLevel,level = c("quiet","average","loud","very_loud","very loud"))
biz_fastfood$NoiseLevel[which(biz_fastfood$NoiseLevel=="very_loud")] = "very loud"
biz_fastfood$NoiseLevel = droplevels(biz_fastfood$NoiseLevel)
plot_NL = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$NoiseLevel)),aes(x=stars,fill=NoiseLevel))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..])) + ggtitle("Noise Level") 
plot_NL + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=4,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot Noise level by Stars (Count)
plot_NL = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$NoiseLevel)),aes(x=stars,fill=NoiseLevel))+geom_histogram(breaks=seq(1,5, by =1)) + ggtitle("Noise Level") 
plot_NL + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=5,name="Darjeeling1"))+
  xlab("Stars")+ylab("Count")

# Plot Average Rating by Noise Level
NL_average_index= which(biz_fastfood$NoiseLevel=="average")
NL_loud_index = which(biz_fastfood$NoiseLevel=="loud")
NL_quiet_index = which(biz_fastfood$NoiseLevel=="quiet")
NL_veryloud_index = which(biz_fastfood$NoiseLevel=="very loud")
avg_rating_NL = c(mean(biz_fastfood$stars[NL_average_index]),mean(biz_fastfood$stars[NL_loud_index]),mean(biz_fastfood$stars[NL_quiet_index]),mean(biz_fastfood$stars[NL_veryloud_index]))
NL = c("average","loud","quiet","very loud")
avg_rating_NL_dat = as.data.frame(cbind(avg_rating_NL,NL))
avg_rating_NL_dat$avg_rating_NL=as.numeric(as.character(avg_rating_NL_dat$avg_rating_NL))
avg_rating_NL_dat$NL = factor(avg_rating_NL_dat$NL,levels= c("quiet","average","loud","very loud"))
colnames(avg_rating_NL_dat)[1] = "AverageRating"; colnames(avg_rating_NL_dat)[2] = "NoiseLevel"
plot_avg_rating_NL = ggplot(avg_rating_NL_dat,aes(x=NoiseLevel,y= AverageRating,fill= NoiseLevel))+geom_bar(stat = "identity") 
plot_avg_rating_NL+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Noise Level") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=5,name="Darjeeling1"))

fligner.test(biz_fastfood$stars ~ biz_fastfood$NoiseLevel)


## Restaurants Take Out
# Plot Restaurants Take Out by Stars (Proportion)
plot_TO = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$RestaurantsTakeOut)),aes(x=stars,fill=RestaurantsTakeOut))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..]),position = position_dodge()) + ggtitle("Restaurants Take Out") 
plot_TO + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot Restaurants Take Out by Stars (Count)
plot_TO = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$RestaurantsTakeOut)),aes(x=stars,fill=RestaurantsTakeOut))+geom_histogram(breaks=seq(1,5, by =1),col="black",position=position_dodge()) + ggtitle("Restaurants Take Out") 
plot_TO + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars")+ylab("Count")

# Plot Average Rating by Restaurant Take Out
TO_false_index= which(biz_fastfood$RestaurantsTakeOut=="False")
TO_true_index = which(biz_fastfood$RestaurantsTakeOut=="True")
avg_rating_TO = c(mean(biz_fastfood$stars[TO_false_index]),mean(biz_fastfood$stars[TO_true_index]))
to = c("False","True")
avg_rating_TO_dat = as.data.frame(cbind(avg_rating_TO,to))
avg_rating_TO_dat$avg_rating_TO=as.numeric(as.character(avg_rating_TO_dat$avg_rating_TO))
colnames(avg_rating_TO_dat)[1] = "AverageRating"; colnames(avg_rating_TO_dat)[2] = "RestaurantsTakeOut"
plot_avg_rating_TO = ggplot(avg_rating_TO_dat,aes(x=RestaurantsTakeOut,y= AverageRating,fill= RestaurantsTakeOut))+geom_bar(stat = "identity") + ggtitle("Average Rating") 
plot_avg_rating_TO+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Restaurants Take Out") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))

fligner.test(biz_fastfood$stars ~ biz_fastfood$RestaurantsTakeOut)


## PriceRange
# Plot Price Range by Stars (Proportion)
plot_PR = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$RestaurantsPriceRange2)),aes(x=stars,fill=RestaurantsPriceRange2))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..])) + ggtitle("Restaurants Price Range") 
plot_PR + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=4,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot PriceRange by Stars (Count)
plot_PR = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$RestaurantsPriceRange2)),aes(x=stars,fill=RestaurantsPriceRange2))+geom_histogram(breaks=seq(1,5, by =1),col="black",position = position_dodge()) + ggtitle("Restaurants Price Range") 
plot_PR + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=5,name="Darjeeling1"))+
  xlab("Stars") +ylab("Count")

# Plot Average rating by Price Range
PR_1_index = which(biz_fastfood$RestaurantsPriceRange2==1)
PR_2_index = which(biz_fastfood$RestaurantsPriceRange2==2)
PR_3_index = which(biz_fastfood$RestaurantsPriceRange2==3)
PR_4_index = which(biz_fastfood$RestaurantsPriceRange2==4)
avg_rating_PR = c(mean(biz_fastfood$stars[PR_1_index]),mean(biz_fastfood$stars[PR_2_index]),mean(biz_fastfood$stars[PR_3_index]),mean(biz_fastfood$stars[PR_4_index]))
PR = c("1","2","3","4")
avg_rating_PR_dat = as.data.frame(cbind(avg_rating_PR,PR))
avg_rating_PR_dat$avg_rating_PR=as.numeric(as.character(avg_rating_PR_dat$avg_rating_PR))
colnames(avg_rating_PR_dat)[1] = "AverageRating"; colnames(avg_rating_PR_dat)[2] = "PriceRange"
plot_avg_rating_PR = ggplot(avg_rating_PR_dat,aes(x=PriceRange,y= AverageRating,fill= PriceRange))+geom_bar(stat = "identity") 
plot_avg_rating_PR+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Price Range") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=5,name="Darjeeling1"))

fligner.test(biz_fastfood$stars ~ biz_fastfood$RestaurantsPriceRange2)


## WiFi
# Plot WiFi by Stars (Proportion)
biz_fastfood$WiFi = factor(biz_fastfood$WiFi,levels = c("no","free","paid"))
plot_WF = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$WiFi)),aes(x=stars,fill=WiFi))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..])) + ggtitle("WiFi") 
plot_WF + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=4,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot WiFi by Stars (Count)
plot_WF = ggplot(subset(biz_fastfood,!is.na(biz_fastfood$WiFi)),aes(x=stars,fill=WiFi))+geom_histogram(breaks=seq(1,5, by = 1),col="black",position=position_dodge()) + ggtitle("WiFi") 
plot_WF + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=3,name="Darjeeling1"))+
  xlab("Stars") +ylab("Count")

# Plot Average Rating by WiFi
WF_free_index= which(biz_fastfood$WiFi=="free")
WF_no_index = which(biz_fastfood$WiFi=="no")
WF_paid_index = which(biz_fastfood$WiFi=="paid")
avg_rating_WF = c(mean(biz_fastfood$stars[WF_free_index]),mean(biz_fastfood$stars[WF_no_index]),mean(biz_fastfood$stars[WF_paid_index]))
WF = c("free","no","paid")
avg_rating_WF_dat = as.data.frame(cbind(avg_rating_WF,WF))
avg_rating_WF_dat$avg_rating_WF=as.numeric(as.character(avg_rating_WF_dat$avg_rating_WF))
colnames(avg_rating_WF_dat)[1] = "AverageRating"; colnames(avg_rating_WF_dat)[2] = "WiFi"
avg_rating_WF_dat$WiFi = factor(avg_rating_WF_dat$WiFi,levels=c("no","free","paid"))
plot_avg_rating_WF = ggplot(avg_rating_WF_dat,aes(x=WiFi,y= AverageRating,fill= WiFi))+geom_bar(stat = "identity") 
plot_avg_rating_WF+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("WiFi") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=3,name="Darjeeling1"))

fligner.test(biz_fastfood$stars ~ biz_fastfood$WiFi)


# Plot fastfood restaurants on world map
library(ggmap)
library(rworldmap)
library(maps)

ggplot(data=biz_fastfood,aes(color=stars)) + borders("world",colour = "gray", fill="gray") +geom_point(aes(x=longitude,y=latitude),size=1) +scale_color_gradient2(low="blue",high="red")+
  xlim(-200,0)+ylim(0,100)+ylab("Latitude")+xlab("Longitude")+theme(text = element_text(size=20))
 




