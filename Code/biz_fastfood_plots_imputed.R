setwd("~/Desktop/STAT 628/Module3/Data")
biz_fastfood = read.csv("biz_imputed.csv",header=TRUE)
variable.names(biz_fastfood) 
str(biz_fastfood)
columns = biz_fastfood[,c(3,4)]
biz_fastfood = biz_fastfood[,-c(3,4)]
biz_fastfood = cbind(biz_fastfood,columns)

# Change to factor
library(tidyverse)
biz_fastfood = mutate_if(biz_fastfood, is.character, as.factor)
biz_fastfood = mutate_if(biz_fastfood, is.logical, as.factor)
biz_fastfood$RestaurantsPriceRange2 = as.factor(biz_fastfood$RestaurantsPriceRange2)


# Fit anova
## TakeOut, Wifi not significant
fit = lm(stars~.,data=biz_fastfood)
summary(fit)
anova(fit)


## TakeOut not significant
# TakeOut p = 0.326  
fit_no_wifi = lm(stars~.,data = biz_fastfood[,-7])
anova(fit_no_wifi)
anova(fit,fit_no_wifi) # p = 0.000137, fit is better

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

plot_RD = ggplot(biz_fastfood,aes(x=stars,fill=RestaurantsDelivery))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..]),position = position_dodge()) + ggtitle("Restaurants Delivery") 
plot_RD + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot Restaurant Delivery by Stars (Count)
plot_RD = ggplot(biz_fastfood,aes(x=stars,fill=RestaurantsDelivery))+geom_histogram(breaks=seq(1,5, by =1),col="black",position = position_dodge()) + ggtitle("Restaurants Delivery") 
plot_RD + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(face="bold",hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars")+ylab("Count")
  
# Plot Average Rating by Restaurant Delivery
RD_false_index= which(biz_fastfood$RestaurantsDelivery=="FALSE")
RD_true_index = which(biz_fastfood$RestaurantsDelivery=="TRUE")
avg_rating_RD = c(mean(biz_fastfood$stars[RD_false_index]),mean(biz_fastfood$stars[RD_true_index]))
rd = c("False","True")
avg_rating_RD_dat = as.data.frame(cbind(avg_rating_RD,rd))
avg_rating_RD_dat$avg_rating_RD=as.numeric(as.character(avg_rating_RD_dat$avg_rating_RD))
colnames(avg_rating_RD_dat)[1] = "AverageRating"; colnames(avg_rating_RD_dat)[2] = "RestaurantsDelivery"
plot_avg_rating_RD = ggplot(avg_rating_RD_dat,aes(x=RestaurantsDelivery,y= AverageRating,fill= RestaurantsDelivery))+geom_bar(stat = "identity") 
plot_avg_rating_RD+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Restaurants Delivery") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))

library(car)
# p < 2.2e-16 
leveneTest(biz_fastfood$stars ~ biz_fastfood$RestaurantsDelivery)

# two-sample t-test
# p-value < 2.2e-16
# not equal
t.test(biz_fastfood$stars[RD_false_index],biz_fastfood$stars[RD_true_index],
       paired = FALSE,var.equal = FALSE)


## Business Parking
# Plot Business Parking by Stars (Proportion)
plot_BP = ggplot(biz_fastfood,aes(x=stars,fill=BusinessParking))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..]),position = position_dodge()) + ggtitle("Business Parking") 
plot_BP + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot Business Parking by Stars (Count)
plot_BP = ggplot(biz_fastfood,aes(x=stars,fill=BusinessParking))+geom_histogram(breaks=seq(1,5, by =1),col="black",position = position_dodge()) + ggtitle("Business Parking") 
plot_BP + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars") +ylab("Count")

# Plot Average Rating by Business Parking
BP_false_index= which(biz_fastfood$BusinessParking=="FALSE")
BP_true_index = which(biz_fastfood$BusinessParking=="TRUE")
avg_rating_BP = c(mean(biz_fastfood$stars[BP_false_index]),mean(biz_fastfood$stars[BP_true_index]))
bp = c("False","True")
avg_rating_BP_dat = as.data.frame(cbind(avg_rating_BP,bp))
avg_rating_BP_dat$avg_rating_BP=as.numeric(as.character(avg_rating_BP_dat$avg_rating_BP))
colnames(avg_rating_BP_dat)[1] = "AverageRating"; colnames(avg_rating_BP_dat)[2] = "BusinessParking"
plot_avg_rating_BP = ggplot(avg_rating_BP_dat,aes(x=BusinessParking,y= AverageRating,fill= BusinessParking))+geom_bar(stat = "identity") 
plot_avg_rating_BP+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Business Parking") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))

# p < 2.2e-16
leveneTest(biz_fastfood$stars ~ biz_fastfood$BusinessParking)
# p-value < 2.2e-16
t.test(biz_fastfood$stars[BP_false_index],biz_fastfood$stars[BP_true_index],
       paired = FALSE,var.equal = FALSE)


## Outdoor Seating
# Plot Outdoor Seating by Stars (Proportion)
plot_OS = ggplot(biz_fastfood,aes(x=stars,fill=OutdoorSeating))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..]),position = position_dodge()) + ggtitle("Outdoor Seating") 
plot_OS + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot Average Rating by Outdoor Seating
OS_false_index= which(biz_fastfood$OutdoorSeating=="FALSE")
OS_true_index = which(biz_fastfood$OutdoorSeating=="TRUE")
avg_rating_OS = c(mean(biz_fastfood$stars[OS_false_index]),mean(biz_fastfood$stars[OS_true_index]))
OS = c("False","True")
avg_rating_OS_dat = as.data.frame(cbind(avg_rating_OS,OS))
avg_rating_OS_dat$avg_rating_OS=as.numeric(as.character(avg_rating_OS_dat$avg_rating_OS))
colnames(avg_rating_OS_dat)[1] = "AverageRating"; colnames(avg_rating_OS_dat)[2] = "OutdoorSeating"
plot_avg_rating_OS = ggplot(avg_rating_OS_dat,aes(x=OutdoorSeating,y= AverageRating,fill= OutdoorSeating))+geom_bar(stat = "identity") 
plot_avg_rating_OS+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Outdoor Seating") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))

# p < 2.2e-16
leveneTest(biz_fastfood$stars ~ biz_fastfood$OutdoorSeating)
# p-value < 2.2e-16
t.test(biz_fastfood$stars[OS_false_index],biz_fastfood$stars[OS_true_index],
       paired = FALSE,var.equal = FALSE)



## Restaurants Reservations
# Plot Restaurnts Reservations by Stars (Proportion)
plot_RR = ggplot(biz_fastfood,aes(x=stars,fill=RestaurantsReservations))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..]),position = position_dodge()) + ggtitle("Restaurants Reservations") 
plot_RR + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot Restaurnts Reservations by Stars (Count)
plot_RR = ggplot(biz_fastfood,aes(x=stars,fill=RestaurantsReservations))+geom_histogram(breaks=seq(1,5, by =1),col="black",position = position_dodge()) + ggtitle("Restaurants Reservations") 
plot_RR + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars")+ylab("Count")

# Plot Average Rating by Restaurant Reservations
RR_false_index= which(biz_fastfood$RestaurantsReservations=="FALSE")
RR_true_index = which(biz_fastfood$RestaurantsReservations=="TRUE")
avg_rating_RR = c(mean(biz_fastfood$stars[RR_false_index]),mean(biz_fastfood$stars[RR_true_index]))
rr = c("False","True")
avg_rating_RR_dat = as.data.frame(cbind(avg_rating_RR,rr))
avg_rating_RR_dat$avg_rating_RR=as.numeric(as.character(avg_rating_RR_dat$avg_rating_RR))
colnames(avg_rating_RR_dat)[1] = "AverageRating"; colnames(avg_rating_RR_dat)[2] = "RestaurantsReservations"
plot_avg_rating_RR = ggplot(avg_rating_RR_dat,aes(x=RestaurantsReservations,y= AverageRating,fill= RestaurantsReservations))+geom_bar(stat = "identity") 
plot_avg_rating_RR+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Restaurants Reservations") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))

# p < 2.2e-16 
leveneTest(biz_fastfood$stars ~ biz_fastfood$RestaurantsReservations)
# p-value < 2.2e-16
t.test(biz_fastfood$stars[RR_false_index],biz_fastfood$stars[RR_true_index],
       paired = FALSE,var.equal = FALSE)


## Noise level
# Plot Noise level by Stars (Proportion)
biz_fastfood$NoiseLevel = factor(biz_fastfood$NoiseLevel,level = c("QUIET","AVERAGE","LOUD","VERY_LOUD","VERY LOUD"))
biz_fastfood$NoiseLevel[which(biz_fastfood$NoiseLevel=="VERY_LOUD")] = "VERY LOUD"
biz_fastfood$NoiseLevel = droplevels(biz_fastfood$NoiseLevel)
plot_NL = ggplot(biz_fastfood,aes(x=stars,fill=NoiseLevel))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..])) + ggtitle("Noise Level") 
plot_NL + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=4,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")


# Plot Average Rating by Noise Level
NL_average_index= which(biz_fastfood$NoiseLevel=="AVERAGE")
NL_loud_index = which(biz_fastfood$NoiseLevel=="LOUD")
NL_quiet_index = which(biz_fastfood$NoiseLevel=="QUIET")
NL_veryloud_index = which(biz_fastfood$NoiseLevel=="VERY LOUD")
avg_rating_NL = c(mean(biz_fastfood$stars[NL_average_index]),mean(biz_fastfood$stars[NL_loud_index]),mean(biz_fastfood$stars[NL_quiet_index]),mean(biz_fastfood$stars[NL_veryloud_index]))
NL = c("average","loud","quiet","very loud")
avg_rating_NL_dat = as.data.frame(cbind(avg_rating_NL,NL))
avg_rating_NL_dat$avg_rating_NL=as.numeric(as.character(avg_rating_NL_dat$avg_rating_NL))
avg_rating_NL_dat$NL = factor(avg_rating_NL_dat$NL,levels= c("quiet","average","loud","very loud"))
colnames(avg_rating_NL_dat)[1] = "AverageRating"; colnames(avg_rating_NL_dat)[2] = "NoiseLevel"
plot_avg_rating_NL = ggplot(avg_rating_NL_dat,aes(x=NoiseLevel,y= AverageRating,fill= NoiseLevel))+geom_bar(stat = "identity") 
plot_avg_rating_NL+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Noise Level") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=5,name="Darjeeling1"))

# p < 2e-16
fit_NL = aov(biz_fastfood$stars~biz_fastfood$NoiseLevel)
summary(fit_NL)
## TukeyHSD for mean comparison of different levels
# all significant except AVERAGE-QUIET, p-value = 0.0106330 
TukeyHSD(fit_NL)


## Restaurants Take Out
# Plot Restaurants Take Out by Stars (Proportion)
plot_TO = ggplot(biz_fastfood,aes(x=stars,fill=RestaurantsTakeOut))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..]),position = position_dodge()) + ggtitle("Restaurants Take Out") 
plot_TO + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot Restaurants Take Out by Stars (Count)
plot_TO = ggplot(biz_fastfood,aes(x=stars,fill=RestaurantsTakeOut))+geom_histogram(breaks=seq(1,5, by =1),col="black",position=position_dodge()) + ggtitle("Restaurants Take Out") 
plot_TO + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars")+ylab("Count")

# Plot Average Rating by Restaurant Take Out
TO_false_index= which(biz_fastfood$RestaurantsTakeOut=="FALSE")
TO_true_index = which(biz_fastfood$RestaurantsTakeOut=="TRUE")
avg_rating_TO = c(mean(biz_fastfood$stars[TO_false_index]),mean(biz_fastfood$stars[TO_true_index]))
to = c("False","True")
avg_rating_TO_dat = as.data.frame(cbind(avg_rating_TO,to))
avg_rating_TO_dat$avg_rating_TO=as.numeric(as.character(avg_rating_TO_dat$avg_rating_TO))
colnames(avg_rating_TO_dat)[1] = "AverageRating"; colnames(avg_rating_TO_dat)[2] = "RestaurantsTakeOut"
plot_avg_rating_TO = ggplot(avg_rating_TO_dat,aes(x=RestaurantsTakeOut,y= AverageRating,fill= RestaurantsTakeOut))+geom_bar(stat = "identity") + ggtitle("Average Rating") 
plot_avg_rating_TO+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Restaurants Take Out") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))

# p = 0.07023
leveneTest(biz_fastfood$stars ~ biz_fastfood$RestaurantsTakeOut)
# p-value = 0.8811, no difference
t.test(biz_fastfood$stars[TO_false_index],biz_fastfood$stars[TO_true_index],
       paired = FALSE,var.equal = TRUE)


## PriceRange
# Plot Price Range by Stars (Proportion)
plot_PR = ggplot(biz_fastfood,aes(x=stars,fill=RestaurantsPriceRange2))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..])) + ggtitle("Restaurants Price Range") 
plot_PR + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=4,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot PriceRange by Stars (Count)
plot_PR = ggplot(biz_fastfood,aes(x=stars,fill=RestaurantsPriceRange2))+geom_histogram(breaks=seq(1,5, by =1),col="black",position = position_dodge()) + ggtitle("Restaurants Price Range") 
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

# p <2e-16
fit_PR = aov(biz_fastfood$stars ~ biz_fastfood$RestaurantsPriceRange2)
summary(fit_PR)
# 2-1, 4-2 significant
TukeyHSD(fit_PR)


## WiFi
# Plot WiFi by Stars (Proportion)
biz_fastfood$WiFi = factor(biz_fastfood$WiFi,levels = c("NO","FREE","PAID"))
plot_WF = ggplot(biz_fastfood,aes(x=stars,fill=WiFi))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..])) + ggtitle("WiFi") 
plot_WF + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=4,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot WiFi by Stars (Count)
plot_WF = ggplot(biz_fastfood,aes(x=stars,fill=WiFi))+geom_histogram(breaks=seq(1,5, by = 1),col="black",position=position_dodge()) + ggtitle("WiFi") 
plot_WF + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=3,name="Darjeeling1"))+
  xlab("Stars") +ylab("Count")

# Plot Average Rating by WiFi
WF_free_index= which(biz_fastfood$WiFi=="FREE")
WF_no_index = which(biz_fastfood$WiFi=="NO")
WF_paid_index = which(biz_fastfood$WiFi=="PAID")
avg_rating_WF = c(mean(biz_fastfood$stars[WF_free_index]),mean(biz_fastfood$stars[WF_no_index]),mean(biz_fastfood$stars[WF_paid_index]))
WF = c("free","no","paid")
avg_rating_WF_dat = as.data.frame(cbind(avg_rating_WF,WF))
avg_rating_WF_dat$avg_rating_WF=as.numeric(as.character(avg_rating_WF_dat$avg_rating_WF))
colnames(avg_rating_WF_dat)[1] = "AverageRating"; colnames(avg_rating_WF_dat)[2] = "WiFi"
avg_rating_WF_dat$WiFi = as.factor(avg_rating_WF_dat$WiFi)
plot_avg_rating_WF = ggplot(avg_rating_WF_dat,aes(x=WiFi,y= AverageRating,fill= WiFi))+geom_bar(stat = "identity") 
plot_avg_rating_WF+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("WiFi") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=3,name="Darjeeling1"))

# p < 2e-16
fit_WF = aov(biz_fastfood$stars ~ biz_fastfood$WiFi)
summary(fit_WF)
# No-Free - significant
TukeyHSD(fit_WF)


## BikeParking
# Plot BikeParking by Stars (Proportion)
plot_bike = ggplot(biz_fastfood,aes(x=stars,fill=BikeParking))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..]),position = position_dodge()) + ggtitle("Bike Parking") 
plot_bike + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot Average Rating by Bike Parking
bike_false_index= which(biz_fastfood$BikeParking=="FALSE")
bike_true_index = which(biz_fastfood$BikeParking=="TRUE")
avg_rating_bike = c(mean(biz_fastfood$stars[bike_false_index]),mean(biz_fastfood$stars[bike_true_index]))
bike = c("False","True")
avg_rating_bike_dat = as.data.frame(cbind(avg_rating_bike,bike))
avg_rating_bike_dat$avg_rating_bike=as.numeric(as.character(avg_rating_bike_dat$avg_rating_bike))
colnames(avg_rating_bike_dat)[1] = "AverageRating"; colnames(avg_rating_bike_dat)[2] = "BikeParking"
plot_avg_rating_bike = ggplot(avg_rating_bike_dat,aes(x=BikeParking,y= AverageRating,fill= BikeParking))+geom_bar(stat = "identity") 
plot_avg_rating_bike+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Bike Parking") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))

# p = 0.0002134 
leveneTest(biz_fastfood$stars ~ biz_fastfood$BikeParking)
# p-value = 1.878e-10
t.test(biz_fastfood$stars[bike_false_index],biz_fastfood$stars[bike_true_index],
       paired = FALSE,var.equal = FALSE)


## GoodForGroups
# Plot GoodForGroups by Stars (Proportion)
plot_groups = ggplot(biz_fastfood,aes(x=stars,fill=RestaurantsGoodForGroups))+geom_bar(aes(y=(..count..)/tapply(..count.., ..fill.. ,sum)[..fill..]),position = position_dodge()) + ggtitle("Restaurants Good For Groups") 
plot_groups + theme(text = element_text(size=20),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))+
  xlab("Stars") +ylab("Proportion")

# Plot Average Rating by GoodForGroups
groups_false_index= which(biz_fastfood$RestaurantsGoodForGroups=="FALSE")
groups_true_index = which(biz_fastfood$RestaurantsGoodForGroups=="TRUE")
avg_rating_groups = c(mean(biz_fastfood$stars[groups_false_index]),mean(biz_fastfood$stars[groups_true_index]))
groups = c("False","True")
avg_rating_groups_dat = as.data.frame(cbind(avg_rating_groups,groups))
avg_rating_groups_dat$avg_rating_groups=as.numeric(as.character(avg_rating_groups_dat$avg_rating_groups))
colnames(avg_rating_groups_dat)[1] = "AverageRating"; colnames(avg_rating_groups_dat)[2] = "GoodForGroups"
plot_avg_rating_groups = ggplot(avg_rating_groups_dat,aes(x=GoodForGroups,y= AverageRating,fill= GoodForGroups))+geom_bar(stat = "identity") 
plot_avg_rating_groups+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("Restaurants Good For Groups") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(AverageRating,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))

# p = 5.992e-06
leveneTest(biz_fastfood$stars ~ biz_fastfood$RestaurantsGoodForGroups)
# p-value < 2.2e-16
t.test(biz_fastfood$stars[groups_false_index],biz_fastfood$stars[groups_true_index],
       paired = FALSE,var.equal = FALSE)




######################################################################
# Plot fastfood restaurants on world map
library(ggmap)
library(rworldmap)
library(maps)
library(tidyverse)

fast_food_map = read_csv("biz_fast_food_features.csv") %>% select(stars,longitude,latitude)

ggplot(data=fast_food_map,aes(color=stars)) + borders("world",colour = "gray", fill="gray") +geom_point(aes(x=longitude,y=latitude),size=1) +scale_color_gradient2(low="blue",high="red")+
  xlim(-200,0)+ylim(0,100)+ylab("Latitude")+xlab("Longitude")+theme(text = element_text(size=20))
 




