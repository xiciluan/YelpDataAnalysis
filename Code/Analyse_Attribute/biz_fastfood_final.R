library(tidyverse)
biz_fast_food = read_csv("biz_fast_food_features_all.csv")
variable.names(biz_fast_food)
biz_fast_food = biz_fast_food %>% select(bussiness_id, 
                                         stars, 
                                         BusinessParking:RestaurantsGoodForGroups)

# Calculate proportion of missingness for each attribute
# Proportion of missingness in each attribute < 0.5
missing_attribute = NULL
for (i in 1:length(variable.names(biz_fast_food))){
  index = sum(is.na(biz_fast_food[,i]))
  missing_attribute[i] = index/nrow(biz_fast_food)
}

# Number of missing attributes for each business
missing_each_business = NULL
for (i in 1:nrow(biz_fast_food)){
 missing_each_business[i] = sum(is.na(biz_fast_food[i,]))
}

# Proportion of missing attributes in data
# 21%
sum(is.na(biz_fast_food)) # Total number of missing attributes
(sum(missing_each_business))/(10*16541)*100

star_nonmissing_attributes = biz_fast_food$stars[which(missing_each_business ==0)]


library(ggplot2)

diff_rating_dat = as.data.frame(cbind(c("Less Than 5 Missing", "At Least 5 Missing"),c(3.162,2.893)))
diff_rating_dat$V2 = as.numeric(as.character(diff_rating_dat$V2))
diff_rating_dat$V1 = as.character(diff_rating_dat$V1)
diff_rating_dat$V1 = factor(diff_rating_dat$V1,levels = c("Less Than 5 Missing", "At Least 5 Missing"))
plot_diff_rating = ggplot(diff_rating_dat,aes(x=V1,y=V2,fill=V1))+geom_bar(stat = "identity")  
plot_diff_rating+ theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+ scale_y_continuous(limits = c(0, 5))+ xlab("") + ylab("Average Rating")+
  geom_text(stat="identity",aes(label=round(V2,digits=4)),size=8)+scale_fill_manual(values=wes_palette(n=2,name="Darjeeling1"))

t.test(biz_fast_food$stars[which(missing_each_business <5)],biz_fast_food$stars[which(missing_each_business >= 5)], paired = FALSE,var.equal = FALSE)


#################################################################
# Comparing star ratings for businesses with nonmissing and businesses with n missing 
# attributes
normality_test = function(n){
  star_missing_attribute = biz_fast_food$stars[which(missing_each_business >= n)]
  shapiro.test(star_missing_attribute) 
}

homogeneity_var = function(n){
  star_missing_attribute = biz_fast_food$stars[which(missing_each_business >= n)]
  temp = c(star_nonmissing_attributes,star_missing_attribute)
  group = c(rep("nonmissing",length(star_nonmissing_attributes)), 
            rep("missing",length(star_missing_attribute)))
  leveneTest(temp~group) 
}

two_sample_test = function(n){
  star_missing_attribute = biz_fast_food$stars[which(missing_each_business >= n)]
  t.test(star_nonmissing_attributes,star_missing_attribute,
         paired = FALSE,var.equal = FALSE)
}

# Businesses with 1 missing attribute
normality_test(1)  
homogeneity_var(1) 
two_sample_test(1) 

# Businesses with 2 missing attribute
normality_test(2)  
homogeneity_var(2)  
two_sample_test(2) 

# Businesses with 3 missing attribute
normality_test(3)  
homogeneity_var(3) 
two_sample_test(3) 

# Businesses with 4 missing attribute
normality_test(4)  
homogeneity_var(4) 
two_sample_test(4) 

# Businesses with 5 missing attribute
normality_test(5) 
homogeneity_var(5)
two_sample_test(5) 

# Businesses with 6 missing attribute
normality_test(6)  
homogeneity_var(6) 
two_sample_test(6) 

# Businesses with 7 missing attribute
normality_test(7)  
homogeneity_var(7)
two_sample_test(7) 

# Businesses with 8 missing attribute
normality_test(8)  
homogeneity_var(8) 
two_sample_test(8) 

# Businesses with 9 missing attribute
normality_test(9)  
homogeneity_var(9) 
two_sample_test(9) 

# Businesses with 10 missing attribute
normality_test(10) 
homogeneity_var(10) 
two_sample_test(10)


#################################################################
# Remove business id
biz_fast_food = biz_fast_food[,-1]

biz_fast_food$RestaurantsDelivery[which(biz_fast_food$RestaurantsDelivery == "None")] = "FALSE"
biz_fast_food$WiFi[which(biz_fast_food$WiFi=="None")] = "NO"
biz_fast_food[which(biz_fast_food[,6]=="None"),6] <- NA

# Change observations to upper case
biz_fast_food$RestaurantsDelivery = toupper(biz_fast_food$RestaurantsDelivery)
biz_fast_food$NoiseLevel = toupper(biz_fast_food$NoiseLevel)
biz_fast_food$WiFi = toupper(biz_fast_food$WiFi)

# Change to factors
biz_fast_food = mutate_if(biz_fast_food, is.character, as.factor)
biz_fast_food = mutate_if(biz_fast_food, is.logical, as.factor)
biz_fast_food$RestaurantsPriceRange2 = as.factor(biz_fast_food$RestaurantsPriceRange2)


#################################################################
## rpart - to deal with missing observations
library(rpart)
library(rpart.plot)

### Business Parking 
rpart_BP_model = rpart(BusinessParking ~ ., data = biz_fast_food, method = 'class',
                       control = list(maxdepth = 3, cp = 0))
rpart.plot(rpart_BP_model)
predict_rpart_BP = predict(rpart_BP_model, biz_fast_food[is.na(biz_fast_food$BusinessParking),])

BP_predict = as.character(rep(0,nrow(predict_rpart_BP)))

# Labelling for missing Business Parking using label with higher proportion
for(j in 1:nrow(predict_rpart_BP)){
  if(predict_rpart_BP[j,1]>predict_rpart_BP[j,2]){
    BP_predict[j] = "FALSE"}
  else{BP_predict[j] = "TRUE"}
}


### Restaurants Delivery
rpart_RD_model = rpart(RestaurantsDelivery~ ., data = biz_fast_food, method = 'class',
                       control = list(maxdepth = 3, cp = 0))
rpart.plot(rpart_RD_model)
predict_rpart_RD = predict(rpart_RD_model, biz_fast_food[is.na(biz_fast_food$RestaurantsDelivery),])

RD_predict = as.character(rep(0,nrow(predict_rpart_RD)))

# Labelling for missing Restaurant Delivery using label with higher proportion
for(i in 1:nrow(predict_rpart_RD)){
  if(predict_rpart_RD[i,1]>predict_rpart_RD[i,2]){ 
    RD_predict[i] = "FALSE"
  }
  else {RD_predict[i] = "TRUE"}
}


### Restaurants Reservations
rpart_RR_model = rpart(RestaurantsReservations~ ., data = biz_fast_food, method = 'class',
                       control = list(maxdepth = 3, cp = 0))
rpart.plot(rpart_RR_model)
predict_rpart_RR = predict(rpart_RR_model, biz_fast_food[is.na(biz_fast_food$RestaurantsReservations),])

RR_predict = as.character(rep(0,nrow(predict_rpart_RR)))

# Labelling for missing Restaurant Reservations using label with higher proportion
for(i in 1:nrow(predict_rpart_RR)){
  if(predict_rpart_RR[i,1]>predict_rpart_RR[i,2]){ 
    RR_predict[i] = "FALSE"
  }
  else {RR_predict[i] = "TRUE"}
}


### Outdoor Seating
rpart_OS_model = rpart(OutdoorSeating~ ., data = biz_fast_food, method = 'class',
                       control = list(maxdepth = 3, cp = 0))
rpart.plot(rpart_OS_model)
predict_rpart_OS = predict(rpart_OS_model, biz_fast_food[is.na(biz_fast_food$OutdoorSeating),])

OS_predict = as.character(rep(0,nrow(predict_rpart_OS)))
# Labelling for missing Outdoor Seating using label with higher proportion
for(i in 1:nrow(predict_rpart_OS)){
  if(predict_rpart_OS[i,1]>predict_rpart_OS[i,2]){ 
    OS_predict[i] = "FALSE"
  }
  else {OS_predict[i] = "TRUE"}
}


### TakeOut
rpart_TO_model = rpart(RestaurantsTakeOut~ ., data = biz_fast_food, method = 'class',
                       control = list(maxdepth = 3, cp = 0))
rpart.plot(rpart_TO_model)
predict_rpart_TO = predict(rpart_TO_model, biz_fast_food[is.na(biz_fast_food$RestaurantsTakeOut),])

TO_predict = as.character(rep(0,nrow(predict_rpart_TO)))

# Labelling for missing TakeOut using label with higher proportion
for(i in 1:nrow(predict_rpart_TO)){
  if(predict_rpart_TO[i,1]>predict_rpart_TO[i,2]){ 
    TO_predict[i] = "FALSE"
  }
  else {TO_predict[i] = "TRUE"}
}


### Bike Parking
rpart_Bike_model = rpart(BikeParking~ ., data = biz_fast_food, method = 'class',
                         control = list(maxdepth = 3, cp = 0))
rpart.plot(rpart_Bike_model)
predict_rpart_Bike = predict(rpart_Bike_model, biz_fast_food[is.na(biz_fast_food$BikeParking),])

Bike_predict = as.character(rep(0,nrow(predict_rpart_Bike)))

# Labelling for missing Bike Parking using label with higher proportion
for(i in 1:nrow(predict_rpart_Bike)){
  if(predict_rpart_Bike[i,1]>predict_rpart_Bike[i,2]){ 
    Bike_predict[i] = "FALSE"
  }
  else {Bike_predict[i] = "TRUE"}
}


### Restaurants Good Gor Groups
rpart_groups_model = rpart(RestaurantsGoodForGroups~ ., data = biz_fast_food, method = 'class',
                           control = list(maxdepth = 3, cp = 0))
rpart.plot(rpart_groups_model)
predict_rpart_groups = predict(rpart_groups_model, biz_fast_food[is.na(biz_fast_food$RestaurantsGoodForGroups),])

groups_predict = as.character(rep(0,nrow(predict_rpart_groups)))

# Labelling for missing Good For Groups using label with higher proportion
for(i in 1:nrow(predict_rpart_groups)){
  if(predict_rpart_groups[i,1]>predict_rpart_groups[i,2]){ 
    groups_predict[i] = "FALSE"
  }
  else {groups_predict[i] = "TRUE"}
}


### NoiseLevel
rpart_NL_model = rpart(NoiseLevel~ ., data = biz_fast_food, method = 'class',
                       control = list(maxdepth = 3, cp = 0))
predict_rpart_NL = predict(rpart_NL_model, biz_fast_food[is.na(biz_fast_food$NoiseLevel),])

NL_predict = as.character(rep(0,nrow(predict_rpart_NL)))

# Labelling for missing NoiseLevel using label with higher proportion
for(i in 1:nrow(predict_rpart_NL)){
  s = which.max(predict_rpart_NL[i,])
  NL_predict[i] = variable.names(predict_rpart_NL)[[s]]
}


### WiFi
rpart_wifi_model = rpart(WiFi~ ., data = biz_fast_food, method = 'class',
                         control = list(maxdepth = 3, cp = 0))
predict_rpart_wifi = predict(rpart_wifi_model, biz_fast_food[is.na(biz_fast_food$WiFi),])

wifi_predict = as.character(rep(0,nrow(predict_rpart_wifi)))

# Labelling for missing WiFi using label with higher proportion
for(i in 1:nrow(predict_rpart_wifi)){
  s = which.max(predict_rpart_wifi[i,])
  wifi_predict[i] = variable.names(predict_rpart_wifi)[[s]]
}


### PriceRange
rpart_price_model = rpart(RestaurantsPriceRange2~ ., data = biz_fast_food, method = 'class',
                          control = list(maxdepth = 3, cp = 0))
predict_rpart_price = predict(rpart_price_model, biz_fast_food[is.na(biz_fast_food$RestaurantsPriceRange2),])

price_predict = as.character(rep(0,nrow(predict_rpart_price)))

# Labelling for missing Price Range using label with higher proportion
for(i in 1:nrow(predict_rpart_price)){
  s = which.max(predict_rpart_price[i,])
  price_predict[i] = variable.names(predict_rpart_price)[[s]]
}


#################################
### Imputation
# Business Parking
j=1
for(i in 1:nrow(biz_fast_food)){
  if(is.na(biz_fast_food$BusinessParking[i])==TRUE){
    biz_fast_food$BusinessParking[i] = BP_predict[j]
    j=j+1
  }
  else{biz_fast_food$BusinessParking[i] = biz_fast_food$BusinessParking[i]}
}


# Delivery
j=1
for(i in 1:nrow(biz_fast_food)){
  if(is.na(biz_fast_food$RestaurantsDelivery[i])==TRUE){
    biz_fast_food$RestaurantsDelivery[i] = RD_predict[j]
    j=j+1
  }
  else{biz_fast_food$RestaurantsDelivery[i] = biz_fast_food$RestaurantsDelivery[i]}
}


# Reservations
j=1
for(i in 1:nrow(biz_fast_food)){
  if(is.na(biz_fast_food$RestaurantsReservations[i])==TRUE){
    biz_fast_food$RestaurantsReservations[i] = RR_predict[j]
    j=j+1
  }
  else{biz_fast_food$RestaurantsReservations[i] = biz_fast_food$RestaurantsReservations[i]}
}

# Outdoor Seating
j=1
for(i in 1:nrow(biz_fast_food)){
  if(is.na(biz_fast_food$OutdoorSeating[i])==TRUE){
    biz_fast_food$OutdoorSeating[i] = OS_predict[j]
    j=j+1
  }
  else{biz_fast_food$OutdoorSeating[i] = biz_fast_food$OutdoorSeating[i]}
}


# TakeOut
j=1
for(i in 1:nrow(biz_fast_food)){
  if(is.na(biz_fast_food$RestaurantsTakeOut[i])==TRUE){
    biz_fast_food$RestaurantsTakeOut[i] = TO_predict[j]
    j=j+1
  }
  else{biz_fast_food$RestaurantsTakeOut[i] = biz_fast_food$RestaurantsTakeOut[i]}
}


# Bike Parking
j=1
for(i in 1:nrow(biz_fast_food)){
  if(is.na(biz_fast_food$BikeParking[i])==TRUE){
    biz_fast_food$BikeParking[i] = Bike_predict[j]
    j=j+1
  }
  else{biz_fast_food$BikeParking[i] = biz_fast_food$BikeParking[i]}
}


# Groups
j=1
for(i in 1:nrow(biz_fast_food)){
  if(is.na(biz_fast_food$RestaurantsGoodForGroups[i])==TRUE){
    biz_fast_food$RestaurantsGoodForGroups[i] = groups_predict[j]
    j=j+1
  }
  else{biz_fast_food$RestaurantsGoodForGroups[i] = biz_fast_food$RestaurantsGoodForGroups[i]}
}


# NoiseLevel
j=1
for(i in 1:nrow(biz_fast_food)){
  if(is.na(biz_fast_food$NoiseLevel[i])==TRUE){
    biz_fast_food$NoiseLevel[i] = NL_predict[j]
    j=j+1
  }
  else{biz_fast_food$NoiseLevel[i] = biz_fast_food$NoiseLevel[i]}
}


# Wifi
j=1
for(i in 1:nrow(biz_fast_food)){
  if(is.na(biz_fast_food$WiFi[i])==TRUE){
    biz_fast_food$WiFi[i] = wifi_predict[j]
    j=j+1
  }
  else{biz_fast_food$WiFi[i] = biz_fast_food$WiFi[i]}
}


# Price Range
j=1
for(i in 1:nrow(biz_fast_food)){
  if(is.na(biz_fast_food$RestaurantsPriceRange2[i])==TRUE){
    biz_fast_food$RestaurantsPriceRange2[i] = price_predict[j]
    j=j+1
  }
  else{biz_fast_food$RestaurantsPriceRange2[i] = biz_fast_food$RestaurantsPriceRange2[i]}
}



write.csv(biz_fast_food,"biz_imputed.csv",row.names = FALSE)
