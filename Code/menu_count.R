setwd("~/Desktop/STAT 628/Module3/Data")
review_summary = read.csv("review_summary.csv",header=TRUE)
count_5 = length(which(review_summary$stars == "5"))
count_4 = length(which(review_summary$stars == "4"))
count_3 = length(which(review_summary$stars == "3"))
count_2 = length(which(review_summary$stars == "2"))
count_1 = length(which(review_summary$stars == "1"))
review_rating_count = c(count_1, count_2, count_3, count_4, count_5)
review_rating_prop = review_rating_count/sum(review_rating_count)


library(ggplot2)
library(xtable)

## Review Proportion
review_prop_data = as.data.frame(cbind(c(1,2,3,4,5), review_rating_prop))
colnames(review_prop_data)[1] = "rating"
plot_review_prop = ggplot(review_prop_data,aes(x = rating, y = review_rating_prop, fill=rating))+geom_bar(stat = "identity") + ggtitle("Review Proportion by Rating")
plot_review_prop+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Proportion")


##########################
## Avocado
rating_avocado = c(644,784,1147,2445,2429)
rating = c(1,2,3,4,5)
rating_avocado_dat = as.data.frame(cbind(rating,rating_avocado/sum(rating_avocado)))
rating_avocado_dat$V2=as.numeric(as.character(rating_avocado_dat$V2))

rating_avocado_dat$V2 = rating_avocado_dat$V2 - review_rating_prop

plot_avg_rating_avocado = ggplot(rating_avocado_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Avocado")
plot_avg_rating_avocado+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

avocado_count = c(177,2953)
attributes = c("negative","positive")
attributes_avocado_dat = as.data.frame(cbind(attributes,avocado_count))
attributes_avocado_dat$avocado_count = as.numeric(as.character(attributes_avocado_dat$avocado_count))
proportion_positive_avocado = avocade_count[2]/sum(avocado_count)

print(xtable(attributes_avocado_dat))


## Mushroom
rating_mushroom = c(1877,2084,3622,7759,7965)
rating_mushroom_dat = as.data.frame(cbind(rating,rating_mushroom/sum(rating_mushroom)))
rating_mushroom_dat$V2=as.numeric(as.character(rating_mushroom_dat$V2))
# Scaled proportion
rating_mushroom_dat$V2 = rating_mushroom_dat$V2 - review_rating_prop

plot_avg_rating_mushroom = ggplot(rating_mushroom_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") +ggtitle("Mushroom")
plot_avg_rating_mushroom+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

mushroom_count = c(542,10026)
attributes = c("negative","positive")
attributes_mushroom_dat = as.data.frame(cbind(attributes,mushroom_count))
attributes_mushroom_dat$mushroom_count = as.numeric(as.character(attributes_mushroom_dat$mushroom_count))
proportion_positive_mushroom = mushroon_count[2]/sum(mushroom_count)

print(xtable(attributes_mushroom_dat))


## Salmon
rating_salmon = c(463,624,1072,2295,2947)
rating_salmon_dat = as.data.frame(cbind(rating,rating_salmon/sum(rating_salmon)))
rating_salmon_dat$V2=as.numeric(as.character(rating_salmon_dat$V2))
# Scaled proportion
rating_salmon_dat$V2 = rating_salmon_dat$V2 - review_rating_prop

plot_avg_rating_salmon = ggplot(rating_salmon_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity")+ggtitle("Salmon") 
plot_avg_rating_salmon+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

salmon_count = c(217,4141)
attributes = c("negative","positive")
attributes_salmon_dat = as.data.frame(cbind(attributes,salmon_count))
attributes_salmon_dat$salmon_count = as.numeric(as.character(attributes_salmon_dat$salmon_count))
proportion_positive_salmon = salmon_count[2]/sum(salmon_count)

print(xtable(attributes_salmon_dat))


## Tuna
rating_tuna = c(385,373,602,1348,1584)
rating_tuna_dat = as.data.frame(cbind(rating,rating_tuna/sum(rating_tuna)))
rating_tuna_dat$V2=as.numeric(as.character(rating_tuna_dat$V2))
# Scaled proportion
rating_tuna_dat$V2 = rating_tuna_dat$V2 - review_rating_prop

plot_avg_rating_tuna = ggplot(rating_tuna_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Tuna")
plot_avg_rating_tuna+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

tuna_count = c(141,2068)
attributes = c("negative","positive")
attributes_tuna_dat = as.data.frame(cbind(attributes,tuna_count))
attributes_tuna_dat$tuna_count = as.numeric(as.character(attributes_tuna_dat$tuna_count))
proportion_positive_tuna = tuna_count[2]/sum(tuna_count)

print(xtable(attributes_tuna_dat))


## Blue Cheese
rating_blue_cheese = c(306,393,660,1318,1322)
rating_blue_cheese_dat = as.data.frame(cbind(rating,rating_blue_cheese/sum(rating_blue_cheese)))
rating_blue_cheese_dat$V2=as.numeric(as.character(rating_blue_cheese_dat$V2))
# Scaled proportion
rating_blue_cheese_dat$V2 = rating_blue_cheese_dat$V2 - review_rating_prop

plot_avg_rating_blue_cheese = ggplot(rating_blue_cheese_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Blue Cheese")
plot_avg_rating_blue_cheese+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

blue_cheese_count = c(95,1875)
attributes = c("negative","positive")
attributes_blue_cheese_dat = as.data.frame(cbind(attributes,blue_cheese_count))
attributes_blue_cheese_dat$tuna_count = as.numeric(as.character(attributes_blue_cheese_dat$blue_cheese_count))
proportion_positive_blue_cheese = blue_cheese_count[2]/sum(blue_cheese_count)

print(xtable(attributes_blue_cheese_dat))


## Pepper jack
rating_pepper_jack = c(26,76,122,286,248)
rating_pepper_jack_dat = as.data.frame(cbind(rating,rating_pepper_jack/sum(rating_pepper_jack)))
rating_pepper_jack_dat$V2=as.numeric(as.character(rating_pepper_jack_dat$V2))
# Scaled proportion
rating_pepper_jack_dat$V2 = rating_pepper_jack_dat$V2 - review_rating_prop

plot_avg_rating_pepper_jack = ggplot(rating_pepper_jack_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Pepper Jack")
plot_avg_rating_pepper_jack+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

pepper_jack_count = c(13,215)
attributes = c("negative","positive")
attributes_pepper_jack_dat = as.data.frame(cbind(attributes,pepper_jack_count))
attributes_pepper_jack_dat$pepper_jack_count = as.numeric(as.character(attributes_pepper_jack_dat$pepper_jack_count))
proportion_positive_pepper_jack = pepper_jack_count[2]/sum(pepper_jack_count)

print(xtable(attributes_pepper_jack_dat))


#################################################
## Truffle fry
rating_truffle = c(106,291,565,1217,1553)
rating_truffle_dat = as.data.frame(cbind(rating,rating_truffle/(sum(rating_truffle))))
rating_truffle_dat$V2=as.numeric(as.character(rating_truffle_dat$V2))
# Scaled Proportion
rating_truffle_dat$V2 = rating_truffle_dat$V2 - review_rating_prop

plot_avg_rating_truffle = ggplot(rating_truffle_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Truffle Fries")
plot_avg_rating_truffle+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")
1553/sum(rating_truffle) - review_rating_prop[5]

truffle_count = c(101,2149)
attributes = c("negative","positive")
attributes_truffle_dat = as.data.frame(cbind(attributes,truffle_count))
attributes_truffle_dat$truffle_count = as.numeric(as.character(attributes_truffle_dat$truffle_count))
proportion_positive_truffle = truffle_count[2]/sum(truffle_count)

print(xtable(attributes_truffle_dat))


## Cajun fry
rating_cajun = c(56,99,199,363,315)
rating_cajun_dat = as.data.frame(cbind(rating,rating_cajun/sum(rating_cajun)))
rating_cajun_dat$V2=as.numeric(as.character(rating_cajun_dat$V2))
# Scaled proportion
rating_cajun_dat$V2 = rating_cajun_dat$V2 - review_rating_prop

plot_avg_rating_cajun = ggplot(rating_cajun_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Cajun Fries")
plot_avg_rating_cajun+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

cajun_count = c(31,418)
attributes = c("negative","positive")
attributes_cajun_dat = as.data.frame(cbind(attributes,cajun_count))
attributes_cajun_dat$cajun_count = as.numeric(as.character(attributes_cajun_dat$cajun_count))
proportion_positive_cajun = cajun_count[2]/sum(cajun_count)

print(xtable(attributes_cajun_dat))


## Garlic fry
rating_garlic = c(99,149,258,536,669)
rating_garlic_dat = as.data.frame(cbind(rating,rating_garlic/sum(rating_garlic)))
rating_garlic_dat$V2=as.numeric(as.character(rating_garlic_dat$V2))
# Scaled proportion
rating_garlic_dat$V2 = rating_garlic_dat$V2 - review_rating_prop

plot_avg_rating_garlic = ggplot(rating_garlic_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Garlic Fries")
plot_avg_rating_garlic+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

garlic_count = c(52,943)
attributes = c("negative","positive")
attributes_garlic_dat = as.data.frame(cbind(attributes,garlic_count))
attributes_garlic_dat$garlic_count = as.numeric(as.character(attributes_garlic_dat$garlic_count))
proportion_positive_garlic = garlic_count[2]/sum(garlic_count)

print(xtable(attributes_garlic_dat))


############################################
##  Muffin
rating_muffin = c(200,204,304,768,1314)
rating_muffin_dat = as.data.frame(cbind(rating,rating_muffin/sum(rating_muffin)))
rating_muffin_dat$V2=as.numeric(as.character(rating_muffin_dat$V2))
# Scaled proportion
rating_muffin_dat$V2 = rating_muffin_dat$V2 - review_rating_prop

plot_avg_rating_muffin = ggplot(rating_muffin_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Muffin")
plot_avg_rating_muffin+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

muffin_count = c(68,1379)
attributes = c("negative","positive")
attributes_muffin_dat = as.data.frame(cbind(attributes,muffin_count))
attributes_muffin_dat$muffin_count = as.numeric(as.character(attributes_muffin_dat$muffin_count))
proportion_positive_muffin = muffin_count[2]/sum(muffin_count)

print(xtable(attributes_muffin_dat))


## Red velvet
rating_red_velvet = c(40,77,127,326,414)
rating_red_velvet_dat = as.data.frame(cbind(rating,rating_red_velvet/sum(rating_red_velvet)))
rating_red_velvet_dat$V2=as.numeric(as.character(rating_red_velvet_dat$V2))
# Scaled proportion
rating_red_velvet_dat$V2 = rating_red_velvet_dat$V2 - review_rating_prop

plot_avg_rating_red_velvet = ggplot(rating_red_velvet_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Red Velvet")
plot_avg_rating_red_velvet+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

red_velvet_count = c(24,417)
attributes = c("negative","positive")
attributes_red_velvet_dat = as.data.frame(cbind(attributes,red_velvet_count))
attributes_red_velvet_dat$red_velvet_count = as.numeric(as.character(attributes_red_velvet_dat$red_velvet_count))
proportion_positive_red_velvet = red_velvet_count[2]/sum(red_velvet_count)

print(xtable(attributes_red_velvet_dat))


## Pancake
rating_pancake = c(245,182,303,520,632)
rating_pancake_dat = as.data.frame(cbind(rating,rating_pancake/sum(rating_pancake)))
rating_pancake_dat$V2=as.numeric(as.character(rating_pancake_dat$V2))
# Scaled proportion
rating_pancake_dat$V2 = rating_pancake_dat$V2 - review_rating_prop

plot_avg_rating_pancake = ggplot(rating_pancake_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Pancake")
plot_avg_rating_pancake+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

pancake_count = c(64,774)
attributes = c("negative","positive")
attributes_pancake_dat = as.data.frame(cbind(attributes,pancake_count))
attributes_pancake_dat$red_velvet_count = as.numeric(as.character(attributes_pancake_dat$pancake_count))
proportion_positive_pancake = pancake_count[2]/sum(pancake_count)

print(xtable(attributes_pancake_dat))


## Smoothie
rating_smoothie = c(234,135,209,406,484)
rating_smoothie_dat = as.data.frame(cbind(rating,rating_smoothie/sum(rating_smoothie)))
rating_smoothie_dat$V2=as.numeric(as.character(rating_smoothie_dat$V2))
# Scaled proportion
rating_smoothie_dat$V2 = rating_smoothie_dat$V2 - review_rating_prop

plot_avg_rating_smoothie = ggplot(rating_smoothie_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Smoothie")
plot_avg_rating_smoothie+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

smoothie_count = c(61,590)
attributes = c("negative","positive")
attributes_smoothie_dat = as.data.frame(cbind(attributes,smoothie_count))
attributes_smoothie_dat$smoothie_count = as.numeric(as.character(attributes_smoothie_dat$smoothie_count))
proportion_smoothie_impossible = smoothie_count[2]/sum(smoothie_count)

print(xtable(attributes_smoothie_dat))


## Sparkling water
rating_sparkling_water = c(38,57,80,154,190)
rating_sparkling_water_dat = as.data.frame(cbind(rating,rating_sparkling_water/sum(rating_sparkling_water)))
rating_sparkling_water_dat$V2=as.numeric(as.character(rating_sparkling_water_dat$V2))
# Scaled proportion
rating_sparkling_water_dat$V2 = rating_sparkling_water_dat$V2 - review_rating_prop

plot_avg_rating_sparkling_water = ggplot(rating_sparkling_water_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Sparkling Water")
plot_avg_rating_sparkling_water+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

sparkling_water_count = c(7,181)
attributes = c("negative","positive")
attributes_sparkling_water_dat = as.data.frame(cbind(attributes,sparkling_water_count))
attributes_sparkling_water_dat$sparkling_water_count = as.numeric(as.character(attributes_sparkling_water_dat$sparkling_water_count))
proportion_positive_sparkling = sparkling_water_count[2]/sum(sparkling_water_count)

print(xtable(attributes_sparkling_water_dat))


## Root beer
rating_root_beer = c(160,140,317,608,609)
rating_root_beer_dat = as.data.frame(cbind(rating,rating_root_beer/sum(rating_root_beer)))
rating_root_beer_dat$V2=as.numeric(as.character(rating_root_beer_dat$V2))
# Scaled proportion
rating_root_beer_dat$V2 = rating_root_beer_dat$V2 - review_rating_prop

plot_avg_rating_root_beer = ggplot(rating_root_beer_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Root Beer")
plot_avg_rating_root_beer+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

root_beer_count = c(45,811)
attributes = c("negative","positive")
attributes_root_beer_dat = as.data.frame(cbind(attributes,root_beer_count))
attributes_root_beer_dat$root_beer_count = as.numeric(as.character(attributes_root_beer_dat$root_beer_count))
proportion_positive_root_beer = root_beer_count[2]/sum(root_beer_count)

print(xtable(attributes_root_beer_dat))


############################################
## Impossible Burger
rating_impossible = c(23,17,31,87,142)
rating_impossible_dat = as.data.frame(cbind(rating,rating_impossible/sum(rating_impossible)))
rating_impossible_dat$V2=as.numeric(as.character(rating_impossible_dat$V2))
# Scaled proportion
rating_impossible_dat$V2 = rating_impossible_dat$V2 - review_rating_prop

plot_avg_rating_impossible = ggplot(rating_impossible_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Impossible Burger")
plot_avg_rating_impossible+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

impossible_count = c(6,99)
attributes = c("negative","positive")
attributes_impossible_dat = as.data.frame(cbind(attributes,impossible_count))
attributes_impossible_dat$red_velvet_count = as.numeric(as.character(attributes_impossible_dat$impossible_count))
proportion_positive_impossible = impossible_count[2]/sum(impossible_count)

print(xtable(attributes_impossible_dat))


## Gluten
rating_gluten = c(642,596,949,2347,4214)
rating_gluten_dat = as.data.frame(cbind(rating,rating_gluten/sum(rating_gluten)))
rating_gluten_dat$V2=as.numeric(as.character(rating_gluten_dat$V2))
# Scaled proportion
rating_gluten_dat$V2 = rating_gluten_dat$V2 - review_rating_prop

plot_avg_rating_gluten = ggplot(rating_gluten_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Gluten")
plot_avg_rating_gluten+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

gluten_count = c(182,3876)
attributes = c("negative","positive")
attributes_gluten_dat = as.data.frame(cbind(attributes,gluten_count))
attributes_gluten_dat$gluten_count = as.numeric(as.character(attributes_gluten_dat$gluten_count))
proportion_positive_gluten = gluten_count[2]/sum(gluten_count)

print(xtable(attributes_gluten_dat))


## homemade
rating_homemade = c(322,497,1151,3408,6329)
rating_homemade_dat = as.data.frame(cbind(rating,rating_homemade/sum(rating_homemade)))
rating_homemade_dat$V2=as.numeric(as.character(rating_homemade_dat$V2))
# Scaled proportion
rating_homemade_dat$V2 = rating_homemade_dat$V2 - review_rating_prop

plot_avg_rating_homemade = ggplot(rating_homemade_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Homemade")
plot_avg_rating_homemade+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

homemade_count = c(192,6773)
attributes = c("negative","positive")
attributes_homemade_dat = as.data.frame(cbind(attributes,homemade_count))
attributes_homemade_dat$homemade_count = as.numeric(as.character(attributes_homemade_dat$homemade_count))
proportion_positive_homemade = homemade_count[2]/sum(homemade_count)

print(xtable(attributes_homemade_dat))


## Organic
rating_organic = c(143,195,364,730,993)
rating_organic_dat = as.data.frame(cbind(rating,rating_organic/sum(rating_organic)))
rating_organic_dat$V2=as.numeric(as.character(rating_organic_dat$V2))
# Scaled proportion
rating_organic_dat$V2 = rating_organic_dat$V2 - review_rating_prop

plot_avg_rating_organic = ggplot(rating_organic_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Organic")
plot_avg_rating_organic+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

organic_count = c(45,964)
attributes = c("negative","positive")
attributes_organic_dat = as.data.frame(cbind(attributes,organic_count))
attributes_organic_dat$organic_count = as.numeric(as.character(attributes_organic_dat$organic_count))
proportion_positive_organic = organic_count[2]/sum(organic_count)

print(xtable(attributes_organic_dat))


## Veggie
rating_veggie = c(961,1065,1906,3811,4157)
rating_veggie_dat = as.data.frame(cbind(rating,rating_veggie/sum(rating_veggie)))
rating_veggie_dat$V2=as.numeric(as.character(rating_veggie_dat$V2))
# Scaled proportion
rating_veggie_dat$V2 = rating_veggie_dat$V2 - review_rating_prop

plot_avg_rating_veggie = ggplot(rating_veggie_dat,aes(x = rating, y = V2, fill=rating))+geom_bar(stat = "identity") + ggtitle("Veggie")
plot_avg_rating_veggie+theme(text = element_text(size=20),legend.position="none",plot.title = element_text(hjust = 0.5))+xlab("Average Rating") + ylab("Scaled Proportion")

veggie_count = c(300,5242)
attributes = c("negative","positive")
attributes_veggie_dat = as.data.frame(cbind(attributes,veggie_count))
attributes_veggie_dat$veggie_count = as.numeric(as.character(attributes_veggie_dat$veggie_count))
proportion_positive_veggie = veggie_count[2]/sum(veggie_count)
  
print(xtable(attributes_veggie_dat))
