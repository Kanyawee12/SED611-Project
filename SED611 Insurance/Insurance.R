# Load libraries
library(ggplot2)
library(ggthemes)
library(psych)
library(relaimpo)
library(tidyverse)

# Read in our dataset
insurance <- read.csv("insurance.csv")

# Structure dataset
head(insurance)
str(insurance)
summary(insurance)
describe(insurance)

#**------------------------------------------------------------------------------------------------***
summary(insurance$charges)

par(mfrow = c(1,2)) # combine the two plots
hist(insurance$charges, main = "Histogram of charges", col = "lightblue")
plot(density(insurance$charges), main = "Density plot of charges")
polygon(density(insurance$charges), col = "orange")

#**------------------------------------------------------------------------------------------------***

#Correlation between Charges and Age
ggplot(insurance, aes(age, charges)) +
  geom_jitter(color = "IndianRed", alpha = 0.5) +
  ylab("Charges (USD$)")+
  ggtitle("Boxplot: Charges and Age") +
  theme_light()

#Correlation between Charges and BMI
ggplot(insurance, aes(bmi, charges)) +
  geom_jitter(color = "red", alpha = 0.5) +
  ylab("Charges (USD$)")+
  ggtitle("Boxplot: Charges and BMI") +
  theme_light()

#Correlation between Charges and Gender
ggplot(insurance, aes(sex, charges)) +
  geom_jitter(aes(color = sex), alpha = 0.7) +
  ylab("Charges (USD$)")+
  ggtitle("Correlation between Charges and Gender") +
  theme_light()

#Correlation between Charges and Children
ggplot(insurance, aes(children, charges)) +
  geom_jitter(aes(color = children), alpha = 0.7) +
  ylab("Charges (USD$)")+
  ggtitle("Correlation between Charges and Children") +
  theme_light()

#Correlation between Charges and smoker
ggplot(insurance, aes(smoker, charges)) +
  geom_jitter(aes(color = smoker), alpha = 0.7) +
  ylab("Charges (USD$)")+
  ggtitle("Correlation between Charges and smoker") +
  theme_light()

#Correlation between Charges and region
ggplot(insurance, aes(region, charges)) +
  geom_jitter(aes(color = region), alpha = 0.7) +
  ylab("Charges (USD$)")+
  ggtitle("Correlation between Charges and region") +
  theme_light()



#**------------------------------------------------------------------------------------------------***
#**------------------------------------------------------------------------------------------------***
insurance$smoker <- insurance$smoker
for (i in 1:length(insurance$smoker)){
  if (insurance$smoker[i] == "yes"){
    insurance$smoker[i] = 1
  } else {
    insurance$smoker[i] = 0
  }
}
str(insurance)
insurance$smoker<-as.numeric(insurance$smoker) 


# lm all column 
mul_model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = insurance)
# mul_model_ <- lm(charges ~ ., data = insurance_new)
summary(mul_model)

# column that have significance
mul_model <- lm(charges ~ age + bmi + children + smoker, data = insurance)
# mul_model_ <- lm(charges ~ ., data = insurance_new)
summary(mul_model)
sqrt(0.7489)

insurance$Multi <- -12102.77 + 257.85*insurance$age + 321.85*insurance$bmi + 
  473.50*insurance$children + 23811.40*insurance$smoker 

insurance$prediction <- predict(mul_model, newdata = insurance)

insurance$prediction <- predict(mul_model, newdata = insurance)
ggplot(insurance, aes(x = prediction, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Multiple linear regression")





insurance %>%
  ggplot(aes(x=smoker, y = charges)) +
  geom_boxplot() + #smokers associated with ~$30,000 higher mean charge amount
  ggtitle("Charges for Smokers vs Non-Smokers")

insurance %>%
  ggplot(aes(x=age, y = charges)) + 
  geom_point(aes(color = smoker)) + #upward trend with age; appears to be four different groups of data; two within each category of smoker 
  ggtitle("Charges vs Age")
insurance %>%
  ggplot(aes(x=age, y = charges)) + 
  geom_point(aes(color = smoker, size = bmi, alpha = .05)) + #four groups: smoker with high BMI, non-smoker with high BMI, smoker with low BMI and non-smoker with low BMI --> could indicate that there needs to be an interaction term between BMI and Smoker. 
  ggtitle("Variables is related to the amount of charges")

insurance %>%
  ggplot(aes(x=as.factor(children), y = charges)) + 
  geom_boxplot() + #no obvious trend between children and charges
  ggtitle("Charges vs Number of Children")

insurance %>%
  ggplot(aes(x= sex, y = charges)) + 
  geom_boxplot() + #no difference in means; male has interquartile range
  ggtitle("Charges for Males vs Females")




#**-------------------------------------------Knn--------------------------------------------------***
#**------------------------------------------------------------------------------------------------***
library(dplyr)

# Read in our dataset
df <- read.csv("insurance.csv")
df <- tibble::rowid_to_column(df, "ID")

# Structure dataset
head(df)
str(df)
summary(df)
describe(df)

##-------------------------------------------------------------------------------------##
# pop smoker yes
smoker_yes <- subset(df,smoker == "yes")
summary(smoker_yes)

##-------------------------------------------------------------------------------------##
# pop smoker no
smoker_no <- subset(df,smoker == "no")
summary(smoker_no)

set.seed(612)
test_no <- sample_n(smoker_no, 135, fac = "ID")$ID
test_yes <- sample_n(smoker_yes, 135, fac = "ID")$ID
test <- c(test_no,test_yes)
sort(test)

# keep just the test data points/rows
all_test <- df[test,-1]
all_train <- df[-(test), -1]

#use the kknn package
library(kknn)

# build the knn model
# Check the data structure
str(all_train)
str(all_test)

# first, change the "smoker" variable to be of a factor type
all_train$smoker <- as.factor(all_train$smoker)
all_test$smoker <- as.factor((all_test$smoker))
# verify the change
str(all_train)
str(all_test)

# Run a weighted kNN / first use dataset train
model_knn = train.kknn(smoker ~ ., data=all_train,kmax=9) #.=use all column

# see the model's details
model_knn
summary(model_knn)

# Do a prediction on the test data
prediction <- predict(model_knn, all_test[, -5])
prediction

# See a confusion matrix. Each column of the matrix represents
# the number of predictions of each class, 
# while each row represents the instances in the actual class
CM <- table(all_test[, 5], prediction)
CM

#Insatll required packages
install.packages('caret')
#Import required library
library(caret)

#Creating confusion matrix
solution <- confusionMatrix(prediction, all_test[, 5])
#Display results 
solution

#*********************************************************************
#install required packages
install.packages('gmodels')
#import required library 
library(gmodels)

#Computes the crosstable calculations
CrossTable(all_test[, 5],prediction)

#*********************************************************************
#Creating confusion matrix
solution <- confusionMatrix(prediction, all_test[, 5])
#Display results 
solution

#**-------------------------------------------Hypothesis--------------------------------------------------***
#**------------------------------------------------------------------------------------------------***
library(BSDA)

# Read in our dataset
df <- read.csv("insurance.csv")

# Structure dataset
head(df)
str(df)
summary(df)
describe(df)


#-------------------------------Charges Distribution------------------------------------#
summary(df$charges)

ggplot(df, aes(x=charges))+
  geom_histogram(color="black", fill="Thistle", bins=40)+
  geom_vline(aes(xintercept= 13270), color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept= 9382), color="red", linetype="dashed", size=1)+
  annotate("text", x= 20000, y=110, size=5, label="Mean=13270", color="blue")+
  annotate("text", x= 20000, y=120, size=5, label="Median=9382", color="red")

# Boxplot: smoker
ggplot(df, aes(x=smoker, y=charges)) +
  geom_boxplot(fill=c(2:3))+
  stat_summary(fun=mean, geom="point", color="blue")+
  coord_flip()

# Boxplot: region
ggplot(df, aes(x=region, y=charges)) +
  geom_boxplot(fill=c(2:5))+
  stat_summary(fun=mean, geom="point", color="blue")+
  coord_flip()


df %>%
  group_by(smoker) %>%
  summarise(
    count = n(),
    min = min(charges),
    median = median(charges),
    max = max(charges),
    SD = sd(charges),
    Var = var(charges)
  ) %>%
  arrange(desc(median))

wilcox.test(df$charges ~ df$smoker)

##-------------------------------------------------------------------------------------##
# pop smoker yes
smoker_yes <- subset(df,smoker == "yes")
summary(smoker_yes)
sd(smoker_yes$charges)
var_data_a <- var(smoker_yes$charges)

##-------------------------------------------------------------------------------------##
# pop smoker no
smoker_no <- subset(df,smoker == "no")
summary(smoker_no)
sd(smoker_no$charges)
var_data_b <- var(smoker_no$charges)

#Compute the observed  z-statistic
z = (32050 - 8434)/sqrt((var(smoker_yes$charges)/274)+(var(smoker_no$charges)/1064))
print(z)

# smoker yes
sd(df$charges[df$smoker== "yes"])
var(df$charges[df$smoker== "yes"])

sd(df$charges[df$smoker== "no"])

##------------------------------------------------------------------------------------------##
##Use this code
z.test(smoker_yes$charges, smoker_no$charges, mu = 0, sigma.x = sd(smoker_yes$charges)
       , sigma.y = sd(smoker_no$charges), conf.level = 0.95)

##------------------------------------------------------------------------------------------##


z.test(df$charges[df$smoker== "yes"], df$charges[df$smoker== "no"],
       mu = 0, sigma.x = sd(df$charges[df$smoker== "yes"]), 
       sigma.y = sd(df$charges[df$smoker== "no"]),conf.level = 0.95)



z.test(df$charges[df$smoker== "yes"], df$charges[df$smoker== "no"], 
       alternative = "greater",mu = 0, sigma.x = sd(df$charges[df$smoker== "yes"]), sigma.y = sd(df$charges[df$smoker== "no"]), 
       conf.level = 0.95)

z.test(smoker_yes$charges, smoker_no$charges, alternative = "greater",
       mu = 0, sigma.x = sd(smoker_yes$charges), sigma.y = sd(smoker_no$charges), 
       conf.level = 0.95)

##-----------------------------------------------------------------------------------------------###

a <- smoker_yes$charges
b <- smoker_no$charges

z.test2sam = function(a, b, var.a, var.b){
  n.a = length(a)
  n.b = length(b)
  zeta = (mean(a) - mean(b)) / (sqrt(var.a/n.a + var.b/n.b))
  return(zeta)
}
z

res <- t.test(smoker_yes$charges, smoker_no$charges, var.equal = FALSE,
              conf.level = 0.95)
res
res$p.value
res$estimate
res$conf.int



x <- c(smoker_yes$charges)
sd_x<- sd(x)
sd_x
y <- c(smoker_no$charges)
sd_y<- sd(y)
sd_y
z.test(x, sigma.x=11541.55, y, sigma.y=5993.782,  alternative = "two.sided" ,conf.level=0.95)



smoker_yes <-  11541.55 / sqrt(274)               
smoker_no <-  5993.782 / sqrt(1064)  
SE      <- sqrt( smoker_yes^2 + smoker_no^2)                        
z       <- (32050 - 8434) / SE
P.z     <- pnorm(z, lower.tail = TRUE) 
P.z

z.test(smoker_yes$charges,  smoker_no$charges,mu=2, alternative = "two.sided",
       sigma.x = var_data_a,  sigma.y = var_data_b,  conf.level = 0.95)