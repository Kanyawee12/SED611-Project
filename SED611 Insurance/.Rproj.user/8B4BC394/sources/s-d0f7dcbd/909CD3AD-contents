# Load libraries
library(ggplot2)
library(ggthemes)
library(psych)
library(relaimpo)
library(tidyverse)
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








