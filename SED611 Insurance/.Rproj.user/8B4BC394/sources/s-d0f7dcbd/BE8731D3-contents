# Load libraries
library(ggplot2)
library(ggthemes)
library(psych)
library(relaimpo)
library(tidyverse)
library(BSDA)
# Read in our dataset
df <- read.csv("insurance.csv")

df %>%
  group_by(smoker) %>%
  summarise(
    count = n(),
    mean = mean(charges),
    SD = sd(charges),
    Var = var(charges)
  ) %>%
  arrange(desc(median))
##------------------------------------------------------------------------------------------##


z.test(df$charges[df$smoker== "yes"], df$charges[df$smoker== "no"],
       mu = 0, sigma.x = sd(df$charges[df$smoker== "yes"]), 
       sigma.y = sd(df$charges[df$smoker== "no"]),conf.level = 0.95)


