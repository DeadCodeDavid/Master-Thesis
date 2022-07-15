### 1) Initialize -------------------------------------------------------------------------------------------------------------------------
setwd("C:/Users/...")
# search for "Users" and enter path

library(readxl)
library(tidyr)
library(dplyr)
library(ggbreak)
library(forecast)
library(stringr)
library(psych)
library(stargazer)
library(car)
library(ggplot2)
library(extrafont)
library(caret)
library(e1071)
library(PMCMRplus)
library(corrplot)
library(cowplot)
library(jtools)
library(factoextra)
library(dendextend)
library(cluster)
library(plot3D)

## Color Codes for Plots

Dark_Blue     = "#150563"
Light_Blue    = "#8ecff5"
Grey          = "#c2cacf"
Blue          = "#02A8F6"

# importing data
All_releases = read.csv("All.csv", sep=";", header= TRUE)


### 2) Initial Descriptive Statistics -------------------------------------------------------------------------------------------------------------------------

columns = c("avg_rating","ma_avg_rating","delta_ma_avg_rating", #performance
            "sd_rating", "v_rating", "skew_rating", "kurt_rating", "skew_rating_date", "kurt_rating_date", #other
            "diff_time_total","intensity","Ye_intensity", #intensity
            "count_rating","rel_count_rating","ma_count_rating","rel_cust_part", #customer participation
            "novelty", "rel_novelty", # novelty
            "fileSizeBytes", #size
            "Lifestyle", "Games", "Photo_Video", "Social", #Genre
            "month", "weekdays", "0to4", "4to8", "8to12", "12to16", "16to20", "20to24" #Time
)

# Create All
All = All_releases[complete.cases(All_releases[ ,columns]),]


# Simple descriptive statistics
Genre_metric1 = round(aggregate(avg_rating ~ mainKey, data = All, FUN = mean),2)
Genre_metric2 = round(aggregate(sd_rating ~ mainKey, data = All, FUN = mean),2)
Genre_metric3 = round(aggregate(v_rating ~ mainKey, data = All, FUN = mean),2)
Genre_metric4 = round(aggregate(skew_rating ~ mainKey, data = All, FUN = mean),2)
Genre_metric5 = round(aggregate(skew_rating_date ~ mainKey, data = All, FUN = mean),2)
Genre_metric6 = round(aggregate(count_rating ~ mainKey, data = All, FUN = mean),2)
Genre_metric7 = round(aggregate(diff_time_total ~ mainKey, data = All, FUN = mean),2)

Genre_metric_all = cbind(Genre_metric1, Genre_metric2$sd_rating, Genre_metric3$v_rating, Genre_metric4$skew_rating, Genre_metric5$skew_rating_date, Genre_metric6$count_rating, Genre_metric7$diff_time_total)
colnames(Genre_metric_all) = c("mainKey", "avg_rating","sd_rating","v_rating","skew_rating","skew_rating_date","count_rating","diff_time_total")
Genre_metric_all$mainGenre = c("Business", "Utilities","Sports","Social Networking","Productivity","Photo & Video","Lifestyle","Games","Finance","Entertainment","Shopping")
All$index = 1
Genre_metric_all$updates = aggregate(index ~ mainKey, data = All, FUN = sum)[,2]
temp = numeric(0)
for(i in unique(All$mainKey)){
  temp = c(temp, length(unique(All$appId[All$mainKey == i])))
}
Genre_metric_all$apps = temp
temp = NULL

Genre_metric1 = NULL
Genre_metric2 = NULL
Genre_metric3 = NULL
Genre_metric4 = NULL
Genre_metric5 = NULL
Genre_metric6 = NULL
Genre_metric7 = NULL

# some company stats
All$updates = 1
companies = aggregate(cbind(updates,count_rating) ~ appName, All,FUN = sum, na.rm=TRUE, na.action=na.pass)
companies$avg_count = round(companies$count_rating/companies$updates,0)

# Create Panel Data
sort(table(All$mainGenre), decreasing = TRUE)

Lifestyle = All[All$mainGenre == "Lifestyle",]
length(unique(Lifestyle$appId))

Games = All[All$mainGenre == "Games",]
length(unique(Games$appId))

Photo_Video = All[All$mainGenre == "Photo & Video",]
length(unique(Photo_Video$appId))

Social = All[All$mainGenre == "Social Networking",]
length(unique(Social$appId))



### 3) Distribution Visulaization -------------------------------------------------------------------------------------------------------------------------

bin = 8

##All

## Novelty
bin = 8
trans = All
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h1 = ggplot(trans, aes(x = log(novelty))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h1 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$novelty+1), na.rm = TRUE), 
                                            sd = sd(log(trans$novelty+1), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Social
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h2 = ggplot(trans, aes(x = log(novelty))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h2 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$novelty+1), na.rm = TRUE), 
                                            sd = sd(log(trans$novelty+1), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Games
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h3 = ggplot(trans, aes(x = log(novelty))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h3 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$novelty+1), na.rm = TRUE), 
                                            sd = sd(log(trans$novelty+1), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Photo_Video
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h4 = ggplot(trans, aes(x = log(novelty))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h4 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$novelty+1), na.rm = TRUE), 
                                            sd = sd(log(trans$novelty+1), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Lifestyle
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h5 = ggplot(trans, aes(x = log(novelty))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h5 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$novelty+1), na.rm = TRUE), 
                                            sd = sd(log(trans$novelty+1), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

##Intensity

#diff_time_total
trans = All
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h6 = ggplot(trans, aes(x = log(diff_time_total))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h6 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$diff_time_total), na.rm = TRUE), 
                                            sd = sd(log(trans$diff_time_total), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Social
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h7 = ggplot(trans, aes(x = log(diff_time_total))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h7 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$diff_time_total), na.rm = TRUE), 
                                            sd = sd(log(trans$diff_time_total), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Games
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h8 = ggplot(trans, aes(x = log(diff_time_total))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h8 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$diff_time_total), na.rm = TRUE), 
                                            sd = sd(log(trans$diff_time_total), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Photo_Video
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h9 = ggplot(trans, aes(x = log(diff_time_total))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h9 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$diff_time_total), na.rm = TRUE), 
                                            sd = sd(log(trans$diff_time_total), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Lifestyle
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h10 = ggplot(trans, aes(x = log(diff_time_total))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h10 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$diff_time_total), na.rm = TRUE), 
                                             sd = sd(log(trans$diff_time_total), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

#Ye_intensity
trans = All
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h11 = ggplot(trans, aes(x = log(Ye_intensity))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h11 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$Ye_intensity), na.rm = TRUE), 
                                             sd = sd(log(trans$Ye_intensity), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Social
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h12 = ggplot(trans, aes(x = log(Ye_intensity))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h12 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$Ye_intensity), na.rm = TRUE), 
                                             sd = sd(log(trans$Ye_intensity), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Games
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h13 = ggplot(trans, aes(x = log(Ye_intensity))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h13 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$Ye_intensity), na.rm = TRUE), 
                                             sd = sd(log(trans$Ye_intensity), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Photo_Video
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h14 = ggplot(trans, aes(x = log(Ye_intensity))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h14 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$Ye_intensity), na.rm = TRUE), 
                                             sd = sd(log(trans$Ye_intensity), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Lifestyle
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h15 = ggplot(trans, aes(x = log(Ye_intensity))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h15 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$Ye_intensity), na.rm = TRUE), 
                                             sd = sd(log(trans$Ye_intensity), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

##Intensity

trans = All
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h16 = ggplot(trans, aes(x = log(intensity))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h16 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$intensity), na.rm = TRUE), 
                                             sd = sd(log(trans$intensity), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Social
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h17 = ggplot(trans, aes(x = log(intensity))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h17 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$intensity), na.rm = TRUE), 
                                             sd = sd(log(trans$intensity), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Games
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h18 = ggplot(trans, aes(x = log(intensity))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h18 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$intensity), na.rm = TRUE), 
                                             sd = sd(log(trans$intensity), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Photo_Video
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h19 = ggplot(trans, aes(x = log(intensity))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h19 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$intensity), na.rm = TRUE), 
                                             sd = sd(log(trans$intensity), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Lifestyle
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h20 = ggplot(trans, aes(x = log(intensity))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h20 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$intensity), na.rm = TRUE), 
                                             sd = sd(log(trans$intensity), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

## Customer Particpation

trans = All
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h21 = ggplot(trans, aes(x = log(count_rating))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h21 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$count_rating), na.rm = TRUE), 
                                             sd = sd(log(trans$count_rating), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Social
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h22 = ggplot(trans, aes(x = log(count_rating))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h22 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$count_rating), na.rm = TRUE), 
                                             sd = sd(log(trans$count_rating), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Games
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h23 = ggplot(trans, aes(x = log(count_rating))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h23 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$count_rating), na.rm = TRUE), 
                                             sd = sd(log(trans$count_rating), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Photo_Video
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h24 = ggplot(trans, aes(x = log(count_rating))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h24 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$count_rating), na.rm = TRUE), 
                                             sd = sd(log(trans$count_rating), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Lifestyle
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h25 = ggplot(trans, aes(x = log(count_rating))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h25 + stat_function(fun = dnorm, args = list(mean = mean(log(trans$count_rating), na.rm = TRUE), 
                                             sd = sd(log(trans$count_rating), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

## Performance

trans = All
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h26 = ggplot(trans, aes(x = (avg_rating))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h26 + stat_function(fun = dnorm, args = list(mean = mean((trans$avg_rating), na.rm = TRUE), 
                                             sd = sd((trans$avg_rating), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Social
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h27 = ggplot(trans, aes(x = (avg_rating))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h27 + stat_function(fun = dnorm, args = list(mean = mean((trans$avg_rating), na.rm = TRUE), 
                                             sd = sd((trans$avg_rating), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Games
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h28 = ggplot(trans, aes(x = (avg_rating))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h28 + stat_function(fun = dnorm, args = list(mean = mean((trans$avg_rating), na.rm = TRUE), 
                                             sd = sd((trans$avg_rating), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Photo_Video
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h29 = ggplot(trans, aes(x = (avg_rating))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h29 + stat_function(fun = dnorm, args = list(mean = mean((trans$avg_rating), na.rm = TRUE), 
                                             sd = sd((trans$avg_rating), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

trans = Lifestyle
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
h30 = ggplot(trans, aes(x = (avg_rating))) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "white", bins= bin) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=12), plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
h30 + stat_function(fun = dnorm, args = list(mean = mean((trans$avg_rating), na.rm = TRUE), 
                                             sd = sd((trans$avg_rating), na.rm = TRUE)), size = 1, linetype="dashed")
trans = NULL

library(cowplot)
plotgrid <- plot_grid(ncol = 5,
                      h1 + stat_function(fun = dnorm, args = list(mean = mean(log(All$novelty+1), na.rm = TRUE), 
                                                                  sd = sd(log(All$novelty+1), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h5 + stat_function(fun = dnorm, args = list(mean = mean(log(Lifestyle$novelty+1), na.rm = TRUE), 
                                                                   sd = sd(log(Lifestyle$novelty+1), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h3 + stat_function(fun = dnorm, args = list(mean = mean(log(Games$novelty+1), na.rm = TRUE), 
                                                                   sd = sd(log(Games$novelty+1), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h4 + stat_function(fun = dnorm, args = list(mean = mean(log(Photo_Video$novelty+1), na.rm = TRUE), 
                                                                   sd = sd(log(Photo_Video$novelty+1), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h2 + stat_function(fun = dnorm, args = list(mean = mean(log(Social$novelty+1), na.rm = TRUE), 
                                                                   sd = sd(log(Social$novelty+1), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h11 + stat_function(fun = dnorm, args = list(mean = mean(log(All$Ye_intensity), na.rm = TRUE), 
                                                                    sd = sd(log(All$Ye_intensity), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h15 + stat_function(fun = dnorm, args = list(mean = mean(log(Lifestyle$Ye_intensity), na.rm = TRUE), 
                                                                    sd = sd(log(Lifestyle$Ye_intensity), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h13 + stat_function(fun = dnorm, args = list(mean = mean(log(Games$Ye_intensity), na.rm = TRUE), 
                                                                    sd = sd(log(Games$Ye_intensity), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h14 + stat_function(fun = dnorm, args = list(mean = mean(log(Photo_Video$Ye_intensity), na.rm = TRUE), 
                                                                    sd = sd(log(Photo_Video$Ye_intensity), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h12 + stat_function(fun = dnorm, args = list(mean = mean(log(Social$Ye_intensity), na.rm = TRUE), 
                                                                    sd = sd(log(Social$Ye_intensity), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h16 + stat_function(fun = dnorm, args = list(mean = mean(log(All$intensity), na.rm = TRUE), 
                                                                    sd = sd(log(All$intensity), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h20 + stat_function(fun = dnorm, args = list(mean = mean(log(Lifestyle$intensity), na.rm = TRUE), 
                                                                    sd = sd(log(Lifestyle$intensity), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h18 + stat_function(fun = dnorm, args = list(mean = mean(log(Games$intensity), na.rm = TRUE), 
                                                                    sd = sd(log(Games$intensity), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h19 + stat_function(fun = dnorm, args = list(mean = mean(log(Photo_Video$intensity), na.rm = TRUE), 
                                                                    sd = sd(log(Photo_Video$intensity), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h17 + stat_function(fun = dnorm, args = list(mean = mean(log(Social$intensity), na.rm = TRUE), 
                                                                    sd = sd(log(Social$intensity), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h21 + stat_function(fun = dnorm, args = list(mean = mean(log(All$count_rating), na.rm = TRUE), 
                                                                    sd = sd(log(All$count_rating), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h25 + stat_function(fun = dnorm, args = list(mean = mean(log(Lifestyle$count_rating), na.rm = TRUE), 
                                                                    sd = sd(log(Lifestyle$count_rating), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h23 + stat_function(fun = dnorm, args = list(mean = mean(log(Games$count_rating), na.rm = TRUE), 
                                                                    sd = sd(log(Games$count_rating), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h24 + stat_function(fun = dnorm, args = list(mean = mean(log(Photo_Video$count_rating), na.rm = TRUE), 
                                                                    sd = sd(log(Photo_Video$count_rating), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h22 + stat_function(fun = dnorm, args = list(mean = mean(log(Social$count_rating), na.rm = TRUE), 
                                                                    sd = sd(log(Social$count_rating), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h26 + stat_function(fun = dnorm, args = list(mean = mean((All$avg_rating), na.rm = TRUE), 
                                                                    sd = sd((All$avg_rating), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h30 + stat_function(fun = dnorm, args = list(mean = mean((Lifestyle$avg_rating), na.rm = TRUE), 
                                                                    sd = sd((Lifestyle$avg_rating), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h28 + stat_function(fun = dnorm, args = list(mean = mean((Games$avg_rating), na.rm = TRUE), 
                                                                    sd = sd((Games$avg_rating), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h29 + stat_function(fun = dnorm, args = list(mean = mean((Photo_Video$avg_rating), na.rm = TRUE), 
                                                                    sd = sd((Photo_Video$avg_rating), na.rm = TRUE)), size = 1, linetype="dashed")
                      ,h27 + stat_function(fun = dnorm, args = list(mean = mean((Social$avg_rating), na.rm = TRUE), 
                                                                    sd = sd((Social$avg_rating), na.rm = TRUE)), size = 1, linetype="dashed"))


plotgrid




{
  par(mfrow=c(2,2))
  
  ##Novelty
  hist((All$novelty))
  hist(1/(All$novelty))
  hist(log(All$novelty+1)) #2best
  hist(sqrt(All$novelty)) #best
  
  ##Intensity
  hist((All$diff_time_total))
  hist(1/(All$diff_time_total))
  hist(log(All$diff_time_total)) #best
  hist(sqrt(All$diff_time_total)) #2best
  #
  hist((All$Ye_intensity))
  hist(1/(All$Ye_intensity))
  hist(log(All$Ye_intensity)) #2best (did they use this?)
  hist(sqrt(All$Ye_intensity)) #best
  #
  hist((All$intensity))
  hist(1/(All$intensity))
  hist(log(All$intensity)) #best
  hist(sqrt(All$intensity)) #2best
  
  ## Customer Participation
  hist((All$count_rating))
  hist(1/(All$count_rating))
  hist(log(All$count_rating)) #best
  hist(sqrt(All$count_rating)) #2best
  
  ## Performance 
  hist((All$avg_rating)) #best
  hist(1/(All$avg_rating))
  hist(log(All$avg_rating)) 
  hist(sqrt(All$avg_rating))
  
  par(mfrow=c(1,1))
  
  ## Social 
  
  par(mfrow=c(2,2))
  ##Novelty
  hist((Social$novelty))
  hist(1/(Social$novelty))
  hist(log(Social$novelty)) #2best
  hist(sqrt(Social$novelty)) #best
  
  ##Intensity
  hist((Social$diff_time_total))
  hist(1/(Social$diff_time_total))
  hist(log(Social$diff_time_total)) #best
  hist(sqrt(Social$diff_time_total)) #2best
  #
  hist((Social$Ye_intensity))
  hist(1/(Social$Ye_intensity))
  hist(log(Social$Ye_intensity)) #2best (did they use this?)
  hist(sqrt(Social$Ye_intensity)) #best
  #
  hist((Social$intensity))
  hist(1/(Social$intensity))
  hist(log(Social$intensity)) #best
  hist(sqrt(Social$intensity)) #2best
  
  ## Customer Participation
  hist((Social$count_rating))
  hist(1/(Social$count_rating))
  hist(log(Social$count_rating)) #best
  hist(sqrt(Social$count_rating)) #2best
  
  ## Performance 
  hist((Social$avg_rating)) #best
  hist(1/(Social$avg_rating))
  hist(log(Social$avg_rating)) 
  hist(sqrt(Social$avg_rating))
  
  par(mfrow=c(1,1))
  
  ## Games 
  
  par(mfrow=c(2,2))
  ##Novelty
  hist((Games$novelty))
  hist(1/(Games$novelty))
  hist(log(Games$novelty)) #2best
  hist(sqrt(Games$novelty)) #best
  
  ##Intensity
  hist((Games$diff_time_total))
  hist(1/(Games$diff_time_total))
  hist(log(Games$diff_time_total)) #best
  hist(sqrt(Games$diff_time_total)) #2best
  #
  hist((Games$Ye_intensity))
  hist(1/(Games$Ye_intensity))
  hist(log(Games$Ye_intensity)) #2best (did they use this?)
  hist(sqrt(Games$Ye_intensity)) #best
  #
  hist((Games$intensity))
  hist(1/(Games$intensity))
  hist(log(Games$intensity)) #best
  hist(sqrt(Games$intensity)) #2best
  
  ## Customer Participation
  hist((Games$count_rating))
  hist(1/(Games$count_rating))
  hist(log(Games$count_rating)) #best
  hist(sqrt(Games$count_rating)) #2best
  
  ## Performance 
  hist((Games$avg_rating)) #best
  hist(1/(Games$avg_rating))
  hist(log(Games$avg_rating)) 
  hist(sqrt(Games$avg_rating))
  
  par(mfrow=c(1,1))
  
  
  ## Photo & Video
  
  par(mfrow=c(2,2))
  ##Novelty
  hist((Photo_Video$novelty))
  hist(1/(Photo_Video$novelty))
  hist(log(Photo_Video$novelty)) #2best
  hist(sqrt(Photo_Video$novelty)) #best
  
  ##Intensity
  hist((Photo_Video$diff_time_total))
  hist(1/(Photo_Video$diff_time_total))
  hist(log(Photo_Video$diff_time_total)) #best
  hist(sqrt(Photo_Video$diff_time_total)) #2best
  #
  hist((Photo_Video$Ye_intensity))
  hist(1/(Photo_Video$Ye_intensity))
  hist(log(Photo_Video$Ye_intensity)) #2best (did they use this?)
  hist(sqrt(Photo_Video$Ye_intensity)) #best
  #
  hist((Photo_Video$intensity))
  hist(1/(Photo_Video$intensity))
  hist(log(Photo_Video$intensity)) #best
  hist(sqrt(Photo_Video$intensity)) #2best
  
  ## Customer Participation
  hist((Photo_Video$count_rating))
  hist(1/(Photo_Video$count_rating))
  hist(log(Photo_Video$count_rating)) #best
  hist(sqrt(Photo_Video$count_rating)) #2best
  
  ## Performance 
  hist((Photo_Video$avg_rating)) #best
  hist(1/(Photo_Video$avg_rating))
  hist(log(Photo_Video$avg_rating)) 
  hist(sqrt(Photo_Video$avg_rating))
  
  par(mfrow=c(1,1))
  
  
  ## Lifestyle
  
  par(mfrow=c(2,2))
  ##Novelty
  hist((Lifestyle$novelty))
  hist(1/(Lifestyle$novelty))
  hist(log(Lifestyle$novelty)) #2best
  hist(sqrt(Lifestyle$novelty)) #best
  
  ##Intensity
  hist((Lifestyle$diff_time_total))
  hist(1/(Lifestyle$diff_time_total))
  hist(log(Lifestyle$diff_time_total)) #best
  hist(sqrt(Lifestyle$diff_time_total)) #2best
  #
  hist((Lifestyle$Ye_intensity))
  hist(1/(Lifestyle$Ye_intensity))
  hist(log(Lifestyle$Ye_intensity)) #2best (did they use this?)
  hist(sqrt(Lifestyle$Ye_intensity)) #best
  #
  hist((Lifestyle$intensity))
  hist(1/(Lifestyle$intensity))
  hist(log(Lifestyle$intensity)) #best
  hist(sqrt(Lifestyle$intensity)) #2best
  
  ## Customer Participation
  hist((Lifestyle$count_rating))
  hist(1/(Lifestyle$count_rating))
  hist(log(Lifestyle$count_rating)) #best
  hist(sqrt(Lifestyle$count_rating)) #2best
  
  ## Performance 
  hist((Lifestyle$avg_rating)) #best
  hist(1/(Lifestyle$avg_rating))
  hist(log(Lifestyle$avg_rating)) 
  hist(sqrt(Lifestyle$avg_rating))
  
  par(mfrow=c(1,1))
}


### 4) Transform Dataset -------------------------------------------------------------------------------------------------------------------------

## Include all necessary data to assess the Outliers

columns = c("avg_rating","ma_avg_rating","delta_ma_avg_rating", #performance
            "sd_rating", "v_rating", "skew_rating", "kurt_rating", "skew_rating_date", "kurt_rating_date", #other
            "diff_time_total","intensity","Ye_intensity", #intensity
            "count_rating","rel_count_rating","ma_count_rating","rel_cust_part", #customer participation
            "novelty", "rel_novelty", # novelty
            "fileSizeBytes", #size
            "Lifestyle", "Games", "Photo_Video", "Social", #Genre
            "month", "weekdays", "0to4", "4to8", "8to12", "12to16", "16to20", "20to24" #Time
)

# transforming
Trans = cbind.data.frame(avg_rating =(All$avg_rating), ma_avg_rating = (All$ma_avg_rating), delta_ma_avg_rating = (All$delta_ma_avg_rating), #performance
                         sd_rating = (All$sd_rating), v_rating = (All$v_rating), skew_rating = (All$skew_rating), kurt_rating = (All$kurt_rating), skew_rating_date = (All$skew_rating_date), kurt_rating_date = (All$kurt_rating_date), #other
                         intensity = -log(All$intensity), Ye_intensity = log(All$Ye_intensity),diff_time_total = log(All$diff_time_total), #intensity
                         count_rating = log(All$count_rating),rel_count_rating = (All$rel_count_rating), ma_count_rating = (All$ma_count_rating), rel_cust_part = (All$rel_cust_part), #customer participation
                         novelty = log(All$novelty), rel_novelty = log(All$rel_novelty), #novelty
                         log_size = All$log_size, version_major = All$version_major, version_minor = All$version_minor, version_patch = All$version_patch, #size
                         mainGenre = All$mainGenre, sellerName = All$sellerName, releaseNotes = All$releaseNotes, dateShort = All$dateShort, #genral
                         Lifestyle = (All$Lifestyle), Games = (All$Games), Social = (All$Social), Photo_Video = (All$Photo_Video), #genre
                         month = (All$month), weekdays = (All$weekdays), "0to4" = (All$`0to4`), "4to8" = (All$`4to8`), "8to12" = (All$`12to16`), "12to16" = (All$`16to20`), "16to20" = (All$`16to20`), "20to24" = (All$`20to24`) #time
)

# Untransformed data for KS-Test in Appendix

# Trans = cbind.data.frame(avg_rating =(All$avg_rating), ma_avg_rating = (All$ma_avg_rating), delta_ma_avg_rating = (All$delta_ma_avg_rating), #performance
#                          sd_rating = (All$sd_rating), v_rating = (All$v_rating), skew_rating = (All$skew_rating), kurt_rating = (All$kurt_rating), skew_rating_date = (All$skew_rating_date), kurt_rating_date = (All$kurt_rating_date), #other
#                          intensity = All$intensity, Ye_intensity = All$Ye_intensity,diff_time_total = All$diff_time_total, #intensity
#                          count_rating = All$count_rating,rel_count_rating = All$rel_count_rating, ma_count_rating = (All$ma_count_rating), rel_cust_part = (All$rel_cust_part), #customer participation
#                          novelty = log(All$novelty), rel_novelty = log(All$rel_novelty), #novelty
#                          log_size = All$log_size, version_major = All$version_major, version_minor = All$version_minor, version_patch = All$version_patch, #size
#                          mainGenre = All$mainGenre, sellerName = All$sellerName, releaseNotes = All$releaseNotes, dateShort = All$dateShort, #genral
#                          Lifestyle = (All$Lifestyle), Games = (All$Games), Social = (All$Social), Photo_Video = (All$Photo_Video), #genre
#                          month = (All$month), weekdays = (All$weekdays), "0to4" = (All$`0to4`), "4to8" = (All$`4to8`), "8to12" = (All$`12to16`), "12to16" = (All$`16to20`), "16to20" = (All$`16to20`), "20to24" = (All$`20to24`) #time
# )


for(i in 1:nrow(Trans)){
  ifelse(is.infinite(Trans$novelty[i]),(Trans$novelty[i]=0),(Trans$novelty[i]=Trans$novelty[i]))
  ifelse(is.infinite(Trans$rel_novelty[i]),(Trans$rel_novelty[i]=0),(Trans$novelty[i]=Trans$novelty[i]))
}
Trans$log_size[is.na(Trans$log_size)] = 0

Trans = Trans[order(Trans$dateShort),]

# transformed panels
sort(table(Trans$mainGenre), decreasing = TRUE)

Trans$frequency = Trans$intensity

Lifestyle = Trans[Trans$mainGenre == "Lifestyle",]

Games = Trans[Trans$mainGenre == "Games",]

Photo_Video = Trans[Trans$mainGenre == "Photo & Video",]

Social = Trans[Trans$mainGenre == "Social Networking",]

### 5) KS Test for Distribution -------------------------------------------------------------------------------------------------------------------------

options(scipen = 999)
KS_Trans = data.frame(dataset = c("Trans","Lifestyle","Games","Photo & Video","Social Networking"),
                      novelty = c(as.numeric(ks.test(Trans$novelty,"pnorm",mean(Trans$novelty),sd(Trans$novelty))[2]),
                                  as.numeric(ks.test(Lifestyle$novelty,"pnorm",mean(Lifestyle$novelty),sd(Lifestyle$novelty))[2]),
                                  as.numeric(ks.test(Games$novelty,"pnorm",mean(Games$novelty),sd(Games$novelty))[2]),
                                  as.numeric(ks.test(Photo_Video$novelty,"pnorm",mean(Photo_Video$novelty),sd(Photo_Video$novelty))[2]),
                                  as.numeric(ks.test(Social$novelty,"pnorm",mean(Social$novelty),sd(Social$novelty))[2])
                      ),
                      diff_time_total = c(as.numeric(ks.test(Trans$diff_time_total,"pnorm",mean(Trans$diff_time_total),sd(Trans$diff_time_total))[2]),
                                          as.numeric(ks.test(Lifestyle$diff_time_total,"pnorm",mean(Lifestyle$diff_time_total),sd(Lifestyle$diff_time_total))[2]),
                                          as.numeric(ks.test(Games$diff_time_total,"pnorm",mean(Games$diff_time_total),sd(Games$diff_time_total))[2]),
                                          as.numeric(ks.test(Photo_Video$diff_time_total,"pnorm",mean(Photo_Video$diff_time_total),sd(Photo_Video$diff_time_total))[2]),
                                          as.numeric(ks.test(Social$diff_time_total,"pnorm",mean(Social$diff_time_total),sd(Social$diff_time_total))[2])
                      ),
                      Ye_intensity = c(as.numeric(ks.test(Trans$Ye_intensity,"pnorm",mean(Trans$Ye_intensity),sd(Trans$Ye_intensity))[2]),
                                       as.numeric(ks.test(Lifestyle$Ye_intensity,"pnorm",mean(Lifestyle$Ye_intensity),sd(Lifestyle$Ye_intensity))[2]),
                                       as.numeric(ks.test(Games$Ye_intensity,"pnorm",mean(Games$Ye_intensity),sd(Games$Ye_intensity))[2]),
                                       as.numeric(ks.test(Photo_Video$Ye_intensity,"pnorm",mean(Photo_Video$Ye_intensity),sd(Photo_Video$Ye_intensity))[2]),
                                       as.numeric(ks.test(Social$Ye_intensity,"pnorm",mean(Social$Ye_intensity),sd(Social$Ye_intensity))[2])
                      ),
                      intensity = c(as.numeric(ks.test(Trans$intensity,"pnorm",mean(Trans$intensity),sd(Trans$intensity))[2]),
                                    as.numeric(ks.test(Lifestyle$intensity,"pnorm",mean(Lifestyle$intensity),sd(Lifestyle$intensity))[2]),
                                    as.numeric(ks.test(Games$intensity,"pnorm",mean(Games$intensity),sd(Games$intensity))[2]),
                                    as.numeric(ks.test(Photo_Video$intensity,"pnorm",mean(Photo_Video$intensity),sd(Photo_Video$intensity))[2]),
                                    as.numeric(ks.test(Social$intensity,"pnorm",mean(Social$intensity),sd(Social$intensity))[2])
                      ),
                      count_rating = c(as.numeric(ks.test(Trans$count_rating,"pnorm",mean(Trans$count_rating),sd(Trans$count_rating))[2]),
                                       as.numeric(ks.test(Lifestyle$count_rating,"pnorm",mean(Lifestyle$count_rating),sd(Lifestyle$count_rating))[2]),
                                       as.numeric(ks.test(Games$count_rating,"pnorm",mean(Games$count_rating),sd(Games$count_rating))[2]),
                                       as.numeric(ks.test(Photo_Video$count_rating,"pnorm",mean(Photo_Video$count_rating),sd(Photo_Video$count_rating))[2]),
                                       as.numeric(ks.test(Social$count_rating,"pnorm",mean(Social$count_rating),sd(Social$count_rating))[2])
                      ),
                      avg_rating = c(as.numeric(ks.test(Trans$avg_rating,"pnorm",mean(Trans$avg_rating),sd(Trans$avg_rating))[2]),
                                     as.numeric(ks.test(Lifestyle$avg_rating,"pnorm",mean(Lifestyle$avg_rating),sd(Lifestyle$avg_rating))[2]),
                                     as.numeric(ks.test(Games$avg_rating,"pnorm",mean(Games$avg_rating),sd(Games$avg_rating))[2]),
                                     as.numeric(ks.test(Photo_Video$avg_rating,"pnorm",mean(Photo_Video$avg_rating),sd(Photo_Video$avg_rating))[2]),
                                     as.numeric(ks.test(Social$avg_rating,"pnorm",mean(Social$avg_rating),sd(Social$avg_rating))[2])
                      )
)

KS_Trans[,2:7] = round(KS_Trans[,2:6], 4)
KS_Trans = cbind(KS_Trans, "Sig1","Sig2","Sig3","Sig4","Sig5","Sig6")
KS_Trans[,8:13] = as.character(KS_Trans[,8:13])
str(KS_Trans)
for(j in 2:7){
  for(i in 1:nrow(KS_Trans)){
    if(as.numeric(KS_Trans[i,j]) > 0.05){
      KS_Trans[i,(j+6)]= ""
    }
    if(as.numeric(KS_Trans[i,j]) < 0.05 && as.numeric(KS_Trans[i,j])>0.01){
      KS_Trans[i,(j+6)]= "*"
    }
    if(as.numeric(KS_Trans[i,j]) < 0.01 && as.numeric(KS_Trans[i,j])>0.001){
      KS_Trans[i,(j+6)]= "**"
    }
    if(as.numeric(KS_Trans[i,j]) < 0.001){
      KS_Trans[i,(j+6)]= "***"
    }
  }
}

#KS_Trans = KS_Trans[ ,c("dataset","novelty","Sig1","diff_time_total","Sig2","Ye_intensity","Sig3","intensity","Sig4","count_rating","Sig5","avg_rating","Sig6")]
KS_Trans = KS_Trans[ ,c(1,2,8,3,9,4,10,5,11,6,12,7,13)]

### 6) Table stats -------------------------------------------------------------------------------------------------------------------------

options(scipen = 999)

Table = data.frame(Observations_total = c(nrow(Trans),nrow(Social),nrow(Games),nrow(Photo_Video),nrow(Lifestyle)),
                   Observations_rel = c(nrow(Trans)/nrow(Trans),nrow(Social)/nrow(Trans),nrow(Games)/nrow(Trans),nrow(Photo_Video)/nrow(Trans),nrow(Lifestyle)/nrow(Trans)),
                   novelty = c("","","","",""),
                   mean = c(mean(Trans$novelty),mean(Social$novelty),mean(Games$novelty),mean(Photo_Video$novelty),mean(Lifestyle$novelty)),
                   sd = c(sd(Trans$novelty),sd(Social$novelty),sd(Games$novelty),sd(Photo_Video$novelty),sd(Lifestyle$novelty)),
                   range = c(summary(Trans$novelty)[6]-summary(Trans$novelty)[6],summary(Social$novelty)[6]-summary(Social$novelty)[1],summary(Games$novelty)[6]-summary(Games$novelty)[1],summary(Photo_Video$novelty)[6]-summary(Photo_Video$novelty)[1],summary(Lifestyle$novelty)[6]-summary(Lifestyle$novelty)[1]),
                   skewness = c(skewness(Trans$novelty),skewness(Social$novelty),skewness(Games$novelty),skewness(Photo_Video$novelty),skewness(Lifestyle$novelty)),
                   Ye_intensity = c("","","","",""),
                   mean = c(mean(Trans$Ye_intensity),mean(Social$Ye_intensity),mean(Games$Ye_intensity),mean(Photo_Video$Ye_intensity),mean(Lifestyle$Ye_intensity)),
                   sd = c(sd(Trans$Ye_intensity),sd(Social$Ye_intensity),sd(Games$Ye_intensity),sd(Photo_Video$Ye_intensity),sd(Lifestyle$Ye_intensity)),
                   range = c(summary(Trans$Ye_intensity)[6]-summary(Trans$Ye_intensity)[6],summary(Social$Ye_intensity)[6]-summary(Social$Ye_intensity)[1],summary(Games$Ye_intensity)[6]-summary(Games$Ye_intensity)[1],summary(Photo_Video$Ye_intensity)[6]-summary(Photo_Video$Ye_intensity)[1],summary(Lifestyle$Ye_intensity)[6]-summary(Lifestyle$Ye_intensity)[1]),
                   skewness = c(skewness(Trans$Ye_intensity),skewness(Social$Ye_intensity),skewness(Games$Ye_intensity),skewness(Photo_Video$Ye_intensity),skewness(Lifestyle$Ye_intensity)),
                   intensity = c("","","","",""),
                   mean = c(mean(Trans$intensity),mean(Social$intensity),mean(Games$intensity),mean(Photo_Video$intensity),mean(Lifestyle$intensity)),
                   sd = c(sd(Trans$intensity),sd(Social$intensity),sd(Games$intensity),sd(Photo_Video$intensity),sd(Lifestyle$intensity)),
                   range = c(summary(Trans$intensity)[6]-summary(Trans$intensity)[6],summary(Social$intensity)[6]-summary(Social$intensity)[1],summary(Games$intensity)[6]-summary(Games$intensity)[1],summary(Photo_Video$intensity)[6]-summary(Photo_Video$intensity)[1],summary(Lifestyle$intensity)[6]-summary(Lifestyle$intensity)[1]),
                   skewness = c(skewness(Trans$intensity),skewness(Social$intensity),skewness(Games$intensity),skewness(Photo_Video$intensity),skewness(Lifestyle$intensity)),
                   count_rating = c("","","","",""),
                   mean = c(mean(Trans$count_rating),mean(Social$count_rating),mean(Games$count_rating),mean(Photo_Video$count_rating),mean(Lifestyle$count_rating)),
                   sd = c(sd(Trans$count_rating),sd(Social$count_rating),sd(Games$count_rating),sd(Photo_Video$count_rating),sd(Lifestyle$count_rating)),
                   range = c(summary(Trans$count_rating)[6]-summary(Trans$count_rating)[6],summary(Social$count_rating)[6]-summary(Social$count_rating)[1],summary(Games$count_rating)[6]-summary(Games$count_rating)[1],summary(Photo_Video$count_rating)[6]-summary(Photo_Video$count_rating)[1],summary(Lifestyle$count_rating)[6]-summary(Lifestyle$count_rating)[1]),
                   skewness = c(skewness(Trans$count_rating),skewness(Social$count_rating),skewness(Games$count_rating),skewness(Photo_Video$count_rating),skewness(Lifestyle$count_rating)),
                   avg_rating = c("","","","",""),
                   mean = c(mean(Trans$avg_rating),mean(Social$avg_rating),mean(Games$avg_rating),mean(Photo_Video$avg_rating),mean(Lifestyle$avg_rating)),
                   sd = c(sd(Trans$avg_rating),sd(Social$avg_rating),sd(Games$avg_rating),sd(Photo_Video$avg_rating),sd(Lifestyle$avg_rating)),
                   range = c(summary(Trans$avg_rating)[6]-summary(Trans$avg_rating)[6],summary(Social$avg_rating)[6]-summary(Social$avg_rating)[1],summary(Games$avg_rating)[6]-summary(Games$avg_rating)[1],summary(Photo_Video$avg_rating)[6]-summary(Photo_Video$avg_rating)[1],summary(Lifestyle$avg_rating)[6]-summary(Lifestyle$avg_rating)[1]),
                   skewness = c(skewness(Trans$avg_rating),skewness(Social$avg_rating),skewness(Games$avg_rating),skewness(Photo_Video$avg_rating),skewness(Lifestyle$avg_rating))
)
Table_stats = data.frame(t(Table))
colnames(Table_stats) = c("Complete Sample","Social Networking","Games","Photo & Video","Lifestyle")


# Mean & Error Bar Plot
ggplot(Table, aes(x=c("Complete Sample","Social Networking","Games","Photo & Video","Lifestyle"), y=mean.4)) + 
  geom_bar(stat="identity", position=position_dodge(), width=.4, fill = Light_Blue) +
  geom_errorbar(aes(ymin=mean.4-sd.4, ymax=mean.4+sd.4), width=.2, position=position_dodge(.9),size = 1.2, color = Dark_Blue)+ 
  labs( x="", y = "Average rating")+
  ylim(0, 5)+
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

### 7) Var & Mean Tests -------------------------------------------------------------------------------------------------------------------------


### Kruskal-Wallis-Test
KruskalWallis = data.frame(var = c("novelty","intensity","frequency","cust part","avg rating")
                           ,chi_sq = 0, df = 0, p_value = 0, sig =0)
options(scipen=999)

#novelty
xValue= c(Trans$novelty,Social$novelty,Games$novelty,Photo_Video$novelty,Lifestyle$novelty)
xGroup = factor(rep(1:5 ,c(nrow(Trans),nrow(Social),nrow(Games),nrow(Photo_Video),nrow(Lifestyle))))
KruskalWallis[1,2] = kruskal.test(xValue,xGroup)[1]
KruskalWallis[1,3] = kruskal.test(xValue,xGroup)[2]
KruskalWallis[1,4] = kruskal.test(xValue,xGroup)[3]

#intensity
xValue= c(Trans$Ye_intensity,Social$Ye_intensity,Games$Ye_intensity,Photo_Video$Ye_intensity,Lifestyle$Ye_intensity)
xGroup = factor(rep(1:5 ,c(nrow(Trans),nrow(Social),nrow(Games),nrow(Photo_Video),nrow(Lifestyle))))
KruskalWallis[2,2] = kruskal.test(xValue,xGroup)[1]
KruskalWallis[2,3] = kruskal.test(xValue,xGroup)[2]
KruskalWallis[2,4] = kruskal.test(xValue,xGroup)[3]

#frequency
xValue= c(Trans$intensity,Social$intensity,Games$intensity,Photo_Video$intensity,Lifestyle$intensity)
xGroup = factor(rep(1:5 ,c(nrow(Trans),nrow(Social),nrow(Games),nrow(Photo_Video),nrow(Lifestyle))))
KruskalWallis[3,2] = kruskal.test(xValue,xGroup)[1]
KruskalWallis[3,3] = kruskal.test(xValue,xGroup)[2]
KruskalWallis[3,4] = kruskal.test(xValue,xGroup)[3]

#customer participation
xValue= c(Trans$count_rating,Social$count_rating,Games$count_rating,Photo_Video$count_rating,Lifestyle$count_rating)
xGroup = factor(rep(1:5 ,c(nrow(Trans),nrow(Social),nrow(Games),nrow(Photo_Video),nrow(Lifestyle))))
KruskalWallis[4,2] = kruskal.test(xValue,xGroup)[1]
KruskalWallis[4,3] = kruskal.test(xValue,xGroup)[2]
KruskalWallis[4,4] = kruskal.test(xValue,xGroup)[3]

#average rating
xValue= c(Trans$avg_rating,Social$avg_rating,Games$avg_rating,Photo_Video$avg_rating,Lifestyle$avg_rating)
xGroup = factor(rep(1:5 ,c(nrow(Trans),nrow(Social),nrow(Games),nrow(Photo_Video),nrow(Lifestyle))))
KruskalWallis[5,2] = kruskal.test(xValue,xGroup)[1]
KruskalWallis[5,3] = kruskal.test(xValue,xGroup)[2]
KruskalWallis[5,4] = kruskal.test(xValue,xGroup)[3]


for(i in 1:nrow(KruskalWallis)){
  if(as.numeric(KruskalWallis[i,4]) > 0.04){
    KruskalWallis[i,5]= ""
  }
  if(as.numeric(KruskalWallis[i,4]) < 0.04 && as.numeric(KruskalWallis[i,3])>0.01){
    KruskalWallis[i,5]= "*"
  }
  if(as.numeric(KruskalWallis[i,4]) < 0.01 && as.numeric(KruskalWallis[i,3])>0.001){
    KruskalWallis[i,5]= "**"
  }
  if(as.numeric(KruskalWallis[i,4]) < 0.001){
    KruskalWallis[i,5]= "***"
  }
}

options(NULL)


### Siegel-Tukey-Test

#creating subset
selected=c("Photo & Video","Social Networking","Games","Lifestyle")
STT = Trans[Trans$mainGenre %in% selected,]

SiegelTukey = data.frame(var = c("novelty","intensity","frequency","cust part","avg rating")
                         ,chi_sq = 0, df = 0, p_value = 0, sig =0)
options(scipen=999)

#novelty
SiegelTukey[1,2] = GSTTest(STT$novelty, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[4]
SiegelTukey[1,3] = GSTTest(STT$novelty, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[5]+1
SiegelTukey[1,4] = GSTTest(STT$novelty, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[3]

#intensity
SiegelTukey[2,2] = GSTTest(STT$Ye_intensity, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[4]
SiegelTukey[2,3] = GSTTest(STT$Ye_intensity, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[5]+1
SiegelTukey[2,4] = GSTTest(STT$Ye_intensity, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[3]

#frequency
SiegelTukey[3,2] = GSTTest(STT$intensity, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[4]
SiegelTukey[3,3] = GSTTest(STT$intensity, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[5]+1
SiegelTukey[3,4] = GSTTest(STT$intensity, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[3]

#customer participation
SiegelTukey[4,2] = GSTTest(STT$count_rating, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[4]
SiegelTukey[4,3] = GSTTest(STT$count_rating, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[5]+1
SiegelTukey[4,4] = GSTTest(STT$count_rating, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[3]

#avgerage rating
SiegelTukey[5,2] = GSTTest(STT$avg_rating, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[4]
SiegelTukey[5,3] = GSTTest(STT$avg_rating, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[5]+1
SiegelTukey[5,4] = GSTTest(STT$avg_rating, STT$mainGenre, dist = c("Chisquare", "KruskalWallis"))[3]

for(i in 1:nrow(SiegelTukey)){
  if(as.numeric(SiegelTukey[i,4]) > 0.04){
    SiegelTukey[i,5]= ""
  }
  if(as.numeric(SiegelTukey[i,4]) < 0.04 && as.numeric(SiegelTukey[i,3])>0.01){
    SiegelTukey[i,5]= "*"
  }
  if(as.numeric(SiegelTukey[i,4]) < 0.01 && as.numeric(SiegelTukey[i,3])>0.001){
    SiegelTukey[i,5]= "**"
  }
  if(as.numeric(SiegelTukey[i,4]) < 0.001){
    SiegelTukey[i,5]= "***"
  }
}

### 8) Multiple linear regression 1 -------------------------------------------------------------------------------------------------------------------------

#renaming variables
Trans$frequency = Trans$intensity
Trans$intensity = Trans$Ye_intensity
Trans$cust_part = Trans$count_rating
Trans$filesize = Trans$log_size

Lifestyle$frequency = Lifestyle$intensity
Lifestyle$intensity = Lifestyle$Ye_intensity
Lifestyle$cust_part = Lifestyle$count_rating
Lifestyle$filesize = Lifestyle$log_size

Social$frequency = Social$intensity
Social$intensity = Social$Ye_intensity
Social$cust_part = Social$count_rating
Social$filesize = Social$log_size

Photo_Video$frequency = Photo_Video$intensity
Photo_Video$intensity = Photo_Video$Ye_intensity
Photo_Video$cust_part = Photo_Video$count_rating
Photo_Video$filesize = Photo_Video$log_size

Games$frequency = Games$intensity
Games$intensity = Games$Ye_intensity
Games$cust_part = Games$count_rating
Games$filesize = Games$log_size

## Correlation Overview

# complete sample
pairs.panels(Trans[, c("novelty","Ye_intensity","frequency","cust_part","avg_rating","filesize")],
             smooth = TRUE, scale = FALSE, density=TRUE,ellipses=FALSE,
             digits = 2,method="pearson", pch = 20, lm=FALSE,cor=TRUE,jiggle=TRUE,factor=2, 
             hist.col= Dark_Blue,stars=TRUE,ci=TRUE,alpha=.05,
             cex.axis=2)

# Lifestyle
pairs.panels(Lifestyle[, c("novelty","Ye_intensity","frequency","cust_part","avg_rating","filesize")],
             smooth = TRUE, scale = FALSE, density=TRUE,ellipses=FALSE,
             digits = 2,method="pearson", pch = 20, lm=FALSE,cor=TRUE,jiggle=TRUE,factor=2, 
             hist.col= Dark_Blue,stars=TRUE,ci=TRUE,alpha=.05,
             cex.axis=2)

# Games
pairs.panels(Games[, c("novelty","Ye_intensity","frequency","cust_part","avg_rating","filesize")],
             smooth = TRUE, scale = FALSE, density=TRUE,ellipses=FALSE,
             digits = 2,method="pearson", pch = 20, lm=FALSE,cor=TRUE,jiggle=TRUE,factor=2, 
             hist.col= Dark_Blue,stars=TRUE,ci=TRUE,alpha=.05,
             cex.axis=2)

# Photo & Video
pairs.panels(Photo_Video[, c("novelty","Ye_intensity","frequency","cust_part","avg_rating","filesize")],
             smooth = TRUE, scale = FALSE, density=TRUE,ellipses=FALSE,
             digits = 2,method="pearson", pch = 20, lm=FALSE,cor=TRUE,jiggle=TRUE,factor=2, 
             hist.col= Dark_Blue,stars=TRUE,ci=TRUE,alpha=.05,
             cex.axis=2)

# Social Networking
pairs.panels(Social[, c("novelty","Ye_intensity","frequency","cust_part","avg_rating","filesize")],
             smooth = TRUE, scale = FALSE, density=TRUE,ellipses=FALSE,
             digits = 2,method="pearson", pch = 20, lm=FALSE,cor=TRUE,jiggle=TRUE,factor=2, 
             hist.col= Dark_Blue,stars=TRUE,ci=TRUE,alpha=.05,
             cex.axis=2)

## Different Models for all Obeservations

#Ye & Kankanhalli
model1 = lm(avg_rating ~ novelty + I(novelty^2) + intensity + cust_part + filesize + Lifestyle + Games + Social + Photo_Video,
            data = sample(Trans))
summary(model1)

model2 = lm(avg_rating ~ novelty  * cust_part + I(novelty^2) * cust_part + intensity * cust_part + filesize + Lifestyle + Games + Social + Photo_Video,
            data = sample(Trans))
summary(model2)

model3 = lm(avg_rating ~ novelty + I(novelty^2) + frequency + cust_part + filesize + Lifestyle + Games + Social + Photo_Video + version_major + version_minor + version_patch,
            data = sample(Trans))
summary(model3)

model4 = lm(avg_rating ~ novelty  * cust_part + I(novelty^2) * cust_part + frequency * cust_part + filesize + Lifestyle + Games + Social + Photo_Video + version_major + version_minor + version_patch,
            data = sample(Trans))
summary(model4)


model_labels = c("Model 1","Model 2","Model 3","Model 4")
stargazer(model1, model2, model3, model4, type = "text", column.labels = model_labels, model.numbers =  FALSE)
## Model4 is the best

### 9) Out-of-Sample Model fit -------------------------------------------------------------------------------------------------------------------------

#import
Test_sample = read.csv("Test_sample.csv", sep = ",")

#run regression
Preds1 = predict(model1,newdata=Test_sample)
Preds2 = predict(model2,newdata=Test_sample)
Preds3 = predict(model3,newdata=Test_sample)
Preds4 = predict(model4,newdata=Test_sample)

#Calculating MAE
MAE1 = sqrt(mean(abs(Preds1 - Test_sample$avg_rating)))
MAE2 = sqrt(mean(abs(Preds2 - Test_sample$avg_rating)))
MAE3 = sqrt(mean(abs(Preds3 - Test_sample$avg_rating)))
MAE4 = sqrt(mean(abs(Preds4 - Test_sample$avg_rating)))

#Calculating RMSE
RMSE1 = sqrt(mean((Preds1 - Test_sample$avg_rating)^2))
RMSE2 = sqrt(mean((Preds2 - Test_sample$avg_rating)^2))
RMSE3 = sqrt(mean((Preds3 - Test_sample$avg_rating)^2))
RMSE4 = sqrt(mean((Preds4 - Test_sample$avg_rating)^2))

#creating table

Out_of_sample_Model_Table = data.frame(MAE = c(MAE1,MAE2,MAE3,MAE4),
                                       RMSE = c(RMSE1,RMSE2,RMSE3,RMSE4))
Out_of_sample_Model_Table = data.frame(t(Out_of_sample_Model_Table))
colnames(Out_of_sample_Model_Table) = c("Model1","Model2", "Model3", "Model4")

## Vizusalizaiton margrins

# Threshold
low = 0.25
moderate = 0.75

low_threshold = as.numeric(quantile(Trans$avg_rating, c(0.5+low/2))) - as.numeric(quantile(Trans$avg_rating, c(0.5-low/2)))
moderate_threshold = as.numeric(quantile(Trans$avg_rating, c(0.5+moderate/2))) - as.numeric(quantile(Trans$avg_rating, c(0.5-moderate/2)))

low_threshold
moderate_threshold

# Margins
visualization_Trans = data.frame(novelty = as.numeric(summary(Trans$novelty)),
                                 intensity = as.numeric(summary(Trans$intensity)),
                                 frequency = as.numeric(summary(Trans$frequency)),
                                 cust_part = as.numeric(summary(Trans$cust_part))
)
rownames(visualization_Trans) = c("Min","1. Quantile","Median","Mean","3. Quantile", "Max")
visualization_Trans = data.frame(t(visualization_Trans))

visualization_Trans$X3..Quantile


## 

## Regression Plots

low_cust_part = list(cust_part = rep(as.numeric(quantile(Trans$cust_part, probs = 0.1)), nrow(Trans)))
high_cust_part = list(cust_part = rep(as.numeric(quantile(Trans$cust_part, probs = 0.9)), nrow(Trans)))

novelty_x_axis = exp(seq(0,3,3/(nrow(Trans)-1)))

loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))

plotgrid_model1 = plot_grid(ncol = 2,
                            effect_plot(model1, pred = novelty,centered = FALSE, interval = TRUE, at = low_cust_part, colors = Dark_Blue)+
                              ggtitle("Low customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              ),
                            effect_plot(model1, pred = novelty,centered = FALSE, interval = TRUE, at = high_cust_part, colors = Dark_Blue)+
                              ggtitle("High customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              )
)
plotgrid_model1


plotgrid_model2 = plot_grid(ncol = 2,
                            effect_plot(model2, pred = novelty,centered = FALSE, interval = TRUE, at = low_cust_part, colors = Dark_Blue)+
                              ggtitle("Low customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              ),
                            effect_plot(model2, pred = novelty,centered = FALSE, interval = TRUE, at = high_cust_part, colors = Dark_Blue)+
                              ggtitle("High customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              )
)
plotgrid_model2


plotgrid_model3 = plot_grid(ncol = 2,
                            effect_plot(model3, pred = novelty,centered = FALSE, interval = TRUE, at = low_cust_part, colors = Dark_Blue)+
                              ggtitle("Low customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              ),
                            effect_plot(model3, pred = novelty,centered = FALSE, interval = TRUE, at = high_cust_part, colors = Dark_Blue)+
                              ggtitle("High customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              )
)
plotgrid_model3


plotgrid_model4 = plot_grid(ncol = 2,
                            effect_plot(model4, pred = novelty,centered = FALSE, interval = TRUE, at = low_cust_part, colors = Dark_Blue)+
                              ggtitle("Low customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              ),
                            effect_plot(model4, pred = novelty,centered = FALSE, interval = TRUE, at = high_cust_part, colors = Dark_Blue)+
                              ggtitle("High customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              )
)
plotgrid_model4

### 10) Multiple linear regression 2 -------------------------------------------------------------------------------------------------------------------------

# Different Models for Panel Data

model5 = lm(avg_rating ~ novelty  * cust_part + I(novelty^2) * cust_part + frequency * cust_part + filesize  + version_major + version_minor, 
            data = sample(Lifestyle))
summary(model5)

model6 = lm(avg_rating ~ novelty  * cust_part + I(novelty^2) * cust_part + frequency * cust_part + filesize + version_major + version_minor + version_patch, 
            data = sample(Games))
summary(model6)

model7 = lm(avg_rating ~ novelty  * cust_part + I(novelty^2) * cust_part + frequency * cust_part + filesize + version_major + version_minor + version_patch, 
            data = sample(Photo_Video))
summary(model7)

model8 = lm(avg_rating ~ novelty  * cust_part + I(novelty^2) * cust_part + frequency * cust_part + filesize + version_major + version_minor, 
            data = sample(Social))
summary(model8)

model_labels2 = c("Lifestyle","Games","Photo&Video","Social Networking")
stargazer(model5, model6, model7, model8, type = "text",column.labels = model_labels2, model.numbers =  FALSE)

## Vizusalizaiton margrins

# Threshold
low = 0.25
moderate = 0.75

#Lifestyle
low_threshold_Lifestyle = as.numeric(quantile(Lifestyle$avg_rating, c(0.5+low/2))) - as.numeric(quantile(Lifestyle$avg_rating, c(0.5-low/2)))
moderate_threshold_Lifestyle = as.numeric(quantile(Lifestyle$avg_rating, c(0.5+moderate/2))) - as.numeric(quantile(Lifestyle$avg_rating, c(0.5-moderate/2)))
low_threshold_Lifestyle
moderate_threshold_Lifestyle

#Games
low_threshold_Games = as.numeric(quantile(Games$avg_rating, c(0.5+low/2))) - as.numeric(quantile(Games$avg_rating, c(0.5-low/2)))
moderate_threshold_Games = as.numeric(quantile(Games$avg_rating, c(0.5+moderate/2))) - as.numeric(quantile(Games$avg_rating, c(0.5-moderate/2)))
low_threshold_Games
moderate_threshold_Games

#Photo & Video
low_threshold_Photo_Video = as.numeric(quantile(Photo_Video$avg_rating, c(0.5+low/2))) - as.numeric(quantile(Photo_Video$avg_rating, c(0.5-low/2)))
moderate_threshold_Photo_Video = as.numeric(quantile(Photo_Video$avg_rating, c(0.5+moderate/2))) - as.numeric(quantile(Photo_Video$avg_rating, c(0.5-moderate/2)))
low_threshold_Photo_Video
moderate_threshold_Photo_Video

#Social Networking
low_threshold_Social = as.numeric(quantile(Social$avg_rating, c(0.5+low/2))) - as.numeric(quantile(Social$avg_rating, c(0.5-low/2)))
moderate_threshold_Social = as.numeric(quantile(Social$avg_rating, c(0.5+moderate/2))) - as.numeric(quantile(Social$avg_rating, c(0.5-moderate/2)))
low_threshold_Social
moderate_threshold_Social

# Margins
visualization_Lifestyle = data.frame(novelty = as.numeric(summary(Lifestyle$novelty)),
                                     intensity = as.numeric(summary(Lifestyle$intensity)),
                                     frequency = as.numeric(summary(Lifestyle$frequency)),
                                     cust_part = as.numeric(summary(Lifestyle$count_rating))
)
rownames(visualization_Lifestyle) = c("Min","1. Quantile","Median","Mean","3. Quantile", "Max")
visualization_Lifestyle = data.frame(t(visualization_Lifestyle))

visualization_Games = data.frame(novelty = as.numeric(summary(Games$novelty)),
                                 intensity = as.numeric(summary(Games$intensity)),
                                 frequency = as.numeric(summary(Games$frequency)),
                                 cust_part = as.numeric(summary(Games$count_rating))
)
rownames(visualization_Games) = c("Min","1. Quantile","Median","Mean","3. Quantile", "Max")
visualization_Games = data.frame(t(visualization_Games))

visualization_Photo_Video = data.frame(novelty = as.numeric(summary(Photo_Video$novelty)),
                                       intensity = as.numeric(summary(Photo_Video$intensity)),
                                       frequency = as.numeric(summary(Photo_Video$frequency)),
                                       cust_part = as.numeric(summary(Photo_Video$count_rating))
)
rownames(visualization_Photo_Video) = c("Min","1. Quantile","Median","Mean","3. Quantile", "Max")
visualization_Photo_Video = data.frame(t(visualization_Photo_Video))

visualization_Social = data.frame(novelty = as.numeric(summary(Social$novelty)),
                                  intensity = as.numeric(summary(Social$intensity)),
                                  frequency = as.numeric(summary(Social$frequency)),
                                  cust_part = as.numeric(summary(Social$count_rating))
)
rownames(visualization_Social) = c("Min","1. Quantile","Median","Mean","3. Quantile", "Max")
visualization_Social = data.frame(t(visualization_Social))

## Regression Plots

low_cust_part = list(cust_part = rep(as.numeric(quantile(Lifestyle$cust_part, probs = 0.1)), nrow(Lifestyle)))
high_cust_part = list(cust_part = rep(as.numeric(quantile(Lifestyle$cust_part, probs = 0.9)), nrow(Lifestyle)))

plotgrid_model5 = plot_grid(ncol = 2,
                            effect_plot(model5, pred = novelty,centered = FALSE, interval = TRUE, at = low_cust_part, colors = Dark_Blue, int.width = 0.4)+
                              ggtitle("Low customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(xlim = c(0,3), ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              ),
                            effect_plot(model5, pred = novelty,centered = FALSE, interval = TRUE, at = high_cust_part, colors = Dark_Blue, int.width = 0.4)+
                              ggtitle("High customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(xlim = c(0,3), ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              )
)
plotgrid_model5

plotgrid_model6 = plot_grid(ncol = 2,
                            effect_plot(model6, pred = novelty,centered = FALSE, interval = TRUE, at = low_cust_part, colors = Dark_Blue, int.width = 0.4)+
                              ggtitle("Low customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(xlim = c(0,3), ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              ),
                            effect_plot(model6, pred = novelty,centered = FALSE, interval = TRUE, at = high_cust_part, colors = Dark_Blue, int.width = 0.4)+
                              ggtitle("High customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(xlim = c(0,3), ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              )
)
plotgrid_model6

plotgrid_model7 = plot_grid(ncol = 2,
                            effect_plot(model7, pred = novelty,centered = FALSE, interval = TRUE, at = low_cust_part, colors = Dark_Blue, int.width = 0.4)+
                              ggtitle("Low customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(xlim = c(0,3), ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              ),
                            effect_plot(model7, pred = novelty,centered = FALSE, interval = TRUE, at = high_cust_part, colors = Dark_Blue, int.width = 0.4)+
                              ggtitle("High customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(xlim = c(0,3), ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              )
)
plotgrid_model7

plotgrid_model8 = plot_grid(ncol = 2,
                            effect_plot(model8, pred = novelty,centered = FALSE, interval = TRUE, at = low_cust_part, colors = Dark_Blue, int.width = 0.4)+
                              ggtitle("Low customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(xlim = c(0,3), ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              ),
                            effect_plot(model8, pred = novelty,centered = FALSE, interval = TRUE, at = high_cust_part, colors = Dark_Blue, int.width = 0.4)+
                              ggtitle("High customer participation") +
                              geom_line(size = 1, color = Dark_Blue) +
                              coord_cartesian(xlim = c(0,3), ylim = c(1, 5))+
                              labs(x = "Log(Novelty)", y= "Average rating") +
                              theme_minimal() +
                              theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
                                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                              )
)
plotgrid_model8

### 11) Diagnostics, Statistical Power -------------------------------------------------------------------------------------------------------------------------

# Creating Table for power analysis

Stat_Power_pre = data.frame(model = c("new model2","new model4","model2"),
                            dataset = c("Lee & Son 2017","Ye & Kankanhalli 2018","Ye & Kankanhalli 2020"),
                            IVs = c(11,23,11),
                            DFs = c(2483,123,3265),
                            f2 = c((0.214 / (1-0.214)), (0.14 / (1-0.14)), (0.182 / (1-0.182))),
                            sig = 0.05,
                            power = 0.8,
                            req_sample = c(73, 155, 86) # calculated with G-power
)

# Pre examination of Sample size

Stat_Power_post = data.frame(model = c("model1","model2","model3","model4","model5","model6","model7","model8"),
                             dataset = c("Trans","Trans","Trans","Trans","Lifestyle","Games","Photo & Video","Social Networking"),
                             IVs = c(model1$rank,model2$rank,model3$rank,model4$rank,model5$rank,model6$rank+1,model7$rank,model8$rank),
                             DFs = c(model1$df.residual,model2$df.residual,model3$df.residual,model4$df.residual,model5$df.residual,model6$df.residual,model7$df.residual,model8$df.residual),
                             f2 = c(as.numeric(summary(model1)[8]) / (1-as.numeric(summary(model1)[8])),as.numeric(summary(model2)[8]) / (1-as.numeric(summary(model2)[8])),as.numeric(summary(model3)[8]) / (1-as.numeric(summary(model3)[8])),as.numeric(summary(model4)[8]) / (1-as.numeric(summary(model4)[8])),as.numeric(summary(model5)[8]) / (1-as.numeric(summary(model5)[8])),as.numeric(summary(model6)[8]) / (1-as.numeric(summary(model6)[8])),as.numeric(summary(model7)[8]) / (1-as.numeric(summary(model7)[8])),as.numeric(summary(model8)[8]) / (1-as.numeric(summary(model8)[8]))),
                             sig = 0.05,
                             power = 0
)

# Calculating Power
library(pwr)

for(i in 1:nrow(Stat_Power_post)){
  Stat_Power_post$power[i] = pwr.f2.test( u = Stat_Power_post$IVs[i], #number of IVS
                                          v = Stat_Power_post$DFs[i], #degress of freedom (n-1 number of IVs)
                                          f2 = Stat_Power_post$f2[i], #effect size
                                          sig.level = Stat_Power_post$sig[i], #alpha
                                          power = NULL# power level, set to NULL to get it as an outcome
  )[5]
}

Stat_Power_post$model = as.character(Stat_Power_post$model)
Stat_Power_post$dataset = as.character(Stat_Power_post$dataset)
Stat_Power_post$power = as.numeric(Stat_Power_post$power)


## Adding prior research

Lee_Son_2017_game = c("-", "Lee & Son, 2020 - Game", 13 , 2483, (0.032 / (1-0.032)), 0.05
                      , as.numeric(pwr.f2.test( u = 13, #number of IVS
                                                v = 2483, #degress of freedom (n-1 number of IVs)
                                                f2 = (0.032 / (1-0.032)), #effect size
                                                sig.level = 0.05, #alpha
                                                power = NULL # power level, set to NULL to get it as an outcome
                      )[5])
)

Lee_Son_2017_hobby = c("-", "Lee & Son, 2020 - Hobby", 13 , 2483, (0.094 / (1-0.094)), 0.05
                       , as.numeric(pwr.f2.test( u = 13, #number of IVS
                                                 v = 2483, #degress of freedom (n-1 number of IVs)
                                                 f2 = (0.094 / (1-0.094)), #effect size
                                                 sig.level = 0.05, #alpha
                                                 power = NULL # power level, set to NULL to get it as an outcome
                       )[5])
)

Lee_Son_2017_information       = c("-", "Lee & Son, 2017 - Information", 13 , 2483, (0.082 / (1-0.082)), 0.05
                                   , as.numeric(pwr.f2.test( u = 13, #number of IVS
                                                             v = 2483, #degress of freedom (n-1 number of IVs)
                                                             f2 = (0.082 / (1-0.082)), #effect size
                                                             sig.level = 0.05, #alpha
                                                             power = NULL # power level, set to NULL to get it as an outcome
                                   )[5])
)


Stat_Power_post = rbind(Stat_Power_post, Lee_Son_2017_game)
Stat_Power_post = rbind(Stat_Power_post, Lee_Son_2017_hobby)
Stat_Power_post = rbind(Stat_Power_post, Lee_Son_2017_information)

Stat_Power_post$model = as.character(Stat_Power_post$model)
Stat_Power_post$dataset = as.character(Stat_Power_post$dataset)
Stat_Power_post$power = as.numeric(Stat_Power_post$power)
Stat_Power_post$f2 = as.numeric(Stat_Power_post$f2)

Stat_Power_post$power = round(Stat_Power_post$power, digits = 4)
Stat_Power_post$f2 = round(Stat_Power_post$f2, digits = 4)

## Effect Size & Statistical Power Graph

Stat_Power = data.frame(IVs = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                        sample = c(652,776,863,934,995,1050,1099,1145,1188,1229,1267,1304,1339,1373,1405,89,107,119,129,138,146,153,160,166,172,178,184,189,194,199,40,48,54,59,63,67,70,74,77,80,83,86,88,91,93),
                        effect = c("small","small","small","small","small","small","small","small","small","small","small","small","small","small","small","medium","medium","medium","medium","medium","medium","medium","medium","medium","medium","medium","medium","medium","medium","medium","large","large","large","large","large","large","large","large","large","large","large","large","large","large","large")
)

loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
Power_plot =ggplot(data = Stat_Power, aes(x = IVs, y= sample, color = effect, linetype = effect, shape = effect)) + 
  geom_line(size = 1) +
  scale_y_break(c(200,600), scale=1, ticklabels = NULL) + 
  geom_point(aes(x=13, y=150), size = 3, shape = 23, fill ="black", color = "black")+ # Model1
  annotate("text", x = 13 , y = 170, label = "Model 1") +
  geom_point(aes(x=11, y=120), size = 3, shape = 23, fill ="black", color = "black")+ # Model2
  annotate("text", x = 11 , y = 140, label = "Model 2") +
  geom_point(aes(x=6, y=820), size = 3, shape = 23, fill ="black", color = "black")+ # Model3
  annotate("text", x = 6 , y = 920, label = "Model 3") +
  labs(x = "Number of predictors", y= "Required sample size") +
  theme_minimal()+
  scale_x_continuous(limits=c(0, 15))+
  theme(text=element_text(family="Times", face="bold", size=14), plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
  )
Power_plot

## Hypothesis Plot

Lifestyle$rating = numeric(nrow(Lifestyle))
for(i in 1:nrow(Lifestyle)){
  if(Lifestyle$avg_rating[i] <= 2){
    Lifestyle$rating[i] = 1
  }
  if(Lifestyle$avg_rating[i] > 2 && Lifestyle$avg_rating[i] <= 3){
    Lifestyle$rating[i] = 2
  }
  if(Lifestyle$avg_rating[i] > 3 && Lifestyle$avg_rating[i] <= 4){
    Lifestyle$rating[i] = 3
  }
  if(Lifestyle$avg_rating[i] > 4 && Lifestyle$avg_rating[i] <= 5){
    Lifestyle$rating[i] = 4
  }
  if(Lifestyle$avg_rating[i] > 4){
    Lifestyle$rating[i] = 5
  }
}

Games$rating = numeric(nrow(Games))
for(i in 1:nrow(Games)){
  if(Games$avg_rating[i] <= 2){
    Games$rating[i] = 1
  }
  if(Games$avg_rating[i] > 2 && Games$avg_rating[i] <= 3){
    Games$rating[i] = 2
  }
  if(Games$avg_rating[i] > 3 && Games$avg_rating[i] <= 4){
    Games$rating[i] = 3
  }
  if(Games$avg_rating[i] > 4 && Games$avg_rating[i] <= 5){
    Games$rating[i] = 4
  }
  if(Games$avg_rating[i] > 4){
    Games$rating[i] = 5
  }
}

Photo_Video$rating = numeric(nrow(Photo_Video))
for(i in 1:nrow(Photo_Video)){
  if(Photo_Video$avg_rating[i] <= 2){
    Photo_Video$rating[i] = 1
  }
  if(Photo_Video$avg_rating[i] > 2 && Photo_Video$avg_rating[i] <= 3){
    Photo_Video$rating[i] = 2
  }
  if(Photo_Video$avg_rating[i] > 3 && Photo_Video$avg_rating[i] <= 4){
    Photo_Video$rating[i] = 3
  }
  if(Photo_Video$avg_rating[i] > 4 && Photo_Video$avg_rating[i] <= 5){
    Photo_Video$rating[i] = 4
  }
  if(Photo_Video$avg_rating[i] > 4){
    Photo_Video$rating[i] = 5
  }
}

Social$rating = numeric(nrow(Social))
for(i in 1:nrow(Social)){
  if(Social$avg_rating[i] <= 2){
    Social$rating[i] = 1
  }
  if(Social$avg_rating[i] > 2 && Social$avg_rating[i] <= 3){
    Social$rating[i] = 2
  }
  if(Social$avg_rating[i] > 3 && Social$avg_rating[i] <= 4){
    Social$rating[i] = 3
  }
  if(Social$avg_rating[i] > 4 && Social$avg_rating[i] <= 5){
    Social$rating[i] = 4
  }
  if(Social$avg_rating[i] > 4){
    Social$rating[i] = 5
  }
}

loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
hyp_Lifestyle = ggplot(data = Lifestyle, aes(x = novelty, y= frequency, color = rating)) + 
  geom_point(size = 2)+
  xlim(0,3)+
  ylim(3,8)+
  theme_minimal()+
  labs(x = "Novelty", y= "Frequency", title = "Lifestyle") +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )

loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
hyp_Games = ggplot(data = Games, aes(x = novelty, y= frequency, color = rating)) + 
  geom_point(size = 2)+
  xlim(0,3)+
  ylim(3,8)+
  theme_minimal()+
  labs(x = "Novelty", y= "Frequency", title = "Games") +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
  )

loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
hyp_Photo_Video = ggplot(data = Photo_Video, aes(x = novelty, y= frequency, color = rating)) + 
  geom_point(size = 2)+
  xlim(0,3)+
  ylim(3,8)+
  theme_minimal()+
  labs(x = "Novelty", y= "Frequency", title = "Photo_Video") +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )

loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
hyp_Social = ggplot(data = Social, aes(x = novelty, y= frequency, color = rating)) + 
  geom_point(size = 2)+
  xlim(0,3)+
  ylim(3,8)+
  theme_minimal()+
  labs(x = "Novelty", y= "Frequency", title = "Social") +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )

plot_grid(ncol = 2,
          hyp_Lifestyle,
          hyp_Games,
          hyp_Photo_Video,
          hyp_Social
)


### 12) Diagnostics, Analyzing Residuals -------------------------------------------------------------------------------------------------------------------------


## Only model none significant models fullfil the requirement of Homoscadacity

### Determining Outliers 

# Only Run 1 of the Trans Models
Trans = cbind(Trans,  data.frame(round(cbind(resid(model4),rstandard(model4),rstudent(model4),cooks.distance(model4),dffits(model4),hatvalues(model4),covratio(model4),dfbeta(model4)),5)))
colnames(Trans)[42:48] = c("resid","std_resid","stud_resid","cook_dist","dffit","leverage","cov_ratio" )

# Panel Data Models
Lifestyle = cbind(Lifestyle, data.frame(round(cbind(resid(model5),rstandard(model5),rstudent(model5),cooks.distance(model5),dffits(model5),hatvalues(model5),covratio(model5),dfbeta(model5)),5)))
colnames(Lifestyle)[42:48] = c("resid","std_resid","stud_resid","cook_dist","dffit","leverage","cov_ratio")

Games = cbind(Games, data.frame(round(cbind(resid(model6),rstandard(model6),rstudent(model6),cooks.distance(model6),dffits(model6),hatvalues(model6),covratio(model6),dfbeta(model6)),5)))
colnames(Games)[42:48] = c("resid","std_resid","stud_resid","cook_dist","dffit","leverage","cov_ratio")

Photo_Video = cbind(Photo_Video, data.frame(round(cbind(resid(model7),rstandard(model7),rstudent(model7),cooks.distance(model7),dffits(model7),hatvalues(model7),covratio(model7),dfbeta(model7)),5)))
colnames(Photo_Video)[42:48] = c("resid","std_resid","stud_resid","cook_dist","dffit","leverage","cov_ratio")

Social = cbind(Social, data.frame(round(cbind(resid(model8),rstandard(model8),rstudent(model8),cooks.distance(model8),dffits(model8),hatvalues(model8),covratio(model8),dfbeta(model8)),5)))
colnames(Social)[42:48] = c("resid","std_resid","stud_resid","cook_dist","dffit","leverage","cov_ratio")


### Outliers

## Std. Residuals

Trans$large_std_resid_20 = Trans$std_resid > 2 | Trans$std_resid < -2
Trans$large_std_resid_25 = Trans$std_resid > 2.5 | Trans$std_resid < -2.5
Trans$large_std_resid_30 = Trans$std_resid > 3 | Trans$std_resid < -3

Lifestyle$large_std_resid_20 = Lifestyle$std_resid > 2 | Lifestyle$std_resid < -2
Lifestyle$large_std_resid_25 = Lifestyle$std_resid > 2.5 | Lifestyle$std_resid < -2.5
Lifestyle$large_std_resid_30 = Lifestyle$std_resid > 3 | Lifestyle$std_resid < -3

Games$large_std_resid_20 = Games$std_resid > 2 | Games$std_resid < -2
Games$large_std_resid_25 = Games$std_resid > 2.5 | Games$std_resid < -2.5
Games$large_std_resid_30 = Games$std_resid > 3 | Games$std_resid < -3

Photo_Video$large_std_resid_20 = Photo_Video$std_resid > 2 | Photo_Video$std_resid < -2
Photo_Video$large_std_resid_25 = Photo_Video$std_resid > 2.5 | Photo_Video$std_resid < -2.5
Photo_Video$large_std_resid_30 = Photo_Video$std_resid > 3 | Photo_Video$std_resid < -3

Social$large_std_resid_20 = Social$std_resid > 2 | Social$std_resid < -2
Social$large_std_resid_25 = Social$std_resid > 2.5 | Social$std_resid < -2.5
Social$large_std_resid_30 = Social$std_resid > 3 | Social$std_resid < -3

## Stud. Residuals

Trans$large_stud_resid_20 = Trans$stud_resid > 2 | Trans$stud_resid < -2
Trans$large_stud_resid_25 = Trans$stud_resid > 2.5 | Trans$stud_resid < -2.5
Trans$large_stud_resid_30 = Trans$stud_resid > 3 | Trans$stud_resid < -3

Lifestyle$large_stud_resid_20 = Lifestyle$stud_resid > 2 | Lifestyle$stud_resid < -2
Lifestyle$large_stud_resid_25 = Lifestyle$stud_resid > 2.5 | Lifestyle$stud_resid < -2.5
Lifestyle$large_stud_resid_30 = Lifestyle$stud_resid > 3 | Lifestyle$stud_resid < -3

Games$large_stud_resid_20 = Games$stud_resid > 2 | Games$stud_resid < -2
Games$large_stud_resid_25 = Games$stud_resid > 2.5 | Games$stud_resid < -2.5
Games$large_stud_resid_30 = Games$stud_resid > 3 | Games$stud_resid < -3

Photo_Video$large_stud_resid_20 = Photo_Video$stud_resid > 2 | Photo_Video$stud_resid < -2
Photo_Video$large_stud_resid_25 = Photo_Video$stud_resid > 2.5 | Photo_Video$stud_resid < -2.5
Photo_Video$large_stud_resid_30 = Photo_Video$stud_resid > 3 | Photo_Video$stud_resid < -3

Social$large_stud_resid_20 = Social$stud_resid > 2 | Social$stud_resid < -2
Social$large_stud_resid_25 = Social$stud_resid > 2.5 | Social$stud_resid < -2.5
Social$large_stud_resid_30 = Social$stud_resid > 3 | Social$stud_resid < -3

## Creating Outlier dataframe

Outliers = data.frame(model = c("model4","model5","model6","model7","model8"),
                      dataset = c("Trans","Lifestyle","Games","Photo & Video","Social Networking"),
                      large_std_resid_20 = c(sum(Trans$large_std_resid_20)/nrow(Trans), sum(Lifestyle$large_std_resid_20)/nrow(Lifestyle),sum(Games$large_std_resid_20)/nrow(Games),sum(Photo_Video$large_std_resid_20)/nrow(Photo_Video),sum(Social$large_std_resid_20)/nrow(Social)),
                      large_std_resid_25 = c(sum(Trans$large_std_resid_25)/nrow(Trans), sum(Lifestyle$large_std_resid_25)/nrow(Lifestyle),sum(Games$large_std_resid_25)/nrow(Games),sum(Photo_Video$large_std_resid_25)/nrow(Photo_Video),sum(Social$large_std_resid_25)/nrow(Social)),
                      large_std_resid_30 = c(sum(Trans$large_std_resid_30)/nrow(Trans), sum(Lifestyle$large_std_resid_30)/nrow(Lifestyle),sum(Games$large_std_resid_30)/nrow(Games),sum(Photo_Video$large_std_resid_30)/nrow(Photo_Video),sum(Social$large_std_resid_30)/nrow(Social)),
                      large_stud_resid_20 = c(sum(Trans$large_stud_resid_20)/nrow(Trans), sum(Lifestyle$large_stud_resid_20)/nrow(Lifestyle),sum(Games$large_stud_resid_20)/nrow(Games),sum(Photo_Video$large_stud_resid_20)/nrow(Photo_Video),sum(Social$large_stud_resid_20)/nrow(Social)),
                      large_stud_resid_25 = c(sum(Trans$large_stud_resid_25)/nrow(Trans), sum(Lifestyle$large_stud_resid_25)/nrow(Lifestyle),sum(Games$large_stud_resid_25)/nrow(Games),sum(Photo_Video$large_stud_resid_25)/nrow(Photo_Video),sum(Social$large_stud_resid_25)/nrow(Social)),
                      large_stud_resid_30 = c(sum(Trans$large_stud_resid_30)/nrow(Trans), sum(Lifestyle$large_stud_resid_30)/nrow(Lifestyle),sum(Games$large_stud_resid_30)/nrow(Games),sum(Photo_Video$large_stud_resid_30)/nrow(Photo_Video),sum(Social$large_stud_resid_30)/nrow(Social))
)


Outliers = cbind(Outliers[,1:2],round(Outliers[,3:8],4))



### Influential Cases

## Std. Residuals

## Leverage

avg_lev = (as.numeric(unlist(summary(model4)[10])[2])+1)/nrow(Trans)
Trans$large_lev = Trans$leverage > (3*avg_lev) | Trans$leverage < -(3*avg_lev)

avg_lev = (as.numeric(unlist(summary(model5)[10])[2])+1)/nrow(Lifestyle)
Lifestyle$large_lev = Lifestyle$leverage > (3*avg_lev) | Lifestyle$leverage < -(3*avg_lev)

avg_lev = (as.numeric(unlist(summary(model6)[10])[2])+1)/nrow(Games)
Games$large_lev = Games$leverage > (3*avg_lev) | Games$leverage < -(3*avg_lev)

avg_lev = (as.numeric(unlist(summary(model7)[10])[2])+1)/nrow(Photo_Video)
Photo_Video$large_lev = Photo_Video$leverage > (3*avg_lev) | Photo_Video$leverage < -(3*avg_lev)

avg_lev = (as.numeric(unlist(summary(model8)[10])[2])+1)/nrow(Social)
Social$large_lev = Social$leverage > (3*avg_lev) | Social$leverage < -(3*avg_lev)


## Covariance Ratio

cov_factor = (as.numeric(unlist(summary(model4)[10])[2])+1)/nrow(Trans)
Trans$large_cov_ratio = Trans$cov_ratio > 1 + 3*cov_factor | Trans$cov_ratio < -1 + 3*cov_factor

cov_factor = (as.numeric(unlist(summary(model5)[10])[2])+1)/nrow(Lifestyle)
Lifestyle$large_cov_ratio = Lifestyle$cov_ratio > 1 + 3*cov_factor | Lifestyle$cov_ratio < -1 + 3*cov_factor

cov_factor = (as.numeric(unlist(summary(model6)[10])[2])+1)/nrow(Games)
Games$large_cov_ratio = Games$cov_ratio > 1 + 3*cov_factor | Games$cov_ratio < -1 + 3*cov_factor

cov_factor = (as.numeric(unlist(summary(model7)[10])[2])+1)/nrow(Photo_Video)
Photo_Video$large_cov_ratio = Photo_Video$cov_ratio > 1 + 3*cov_factor | Photo_Video$cov_ratio < -1 + 3*cov_factor

cov_factor = (as.numeric(unlist(summary(model8)[10])[2])+1)/nrow(Social)
Social$large_cov_ratio = Social$cov_ratio > 1 + 3*cov_factor | Social$cov_ratio < -1 + 3*cov_factor


## Cook Distance

cov_factor = (as.numeric(unlist(summary(model4)[10])[2])+1)/nrow(Trans)
Trans$large_cook_dist = Trans$cook_dist > 1 + 3*cov_factor | Trans$cook_dist < -1 + 3*cov_factor

cov_factor = (as.numeric(unlist(summary(model5)[10])[2])+1)/nrow(Lifestyle)
Lifestyle$large_cook_dist = Lifestyle$cook_dist > 1 + 3*cov_factor | Lifestyle$cook_dist < -1 + 3*cov_factor

cov_factor = (as.numeric(unlist(summary(model6)[10])[2])+1)/nrow(Games)
Games$large_cook_dist = Games$cook_dist > 1 + 3*cov_factor | Games$cook_dist < -1 + 3*cov_factor

cov_factor = (as.numeric(unlist(summary(model7)[10])[2])+1)/nrow(Photo_Video)
Photo_Video$large_cook_dist = Photo_Video$cook_dist > 1 + 3*cov_factor | Photo_Video$cook_dist < -1 + 3*cov_factor

cov_factor = (as.numeric(unlist(summary(model8)[10])[2])+1)/nrow(Social)
Social$large_cook_dist = Social$cook_dist > 1 + 3*cov_factor | Social$cook_dist < -1 + 3*cov_factor


## Creating Influential_Cases dataframe

Influential_Cases = data.frame(model = c("model4","model5","model6","model7","model8"),
                               dataset = c("Trans","Lifestyle","Games","Photo & Video","Social Networking"),
                               leverage = c(sum(Trans$large_lev)/nrow(Trans),sum(Lifestyle$large_lev)/nrow(Lifestyle),sum(Games$large_lev)/nrow(Games),sum(Photo_Video$large_lev)/nrow(Photo_Video),sum(Social$large_lev)/nrow(Social)),
                               cov_ratio = c(sum(Trans$large_cov_ratio)/nrow(Trans),sum(Lifestyle$large_cov_ratio)/nrow(Lifestyle),sum(Games$large_cov_ratio)/nrow(Games),sum(Photo_Video$large_cov_ratio)/nrow(Photo_Video),sum(Social$large_cov_ratio)/nrow(Social)),
                               cook_dist = c(sum(Trans$large_cook_dist)/nrow(Trans),sum(Lifestyle$large_cook_dist)/nrow(Lifestyle),sum(Games$large_cook_dist)/nrow(Games),sum(Photo_Video$large_cook_dist)/nrow(Photo_Video),sum(Social$large_cook_dist)/nrow(Social))
)


Influential_Cases = cbind(Influential_Cases[,1:2],round(Influential_Cases[,3:5],4))


### Residual Plots

path_photos = "C:/Users/.../Picture"

## Plots for Trans Model

# Preparing
plots = data.frame(fitted = model4$fitted.values, stud_resid = Trans$stud_resid)

#Histogram
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
histogram = ggplot(plots, aes(x = stud_resid)) + 
  ggtitle("Histogram") +
  geom_histogram(aes(y =..density..), colour = "black", fill = "white") +
  labs(x = "Studentized residual", y = "Density") +
  xlim(-3,3) +
  ylim(0,0.45) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
histogram + stat_function(fun = dnorm, args = list(mean = mean(plots$stud_resid, na.rm = TRUE), sd = sd(plots$stud_resid, na.rm = TRUE)), size = 1, linetype="dashed")

#Q-Q Plot
qqplot.resid = qplot(sample = plots$stud_resid , stat="qq") + 
  ggtitle("Q-Q Plot") +
  labs(x = "Theoretical values", y = "Observed values") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  xlim(-3,3) +
  ylim(-3,3) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
qqplot.resid

#Residuals
scatter = ggplot(plots, aes(fitted, stud_resid)) + 
  ggtitle("Residuals") +
  geom_point() + 
  geom_hline(yintercept=0, linetype="dashed") +
  xlim(0,5) +
  ylim(-3,3) +
  labs(x = "Fitted values", y= "Studentized residual") +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
scatter 


library(cowplot)
plotgrid <- plot_grid(ncol = 3,
                      histogram + stat_function(fun = dnorm, args = list(mean = mean(plots$stud_resid, na.rm = TRUE), sd = sd(plots$stud_resid, na.rm = TRUE)), size = 1, linetype="dashed")
                      ,qqplot.resid
                      ,scatter
)
plotgrid


## Plots for Lifestyle Model

# Preparing
plots = data.frame(fitted = model5$fitted.values, stud_resid = Lifestyle$stud_resid)

#Histogram
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
histogram = ggplot(plots, aes(x = stud_resid)) + 
  ggtitle("Histogram") +
  geom_histogram(aes(y =..density..), colour = "black", fill = "white") +
  labs(x = "Studentized residual", y = "Density") +
  xlim(-3,3) +
  ylim(0,0.45) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
histogram + stat_function(fun = dnorm, args = list(mean = mean(plots$stud_resid, na.rm = TRUE), sd = sd(plots$stud_resid, na.rm = TRUE)), size = 1, linetype="dashed")

#Q-Q Plot
qqplot.resid = qplot(sample = plots$stud_resid , stat="qq") + 
  ggtitle("Q-Q Plot") +
  labs(x = "Theoretical values", y = "Observed values") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  xlim(-3,3) +
  ylim(-3,3) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
qqplot.resid

#Residuals
scatter = ggplot(plots, aes(fitted, stud_resid)) + 
  ggtitle("Residuals") +
  geom_point() + 
  geom_hline(yintercept=0, linetype="dashed") +
  xlim(0,5) +
  ylim(-3,3) +
  labs(x = "Fitted values", y= "Studentized residual") +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
scatter 


library(cowplot)
plotgrid <- plot_grid(ncol = 3,
                      histogram + stat_function(fun = dnorm, args = list(mean = mean(plots$stud_resid, na.rm = TRUE), sd = sd(plots$stud_resid, na.rm = TRUE)), size = 1, linetype="dashed")
                      ,qqplot.resid
                      ,scatter
)
plotgrid


## Plots for Games Model

# Preparing
plots = data.frame(fitted = model6$fitted.values, stud_resid = Games$stud_resid)

#Histogram
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
histogram = ggplot(plots, aes(x = stud_resid)) + 
  ggtitle("Histogram") +
  geom_histogram(aes(y =..density..), colour = "black", fill = "white") +
  labs(x = "Studentized residual", y = "Density") +
  xlim(-3,3) +
  ylim(0,0.45) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
histogram + stat_function(fun = dnorm, args = list(mean = mean(plots$stud_resid, na.rm = TRUE), sd = sd(plots$stud_resid, na.rm = TRUE)), size = 1, linetype="dashed")


#Q-Q Plot
qqplot.resid = qplot(sample = plots$stud_resid , stat="qq") + 
  ggtitle("Q-Q Plot") +
  labs(x = "Theoretical values", y = "Observed values") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  xlim(-3,3) +
  ylim(-3,3) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
qqplot.resid

#Residuals
scatter = ggplot(plots, aes(fitted, stud_resid)) + 
  ggtitle("Residuals") +
  geom_point() + 
  geom_hline(yintercept=0, linetype="dashed") +
  xlim(0,5) +
  ylim(-3,3) +
  labs(x = "Fitted values", y= "Studentized residual") +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
scatter 


library(cowplot)
plotgrid <- plot_grid(ncol = 3,
                      histogram + stat_function(fun = dnorm, args = list(mean = mean(plots$stud_resid, na.rm = TRUE), sd = sd(plots$stud_resid, na.rm = TRUE)), size = 1, linetype="dashed")
                      ,qqplot.resid
                      ,scatter
)
plotgrid


## Plots for Photo_Video Model

# Preparing
plots = data.frame(fitted = model7$fitted.values, stud_resid = Photo_Video$stud_resid)

#Histogram
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
histogram = ggplot(plots, aes(x = stud_resid)) + 
  ggtitle("Histogram") +
  geom_histogram(aes(y =..density..), colour = "black", fill = "white") +
  labs(x = "Studentized residual", y = "Density") +
  xlim(-3,3) +
  ylim(0,0.45) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
histogram + stat_function(fun = dnorm, args = list(mean = mean(plots$stud_resid, na.rm = TRUE), sd = sd(plots$stud_resid, na.rm = TRUE)), size = 1, linetype="dashed")

#Q-Q Plot
qqplot.resid = qplot(sample = plots$stud_resid , stat="qq") + 
  ggtitle("Q-Q") +
  labs(x = "Theoretical values", y = "Observed values") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  xlim(-3,3) +
  ylim(-3,3) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
qqplot.resid

#Residuals
scatter = ggplot(plots, aes(fitted, stud_resid)) + 
  ggtitle("Residuals") +
  geom_point() + 
  geom_hline(yintercept=0, linetype="dashed") +
  xlim(0,5) +
  ylim(-3,3) +
  labs(x = "Fitted values", y= "Studentized residual") +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
scatter 

library(cowplot)
plotgrid <- plot_grid(ncol = 3,
                      histogram + stat_function(fun = dnorm, args = list(mean = mean(plots$stud_resid, na.rm = TRUE), sd = sd(plots$stud_resid, na.rm = TRUE)), size = 1, linetype="dashed")
                      ,qqplot.resid
                      ,scatter
)
plotgrid


## Plots for Social Model

# Preparing
plots = data.frame(fitted = model8$fitted.values, stud_resid = Social$stud_resid)

#Histogram
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
histogram = ggplot(plots, aes(x = stud_resid)) + 
  ggtitle("Histogram") +
  geom_histogram(aes(y =..density..), colour = "black", fill = "white") +
  labs(x = "Studentized residual", y = "Density") +
  xlim(-3,3) +
  ylim(0,0.45) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
histogram + stat_function(fun = dnorm, args = list(mean = mean(plots$stud_resid, na.rm = TRUE), sd = sd(plots$stud_resid, na.rm = TRUE)), size = 1, linetype="dashed")

#Q-Q Plot
qqplot.resid = qplot(sample = plots$stud_resid , stat="qq") + 
  ggtitle("Q-Q Plot") +
  labs(x = "Theoretical values", y = "Observed values") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  xlim(-3,3) +
  ylim(-3,3) +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
qqplot.resid

#Residuals
scatter = ggplot(plots, aes(fitted, stud_resid)) + 
  ggtitle("Residuals") +
  geom_point() + 
  geom_hline(yintercept=0, linetype="dashed") +
  xlim(0,5) +
  ylim(-3,3) +
  labs(x = "Fitted values", y= "Studentized residual") +
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5))
scatter 


library(cowplot)
plotgrid <- plot_grid(ncol = 3,
                      histogram + stat_function(fun = dnorm, args = list(mean = mean(plots$stud_resid, na.rm = TRUE), sd = sd(plots$stud_resid, na.rm = TRUE)), size = 1, linetype="dashed")
                      ,qqplot.resid
                      ,scatter
)
plotgrid


### 13) Diagnostics, Revisiting Assumptions -------------------------------------------------------------------------------------------------------------------------

# Normaly distributed residuals

Quality = data.frame(model = c("model1","model2","model3","model4","model5","model6","model7","model8"),
                     dataset = c("Trans","Trans","Trans","Trans","Lifestyle","Games","Photo & Video","Social Networking"),
                     mean = c(mean(resid(model1)),
                              mean(resid(model2)),
                              mean(resid(model3)),
                              mean(resid(model4)),
                              mean(resid(model5)),
                              mean(resid(model6)),
                              mean(resid(model7)),
                              mean(resid(model8))),
                     sd = c(sd(resid(model1)),
                            sd(resid(model2)),
                            sd(resid(model3)),
                            sd(resid(model4)),
                            sd(resid(model5)),
                            sd(resid(model6)),
                            sd(resid(model7)),
                            sd(resid(model8))),
                     D = c(as.numeric(ks.test(resid(model1),"pnorm",mean(resid(model1)),sd(resid(model1)))[1]),
                           as.numeric(ks.test(resid(model2),"pnorm",mean(resid(model2)),sd(resid(model2)))[1]),
                           as.numeric(ks.test(resid(model3),"pnorm",mean(resid(model3)),sd(resid(model3)))[1]),
                           as.numeric(ks.test(resid(model4),"pnorm",mean(resid(model4)),sd(resid(model4)))[1]),
                           as.numeric(ks.test(resid(model5),"pnorm",mean(resid(model5)),sd(resid(model5)))[1]),
                           as.numeric(ks.test(resid(model6),"pnorm",mean(resid(model6)),sd(resid(model6)))[1]),
                           as.numeric(ks.test(resid(model7),"pnorm",mean(resid(model7)),sd(resid(model7)))[1]),
                           as.numeric(ks.test(resid(model8),"pnorm",mean(resid(model8)),sd(resid(model8)))[1])),
                     KS_test = c(as.numeric(ks.test(resid(model1),"pnorm",mean(resid(model1)),sd(resid(model1)))[2]),
                                 as.numeric(ks.test(resid(model2),"pnorm",mean(resid(model2)),sd(resid(model2)))[2]),
                                 as.numeric(ks.test(resid(model3),"pnorm",mean(resid(model3)),sd(resid(model3)))[2]),
                                 as.numeric(ks.test(resid(model4),"pnorm",mean(resid(model4)),sd(resid(model4)))[2]),
                                 as.numeric(ks.test(resid(model5),"pnorm",mean(resid(model5)),sd(resid(model5)))[2]),
                                 as.numeric(ks.test(resid(model6),"pnorm",mean(resid(model6)),sd(resid(model6)))[2]),
                                 as.numeric(ks.test(resid(model7),"pnorm",mean(resid(model7)),sd(resid(model7)))[2]),
                                 as.numeric(ks.test(resid(model8),"pnorm",mean(resid(model8)),sd(resid(model8)))[2]))
)

Quality$mean = round(Quality$mean, 4)
Quality$sd = round(Quality$sd, 4)
Quality$D = round(Quality$D, 4)
Quality$KS_test = round(Quality$KS_test, 4)

for(i in 1:nrow(Quality)){
  if(as.numeric(Quality$KS_test[i]) > 0.05){
    Quality$p_value_KS[i]= ""
  }
  if(as.numeric(Quality$KS_test[i]) < 0.05 && as.numeric(Quality$KS_test[i])>0.01){
    Quality$p_value_KS[i]= "*"
  }
  if(as.numeric(Quality$KS_test[i]) < 0.01 && as.numeric(Quality$KS_test[i])>0.001){
    Quality$p_value_KS[i]= "**"
  }
  if(as.numeric(Quality$KS_test[i]) < 0.001){
    Quality$p_value_KS[i]= "***"
  }
}

# Assessing Independence of Errors / Autokorrelation

Durbin_Watson = data.frame(model = c("model1","model2","model3","model4","model5","model6","model7","model8"),
                           dataset = c("Trans","Trans","Trans","Trans","Lifestyle","Games","Photo & Video","Social Networking"),
                           Auto_cor = c(as.numeric(durbinWatsonTest(model1)[1]),
                                        as.numeric(durbinWatsonTest(model2)[1]),
                                        as.numeric(durbinWatsonTest(model3)[1]),
                                        as.numeric(durbinWatsonTest(model4)[1]),
                                        as.numeric(durbinWatsonTest(model5)[1]),
                                        as.numeric(durbinWatsonTest(model6)[1]),
                                        as.numeric(durbinWatsonTest(model7)[1]),
                                        as.numeric(durbinWatsonTest(model8)[1])),
                           D_W_Stat = c(as.numeric(durbinWatsonTest(model1)[2]),
                                        as.numeric(durbinWatsonTest(model2)[2]),
                                        as.numeric(durbinWatsonTest(model3)[2]),
                                        as.numeric(durbinWatsonTest(model4)[2]),
                                        as.numeric(durbinWatsonTest(model5)[2]),
                                        as.numeric(durbinWatsonTest(model6)[2]),
                                        as.numeric(durbinWatsonTest(model7)[2]),
                                        as.numeric(durbinWatsonTest(model8)[2])),
                           p_value  = c(as.numeric(durbinWatsonTest(model1)[3]),
                                        as.numeric(durbinWatsonTest(model2)[3]),
                                        as.numeric(durbinWatsonTest(model3)[3]),
                                        as.numeric(durbinWatsonTest(model4)[3]),
                                        as.numeric(durbinWatsonTest(model5)[3]),
                                        as.numeric(durbinWatsonTest(model6)[3]),
                                        as.numeric(durbinWatsonTest(model7)[3]),
                                        as.numeric(durbinWatsonTest(model8)[3]))
)

Durbin_Watson$Auto_cor = round(Durbin_Watson$Auto_cor, 4)
Durbin_Watson$D_W_Stat = round(Durbin_Watson$D_W_Stat, 4)
Durbin_Watson$p_value = round(Durbin_Watson$p_value, 4)

for(i in 1:nrow(Durbin_Watson)){
  if(as.numeric(Durbin_Watson$p_value[i]) > 0.05){
    Durbin_Watson$sig_DB[i]= ""
  }
  if(as.numeric(Durbin_Watson$p_value[i]) < 0.05 && as.numeric(Durbin_Watson$p_value[i])>0.01){
    Durbin_Watson$sig_DB[i]= "*"
  }
  if(as.numeric(Durbin_Watson$p_value[i]) < 0.01 && as.numeric(Durbin_Watson$p_value[i])>0.001){
    Durbin_Watson$sig_DB[i]= "**"
  }
  if(as.numeric(Durbin_Watson$p_value[i]) < 0.001){
    Durbin_Watson$sig_DB[i]= "***"
  }
}

# Assessing Multicollineraity 

#Will always return significant values >5 due to the interaction effects

### 14) Theory Plots -------------------------------------------------------------------------------------------------------------------------

## 3. Mobile App Ecosystem


# Global average media consumption per individual (read as excel)

loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
ggplot(Trend_media_consumption, aes(fill=factor(Medium, levels=c("Mobile","Desktop","TV","Print" )), y=Usage, x=Year)) +
  geom_bar(position="stack", stat="identity", width = 0.6)+
  scale_fill_manual(values= c(Dark_Blue,Blue,Light_Blue,Grey))+
  labs(x = "", y= "Minutes per day") +
  geom_text(data = Trend_media_consumption %>% filter(Year == 2012),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Usage, 
                label = scales::percent(Usage/sum(Usage))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Trend_media_consumption %>% filter(Year == 2013),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Usage, 
                label = scales::percent(Usage/sum(Usage))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Trend_media_consumption %>% filter(Year == 2014),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Usage, 
                label = scales::percent(Usage/sum(Usage))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Trend_media_consumption %>% filter(Year == 2015),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Usage, 
                label = scales::percent(Usage/sum(Usage))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Trend_media_consumption %>% filter(Year == 2016),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Usage, 
                label = scales::percent(Usage/sum(Usage))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Trend_media_consumption %>% filter(Year == 2017),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Usage, 
                label = scales::percent(Usage/sum(Usage))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Trend_media_consumption %>% filter(Year == 2018),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Usage, 
                label = scales::percent(Usage/sum(Usage))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Trend_media_consumption %>% filter(Year == 2019),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Usage, 
                label = scales::percent(Usage/sum(Usage))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Trend_media_consumption %>% filter(Year == 2020),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Usage, 
                label = scales::percent(Usage/sum(Usage))) ,
            position = position_stack(vjust = .5))+
  scale_x_continuous(breaks=seq(2012,2020,1))+
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        legend.title = element_blank()
  )


# Share of revenue across medium (read as excel)

loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
ggplot(Medium_revenue, aes(fill=factor(Medium, levels=c("Mobile","Desktop","Console" )), y=Value, x=Year)) +
  geom_bar(position="stack", stat="identity", width = 0.6)+
  scale_fill_manual(values= c(Dark_Blue,Light_Blue,Grey))+
  labs(x = "", y= "Annual revenue in $bn") +
  geom_text(data = Medium_revenue %>% filter(Year == 2012),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Value, 
                label = scales::percent(Value/sum(Value))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Medium_revenue %>% filter(Year == 2013),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Value, 
                label = scales::percent(Value/sum(Value))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Medium_revenue %>% filter(Year == 2014),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Value, 
                label = scales::percent(Value/sum(Value))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Medium_revenue %>% filter(Year == 2015),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Value, 
                label = scales::percent(Value/sum(Value))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Medium_revenue %>% filter(Year == 2016),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Value, 
                label = scales::percent(Value/sum(Value))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Medium_revenue %>% filter(Year == 2017),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Value, 
                label = scales::percent(Value/sum(Value))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Medium_revenue %>% filter(Year == 2018),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Value, 
                label = scales::percent(Value/sum(Value))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Medium_revenue %>% filter(Year == 2019),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Value, 
                label = scales::percent(Value/sum(Value))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Medium_revenue %>% filter(Year == 2020),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Value, 
                label = scales::percent(Value/sum(Value))) ,
            position = position_stack(vjust = .5))+
  geom_text(data = Medium_revenue %>% filter(Year == 2021),
            colour="white",
            fontface = "bold",
            aes(x = Year, y = Value, 
                label = scales::percent(Value/sum(Value))) ,
            position = position_stack(vjust = .5))+
  scale_x_continuous(breaks=seq(2012,2021,1))+
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        legend.title = element_blank()
  )

# Share of revenue across medium (read as excel)
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
ggplot(Monetization, aes(fill=factor(Category, levels=c("User Penetration","Total Revenue Share")), y=Value, x=factor(Model, levels=c("Subscription","Freemium","Ad-supported","Other")))) +
  geom_bar(position="dodge", stat="identity", width = 0.6)+
  scale_fill_manual(values= c(Dark_Blue,Light_Blue))+
  labs(x = "", y= "") +
  scale_y_continuous(labels = scales::percent, limits=c(0,1))+
  theme_minimal() +
  theme(text=element_text(family="Times", face="bold", size=20), plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        legend.title = element_blank()
  )
