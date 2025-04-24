#' ---
#' title: "CovidProject_Mahoney"
#' subtitle: "Biology 470"
#' author: Peter Mahoney
#' date: '`r paste("Created 2025-18-03. Last updated on ", Sys.Date())`'
#' output: html_document
#' ---

#' Dependencies:
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("gridExtra")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(mgcv)


# Data import and processing Processing ----
hosp_data <- read.csv("./ProjectData/Hospitalizations.csv")

#Converts the dates in the original data from a string to a date.
hosp_data$X_WeekendDate <- as.Date(hosp_data$X_WeekendDate)
hosp_data <- hosp_data[order(hosp_data$X_WeekendDate), ]


# Visual ----
#' 1. **Visualizations** using histograms:

#' **Race Visualization**
#' Creates a new object with only the relevant columns, and selects only data rows
#' which are from all states and not corrected (crude).

racedat <- subset(hosp_data[,c("Season","Race_Label","WeeklyRate")],
                  hosp_data$Type=="Crude Rate"
                & hosp_data$AgeCategory_Legend =="All"
                & hosp_data$Sex_Label == "All"
                & hosp_data$State =="COVID-NET"
                & hosp_data$Race_Label != "All"
                 )
#' Next, we need to combine race categories for each year. i.e. we want to have 
#' total for each race for each year. Note that year is called season in this 
#' dataset. 
racecomb <- racedat %>%
  group_by(Season,Race_Label) %>%
  summarise(tr = sum(WeeklyRate),.groups = "drop")

#' Next, we need to create our histograms. This will be accomplished by 
#' creating each plot and then calling them all at once.
r1 <-ggplot(subset(racecomb,racecomb$Season=="2019-20"),
            aes(x=Race_Label,y=tr/max(tr),fill = as.factor(Race_Label)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
        labs(title = "2019-2020")+
  ylab("Relative rate")
  

r2 <-ggplot(subset(racecomb,racecomb$Season=="2020-21"),
            aes(x=Race_Label,y=tr/max(tr),fill = as.factor(Race_Label)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "None")+
        labs(title = "2020-2021")+
  ylab("Relative rate")

r3 <-ggplot(subset(racecomb,racecomb$Season=="2021-22"),
            aes(x=Race_Label,y=tr/max(tr),fill = as.factor(Race_Label)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "None")+
        labs(title = "2021-2022")+
  ylab("Relative rate")

r4 <-ggplot(subset(racecomb,racecomb$Season=="2022-23"),
            aes(x=Race_Label,y=tr/max(tr),fill = as.factor(Race_Label)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "None")+
        labs(title = "2022-2023")+
  ylab("Relative rate")

r5 <-ggplot(subset(racecomb,racecomb$Season=="2023-24"),
            aes(x=Race_Label,y=tr/max(tr),fill = as.factor(Race_Label)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "None")+
  labs(title = "2023-2024")+
  ylab("Relative rate")

#' This line calls all the race plots and displays them in one column.
rplots <- grid.arrange(r1,r2,r3,r4,r5,ncol=1)

#' Next, we save our plots, if needed. Uncomment next lines to save.
 # jpeg("./Figures/r1.jpeg"); a1; dev.off()
 # jpeg("./Figures/r2.jpeg"); a2; dev.off()
 # jpeg("./Figures/r3.jpeg"); a3; dev.off()
 # jpeg("./Figures/r4.jpeg"); a4; dev.off()
 # jpeg("./Figures/r5.jpeg"); a5; dev.off()
 # jpeg("./Figures/allrace.jpeg"); grid.arrange(r1,r2,r3,r4,r5,ncol=1); dev.off()


#' **AGE**\
#' In this section, we will visualize the change in age distribution across the 
#' five years of covid. Note that years are referred to as season in the data.\
#' \
#' First, we need to create a data set with only season (year), age group, and 
#' the rate. Additionally, we can refine which age categories we care about
#' (Note that the data has several overlapping categories, so this step is 
#' essential).
agedat <- subset(hosp_data[,c("Season","AgeCategory_Legend","WeeklyRate")],hosp_data$Type=="Crude Rate"
                 & hosp_data$Race_Label =="All"
                 & hosp_data$Sex_Label == "All"
                 & hosp_data$State =="COVID-NET"
                 & hosp_data$AgeCategory_Legend %in% c("0-17 years (Children)",
                                                       "18-49 years",
                                                       "50-64 years",
                                                       "65-74 years",
                                                       "75-84 years",
                                                       "≥85 years")
                 )

#' Next, we need to sum the hospitalizations for each age category each year. 
#' In other words, we want a data frame with the total number for each category 
#' for each season. Note that the units will no longer be in hospitalizations 
#' per 100,000. It is best to simply consider the units as relative units (i.e. 
#' only for comparison!).
agecomb <- agedat %>%
  group_by(Season,AgeCategory_Legend) %>%
  summarise(tr = sum(WeeklyRate),.groups = "drop")

#' This will order the ages correctly on the histograms.
ageorder = c("0-17 years (Children)",
             "18-49 years",
             "50-64 years",
             "65-74 years",
             "75-84 years",
             "≥85 years")
#' Next, we need to create all of our plots. Note that the x value is passed as 
#' a factor to include the correct order.\
#' \
#' 2019-2020:
a1 <-ggplot(subset(agecomb,agecomb$Season=="2019-20"),
            aes(x=factor(AgeCategory_Legend,ageorder),y=tr/max(tr),fill = as.factor(AgeCategory_Legend)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(title = "2019-2020")+
  ylab("Relative rate")

#' 2020-2021
a2 <-ggplot(subset(agecomb,agecomb$Season=="2020-21"),
            aes(x=factor(AgeCategory_Legend,ageorder),y=tr/max(tr),fill = as.factor(AgeCategory_Legend)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "None")+
  labs(title = "2020-2021")+
  ylab("Relative rate")

#' 2021-2022
a3 <-ggplot(subset(agecomb,agecomb$Season=="2021-22"),
            aes(x=factor(AgeCategory_Legend,ageorder),y=tr/max(tr),fill = as.factor(AgeCategory_Legend)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "None")+
  labs(title = "2021-2022")+
  ylab("Relative rate")

#' 2022-2023
a4 <-ggplot(subset(agecomb,agecomb$Season=="2022-23"),
            aes(x=factor(AgeCategory_Legend,ageorder),y=tr/max(tr),fill = as.factor(AgeCategory_Legend)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "None")+
  labs(title = "2022-2023")+
  ylab("Relative rate")
#' 2023-2024
a5 <-ggplot(subset(agecomb,agecomb$Season=="2023-24"),
            aes(x=factor(AgeCategory_Legend,ageorder),y=tr/max(tr),fill = as.factor(AgeCategory_Legend)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "None")+
  labs(title = "2023-2024")+
  ylab("Relative rate")

#' This line simply calls all the plots and displays them in a single column.
allage <- grid.arrange(a1,a2,a3,a4,a5,ncol=1,nrow = 5)

#' Next, we save our plots, if needed. Uncomment next lines to save.
# jpeg("./Figures/a1.jpeg"); a1; dev.off()
# jpeg("./Figures/a2.jpeg"); a2; dev.off()
# jpeg("./Figures/a3.jpeg"); a3; dev.off()
# jpeg("./Figures/a4.jpeg"); a4; dev.off()
# jpeg("./Figures/a5.jpeg"); a5; dev.off()
# jpeg("./Figures/allage.jpeg"); grid.arrange(a1,a2,a3,a4,a5,ncol=1); dev.off()



#' **Sex**
#' This section visually examines sex distribution.\
#' First, we create a subset with only the relevant data and columns. In short, 
#' we want all varliable except for sex to be set to all.
sexdat <- subset(hosp_data[,c("Season","Sex_Label","WeeklyRate")],hosp_data$Type=="Crude Rate"
                 & hosp_data$Race_Label =="All"
                 & hosp_data$AgeCategory_Legend == "All"
                 & hosp_data$State =="COVID-NET"
                 & hosp_data$Sex_Label %in% c("Male","Female")
)

#' In the same way as before, we next need to combine each category for each 
#' year.
sexcomb <- sexdat %>%
  group_by(Season,Sex_Label) %>%
  summarise(tr = sum(WeeklyRate),.groups = "drop")

#' Next, we create our individual histograms.
s1 <-ggplot(subset(sexcomb,sexcomb$Season=="2019-20"),
            aes(x=Sex_Label,y=tr/max(tr),fill = as.factor(Sex_Label)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(title = "2019-2020")+
  ylab("Relative rate")
  

s2 <-ggplot(subset(sexcomb,sexcomb$Season=="2020-21"),
            aes(x=Sex_Label,y=tr/max(tr),fill = as.factor(Sex_Label)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "None")+
  labs(title = "2020-2021")+
  ylab("Relative rate")

s3 <-ggplot(subset(sexcomb,sexcomb$Season=="2021-22"),
            aes(x=Sex_Label,y=tr/max(tr),fill = as.factor(Sex_Label)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "None")+
  labs(title = "2021-2022")+
  ylab("Relative rate")
s4 <-ggplot(subset(sexcomb,sexcomb$Season=="2022-23"),
            aes(x=Sex_Label,y=tr/max(tr),fill = as.factor(Sex_Label)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "None")+
  labs(title = "2022-2023")+
  ylab("Relative rate")

s5 <-ggplot(subset(sexcomb,sexcomb$Season=="2023-24"),
            aes(x=Sex_Label,y=tr/max(tr),fill = as.factor(Sex_Label)))+
  geom_bar(stat = "Identity")+
  scale_fill_hue(c=40)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "None")+
  labs(title = "2023-2024")+
  ylab("Relative rate")

#' This line calls all the plots and displays them in one column.
splots <- grid.arrange(s1,s2,s3,s4,s5,ncol=1)


#' Next, we save our plots, if needed. Uncomment next lines to save.
# jpeg("./Figures/s1.jpeg"); s1; dev.off()
# jpeg("./Figures/s2.jpeg"); s2; dev.off()
# jpeg("./Figures/s3.jpeg"); s3; dev.off()
# jpeg("./Figures/s4.jpeg"); s4; dev.off()
# jpeg("./Figures/s5.jpeg"); s5; dev.off()
# jpeg("./Figures/allsex.jpeg"); grid.arrange(s1,s2,s3,s4,s5,ncol=1); dev.off()


#' It appears that sex ratios stay relatively consistient over the 5 year span.
#' Thus, it is expected to see a relatively small difference in slope in the 
#' models between years.\
#' \


# Models ----
#' **Modeling**:\
#' This section aims to see the differences in models between the years. Note 
#' that I intend to create non-linear models and use statistical tests between 
#' them for the final project.\

#' **Race Models*

#' 2019-2020
rlm1 <- lm(WeeklyRate ~ Race_Label,
           data = subset(racedat,racedat$Season=="2019-20"))
summary(rlm1)
anova(rlm1)

#' 2020-2021
rlm2 <- lm(WeeklyRate ~ Race_Label,
           data = subset(racedat,racedat$Season=="2020-21"))
summary(rlm2)
anova(rlm2)

#' 2021-2022
rlm3 <- lm(WeeklyRate ~ Race_Label,
           data = subset(racedat,racedat$Season=="2021-22"))
summary(rlm3)
anova(rlm3)

#' 2022-2023
rlm4 <- lm(WeeklyRate ~ Race_Label,
           data = subset(racedat,racedat$Season=="2022-23"))
summary(rlm4)
anova(rlm4)

#' 2023-2024
rlm5 <- lm(WeeklyRate ~ Race_Label,
           data = subset(racedat,racedat$Season=="2023-24"))
summary(rlm5)
anova(rlm5)

#' While the sum sq is different for each season, the F value is similar, 
#' indicating that there is a similar trend all 5 years.\
#' \

#' **Age Models**

#' 2019-2020
alm1 <- lm(WeeklyRate ~ AgeCategory_Legend,
           data = subset(agedat,agedat$Season=="2019-20"))
summary(alm1)
anova(alm1)

#' 2020-2021
alm2 <- lm(WeeklyRate ~ AgeCategory_Legend,
           data = subset(agedat,agedat$Season=="2020-21"))
summary(alm2)
anova(alm2)

#' 2021-2022
alm3 <- lm(WeeklyRate ~ AgeCategory_Legend,
           data = subset(agedat,agedat$Season=="2021-22"))
summary(alm3)
anova(alm3)

#' 2022-2023
alm4 <- lm(WeeklyRate ~ AgeCategory_Legend,
           data = subset(agedat,agedat$Season=="2022-23"))
summary(alm4)
anova(alm4)

#' 2023-2024
alm5 <- lm(WeeklyRate ~ AgeCategory_Legend,
           data = subset(agedat,agedat$Season=="2023-24"))
summary(alm5)
anova(alm5)
#' The F-value appears to be increasing over the years, indicating that the 
#' difference between the groups is increasing. In other words, it appears that 
#' that distribution is becoming less and less uniform.\
#' \

#' **Sex Modeling**
#' 2019-2020
slm1 <- lm(WeeklyRate ~ Sex_Label,
           data = subset(sexdat,sexdat$Season=="2019-20"))
summary(slm1)
anova(slm1)

#' 2020-2021
slm2 <- lm(WeeklyRate ~ Sex_Label,
           data = subset(sexdat,sexdat$Season=="2020-21"))
summary(slm2)
anova(slm2)

#' 2021-2022
slm3 <- lm(WeeklyRate ~ Sex_Label,
           data = subset(sexdat,sexdat$Season=="2021-22"))
summary(slm3)
anova(slm3)

#' 2022-2023
slm4 <- lm(WeeklyRate ~ Sex_Label,
           data = subset(sexdat,sexdat$Season=="2022-23"))
summary(slm4)
anova(slm4)

#' 2023-2024
slm5 <- lm(WeeklyRate ~ Sex_Label,
           data = subset(sexdat,sexdat$Season=="2023-24"))
summary(slm5)
anova(slm5)

#' The F-stat appears to be constant or decreasing, indicating that the 
#' difference between groups is decreasing.

