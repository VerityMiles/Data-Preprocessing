#Importing packages
library(readr)
#library(xlsx)
library(readxl)
#library(foreign)
#library(gdata)
#library(rvest)
library(dplyr)
library(tidyr)
#library(deductive)
#library(validate)
#library(Hmisc)
#library(forecast)
#library(stringr)
#library(lubridate)
#library(outliers)
#library(MVN)
#library(infotheo)
#library(MASS)
#library(caret)
#library(ggplot2)
#library(knitr)

#setwd("C:/Users/verit/Documents/RMIT/DataPreprocessing/Assessment/Assignment3")

#Bringing in census data
education <- read_excel("allVic_education.xls", skip = 8, col_names = TRUE)
education <- education[-1,-1] #Removing first row and column
colnames(education)[1] <- "SA1"

totalrow <- which(education$SA1 == 'Total')
education <- education[1:totalrow-1,] #Removing empty rows at end of data frame

#Tidyr-ing
edu_tidy <- gather(education, "Ed_level", "People", 2:length(education))
#View(edu_tidy)
edu_tidy <- edu_tidy %>% mutate(Ed_level = factor(Ed_level, levels = c("Postgraduate Degree Level", "Graduate Diploma and Graduate Certificate Level",
                                                                       "Bachelor Degree Level", "Advanced Diploma and Diploma Level", "Certificate III & IV Level",
                                                                       "Secondary Education - Years 10 and above", "Certificate I & II Level", "Secondary Education - Years 9 and below",
                                                                       "Supplementary Codes", "Not stated", "Not applicable", "Total"),
                                                  labels = c("Postgraduate Degree Level", "Graduate Diploma and Graduate Certificate Level",
                                                             "Bachelor Degree Level", "Advanced Diploma and Diploma Level", "Certificate III & IV Level",
                                                             "Secondary Education - Years 10 and above", "Certificate I & II Level", "Secondary Education - Years 9 and below",
                                                             "Supplementary Codes", "Not stated", "Not applicable", "Total"),
                                                  ordered = TRUE))
edu_tidy <- transform(edu_tidy, SA1 = as.numeric(SA1))


#Bringing in lookup data
geo_lookup <- read_csv("SA1_LGA_LookUp.csv")
#View(geo_lookup)

#Adding LGA column to education data
edu_tidy_join <- edu_tidy %>% left_join(geo_lookup[,c("SA1_7DIG16", "LGA_NAME17", "MetroMelbourne")], by= c("SA1" = "SA1_7DIG16"))
View(edu_tidy_join)
summary(edu_tidy_join)
str(edu_tidy_join)

#Bringing in health-based data
LGA_health <- read_excel("LGA_Profile_2015.xlsx", col_names = TRUE, sheet = "LGAs")
View(LGA_health)

Subset_Columns <- c("LGA Name", "Travel time to Melbourne", "Total fertility rate", "Top 5 overseas countries of birth - country 1",
                    "Top 5 ancestries - ancestry 1", "People who believe multiculturalism makes life better", "People reporting high/very high psychological distress",
                    "People who live near public transport", "Primary Health Network (PHN)")
LGA_health_ss <- LGA_health[Subset_Columns]
View(LGA_health_ss)
summary(LGA_health_ss)
str(LGA_health_ss)
