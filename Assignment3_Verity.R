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
education <- education[1:totalrow - 1,] #Removing empty rows at end of data frame

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
names(edu_tidy_join)[4] <- "LGA"
View(edu_tidy_join)
summary(edu_tidy_join)
str(edu_tidy_join)
glimpse(edu_tidy_join)

#Grouping by LGA
edu_by_LGA <- edu_tidy_join %>% group_by(LGA, Ed_level) %>% summarise(People = sum(People))
head(edu_by_LGA)
glimpse(edu_by_LGA)

#Bringing in health-based data
LGA_health <- read_excel("LGA_Profile_2015.xlsx", col_names = TRUE, sheet = "LGAs")
View(LGA_health)

Subset_Columns <- c("LGA Name", "Travel time to Melbourne", "Total fertility rate", "Top 5 overseas countries of birth - country 1",
                    "Top 5 ancestries - ancestry 1", "People who believe multiculturalism makes life better", "People reporting high/very high psychological distress",
                    "People who live near public transport", "Primary Health Network (PHN)", "Median household income")
LGA_health_ss <- LGA_health[Subset_Columns]
View(LGA_health_ss)
summary(LGA_health_ss)
str(LGA_health_ss)
glimpse(LGA_health_ss)

#########################
# Outliers with Boxplots
#########################

# Post grad and Under Grad count
edu_uni <- edu_tidy_join %>% filter(Ed_level <= 'Bachelor Degree Level')
summary(edu_uni)
glimpse(edu_uni)

# Refactor levels of education
edu_uni$Ed_level <- factor(edu_uni$Ed_level,
                           labels = c("Postgraduate Degree Level", "Graduate Diploma and Graduate Certificate Level",
                                      "Bachelor Degree Level"),
                           levels = c("Postgraduate Degree Level", "Graduate Diploma and Graduate Certificate Level",
                                      "Bachelor Degree Level"))
boxplot(People~Ed_level, data = edu_uni) 


#let's mutate post and grad into one (total counts)
edu_uni_tot <- education %>%  mutate(Post = (education$`Postgraduate Degree Level` + education$`Graduate Diploma and Graduate Certificate Level`),
                                     UnderGrad = education$`Bachelor Degree Level`) %>% 
                                      select(SA1, Post, UnderGrad)
head(edu_uni_tot)
edu_uni_tot$SA1 <- as.numeric(edu_uni_tot$SA1)
edu_uni_tot2 <- edu_uni_tot %>% left_join(geo_lookup[, c('SA1_7DIG16','LGA_NAME17')], by = c('SA1' = 'SA1_7DIG16'))
head(edu_uni_tot2)
glimpse(edu_uni_tot2)

# spread then mutate, will be easier
box_post_tot <- boxplot(edu_uni_tot2[,2], main  = 'Post Grad Students per Region')
length(box_post_tot$out) # 532 outliers for post grad
box_under_tot <- boxplot(edu_uni_tot2[,3], main = 'UnderGrad Students per Region')
length(box_under_tot$out) # 454 outliers for under grad

#let's mutate post and grad into one (but as a proportion of the total)
edu_uni_prop <- education %>%  mutate(Post = (education$`Postgraduate Degree Level` + education$`Graduate Diploma and Graduate Certificate Level`)/education$Total,
                                      UnderGrad = education$`Bachelor Degree Level`/education$Total) %>% 
                                      select(SA1, Post, UnderGrad)
head(edu_uni_prop)
glimpse(edu_uni_prop)
edu_uni_prop$SA1 <- as.numeric(edu_uni_prop$SA1)
edu_uni_prop2 <- edu_uni_prop %>% left_join(geo_lookup[, c('SA1_7DIG16','LGA_NAME17')], by = c('SA1' = 'SA1_7DIG16'))
# spread then mutate, will be easier
box_post_prop <- boxplot(edu_uni_prop2[,2], main = 'Proportion of Post Grad Students per Region')
length(box_post_prop$out) # 203 outliers, significant drop in outliers when you take into account the proportion
box_under_prop <- boxplot(edu_uni_prop2[,3], main = 'Proportion of Under Grad Students per Region')
length(box_under_prop$out) # 122 outliers

boxplot(edu_uni_tot2[,2:3])
boxplot(edu_uni_prop2[,2:3]) # both visuals
# finding proportions drastically reduces the count of outliers

### Now what to do with outliers?


##############################
# Transformation
##############################

edu_post <- edu_tidy_join %>%  filter(Ed_level == 'Postgraduate Degree Level')

hist(edu_post$People)

edu_post_log <- log(edu_post$People)

hist(edu_post_log) # log transformation shows a clear normal distribution


edu_under <- edu_tidy_join %>%  filter(Ed_level == 'Bachelor Degree Level')

edu_under_log <- log(edu_under$People)
hist(edu_under$People)
hist(edu_under_log)



##############################
# Summary
##############################

edu_prop_sum <- edu_uni_prop2 %>%  group_by(LGA_NAME17) %>%  summarise(meanPost = mean(Post, na.rm = TRUE),
                                                                       medianPost = median(Post, na.rm = TRUE),
                                                                       maxPost = max(Post, na.rm = TRUE),
                                                                       minPost = min(Post, na.rm = TRUE),
                                                                       meanUnder = mean(UnderGrad, na.rm = TRUE),
                                                                       medianUnder = median(UnderGrad, na.rm = TRUE),
                                                                       maxUnder = max(UnderGrad, na.rm = TRUE),
                                                                       minUnder = min(UnderGrad, na.rm = TRUE))
View(edu_prop_sum)

##############################
# Regression Machine Learning
##############################

# If we know how many undergrad students we have, can you predict how many students will undertake post grad courses?
# Select data for ml
data <- edu_uni_tot2[2:3]

# Make task
task <- makeRegrTask(data = data, target = 'Post')

# Make learner
learner <- makeLearner('regr.glm')

# Fit model
n <- nrow(data)
training.set <- sample(n, size = 2*n/3)
test.set <- setdiff(1:n, training.set)

model <- mlr::train(learner, task, subset = training.set)

# Predict
pred <- predict(model, task = task, subset = test.set)

# Evaluate
performance(pred, measures = list(mse, mae))

x <- pred$data$truth
y <- pred$data$response
plot(x, y, xlab = 'Actual Value', ylab = 'Predicted Value', col = 'blue', main = 'Regression Machine Learning')
abline(1:500, 1:500, lwd = 2, col = 'red')


####################################
#LGA based outlier work

# Post grad and Under Grad count
edu_uni_lga <- edu_by_LGA %>% filter(Ed_level <= 'Bachelor Degree Level')
summary(edu_uni_lga)
glimpse(edu_uni_lga)

# Refactor levels of education
edu_uni_lga$Ed_level <- factor(edu_uni_lga$Ed_level,
                           labels = c("Postgraduate Degree Level", "Graduate Diploma and Graduate Certificate Level",
                                      "Bachelor Degree Level"),
                           levels = c("Postgraduate Degree Level", "Graduate Diploma and Graduate Certificate Level",
                                      "Bachelor Degree Level"))
boxplot(People~Ed_level, data = edu_uni_lga) 

#let's mutate post and grad into one (total counts)
edu_uni_tot_lga <- education %>%  mutate(Post = (education$`Postgraduate Degree Level` + education$`Graduate Diploma and Graduate Certificate Level`),
                                     UnderGrad = education$`Bachelor Degree Level`) %>% 
  select(LGA_NAME17, Post, UnderGrad)
head(edu_uni_tot)
edu_uni_tot$SA1 <- as.numeric(edu_uni_tot$SA1)
edu_uni_tot2 <- edu_uni_tot %>% left_join(geo_lookup[, c('SA1_7DIG16','LGA_NAME17')], by = c('SA1' = 'SA1_7DIG16'))
head(edu_uni_tot2)
glimpse(edu_uni_tot2)


