#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("report")
#install.packages("stringr")
#install.packages("DT")
#install.packages("ggpubr")

library(DT)
library(dplyr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(report)
library(stringr)

# mean, sd, median
# count; hist; plot; barplot

##read in the data
setwd("C:/Users/tongk2/OneDrive - National Institutes of Health/slipsSlips")
recig_final <- read.csv("recig_final.csv")

#summarizing the data
recig_final %>%
  group_by(SEX.x) %>%
  group_by(did.relapse) %>%
  count() %>%
  ungroup()

recig_final %>%
  group_by(ETHNICITY) %>%
  count() %>%
  ungroup()

recig_final %>%
  group_by(slips) %>%
  count() %>%
  ungroup()

hist(recig_final$slips)
plot(recig_final_nona$rbias_2minus1, recig_final_nona$slips)

#testing correlation
#cor.test(recig_analyze$bias, recig_analyze$slips)

#testing correlation when accounting for sex and age covariates
x <- lm(slips ~ bias + sex + age, data = recig_final)
results(x)
report(x)

#finalizing data
recig_final_nona <- recig_final %>%
  subset(!is.na(slips))

#subset(qc_status == "pass")
#subset(SUBJECT.ID == "subID")

#make groups based off of variable
recig_final_nona %>%
  filter(slips == 0) %>%
  mutate(SLIPPED = "NO")

#inner_join by = c("subject", "session")
#take out only the numbers of subject ids
recig_data <- recig_data %>%   
  mutate(subject = as.numeric(str_extract(SUBJECT.ID, "\\d+")))

#remove said rows from test
recig_final <- test[, -c(83, 82, 81, 80, 79, 78, 77, 76, 75, 74)]

#create new column that is average of two other columns
recig_sigdetSlipsHDDM$total_reward <- (recig_sigdetSlipsHDDM$b1_disc + recig_sigdetSlipsHDDM$b2_disc)/2

