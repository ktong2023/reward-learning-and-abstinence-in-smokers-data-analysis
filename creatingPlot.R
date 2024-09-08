# histograms for recig metrics
hist(recig_sigdetSlipsHDDM$mean_z)
summary(recig_final$AGE.x)

hist(recig_final$EDUCATION)
summary(recig_final$EDUCATION)

hist(recig_final$AVG.CIG.DAY.x)
summary(recig_final$AVG.CIG.DAY.x)

hist(recig_final$YEARS.SMOKED.x)
summary(recig_final$YEARS.SMOKED.x)

recig_final %>%
  group_by(SEX.x) %>%
  count() %>%
  ungroup()

recig_final %>%
  group_by(RACE.x) %>%
  count() %>%
  ungroup()

# Correlations of sigdet vs. HDDM
plot(recig_sigdetSlipsHDDM$slips, recig_sigdetSlipsHDDM$mean_v)
cor.test(recig_sigdetSlipsHDDM$slips, recig_sigdetSlipsHDDM$mean_v)

plot(recig_sigdetSlipsHDDM$percent.slipped, recig_sigdetSlipsHDDM$mean_v)
cor.test(recig_sigdetSlipsHDDM$percent.slipped, recig_sigdetSlipsHDDM$mean_v)
plot(test$yearsmokedasnumeric, test$rbias_2minus1)
cor.test(test$yearsmokedasnumeric, test$rbias_2minus1)
plot(test$yearsmokedasnumeric, test$disc_avg)


test$YEARS.SMOKED

test$yearsmokedasnumeric <- as.numeric(test$YEARS.SMOKED)
test$yearsmokedasnumeric
test$agestartedsmokingasnumeric
x <- recig_sigdetSlipsHDDM %>%
  filter(slips > 0)
plot(x$percent.slipped, x$mean_v)
cor.test(x$percent.slipped, x$mean_v)


plot(recig_sigdetSlipsHDDM$b1_disc, recig_sigdetSlipsHDDM$mean_a)
cor.test(recig_sigdetSlipsHDDM$b1_disc, recig_sigdetSlipsHDDM$mean_a)

plot(recig_sigdetSlipsHDDM$b2_disc, recig_sigdetSlipsHDDM$mean_a)
cor.test(recig_sigdetSlipsHDDM$b2_disc, recig_sigdetSlipsHDDM$mean_a)
    
plot(recig_sigdetSlipsHDDM$rbias_2minus1, recig_sigdetSlipsHDDM$mean_z)
cor.test(recig_sigdetSlipsHDDM$b1_rbias, recig_sigdetSlipsHDDM$mean_z)
cor.test(recig_sigdetSlipsHDDM$b2_rbias, recig_sigdetSlipsHDDM$mean_z)
hist(test$irritability)
plot(test$difficulty.concentrating, test$rbias_2minus1)
test$Subject
pastdiagnosis$Subject
plot(test$Tiffany_Post_Total, test$rbias_2minus1)
pastdiagnosis_test <- test %>%
  inner_join(pastdiagnosis, by = "Subject")
pastdiagnosis_test %>%
  group_by(nonedepenceorabuse) %>%
  summarise(meanrb = mean(rbias_2minus1))

pastdiagnosis_test$pastorcurrentotherdrugdependence

pastdiagnosis_test %>%
  group_by(nonedepenceorabuse) %>%
  count()

ggplot()
boxplot(pastdiagnosis_test$rbias_2minus1, pastdiagnosis_test$pastorcurrentotherdrugdependence)

pastdiagnosis_test$histofdrugd <- as.factor(pastdiagnosis_test$pastorcurrentotherdrugdependence)
levels(pastdiagnosis_test$histofdrugd) <- c("none", "history of other drug use")


ggplot(pastdiagnosis_test, aes(x = nonedepenceorabuse, y = Tiffany_Post_Total)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue", color = "black", width = 0.6) +
  geom_jitter(width = 0.2, size = 2, aes(color = nonedepenceorabuse))


pastdiagnosis_test$rbias_2minus1
plot(test$Tiffany_Post_Total, test$mean_z)
plot(test$Tiffany_Post_Total, test$disc_avg)

cor.test(test$Tiffany_Post_Total, test$disc_avg)
plot(test$WSWS_Total, test$disc_avg)
recig_data$WSWS_11
test$Urge.to.smoketimesfour <- ((test$Urge.to.smoke)*4)


plot(test$Tiffany_Post_Total, test$mean_z)

cor.test(test$Tiffany_Post_Total, test$mean_z)
plot(test$Tiffany_Post_Total, test$mean_v)
cor.test(test$Tiffany_Post_Total, test$rbias_2minus1)
plot(test$Tiffany_Post_Total, test$b1_rbias)
plot(test$Tiffany_Post_Total, test$b2_rbias)

cor.test(test$Tiffany_Post_Total, test$b2_rbias)



# bar plots for sigdet variables
rb_mean_b1 = mean(recig_final$b1_rbias)
rb_mean_b2 = mean(recig_final$b2_rbias)
disc_mean_b1 = mean(recig_final$b1_disc)
disc_mean_b2 = mean(recig_final$b2_disc)
rt_mean_b1l = mean(recig_final$b1_lean_all_rt)
rt_mean_b1r = mean(recig_final$b1_rich_all_rt)
rt_mean_b2l = mean(recig_final$b2_lean_all_rt)
rt_mean_b2r = mean(recig_final$b2_rich_all_rt)
acc_mean_b1l = mean(recig_final$b1_lean_acc)
acc_mean_b1r = mean(recig_final$b1_rich_acc)
acc_mean_b2l = mean(recig_final$b2_lean_acc)
acc_mean_b2r = mean(recig_final$b2_rich_acc)

barplot(c(rb_mean_b1, rb_mean_b2), names.arg = c("rb_mean_b1", "rb_mean_b2"), col = c("red", "green"), 
        main = "Response Bias", xlab = "Blocks", ylab = "Response Bias")

barplot(c(disc_mean_b1, disc_mean_b2), names.arg = c("disc_mean_b1", "disc_mean_b2"), col = c("purple", "magenta"), 
        main = "Discriminability", xlab = "Blocks", ylab = "Discriminability")

barplot(c(rt_mean_b1l, rt_mean_b1r, rt_mean_b2l, rt_mean_b2r), names.arg = c("rt_mean_b1l", "rt_mean_b1r", "rt_mean_b2l", "rt_mean_b2r"), col = c("orange", "yellow", "green", "cyan"), 
        main = "Reaction Times", ylab = "Reaction Time")

barplot(c(acc_mean_b1l, acc_mean_b1r, acc_mean_b2l, acc_mean_b2r), names.arg = c("acc_mean_b1l", "acc_mean_b1r", "acc_mean_b2l", "acc_mean_b2r"), col = c("pink", "magenta", "purple", "blue"), 
        main = "Accuracy", ylab = "Accuracy")

# historgrams for PRT fMRI ITI times
setwd("C:/Users/tongk2/Downloads")
b1_cats <- read.csv("PRT_var_rich_cats_B1.csv")
b2_cats <- read.csv("PRT_var_rich_cats_B2.csv")
b3_cats <- read.csv("PRT_var_rich_cats_B3.csv")
b1_dogs <- read.csv("PRT_var_rich_dogs_B1.csv")
b2_dogs <- read.csv("PRT_var_rich_dogs_B2.csv")
b3_dogs <- read.csv("PRT_var_rich_dogs_B3.csv")

hist(b1_cats$ITI)
hist(b2_cats$ITI)
hist(b3_cats$ITI)
hist(b3_dogs$ITI)
hist(b2_dogs$ITI)
hist(b1_dogs$ITI)

# count the ITI durations
b1_cats%>%
  group_by(ITI) %>%
  count() %>%
  ungroup()
recig_final$SEX.x

b1lean <- recig_final %>%
  dplyr::select(subject, AGE.x, SEX.x, b1_lean_all_rt, b1_lean_acc) %>%
  mutate(RT = b1_lean_all_rt, ACC = b1_lean_acc, block = "one", stimtype = "lean") %>%
  dplyr::select(subject, AGE.x, SEX.x, RT, ACC, block, stimtype)

b1rich <- recig_final %>%
  dplyr::select(subject, AGE.x, SEX.x, b1_rich_all_rt, b1_rich_acc) %>%
  mutate(RT = b1_rich_all_rt, ACC = b1_rich_acc, block = "one", stimtype = "rich") %>%
  dplyr::select(subject, AGE.x, SEX.x, RT, ACC, block, stimtype)

b2lean <- recig_final %>%
  dplyr::select(subject, AGE.x, SEX.x, b2_lean_all_rt, b2_lean_acc) %>%
  mutate(RT = b2_lean_all_rt, ACC = b2_lean_acc, block = "two", stimtype = "lean") %>%
  dplyr::select(subject, AGE.x, SEX.x, RT, ACC, block, stimtype)

b2rich <- recig_final %>%
  dplyr::select(subject, AGE.x, SEX.x, b2_rich_all_rt, b2_rich_acc) %>%
  mutate(RT = b2_rich_all_rt, ACC = b2_rich_acc, block = "two", stimtype = "rich") %>%
  dplyr::select(subject, AGE.x, SEX.x, RT, ACC, block, stimtype)

leanrich <- bind_rows(b1lean, b1rich, b2lean, b2rich)

library(lmerTest)
library(lme4)

x <- lmer(RT ~ block*stimtype + (1|subject), data = leanrich)
anova(x)

x <- lmer(ACC ~ block*stimtype + (1|subject), data = leanrich)
anova(x)

leanrich %>%
  group_by(block, stimtype) %>%
  summarise(
    meanRT = mean(RT),
    meanACC = mean(ACC))
  )
  

rt_mean_b1l = mean(recig_final$b1_lean_all_rt)
rt_mean_b1r = mean(recig_final$b1_rich_all_rt)
rt_mean_b2l = mean(recig_final$b2_lean_all_rt)
rt_mean_b2r = mean(recig_final$b2_rich_all_rt)
acc_mean_b1l = mean(recig_final$b1_lean_acc)
acc_mean_b1r = mean(recig_final$b1_rich_acc)
acc_mean_b2l = mean(recig_final$b2_lean_acc)
acc_mean_b2r = mean(recig_final$b2_rich_acc)
