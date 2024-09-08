
library(ggplot2)
library(ggpubr)

# create tables for summary
rb1_table <- recig_final %>%
  dplyr::select(subject, b1_rbias, b1_disc) %>%
  mutate(rbias = b1_rbias, disc = b1_disc, block = "Block 1") %>%
  dplyr::select(subject, rbias, disc, block)

rb2_table <- recig_final %>%
  dplyr::select(subject, b2_rbias, b2_disc) %>%
  mutate(rbias = b2_rbias, disc = b2_disc, block = "Block 2") %>%
  dplyr::select(subject, rbias, disc, block)

rb_table <- rbind(rb1_table, rb2_table)
rb_table$rbias
rbias_wide <- rb_table %>%
  pivot_wider(id_cols = subject, names_from = block, values_from = rbias)
t.test(rbias_wide$`Block 1`, rbias_wide$`Block 2`)

rb_table_sum <- rb_table %>%
  group_by(block) %>% #block, or stim tympe or both 
  summarise(
     meanRbias = mean(rbias),
     meanDisc = mean(disc),
     sdRbias = sd(rbias),
     sdDisc = sd(disc),
     seRbias = (sd(rbias))/(sqrt(34)),
     seDisc = (sd(disc))/(sqrt(34))
     )

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



RT_table_sum <- leanrich %>%
  group_by(block, stimtype) %>% #block, or stim tympe or both 
  summarise(
     meanRT = mean(RT),
     meanACC = mean(ACC),
     sdRT = sd(RT),
     sdACC = sd(ACC),
     seRT = (sd(RT))/(sqrt(34)),
     seACC = (sd(ACC))/(sqrt(34)))
  
RT_table_sum$xAxis <- c("Block 1 Lean", "Block 1 Rich", "Block 2 Lean", "Block 2 Rich")


# graph the response bias bar plot
ggplot(rb_table_sum, aes(x=block, y=meanRbias,
                                  fill=block)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("Block 1" = "#FDF0D5", "Block 2" = "#C1121F")) +
  #labs(y = "DMN-DAN Anti-Correlation (z)", x = "Drug Condition ", title = "DMN-DAN Correlation by Drug", size = 8) +
  #coord_cartesian(ylim = c(-2.5, -1)) +
  geom_errorbar(aes(ymin=meanRbias - seRbias, ymax=meanRbias + seRbias, width =.2)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(size=6,  family="sans"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

#rb_bar + geom_jitter(data = rb_table, aes(block, rbias), shape = 21, alpha = .5, size =.6, stroke = .1,
          #                              position = position_jitterdodge(jitter.height = .1, jitter.width = .3, dodge.width = .9))


# graph the discriminability bar plot
ggplot(rb_table_sum, aes(x=block, y=meanDisc,
                         fill=block)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("Block 1" = "#FDF0D5", "Block 2" = "#C1121F")) +
  #labs(y = "DMN-DAN Anti-Correlation (z)", x = "Drug Condition ", title = "DMN-DAN Correlation by Drug", size = 8) +
  #coord_cartesian(ylim = c(-2.5, -1)) +
  geom_errorbar(aes(ymin=meanDisc - seDisc, ymax=meanDisc + seDisc, width =.2)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(size=6,  family="sans"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

#rb_bar + geom_jitter(data = rb_table, aes(block, rbias), shape = 21, alpha = .5, size =.6, stroke = .1,
#                                       position = position_jitterdodge(jitter.height = .1, jitter.width = .3, dodge.width = .9))

# graph the reaction time bar plot
ggplot(RT_table_sum, aes(x=xAxis, y=meanRT,
                         fill=xAxis)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("Block 1 Lean" = "navajowhite", "Block 1 Rich" = "lightgoldenrod1", "Block 2 Lean" = "rosybrown1", "Block 2 Rich" = "palevioletred1")) +
  #labs(y = "DMN-DAN Anti-Correlation (z)", x = "Drug Condition ", title = "DMN-DAN Correlation by Drug", size = 8) +
  coord_cartesian(ylim = c(350, 550)) +
  geom_errorbar(aes(ymin=meanRT - seRT, ymax=meanRT + seRT, width =.2)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(size=6,  family="sans"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

#rb_bar + geom_jitter(data = rb_table, aes(block, rbias), shape = 21, alpha = .5, size =.6, stroke = .1,
#                                       position = position_jitterdodge(jitter.height = .1, jitter.width = .3, dodge.width = .9))



# Plot for comparing discriminability and threshold
ggscatter(recig_sigdetSlipsHDDM, x = "disc_avg", y = "mean_a",
          add = "reg.line", #this adds the line
          size = 3,
          color = "#C1121F",
          alpha = .7) +
  stat_cor(method = "pearson", size = 7, family = "sans",
           label.y=1.65, label.x.npc = "left") + #this adds stats of your correlation
  coord_cartesian(ylim = c(.7, 1.75)) + #CHANGE THIS to your y limits or just comment out
  geom_smooth(method="lm", color = "black", size=0.3) + #this adds smoothing along the line to see variability
  # scale_color_manual(values = c("Cyan4", "thistle3", "darkorange3")) + #I had 3 catecgories for my color, so these 3 colors lined up with that
  ylab("threshold") + #remember units
  xlab("discriminability") + #remember units
  ggtitle("") +
  theme(plot.title = element_text(hjust = .5)) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        text=element_text(size=6,  family="sans"),
        axis.text.x = element_text(size = 10), #you can play around with these things, just keep size, color, font etc. consistent
        axis.text.y = element_text(size = 10))

# Plot for comparing response bias and starting point bias
#min(recig_sigdetSlipsHDDM$mean_v)
ggscatter(recig_sigdetSlipsHDDM, x = "rb_avg", y = "mean_z",
          add = "reg.line", #this adds the line
          size = 3,
          color = "#C1121F",
          alpha = .7) +
  stat_cor(method = "pearson", size = 7, family = "sans",
           label.y=.68, label.x.npc = "left") + #this adds stats of your correlation
  coord_cartesian(ylim = c(0.35, .7)) + #CHANGE THIS to your y limits or just comment out
  geom_smooth(method="lm", color = "black", size=0.3) + #this adds smoothing along the line to see variability
  # scale_color_manual(values = c("Cyan4", "thistle3", "darkorange3")) + #I had 3 catecgories for my color, so these 3 colors lined up with that
  ylab("sp bias") + #remember units
  xlab("response bias") + #remember units
  ggtitle("") +
  theme(plot.title = element_text(hjust = .5)) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        text=element_text(size=6,  family="sans"),
        axis.text.x = element_text(size = 10), #you can play around with these things, just keep size, color, font etc. consistent
        axis.text.y = element_text(size = 10))

recig_sigdetSlipsHDDM$color <- ifelse(recig_sigdetSlipsHDDM$slips == 0, "blue", "orange")

#Plot for comparing slips and drift rate
ggscatter(recig_sigdetSlipsHDDM, x = "slips", y = "mean_v",
          add = "none", #this adds the line
          size = 3,
          color = "color",
          alpha = .7) +
  stat_cor(method = "pearson", size = 7, family = "sans",
           label.y=2.7, label.x.npc = "left") + #this adds stats of your correlation
  coord_cartesian(ylim = c(0.2, 3)) + #CHANGE THIS to your y limits or just comment out
  geom_smooth(method="lm", color = "black", size=.8) + #this adds smoothing along the line to see variability
  # scale_color_manual(values = c("Cyan4", "thistle3", "darkorange3")) + #I had 3 catecgories for my color, so these 3 colors lined up with that
  ylab("drift rate") + #remember units
  xlab("slips") + #remember units
  ggtitle("") +
  theme(plot.title = element_text(hjust = .5)) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        text=element_text(size=10,  family="sans"),
        axis.text.x = element_text(size = 10), #you can play around with these things, just keep size, color, font etc. consistent
        axis.text.y = element_text(size = 10)) +
  scale_color_manual(values = c("blue", "orange"))

