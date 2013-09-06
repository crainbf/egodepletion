# Actual analysis
source('clean.r')

library(scales)
library(depmixS4)
library(extrafont)
library(reshape2)
library(lme4)
library(zoo)


# ================= 4.3.1. Preprocessing =================
# ----------------- STROOP TASK -----------------
# Experiment Duration in minutes
cw_duration <- ddply(cw_trials_f, .(session_id), summarise, duration = sum(trial_duration) / 1000 / 60)


# ----------------- 2-BACK -----------------
# Experiment Duration in minutes
bb_duration <- ddply(bb_trials_f, .(session_id), summarise, duration = sum(trial_duration) / 1000 / 60)

# ================= 4.3.2. Accuracy and Reaction Time Changes =================
# ----------------- STROOP TASK -----------------
# ANOVA: reaction time on switch, rule and concordant
cw_aov_432_1 <- aov(reaction_time ~ rule + switch + concordant, data=cw_trials_f)
drop1(cw_lm_432_1, ~., test="F")  # Use marginal effects
# ANOVA: reaction time for correct/incorrect trials
aov(reaction_time ~ correct, data=cw_trials_f)

# BINOMIAL: ACCURACY by TRIAL VARIATIONS
cw_glm_432_1 <- glmer(correct ~ difficulty + (1|session_id), data=cw_trials_f, family="binomial")

# Regression of reaction time on difficulty AND trial number
cw_aov_432_2 <- aov(reaction_time ~ difficulty + trial_number, data=cw_trials_f)

# BINOMIAL: Accuracy by trial number
cw_glm_432_2 <- glmer(correct ~ difficulty + trial_number + (1|session_id), data=cw_trials_f, family="binomial")

# ----------------- 2-BACK -----------------
# ANOVA: Reaction time over time
bb_aov_432_1 <- aov(reaction_time ~ trial_number, data=bb_trials_f)
# ANOVA: Reaction time for correct/incorrect trials
bb_aov_432_2 <- aov(reaction_time ~ correct, data=bb_trials_f)

# BINOMIAL: Accuracy by trial number
bb_glm_432_2 <- glmer(correct ~ trial_number + (1|session_id), data=bb_trials_f, family="binomial")


# ----------------- Figure 1 & 2 -----------------
# These are two side-by-side plots of reaction time by trial number for each experiment
# STROOP
# Get mean over all participants for reaction time and accuracy for each trial
A <- ddply(cw_trials_f, .(trial_number, difficulty), summarise, mean_rt = mean(reaction_time, na.rm=TRUE), mean_acc = sum(correct=="yes")/length(correct))
A <- melt(A, id=1:2)
levels(A$variable)[levels(A$variable)=="mean_rt"] <- "Reaction Time"
levels(A$variable)[levels(A$variable)=="mean_acc"] <- "Accuracy"
A <- cbind(trial_number = A[,1], experiment=c("Stroop Task"), A[,2:4])

# 2-BACK
B <- ddply(bb_trials_f, .(trial_number), summarise, mean_rt = mean(reaction_time, na.rm=TRUE), mean_acc = sum(correct=="yes")/length(correct))
B <- melt(B, id=1)
levels(B$variable)[levels(B$variable)=="mean_rt"] <- "Reaction Time"
levels(B$variable)[levels(B$variable)=="mean_acc"] <- "Accuracy"
B <- cbind(trial_number = B[,1], experiment="2-Back Task", difficulty="none", B[,2:3])

# Create a data.frame with values for both experiments
AB <- rbind(A,B)
colnames(AB)[3] <- "Difficulty"

# Plot for Reaction Time
ggplot(subset(AB, variable=="Reaction Time"), aes(x=trial_number, y=value, col=Difficulty)) + facet_wrap(~experiment) + geom_smooth() + ggtitle("Reaction Time") + ylab("ms") + xlab("Trial Number") + scale_x_continuous(breaks=seq(0,900,by=150)) + scale_colour_discrete(labels=c("Easy", "Medium", "Hard")) + theme_few() + theme(text = element_text(size=12), title=element_text(size=17), strip.text = element_text(size=rel(1.5)))
# Plot for Accuracy
ggplot(subset(AB, variable=="Accuracy"), aes(x=trial_number, y=value, col=Difficulty)) + facet_wrap(~experiment) + geom_smooth() + ggtitle("Accuracy") + ylab("") + xlab("Trial Number") + scale_x_continuous(breaks=seq(0,900,by=150)) + scale_y_continuous(labels=percent_format()) + scale_colour_discrete(labels=c("Easy", "Medium", "Hard")) + theme_few() + theme(text = element_text(size=12), title=element_text(size=17), strip.text = element_text(size=rel(1.5)))
# ================= 4.3.3. Response Variability =================
# ----------------- STROOP TASK -----------------
# Reaction Time and Accuracy Variability 
C <- ddply(cw_trials_f, .(session_id), function(x) transform(x, sd_rt = rollapply(x$reaction_time, 50, sd, fill=NA, na.rm=TRUE, align="right"), sd_acc = rollapply(x$correct=="yes", 50, sd, fill=NA, na.rm=TRUE, align="right")))
aov(sd_rt ~ difficulty, data=C)

# Aggregate Values
C1 <- ddply(C, .(trial_number), summarise, mean_sd_rt = mean(sd_rt, na.rm=TRUE), mean_sd_acc = mean(sd_acc, na.rm=TRUE))

# ANOVA: Reaction Time Variability ~ Trial Number
aov(C$sd_rt ~ C$trial_number)


# ----------------- 2-BACK -----------------
# Reaction Time and Accuracy Variability 
D <- ddply(bb_trials_f, .(session_id), function(x) transform(x, sd_rt = rollapply(x$reaction_time, 50, sd, fill=NA, na.rm=TRUE, align="right"), sd_acc = rollapply(x$correct=="yes", 50, sd, fill=NA, na.rm=TRUE, align="right")))
# Aggregate Values
D1 <- ddply(D, .(trial_number), summarise, mean_sd_rt = mean(sd_rt, na.rm=TRUE), mean_sd_acc = mean(sd_acc, na.rm=TRUE))


D <- melt(D, id=1)
levels(D$variable)[levels(D$variable)=="sd_rt"] <- "Reaction Time"
levels(D$variable)[levels(D$variable)=="sd_acc"] <- "Accuracy"

# ----------------- Figure 3 & 4 -----------------
# These are two side-by-side plots of reaction time by trial number for each experiment
# STROOP
# Get mean over all participants for reaction time and accuracy for each trial
C1 <- melt(C1, id=1)
levels(C1$variable)[levels(C1$variable)=="mean_sd_rt"] <- "Reaction Time"
levels(C1$variable)[levels(C1$variable)=="mean_sd_acc"] <- "Accuracy"
C1 <- cbind(trial_number = C1[,1], experiment=c("Stroop Task"), C1[,2:3])

# 2-BACK
D1 <- melt(D1, id=1)
levels(D1$variable)[levels(D1$variable)=="mean_sd_rt"] <- "Reaction Time"
levels(D1$variable)[levels(D1$variable)=="mean_sd_acc"] <- "Accuracy"
D1 <- cbind(trial_number = D1[,1], experiment=c("2-Back Task"), D1[,2:3])

# Combine the data frames for both experiments
CD <- rbind(C1, D1)
colnames(CD)[2] <- "Experiment"

# Make the plots
# Plot for Reaction Time
ggplot(subset(CD, variable=="Reaction Time"), aes(x=trial_number, y=value)) + facet_wrap(~Experiment) + geom_smooth() + ggtitle("Variability: Reaction Time") + ylab("ms") + xlab("Trial Number") + scale_x_continuous(breaks=seq(0,900,by=150)) + scale_colour_discrete(labels=c("Easy", "Medium", "Hard")) + theme_few() + theme(text = element_text(size=12), title=element_text(size=17), strip.text = element_text(size=rel(1.5)))
# Plot for Accuracy
ggplot(subset(CD, variable=="Accuracy"), aes(x=trial_number, y=value)) + facet_wrap(~Experiment) + geom_smooth() + ggtitle("Variability: Accuracy") + xlab("Trial Number") + scale_x_continuous(breaks=seq(0,900,by=150)) + scale_colour_discrete(labels=c("Easy", "Medium", "Hard")) + theme_few() + theme(text = element_text(size=12), title=element_text(size=17), strip.text = element_text(size=rel(1.5)))

# OR if we want to do them in one plot...
ggplot(subset(CD, variable=="Accuracy"), aes(x=trial_number, y=value, col=Experiment)) + geom_smooth() + ggtitle("Variability: Accuracy") + xlab("Trial Number") + ylab("")+ scale_x_continuous(breaks=seq(0,900,by=150)) + theme_few() + theme(text = element_text(size=12), title=element_text(size=17), strip.text = element_text(size=rel(1.5)))
# Could also do them easily side by side for accuracy and RT
ggplot(CD, aes(x=trial_number, y=value, col=Experiment)) + facet_wrap(~variable, scales="free_y") + geom_smooth() + ggtitle("Response Variability") + xlab("Trial Number") + ylab("")+ scale_x_continuous(breaks=seq(0,900,by=150)) + theme_few() + theme(text = element_text(size=12), title=element_text(size=17), strip.text = element_text(size=rel(1.5)))

# ================= 4.3.4. Error Monitoring =================
# ----------------- STROOP TASK -----------------
# ANOVA: post-error effect
cw_lm_434_1 <- aov(reaction_time ~ post_error + difficulty, data=cw_trials_f)
drop1(cw_lm_434_1, ~., test="F")

# ANOVA: post-error ~ trial number
cw_lm_434_2 <- aov(reaction_time ~ post_error + difficulty + trial_number, data=cw_trials_f)

# ----------------- 2-BACK -----------------
# ANOVA: post-error slowdown ~ trial number
bb_lm_434_1 <- aov(reaction_time ~ post_error + trial_number, data=bb_trials_f)
# Check if post-error slowdown is affected by trial number
bb_lm_434_2 <- aov(reaction_time ~ post_error + trial_number + trial_number * post_error, data=bb_trials_f)


ggplot(subset(cw_trials_f, post_error=="yes"), aes(x=trial_number, y=post_error_slowing)) + geom_smooth() + xlab("Trial Number") + scale_x_continuous(breaks=seq(0,900,by=150)) + ylab("Post Error Slowing")

lm(cw_trials_f$post_error_slowing ~ cw_trials_f$trial_number)

# ----------------- PLOTS -----------------
# POST ERROR SLOWING PLOT
ggplot(subset(bb_trials_f, post_error=="yes"), aes(x=trial_number, y=post_error_slowing)) + geom_smooth() + xlab("Trial Number") + scale_x_continuous(breaks=seq(0,800,by=200)) + ylab("Post Error Slowing")
# Regression of post-error-slowing on trial_number. 
lm(bb_trials_f$post_error_slowing ~ bb_trials_f$trial_number)

# ================= 4.3.5. Motivation =================
# ----------------- STROOP TASK -----------------
# For 5s+ trials
glm(outlier == "high" ~ trial_number, data=cw_trials_f, family="binomial")

# For <100ms trials
glm(outlier == "low" ~ trial_number, data=cw_trials_f, family="binomial")



# ----------------- 2-BACK -----------------
# For 5s+ trials
glm(!is.na(outlier == "high") ~ trial_number, data=bb_trials_f, family="binomial")

# For <100ms trials
glm(!is.na(outlier == "low") ~ trial_number, data=bb_trials_f, family="binomial")


# ================= 4.3.7. Stroop Effects =================
# ----------------- STROOP TASK -----------------



# ----------------- DESCRIPTIVE RESULTS -----------------




# ----------------- Reaction Time & Accuracy Variability -----------------









# ================= COMPARING MODELS =================
# Reaction Time Variability
# Get reaction time variability into data.frame
# Get standard deviations of difficult Stroop Trials from above
W_dif <- subset(W, difficulty=="Hard")
colnames(W_dif)[3:4] <- c("reaction_time", "accuracy")
W_dif <- cbind(W_dif[,1], Experiment="Stroop", W_dif[,3:4])
W2 <- melt(W_dif, id=1:2)

colnames(V)[2:3] <- c("reaction_time", "accuracy")
V <- cbind(V[,1], Experiment="2-Back", V[,2:3])
V <- melt(V, id=1:2)
colnames(V)[1] <- "trial_number"
colnames(W2)[1] <- "trial_number"

# Data.frame with all the variability data for both experiments
VW <- rbind(V,W2)
levels(VW$variable)[levels(VW$variable)=="reaction_time"] <- "Reaction Time"
levels(VW$variable)[levels(VW$variable)=="accuracy"] <- "Accuracy"

# Excellent Plot with variability for reaction time and accuracy
ggplot(VW, aes(trial_number, y=value, col=Experiment)) + facet_wrap(~variable, scales="free_y") + geom_smooth()

# Do a 2nd degree polynomial regression of StDev reaction time on trial number / NOTE SOME COEFFICIENTS NOT SIGNIFICANT
lm(W_dif$sd_rt ~ W_dif$trial_number + I(W_dif$trial_number^2))

# Standard deviations of 2-Back from above: V and V2
# Make one unitary data.frame (Discarding first 100 trials of Stroop Task)
Q <- cbind(1:800, W_dif[101:900,2:4], V[,2:3])
colnames(Q) <- c("Trial Number", "Difficulty", "Stroop_RT_SD", "Stroop_AC_SD", "2-Back_RT_SD", "2-Back_AC_SD")


