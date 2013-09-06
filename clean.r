library(plyr)
library(ggplot2)
source('functions.r')


# ================= 1. LOAD DATA =================
bb_trials <- read.csv('2_back_test_20130723_trials_simple_n_back_sp2b-800.csv')
cw_trials <- read.csv('color_word_test2_20130723_trials_color_word_mixed_150_hard.csv')

# ================= 2. COMPLETE DATA =================
# REACTION TIME FOR INCORRECT TRIALS
bb_trials[is.na(bb_trials$reaction_time), "reaction_time"] <- bb_trials[is.na(bb_trials$reaction_time), "trial_duration"] - 500
cw_trials[is.na(cw_trials$reaction_time), "reaction_time"] <- cw_trials[is.na(cw_trials$reaction_time), "trial_duration"] - 500

# STROOP: ADD DIFFICULTY
cw_trials <- assign.difficulty(cw_trials)

# ----------------- Post Error Slowing -----------------
# STROOP
cw_trials[2:38700, "post_error"] <- ifelse(cw_trials[1:38699, "correct"]=="no", "yes", "no")
cw_trials[cw_trials[,"trial_number"]==1, "post_error"] <- "no"
cw_trials[2:38700,"post_error_slowing"] <- ifelse(cw_trials[2:38700,"post_error"]=="yes", cw_trials[2:38700,"reaction_time"] - cw_trials[1:38699,"reaction_time"], 0 )

# 2-BACK
bb_trials[2:32800, "post_error"] <- ifelse(bb_trials[1:32799, "correct"]=="no", "yes", "no")
bb_trials[bb_trials[,"trial_number"]==1, "post_error"] <- "no"
bb_trials[2:32800,"post_error_slowing"] <- ifelse(bb_trials[2:32800,"post_error"]=="yes", bb_trials[2:32800,"reaction_time"] - bb_trials[1:32799,"reaction_time"], 0)

# ----------------- Mark Outlying Trials -----------------
# Stroop
cw_trials$outlier <- "no"  # set default to 'no' to avoid NA nightmares
cw_trials[cw_trials$reaction_time < 100,"outlier"] <- "low"
cw_trials[cw_trials$reaction_time > 5000,"outlier"] <- "high"

# 2-Back
bb_trials$outlier <- "no"
bb_trials[bb_trials$reaction_time < 100,"outlier"] <- "low"
bb_trials[bb_trials$reaction_time > 5000,"outlier"] <- "high"

# Stroop
cw_trials_f$outlier <- "no"  # set default to 'no' to avoid NA nightmares
cw_trials_f[cw_trials_f$trial_duration < 600,"outlier"] <- "low"
cw_trials_f[cw_trials_f$trial_duration > 5500,"outlier"] <- "high"

# 2-Back
bb_trials_f$outlier <- "no"
bb_trials_f[bb_trials_f$trial_duration < 600,"outlier"] <- "low"
bb_trials_f[bb_trials_f$trial_duration > 5600,"outlier"] <- "high"






# ================= 3. FILTER PARTICIPANTS =================
# 2-BACK
# ACCURACY - Get participants with accuracy is not above chance
bb_chance <- ddply(bb_trials, .(session_id), summarise, chance=pbinom(sum(correct=="yes"), 800, 0.5))
bb_excl_par_ac <-  bb_chance[bb_chance$chance<0.95, "session_id"]
# SESSION IDs: 11201156 11419032 11450082 11547112 11610064 11680005 11680009

# BREAKS - Get participants who took long breaks
bb_excl_par_break <- unique(bb_trials[bb_trials$reaction_time > 30000, "session_id"])
# SESSION IDs: 11403022 11417009 11450082 11595022 11201156 11609040

# RANDOM - Get participants who responded randomly (<100ms) for 10+ trials
bb_fast <- ddply(bb_trials, .(session_id), summarise, fast = sum(reaction_time < 100))
bb_excl_par_rand <- unique(bb_fast$session_id[bb_fast$fast > 10])
# SESSION IDs: 11201156 11357026 11403022 11417009 11610064 11643022 11672008 11680009

# Create excluded participant list. (A total of 14 participants)
bb_excl_par <- unique(c(bb_excl_par_ac, bb_excl_par_break, bb_excl_par_rand))
# Make filtered datasets - (27 participants in total)
bb_trials_f <- bb_trials[!(bb_trials$session_id %in% bb_excl_par),]


# ------------------------------------------------------
# STROOP
# ACCURACY - Get participants with accuracy is not above chance
cw_chance <- ddply(cw_trials, .(session_id), summarise, chance=pbinom(sum(correct=="yes"), 900, 1/3))
cw_excl_par_ac <- cw_chance[cw_chance$chance<0.95, "session_id"] 
# Get participants 3+ SD below mean accuracy
cw_mean_accuracy <- ddply(cw_trials, .(session_id), summarise, accuracy = sum(correct=="yes")/length(correct))
cutoff <- mean(cw_mean_accuracy[,"accuracy"]) - 3 * sd(cw_mean_accuracy[,"accuracy"])
cw_excl_par_ac <- c(cw_excl_par_ac, cw_mean_accuracy[cw_mean_accuracy$accuracy < cutoff, "session_id"])
# SESSION ID: 11234032

# BREAKS - Get participants who took long breaks
cw_excl_par_break <- unique(cw_trials[cw_trials$reaction_time > 30000, "session_id"])
# SESSION ID: 11678007 11320033 11656008 11385093 11610067 11238085 11461060

# RANDOM - Get participants who responded randomly (<100ms) for 10+ trials
cw_fast <- ddply(cw_trials, .(session_id), summarise, fast = sum(reaction_time < 100))
cw_excl_par_rand <- unique(cw_fast$session_id[cw_fast$fast > 10])
# SESSION ID: 11385093 11417006 11478025 11668044

# Create excluded participant list. (A total of 11 participants)
cw_excl_par <- unique(c(cw_excl_par_ac, cw_excl_par_break, cw_excl_par_rand))
# Make filtered datasets - (32 participants in total)
cw_trials_f <- cw_trials[!(cw_trials$session_id %in% cw_excl_par),]


# ================= 4. REMOVE OUTLIERS =================
# Set reaction times for <100ms trials and >5000ms trials to missing values
# 2-Back
bb_trials_f[!is.na(bb_trials_f$outlier =="low" | bb_trials_f$outlier == "high"), "reaction_time"] <- NA
cw_trials_f[!is.na(cw_trials_f$outlier =="low" | cw_trials_f$outlier == "high"), "reaction_time"] <- NA


# Set correct parameter for <100ms trials and >5000ms trials to missing values
bb_trials_f[!is.na(bb_trials_f$outlier =="low" | bb_trials_f$outlier == "high"), "correct"] <- NA
cw_trials_f[!is.na(cw_trials_f$outlier =="low" | cw_trials_f$outlier == "high"), "correct"] <- NA
