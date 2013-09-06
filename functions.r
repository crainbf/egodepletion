# All functions used

assign.difficulty <- function(trials) {
    trials[trials[,"concordant"] == "concordant", "difficulty"] <- "easy"
    trials[trials[,"concordant"] == "discordant" & trials[,"switch"] == "no", "difficulty"] <- "medium"
    trials[trials[,"concordant"] == "discordant" & trials[,"switch"] == "yes", "difficulty"] <- "hard"
    trials[,"difficulty"] <- factor(trials[,"difficulty"], levels=c("easy","medium","hard"), labels=c("Easy", "Medium", "Hard"))
    trials
}

# ================= HMM FUNCTIONS =================

hmm.bb.2state <- function(trials) {
    # This function runs a 2-state HMM for all participants and returns a data.frame 
    # with the reaction time and accuracy for each state & participant 
    # (Excludes first 100 trials)
    L <- data.frame()
    i <- 1
    # Run a loop through all participants
    for (participant in unique(trials$session_id)){
        set.seed(1)
        hmm <- fit(depmix(list(reaction_time ~ 1, correct ~ 1), data=subset(trials, session_id==participant & trial_number > 100), family=list(gaussian(), multinomial('identity')), nstates=2), verbose=F)
        parameters <- getpars(hmm)
        L[i, "Participant"] <- participant
        L[i, "State"] <- 1
        L[i, "Accuracy"] <- parameters[10]
        L[i, "Reaction Time"] <- parameters[7]

        L[i+1, "Participant"] <- participant
        L[i+1, "State"] <- 2
        L[i+1, "Accuracy"] <- parameters[14]
        L[i+1, "Reaction Time"] <- parameters[11]

        i <- i + 2
    # Return data.frame with results
    }
    L 
}

hmm.bb.3state <- function(trials) {
    # This function runs a 3-state HMM for all participants and returns a data.frame 
    # with the reaction time and accuracy for each state & participant 
    # (Excludes first 100 trials)
    L <- data.frame()
    i <- 1
    for (participant in unique(bb_trials_f$session_id)){
        set.seed(1)
        hmm <- fit(depmix(list(reaction_time ~ 1, correct ~ 1), data=subset(bb_trials_f, session_id==participant & trial_number > 100), family=list(gaussian(), multinomial('identity')), nstates=3), verbose=F)
        parameters <- getpars(hmm)
        L[i, "Participant"] <- participant
        L[i, "State"] <- 1
        L[i, "Accuracy"] <- parameters[16]
        L[i, "Reaction Time"] <- parameters[13]

        L[i+1, "Participant"] <- participant
        L[i+1, "State"] <- 2
        L[i+1, "Accuracy"] <- parameters[20]
        L[i+1, "Reaction Time"] <- parameters[17]

        L[i+2, "Participant"] <- participant
        L[i+2, "State"] <- 3
        L[i+2, "Accuracy"] <- parameters[24]
        L[i+2, "Reaction Time"] <- parameters[21]    

        i <- i + 3
    }
    L
}

hmm.bb.model.comparison <- function(trials){
    # This function runs HMMs for all participants and for models with 1 up to 6 states
    # A data.frame is returned with the resulting BICs for each model and participant
    # NOTE: Takes very long to run 30+ minutes [start time ~8.15pm]
    L <- data.frame()
    i <- 1
    # Loop through all participants
    for (participant in unique(trials$session_id)){
        L[i,1] <- participant
        for (k in 1:6){
            set.seed(1)
            hmm <- fit(depmix(list(reaction_time ~ 1, correct ~ 1), data=subset(trials, session_id==participant), family=list(gaussian(), multinomial('identity')), nstates=k), verbose=F)
            L[i, k+1] <- BIC(hmm)
        }
        i <- i + 1
    }
    # Return data.frame with results
    L
}

# ================= GGPLOT2: THEME =================
theme.nr <- theme(
    text=element_text(family="Lato Light", size=14),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_line(colour="#ECECEC", size=0.5, linetype=1),
    axis.ticks.y=element_blank(),
    panel.background=element_blank(),
    legend.title=element_blank(),
    legend.key=element_rect(fill="white"),
    #legend.key.size=unit(1.5, "cm"),
    legend.text=element_text(size=22),
    axis.title=element_text(size=24),
    axis.text=element_text(color="black",size=13)
)



