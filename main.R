# =============================================================================
# University of St.Gallen
# Course: Master's Thesis
# Author: Helena Aebersold
# Supervisors: Thomas Epper (FGN HSG), Rafael Polania (BLU UZH) 
# Date: 10.03.2017
# =============================================================================


# =============================================================================
# SCRIPT FILE
# - This file contains the working code
#   . it sources the functions file (functions.R)
#   . it sources the model comparison file (comparison.R)
# =============================================================================


###############################################################################
# -----------------------------------------------------------------------------
# 0. Preliminaries
# -----------------------------------------------------------------------------
# Clear environment
rm(list=ls())

# Set working directory
dir <- getwd()
setwd(dir)

# Source function file (from wd)
source("./functions.R")

# Used packages in this analaysis
packageList <- c("ggplot2", "plyr", "xtable", "reshape2", "gridExtra", "MASS",
                 "R.matlab", "dplyr", "lme4", "corrplot", "Rmisc", "mfx",
                 "optimx", "rms", "truncnorm", "bbmle", "tibble"
)
# Install and/or load all needed packages
invisible(GetPackages(packageList))

# set path of study
pathStudy = paste(dir, "/RawData/", sep = "")




###############################################################################
# -----------------------------------------------------------------------------
# 1. Load data, create matrix containing:
#   rating info: ratingsMat
#   choice info: choicesMat
# -----------------------------------------------------------------------------

# names of subjects (individual folder containing all files of subject)
subjects = c(
  "01", "02", "03", "04", "05", "06", "07", "08", "09", #"10" -> problems with data in subject 10
  "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
  "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
  "31", "32", "33", "34", "35"
)
ns = length(subjects)

# prepare matrices
ratingsMat = NULL
choicesMat = NULL
ratingsTab = NULL

# create choicesMat and ratingsTab containing all choice and rating info of all subjects
for (s in 1:ns) {
  subjectID = subjects[s] # set subject ID
  
  # get all rating info into one matrix (all subjects in one)
  for (i in 1:8) { # we had 8 rounds, 2 x 4 rating blocks in first part of experiment
    pathname <- file.path(paste(pathStudy,subjectID,"/ratingRunInfo",i,".mat",sep=""))
    aux = readMat(pathname)
    ratingsMat = rbind(ratingsMat, cbind(aux$runInfo,rep(s,dim(aux$runInfo)[1])))
  }
  
  # get all choice info, make a separate matrix for each subject (choicesMat_aux), add to one big matrix at end (choicesMat)  
  choicesMat_aux = NULL
  for (i in 1:4) { # we had 4 rounds in second part of experiment
    pathname <- file.path(paste(pathStudy,subjectID,"/choiceRunInfo",i,".mat",sep=""))
    aux = readMat(pathname)
    choicesMat_aux = rbind(choicesMat_aux, cbind(aux$runInfo,rep(s,dim(aux$runInfo)[1])))
  }
  
  choicesMat_aux = as.data.frame(choicesMat_aux) # rename choice matrix with variable names
  colnames(choicesMat_aux) = c(
    "IUp", # image number up
    "upVal", # value of image up (short scale)
    "IDown", # image number down
    "downVal", # value of image down (short scale)
    "VD", # value difference (upVal - downVal) --> negative VD == lower image was valued more
    "CorrectUp", # correct image to choose according to rating is up (== 1), else == 0 (correct image is down)
    "ISIti", # interstimulus interval time initialised (when did the fixation cross appear)
    "ISIt", # interstimulus interval time (how long was the fixation cross shown on screen)
    "Choiceti", # choice time initialised (when did the images appear)
    "Choicet", # choice time (how long the image is displayed maximal, in milliseconds)
    "ChosenI", # chosen image, 78 == upper image, 28 == lower image
    "Correct", # if correct image chosen == 1, else == 0
    "RT", # reaction time in milliseconds
    "runNr", # choice round number (1 to 4)
    "Confti", # moment when confidence rating is initialised
    "RTconf", # reaction time to state confidence in milliseconds
    "Conf", # confidence rating (values from 0 to 922 [pixels])
    "iniConf", # initial place of mouse on confidence scale (to get a cleaner measure of RT with eye tracking)
    "subject" # subject number
  )
  
  # add information on repetitions (0 = was never repeated, 1 = appeared the first time, 2 = appeared the second time)
  rep_com <- repQuestion(choicesMat_aux)
  choicesMat_aux$Chosenrep <- rep_com[, 3]
  choicesMat_aux <- cbind(choicesMat_aux, rep = rep_com[, 1], com = rep_com[, 2])
  
  
  # get ratings and calculate mean and std, add all to one matrix
  pathname <- file.path(paste(pathStudy,subjectID,"/mValueRatings.mat",sep=""))
  aux = readMat(pathname)
  aux = aux$mValueRatings
  aux = as.data.frame(aux)
  aux$V3 = apply(aux[,1:2],1,mean)
  aux$V4 = apply(aux[,1:2],1,sd)
  aux$V5 = 1:(dim(aux)[1])
  aux$V6 = s
  aux = as.data.frame(aux)
  colnames(aux) = c(
    "r1", # value of first rating
    "r2", # value of second rating
    "mr", # mean of the two ratings
    "sdr", # standard deviation of the two ratings
    "figID", # image number
    "subject" # subject number
  )
  
  
  ratingsTab = rbind(ratingsTab, aux)
  
  
  # add rating infos of upper and lower images to choicesMat_aux
  idxP = mapvalues(choicesMat_aux$IUp, 1:64, to=aux$r1, warn_missing = FALSE) # replace the image number with its rating value in the choicesMat_aux
  choicesMat_aux$r1Up = idxP # add the rating values to choicesMat_aux naming it r1Up (first rating of the upper image)
  
  idxP = mapvalues(choicesMat_aux$IUp, 1:64, to=aux$r2, warn_missing = FALSE) 
  choicesMat_aux$r2Up = idxP
  
  idxP = mapvalues(choicesMat_aux$IUp, 1:64, to=aux$sdr, warn_missing = FALSE) 
  choicesMat_aux$sdrUp = idxP
  
  idxP = mapvalues(choicesMat_aux$IUp, 1:64, to=aux$mr, warn_missing = FALSE) 
  choicesMat_aux$mrUp = idxP
  
  # down images
  idxP = mapvalues(choicesMat_aux$IDown, 1:64, to=aux$r1, warn_missing = FALSE) 
  choicesMat_aux$r1Down = idxP
  
  idxP = mapvalues(choicesMat_aux$IDown, 1:64, to=aux$r2, warn_missing = FALSE) 
  choicesMat_aux$r2Down = idxP
  
  idxP = mapvalues(choicesMat_aux$IDown, 1:64, to=aux$sdr, warn_missing = FALSE) 
  choicesMat_aux$sdrDown = idxP
  
  idxP = mapvalues(choicesMat_aux$IDown, 1:64, to=aux$mr, warn_missing = FALSE)
  choicesMat_aux$mrDown = idxP
  
  # add value differences (image up - image down) in absolute terms to choicesMat_aux
  choicesMat_aux$absVD.r1 = abs(choicesMat_aux$r1Up - choicesMat_aux$r1Down) # given first rating
  choicesMat_aux$absVD.r2 = abs(choicesMat_aux$r2Up - choicesMat_aux$r2Down) # given second rating
  choicesMat_aux$absVD.m = abs(choicesMat_aux$mrUp - choicesMat_aux$mrDown) # given mean of ratings
  # value differences (image up - image down)
  choicesMat_aux$VD.r1 = choicesMat_aux$r1Up - choicesMat_aux$r1Down # given first rating
  choicesMat_aux$VD.r2 = choicesMat_aux$r2Up - choicesMat_aux$r2Down # given second rating
  choicesMat_aux$VD.m = choicesMat_aux$mrUp - choicesMat_aux$mrDown # given mean of ratings
  
  
  choicesMat = rbind(choicesMat, choicesMat_aux) # combine all individual choicesMat_aux to one big matrix called choicesMat
}

# rename the matrix containing all rating infos
ratingsMat = as.data.frame(ratingsMat)
colnames(ratingsMat) = c(
  "runNr", # rating round number (8 rounds in total)
  "ISI.ti", # interstimulus interval time initialised
  "ISI.t", # interstimulus interval time
  "I.ti", # image time initialised
  "I.t", # image time (how long is image shown on the screen, in milliseconds)
  "INr",  # image number
  "wait.ti", # wait time initialised
  "wait.t", # wait time (how long until rating scale is shown after image is gone, in milliseconds)
  "rate.ti", # rate time initialised (when is the rating scale initialised)
  "RT", # reaction time in milliseconds
  "Val", # rating value given by the subject (0 = don't like it at all, 900 = like it very much)
  "posIni", # initial position of mouse on rating scale
  "subject" # subject number
)


choicesMat$absVD = abs(choicesMat$VD) # add absolute value difference 
choicesMat$sdrAll = sqrt((choicesMat$sdrUp)^2 + (choicesMat$sdrDown)^2) # measure of total variation



# data set with all questions where a decision was made
dataChoice <- choicesMat[which(choicesMat$ChosenI %in% c(78, 28)), !(colnames(choicesMat) %in% c("Chosenrep"))]
# data set with all possible change of mind (COM) decisions
dataCOM <- choicesMat[which(choicesMat$Chosenrep %in% c(78, 28)), !(colnames(choicesMat) %in% c("Chosenrep"))]


# Definition of correct decision
# define correctly chosen, given first and second rating
correctdataChoice <- correctAnswers(dataChoice)
dataChoice <- cbind(dataChoice, Correct.r1 = correctdataChoice[, 1], Correct.r2 = correctdataChoice[, 2])
correctdataCOM <- correctAnswers(dataCOM)
dataCOM <- cbind(dataCOM, Correct.r1 = correctdataCOM[, 1], Correct.r2 = correctdataCOM[, 2])


# add total value of question (both values summed)
dataChoice$mValtot <- dataChoice$mrUp + dataChoice$mrDown
dataCOM$mValtot <- dataCOM$mrUp + dataCOM$mrDown






###############################################################################
# -----------------------------------------------------------------------------
# 2. preparing data
#     standardize, add first repetition values to repeated questions
# -----------------------------------------------------------------------------

####
# standardize data
####
# scale the selected variables in dataChoice and dataCOM
zVariables <- c("upVal",
                "downVal",
                "VD",
                "RT",
                "RTconf",
                "Conf",
                "r1Up", "r2Up", "sdrUp", "mrUp",
                "r1Down", "r2Down", "sdrDown", "mrDown",
                "absVD.r1", "absVD.r2", "absVD.m",
                "VD.r1", "VD.r2", "VD.m", "absVD",
                "sdrAll",
                "mValtot")
dataChoice_z <- setNames(as.data.frame(
  apply(dataChoice[which(colnames(dataChoice) %in% zVariables)],
        2, scale)),
  paste(zVariables, "z", sep = "."))
dataChoice <- cbind(dataChoice, dataChoice_z)

dataCOM_z <- setNames(as.data.frame(
  apply(dataCOM[which(colnames(dataCOM) %in% zVariables)],
        2, scale)),
  paste(zVariables, "z", sep = "."))
dataCOM <- cbind(dataCOM, dataCOM_z)



####
# values from first round, which should be added to second round information
dataChoice <- firstRoundAnswers(dataChoice, c("Conf", "RT", "Conf.z", "RT.z"))
dataCOM <- firstRoundAnswers(dataCOM, c("Conf", "RT", "Conf.z", "RT.z"))

#### chose the upper image
# chose up (= 1) or down (= 0)
dataChoice$Up <- as.numeric(dataChoice$ChosenI == 78)







###############################################################################
# -----------------------------------------------------------------------------
# 3. descriptive statistics
# -----------------------------------------------------------------------------

###########################################
# 3.1 Rating 
###########################################
# summary of data
summary(ratingsMat)

# different histograms
#   RT
ggplot(data = ratingsMat, aes(RT)) + geom_histogram() 
#   Ratings Value
ggplot(data = ratingsMat, aes(Val)) + geom_histogram() 
# check correlation between RT and Value
cor(ratingsMat$RT, ratingsMat$Val) 


# pdf with histogram of rating for each subject
indivpdf(ratingsMat, "Val")


#################
# 3.1.1 inconsistency
#################
# add rating differences to each image
ratingsMat <- ratingdiff(ratingsMat)
ratingsDiff <- na.omit(ratingsMat) 

#####
# plots
#####
# pdf for histograms of different variables and for each individual, including a N and kernel density
indivpdf(ratingsDiff, "VD")
indivpdf(ratingsDiff, "VD.z")
indivpdf(ratingsDiff, "absVD")
indivpdf(ratingsDiff, "absVD.z")

# all individual normal put in one plot (VD)
ratingsPlot <- ratingN.oneplot(ratingsDiff, "absVD")


# rating difference histogram over all individuals
ggplot(ratingsDiff, aes(absVD)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(ratingsDiff$absVD), sd = sd(ratingsDiff$absVD)), 
                lwd = 0.5, 
                col = 'red') + # add normal distribution with sample mean and sample sd
  geom_density(col = 'blue') + # add kernel density 
  ggtitle(paste("overall ratings diff"))


# calculte the mean VD of every individual
mean_allVD <- c()
for(i in 1:ns){
  individ <- subset(ratingsDiff, ratingsDiff$subject == i)
  mean_individ <- mean(individ$absVD)
  mean_allVD <- c(mean_allVD, mean_individ)
}
hist(mean_allVD, breaks = 10)
mean_allVD
# add mean_allVD of each subject to data frame of choice and com
meanVD_com <- meanVD_choice <- meanVD.z_com <- meanVD.z_choice <- c()
for(i in 1:ns){
  individ_com <- subset(dataCOM, dataCOM$subject == i)
  individ_choice <- subset(dataChoice, dataChoice$subject == i)
  meanVD_com_indiv <- rep(mean_allVD[i], dim(individ_com)[[1]])
  meanVD_choice_indiv <- rep(mean_allVD[i], dim(individ_choice)[[1]])
  meanVD.z_com_indiv <- rep(scale(mean_allVD)[i], dim(individ_com)[[1]])
  meanVD.z_choice_indiv <- rep(scale(mean_allVD)[i], dim(individ_choice)[[1]])
  meanVD_com <- c(meanVD_com, meanVD_com_indiv)
  meanVD_choice <- c(meanVD_choice, meanVD_choice_indiv)
  meanVD.z_com <- c(meanVD.z_com, meanVD.z_com_indiv)
  meanVD.z_choice <- c(meanVD.z_choice, meanVD.z_choice_indiv)
}
dataCOM$meanVD <- meanVD_com
dataChoice$meanVD <- meanVD_choice
dataCOM$meanVD.z <- meanVD.z_com
dataChoice$meanVD.z <- meanVD.z_choice


# rating changes from round 1 to round 2 in % for each subject
percent_changes_individualRating <- ratingChange(ratingsDiff)



###########################################
# 3.2 Choice Part
###########################################
# summary of data
summary(dataChoice)


# how many change of minds (define it as have changed decision (regardless if correct or not) in both repetitions of the same image set)
per_changemind <- table(dataCOM$com)[["1"]] / length(which(dataCOM$rep == 2)) * 100

# % change of mind for each subject 
changemind_all <- c()
for(i in 1:ns){
  individ <- subset(dataCOM, dataCOM$subject == i)
  changemind_indiv <- table(individ$com)[["1"]] / length(which(individ$rep == 2)) * 100
  changemind_all <- c(changemind_all, changemind_indiv)
}

# how many % of all questions were answered 
# correct according to first rating
per_corr1 <- sum(dataChoice$Correct.r1 == 1) / dim(dataChoice)[1] * 100
# correct according to second rating
per_corr2 <- sum(dataChoice$Correct.r2 == 1) / dim(dataChoice)[1] * 100
# wrong according to first rating
per_wrong1 <- sum(dataChoice$Correct.r1 == 0) / dim(dataChoice)[1] * 100
# wrong according to second rating
per_wrong2 <- sum(dataChoice$Correct.r2 == 0) / dim(dataChoice)[1] * 100
# correct / wrong in the mean
per_mean_correct <- mean(c(per_corr1, per_corr2))
per_mean_wrong <- mean(c(per_wrong1, per_wrong2))



# distributions -> histograms
grid.arrange(ggplot(data = dataChoice, aes(RT)) + geom_histogram(), 
             ggplot(data = dataChoice, aes(Conf)) + geom_histogram(),
             ggplot(data = dataChoice, aes(absVD.m)) + geom_histogram(),
             ggplot(data = dataChoice, aes(sdrAll)) + geom_histogram(), 
             ggplot(data = dataChoice, aes(mValtot)) + geom_histogram(), 
             ncol = 1, nrow = 5)


# histogram of confidence for each subject (print into a PDF) --> check how they used scale, ev. we have to remove some subjects
pdf("indivConf.pdf")
for(i in 1:ns){
  plot <- ggplot(data = dataChoice[which(dataChoice$subject %in% i), ], aes(Conf)) + 
    geom_histogram() +  
    theme_bw() + 
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank() )+
    theme(panel.border= element_blank())+
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black")) +
    
    ggtitle(paste("subject", as.character(i)))
  print(plot)
}
dev.off()
# !!! result: remove subjects 12 and 29 for confidence informations!!!!
rm_conf_subjects <- c(12, 29)



# correlation matrix (correlogram)
# data which make sense for correlation, then plot correlation 
data_corr <- dataCOM[, c("Correct", "RT", "Conf", "absVD.m", "mValtot", "sdrAll")]
colnames(data_corr) <- c("Correct", "RT", "Conf", "absVD", "SV", "SD")
corrplot(cor(data_corr), method = "color")
corrplot(cor(data_corr), method="color",tl.cex=0.7, tl.col = "black",  
         cl.lim=c(-1,1), col=colorRampPalette(c("red","white","blue"))(200))
corrplot.mixed(cor(data_corr), upper = "color")
# correlation matrix with significance test:
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  print(p.mat)
  return(list(p.mat, lowCI.mat, uppCI.mat))
}
res1 <- cor.mtest(data_corr,0.95)
res2 <- cor.mtest(data_corr,0.99)
## specialized the insignificant value according to the significant level
corrplot(cor(data_corr), p.mat = res1[[1]], sig.level=0.2)




# median split difficulty of questions --> difficult = small absVD.m, easy = higher absVD.m
summary(dataChoice$absVD.m)
easy <- subset(dataChoice, absVD.m >= median(dataChoice$absVD.m))
hard <- subset(dataChoice, absVD.m < median(dataChoice$absVD.m))
# how many correct in these two subsets?
easy_corr1 <- sum(easy$Correct.r1 == 1) / dim(easy)[1] * 100
easy_corr2 <- sum(easy$Correct.r2 == 1) / dim(easy)[1] * 100
easy_wrong1 <- sum(easy$Correct.r1 == 0) / dim(easy)[1] * 100
easy_wrong2 <- sum(easy$Correct.r2 == 0) / dim(easy)[1] * 100
easy_mean_correct <- mean(c(easy_corr1, easy_corr2)) # with easy questions ~ 70% correct
easy_mean_wrong <- mean(c(easy_wrong1, easy_wrong2))
hard_corr1 <- sum(hard$Correct.r1 == 1) / dim(hard)[1] * 100
hard_corr2 <- sum(hard$Correct.r2 == 1) / dim(hard)[1] * 100
hard_wrong1 <- sum(hard$Correct.r1 == 0) / dim(hard)[1] * 100
hard_wrong2 <- sum(hard$Correct.r2 == 0) / dim(hard)[1] * 100
hard_mean_correct <- mean(c(hard_corr1, hard_corr2)) # with difficult questions ~ 60% correct
hard_mean_wrong <- mean(c(hard_wrong1, hard_wrong2))




#### average correct responses as function of ..., binned 
# absVD.m, all data
av_cor_absVDm_all_4 <- binned_mean(dataChoice, "absVD.m", 4) 
av_cor_absVDm_all_2 <- binned_mean(dataChoice, "absVD.m", 2) 
# absVD.m, COM data
av_cor_absVD_com_4 <- binned_mean(dataCOM, "absVD.m", 4) 
av_cor_absVD_com_2 <- binned_mean(dataCOM, "absVD.m", 2) 

# RT, all data
av_corr_rt_all_4 <- binned_mean(dataChoice, "RT", 4)
av_corr_rt_all_2 <- binned_mean(dataChoice, "RT", 2)
# RT, COM data
av_corr_rt_com_4 <- binned_mean(dataCOM, "RT", 4)
av_corr_rt_com_2 <- binned_mean(dataCOM, "RT", 2)

# Conf, all data
av_corr_conf_all_4 <- binned_mean(dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],  
                                  "Conf", 4)
av_corr_conf_all_2 <- binned_mean(dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],  
                                  "Conf", 2)
# Conf, COM data
av_corr_conf_com_4 <- binned_mean(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ],
                                  "Conf", 4)
av_corr_conf_com_2 <- binned_mean(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ],
                                  "Conf", 2)

# Conf.rep1, COM data
av_corr_conf1_com_4 <- binned_mean(na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                                   "Conf.rep1", 4)
av_corr_conf1_com_2 <- binned_mean(na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                                   "Conf.rep1", 2)

# --> very small difference between the two data sets, almost none






# plot absVD and conf for correct answers and incorrect ones (remove rm_conf_subjects for conf)
conf_abs_correct <- cbind(binned_mean_conf(subset(dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], Correct == 1),
                                           "absVD.m", 4),
                          group = rep("correct", 4))
conf_abs_wrong <- cbind(binned_mean_conf(subset(dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], Correct == 0),
                                         "absVD.m", 4),
                        group = rep("wrong", 4))
conf_abs_all <- cbind(binned_mean_conf(dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],
                                       "absVD.m", 4),
                      group = rep("all", 4))
conf_abs <- rbind(conf_abs_all, conf_abs_correct, conf_abs_wrong)
conf_abs_plot <- ggplot(conf_abs, aes(x = tile, y = acc2, linetype = group)) + geom_line() +     
  theme_bw() + 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black")) +
  labs(y = "Conf", x = "bin")







###############################################################################
# -----------------------------------------------------------------------------
# 4. Ranking effect on choice
#   for each individual and overall with GLMER
# -----------------------------------------------------------------------------


# Up ~ VD.m.z + RT.z + mValtot.z + Conf.z + (VD.m.z * mValtot.z) + (VD.m.z * Conf.z)
#   logit regression for each individual:
up_glm <- LogitMarginEffect(Up ~ VD.m.z + RT.z + mValtot.z + Conf.z + (VD.m.z * mValtot.z) + (VD.m.z * Conf.z), dataChoice, Conf = T)
up_glm_logit <- as.data.frame(up_glm[1])
up_glm_marginal <- as.data.frame(up_glm[2])
#   generalized linear mixed effects model for overall:
#     remove mValtot.z in random effects as it leads to negative Eigenvalues in Hessian due to too less variation
up_glmer <- glmer(Up ~ VD.m.z + RT.z + mValtot.z + Conf.z + (mValtot.z * VD.m.z) +  (VD.m.z * Conf.z) +
                    (1 + VD.m.z + RT.z + Conf.z + (mValtot.z * VD.m.z) +  (VD.m.z * Conf.z) | subject),
                  data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], # data without the removed ones for conf analysis
                  family = binomial(link = "logit"),
                  control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
up_glmer_marginal <- glmermfx(up_glmer) # get marginal effects
summary(up_glmer)
#   plot estimtes
up_glmer_plot <- marginal.effects.plot(up_glmer, up_glmer_marginal,
                                       c("Intercept", "VD", "RT", "SV", "Conf", "VD x SV", "VD x Conf"), y = "Choice")
#   generate table for latex with regression results
up_estimates <- as.data.frame(estimate.table(up_glm_marginal, c("VD.m.z", "RT.z", "mValtot.z", "Conf.z", "VD.m.z.mValtot.z", "VD.m.z.Conf.z")))
colnames(up_estimates) <- c("{subject}", "{VD}", "{RT}", "{SV}", "{Conf}", "{VD x sv}", "{VD x Conf}")

up_glmer_estimates <- glmerestimate.table(up_glmer, up_glmer_marginal)
colnames(up_glmer_estimates) <- c("{Intercept}", "{VD}", "{RT}", "{SV}", "{Conf}", "{VD x SV}", "{VD x Conf}")




# Correct ~ absVD.m.z + RT.z + mValtot.z + Conf.z
#   logit regression for each individual:
correct_glm <- LogitMarginEffect(Correct ~ absVD.m.z + RT.z + mValtot.z + Conf.z ,
                                 dataChoice, Conf = T)
correct_glm_logit <- as.data.frame(correct_glm[1])
correct_glm_marginal <- as.data.frame(correct_glm[2])
#   generalized linear mixed effects model for overall:
correct_glmer <- glmer(Correct ~ absVD.m.z + RT.z + mValtot.z + Conf.z + 
                         (1 + absVD.m.z + RT.z + Conf.z | subject),
                       data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], # data without the removed ones for conf analysis
                       family = binomial(link = "logit"),
                       control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
correct_glmer_marginal <- glmermfx(correct_glmer) # get marginal effects
summary(correct_glmer)
#   plot estimtes
correct_glmer_plot <- marginal.effects.plot(correct_glmer, correct_glmer_marginal,
                                            c("Intercept", "absVD", "RT", "SV", "Conf"), y = "Correct")
#   generate table for latex with regression results
correct_estimates <- as.data.frame(estimate.table(correct_glm_marginal, c("absVD.m.z", "RT.z", "mValtot.z", "Conf.z")))
colnames(correct_estimates) <- c("{subject}", "{absVD}", "{RT}", "{SV}", "{Conf}")

correct_glmer_estimates <- glmerestimate.table(correct_glmer, correct_glmer_marginal)
colnames(correct_glmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{Conf}")




# Correct ~ absVD.m.z + RT.z + mValtot.z + Conf.z + meanVD.z
#   logit regression for each individual doesn't work as we only have one meanVD.z per subject
#   generalized linear mixed effects model for overall (add only to fixed effects):
correct_meanVD_glmer <- glmer(Correct ~ absVD.m.z + RT.z + mValtot.z + Conf.z + meanVD.z +
                                (1 + absVD.m.z + RT.z + Conf.z | subject),
                              data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], # data without the removed ones for conf analysis
                              family = binomial(link = "logit"),
                              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
correct_meanVD_glmer_marginal <- glmermfx(correct_meanVD_glmer) # get marginal effects
summary(correct_meanVD_glmer)
#   plot estimtes
corect_meanVD_glmer_plot <- marginal.effects.plot(correct_meanVD_glmer, correct_meanVD_glmer_marginal,
                                                  c("Intercept", "absVD", "RT", "SV", "Conf", "mPD"), y = "Correct")
#   generate table for latex with regression results
correct_meanVDglmer_estimates <- glmerestimate.table(correct_meanVD_glmer, correct_meanVD_glmer_marginal)
colnames(correct_meanVDglmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{Conf}", "{mPD}")






# Correct ~ absVD.m.z + RT.z + mValtot.z + Conf.z + sdrAll.z
#   logit regression for each individual:
correct_sdrAll_glm <- LogitMarginEffect(Correct ~ absVD.m.z + RT.z + mValtot.z + Conf.z + sdrAll.z,
                                        dataChoice, Conf = T)
correct_sdrAll_glm_logit <- as.data.frame(correct_sdrAll_glm[1])
correct_sdrAll_glm_marginal <- as.data.frame(correct_sdrAll_glm[2])
#   generalized linear mixed effects model for overall:
correct_sdrAll_glmer <- glmer(Correct ~ absVD.m.z + RT.z + mValtot.z + Conf.z  + sdrAll.z + 
                                (1 + absVD.m.z + RT.z + Conf.z + sdrAll.z | subject),
                              data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], # data without the removed ones for conf analysis
                              family = binomial(link = "logit"),
                              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
correct_sdrAll_glmer_marginal <- glmermfx(correct_sdrAll_glmer) # get marginal effects
summary(correct_sdrAll_glmer)
#   plot estimtes
corect_sdrAll_glmer_plot <- marginal.effects.plot(correct_sdrAll_glmer, correct_sdrAll_glmer_marginal,
                                                  c("Intercept", "absVD", "RT", "SV", "Conf", "SD"), y = "Correct")
#   generate table for latex with regression results
correct_sdrAll_estimates <- as.data.frame(estimate.table(correct_sdrAll_glm_marginal, c("absVD.m.z", "RT.z", "mValtot.z", "Conf.z", "sdrAll.z")))
colnames(correct_sdrAll_estimates) <- c("{subject}", "{VD}", "{RT}", "{SV}", "{Conf}", "{SD}")

correct_sdrAllglmer_estimates <- glmerestimate.table(correct_sdrAll_glmer, correct_sdrAll_glmer_marginal)
colnames(correct_sdrAllglmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{Conf}", "{SD}")










###############################################################################
# -----------------------------------------------------------------------------
# 5. Standard choice behavior
#   with Generalized Linear Mixed-Effects Model (glmer)
#   to check data 
# -----------------------------------------------------------------------------
# correct ~ absVD.m + Conf
cor_abs_conf <- glmer(Correct ~ absVD.m.z + Conf.z + 
                        (1 + absVD.m.z + Conf.z | subject),
                      data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
                      family = binomial(link = "logit"),
                      control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
glmermfx(cor_abs_conf)
summary(cor_abs_conf)

# correct ~ absVD.m + sdrAll
cor_abs_sdr <- glmer(Correct ~ absVD.m.z + sdrAll.z + 
                       (1 + absVD.m.z + sdrAll.z | subject),
                     data = dataChoice, 
                     family = binomial(link = "logit"),
                     control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))


# correct ~ absVD.m + Conf + sdrAll
cor_abs_conf_sdr <- glmer(Correct ~ absVD.m.z + Conf.z + sdrAll.z +
                            (1 + absVD.m.z + Conf.z + sdrAll.z | subject),
                          data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
                          family = binomial(link = "logit"),
                          control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))


# Up ~ (mrUp - mrDown) * Conf
up_mr_conf <- glmer(Up ~ (mrUp.z - mrDown.z) * Conf.z +
                      (1 + (mrUp.z - mrDown.z) * Conf.z | subject),
                    data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
                    family = binomial(link = "logit"),
                    control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

# Up ~ (mrUp - mrDown) * sdrAll
up_mr_sdr <- glmer(Up ~ (mrUp.z - mrDown.z) * sdrAll.z +
                     (1 + (mrUp.z - mrDown.z) * sdrAll.z | subject),
                   data = dataChoice, 
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
summary(up_mr_conf_sdr)

# Up ~ (mrUp - mrDown) * Conf + (mrUp- mrDown) * sdrAll
up_mr_conf_sdr <- glmer(Up ~ (mrUp.z - mrDown.z) * Conf.z + (mrUp.z - mrDown.z) * sdrAll.z +
                          (1 + (mrUp.z - mrDown.z) * Conf.z + (mrUp.z - mrDown.z) * sdrAll.z | subject),
                        data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
                        family = binomial(link = "logit"),
                        control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
summary(up_mr_conf_sdr)





###############################################################################
# -----------------------------------------------------------------------------
# 6. Effects on confidence
# -----------------------------------------------------------------------------
# Conf.z ~ absVD.m.z + RT.z + mValtot.z
#   linear regression model for each individual
conf_lm <- conf.estimate(dataChoice, Conf.z ~ absVD.m.z + RT.z + mValtot.z)
#   linear mixed effects model
conf_lmer <- lmer(Conf.z ~ absVD.m.z + RT.z + mValtot.z +
                    (1 + absVD.m.z + RT.z + mValtot.z | subject),
                  data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ])
conf_lmer_estimates <- conf.lmer.estimate(conf_lmer)
#   generate table for latex 
conf_lm_table <- estimate.table(conf_lm, c("(Intercept)", "absVD.m.z", "RT.z", "mValtot.z"))
colnames(conf_lm_table) <- c("{subject}", "{Intercept}", "{absVD}", "{RT}", "{SV}")

conf_lmer_table <- lmerestimate.table(conf_lmer_estimates)
colnames(conf_lmer_table) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}")
#   plot marginal effects
conf_lmer_estimates_marginal <- as.matrix(conf_lmer_estimates[, 1:2])
colnames(conf_lmer_estimates_marginal) <- c("marginal.effects", "standard.error")
conf_lmer_plot <- marginal.effects.plot(conf_lmer_estimates, conf_lmer_estimates_marginal, 
                                        c("Intercept", "absVD", "RT", "SV"), y = "Confidence")



# Conf.z ~ absVD.m.z + RT.z + mValtot.z + meanVD.z
#   only lmer (as only one value of meanVD.z per subject)
conf_meanVD_lmer <- lmer(Conf.z ~ absVD.m.z + RT.z + mValtot.z + meanVD.z +
                           (1 + absVD.m.z + RT.z + mValtot.z | subject),
                         data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ])
conf_meanVD_lmer_estimates <- conf.lmer.estimate(conf_meanVD_lmer)
#   generate table for latex 
conf_meanVDlmer_table <- lmerestimate.table(conf_meanVD_lmer_estimates)
colnames(conf_meanVDlmer_table) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{mPD}")
#   plot marginal effects
conf_meanVD_lmer_estimates_marginal <- as.matrix(conf_meanVD_lmer_estimates[, 1:2])
colnames(conf_meanVD_lmer_estimates_marginal) <- c("marginal.effects", "standard.error")
conf_meanVD_lmer_plot <- marginal.effects.plot(conf_meanVD_lmer_estimates, conf_meanVD_lmer_estimates_marginal, 
                                               c("Intercept", "absVD", "RT", "SV", "mPD"), y = "Confidence")




# Conf.z ~ absVD.m.z + RT.z + mValtot.z + sdrAll.z
#   linear regression model for each individual
conf_sdrAll_lm <- conf.estimate(dataChoice, Conf.z ~ absVD.m.z + RT.z + mValtot.z + sdrAll.z)
#   linear mixed effects model
conf_sdrAll_lmer <- lmer(Conf.z ~ absVD.m.z + RT.z + mValtot.z + sdrAll.z +
                           (1 + absVD.m.z + RT.z + mValtot.z + sdrAll.z | subject),
                         data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ])
conf_sdrAll_lmer_estimates <- conf.lmer.estimate(conf_sdrAll_lmer)
#   generate table for latex 
conf_sdrAlllm_table <- estimate.table(conf_sdrAll_lm, c("(Intercept)", "absVD.m.z", "RT.z", "mValtot.z", "sdrAll.z"))
colnames(conf_sdrAlllm_table) <- c("{subject}", "{Intercept}", "{absVD}", "{RT}", "{SV}", "{SD}")

conf_sdrAlllmer_table <- lmerestimate.table(conf_sdrAll_lmer_estimates)
colnames(conf_sdrAlllmer_table) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{SD}")
#   plot marginal effects
conf_sdrAll_lmer_estimates_marginal <- as.matrix(conf_sdrAll_lmer_estimates[, 1:2])
colnames(conf_sdrAll_lmer_estimates_marginal) <- c("marginal.effects", "standard.error")
conf_sdrAll_lmer_plot <- marginal.effects.plot(conf_sdrAll_lmer_estimates, conf_sdrAll_lmer_estimates_marginal, 
                                               c("Intercept", "absVD", "RT", "SV", "SD"), y = "Confidence")






###############################################################################
# -----------------------------------------------------------------------------
# 7. Change of mind (COM) behavior
#   with Generalized Linear Mixed-Effects Model (glmer)
# -----------------------------------------------------------------------------
#   individual regressions don't work, because there are some subjects which have only 1 oder 2 com in all their data. there we get a p-value of 1 and have problems
#     thus only overall glmer regressions in this section


# how many % COM in each subject
per_com <- c()
for(i in 1:ns){
  individ <- subset(dataCOM, dataCOM$subject == i)
  per_com_individ <- table(individ$com)["1"] / length(na.omit(individ$com)) * 100
  per_com <- c(per_com, per_com_individ)
}



#####
# 7.1 Conf and sdrAll, meanVD
#####

# COM ~ absVD.m + RT1 + SV + Conf1
#   generalized linear mixed effects model for overall:
com_conf_glmer <- glmer(com ~ absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + 
                          (1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 | subject),
                        data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]), 
                        family = binomial(link = "logit"),
                        control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_conf_glmer_marginal <- glmermfx(com_conf_glmer)
summary(com_conf_glmer)
#   plot estimtes
com_conf_glmer_plot <- marginal.effects.plot(com_conf_glmer, com_conf_glmer_marginal,
                                             c("Intercept", "absVD", "RT", "SV", "Conf"), y = "COM")
#   generate table for latex with regression results
com_confglmer_estimates <- glmerestimate.table(com_conf_glmer, com_conf_glmer_marginal)
colnames(com_confglmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{Conf}")




# COM ~ absVD.m + RT1 + SV + sdrAll
#   generalized linear mixed effects model for overall:
com_sdrAll_glmer <- glmer(com ~ absVD.m.z + RT.z.rep1 + mValtot.z + sdrAll.z + 
                            (1 + absVD.m.z + RT.z.rep1 + mValtot.z + sdrAll.z | subject),
                          data = na.omit(dataCOM), 
                          family = binomial(link = "logit"),
                          control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_sdrAll_glmer_marginal <- glmermfx(com_sdrAll_glmer)
summary(com_sdrAll_glmer)
#   plot estimtes
com_sdrAll_glmer_plot <- marginal.effects.plot(com_sdrAll_glmer, com_sdrAll_glmer_marginal,
                                               c("Intercept", "absVD", "RT", "SV", "SD"), y = "COM")
#   generate table for latex with regression results
com_sdrAllglmer_estimates <- glmerestimate.table(com_sdrAll_glmer, com_sdrAll_glmer_marginal)
colnames(com_sdrAllglmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{SD}")





# COM ~ absVD.m + RT1 + SV + Conf + sdrAll
#   generalized linear mixed effects model for overall:
com_confsdrAll_glmer <- glmer(com ~ absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + sdrAll.z + 
                                (1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + sdrAll.z | subject),
                              data = na.omit(dataCOM), 
                              family = binomial(link = "logit"),
                              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_confsdrAll_glmer_marginal <- glmermfx(com_confsdrAll_glmer)
summary(com_confsdrAll_glmer)
#   plot estimtes
com_confsdrAll_glmer_plot <- marginal.effects.plot(com_confsdrAll_glmer, com_confsdrAll_glmer_marginal,
                                                   c("Intercept", "absVD", "RT", "SV", "Conf", "SD"), y = "COM")
#   generate table for latex with regression results
com_confsdrAllglmer_estimates <- glmerestimate.table(com_confsdrAll_glmer, com_confsdrAll_glmer_marginal)
colnames(com_confsdrAllglmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{Conf}", "{SD}")






# COM ~ absVD.m + RT1 + SV + meanVD 
#   generalized linear mixed effects model for overall:
com_meanVD_glmer <- glmer(com ~ absVD.m.z + RT.z.rep1 + mValtot.z + meanVD.z + 
                            (1 + absVD.m.z + RT.z.rep1 + mValtot.z | subject),
                          data = na.omit(dataCOM), 
                          family = binomial(link = "logit"),
                          control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_meanVD_glmer_marginal <- glmermfx(com_meanVD_glmer)
summary(com_meanVD_glmer)
#   plot estimtes
com_meanVD_glmer_plot <- marginal.effects.plot(com_meanVD_glmer, com_meanVD_glmer_marginal,
                                               c("Intercept", "absVD", "RT", "SV", "mPD"), y = "COM")
#   generate table for latex with regression results
com_meanVDglmer_estimates <- glmerestimate.table(com_meanVD_glmer, com_meanVD_glmer_marginal)
colnames(com_meanVDglmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{mPD}")





# COM ~ absVD.m + RT1 + SV + Conf + meanVD 
#   generalized linear mixed effects model for overall:
com_confmeanVD_glmer <- glmer(com ~ absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + meanVD.z + 
                                (1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 | subject),
                              data = na.omit(dataCOM), 
                              family = binomial(link = "logit"),
                              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_confmeanVD_glmer_marginal <- glmermfx(com_confmeanVD_glmer)
summary(com_confmeanVD_glmer)
#   plot estimtes
com_confmeanVD_glmer_plot <- marginal.effects.plot(com_confmeanVD_glmer, com_confmeanVD_glmer_marginal,
                                                   c("Intercept", "absVD", "RT", "SV", "Conf", "mPD"), y = "COM")
#   generate table for latex with regression results
com_confmeanVDglmer_estimates <- glmerestimate.table(com_confmeanVD_glmer, com_confmeanVD_glmer_marginal)
colnames(com_confmeanVDglmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{Conf}", "{mPD}")






#####
# 7.2 probability of choosing correct item as covariate
#####

#####
# 7.2.1 simple model: constant noise (Normal distribution) accross scale
#####

# prepare column to add to dataCOM
dataCOM$p.c.all <- NA
dataCOM$p.c.ind <- NA

# overall estimation for sigman
p_const_overall <- p_constant(subset(dataCOM, dataCOM$rep != 2),
                              start = 100) # start at about 10% of scale (0-922) 
dataCOM$p.c.all[1:length(p_const_overall[[1]])] <- p_const_overall[[1]] # add to dataCOM

# individual estimation
p_const_individ <- c()
aic_const_individ <- c()
sigma_const_ind <- c()
for(i in 1:ns){
  individ <- subset(dataCOM, dataCOM$subject == i)
  individrep1 <- subset(individ, individ$rep != 2)
  p <- p_constant(individrep1, start = 100)
  p_const_individ <- c(p_const_individ, p[[1]])
  aic_const_individ <- c(aic_const_individ, p[[2]])
  sigma_const_ind <- c(sigma_const_ind, p[[3]])
}
dataCOM$p.c.ind[1:length(p_const_individ)] <- p_const_individ




#####
# 7.2.2 extended model: efficient measure
#####

# prepare column to add to dataCOM
dataCOM$p.eff.all <- NA
dataCOM$p.eff.ind <- NA

# overall estimation for sigman
p_eff_overall <- p_efficient(subset(dataCOM, dataCOM$rep != 2), 
                             start.sigman = 0.1, # start at about 10% of scale (fitted 0-922 to 0-1)
                             start.mu.prior = 0,
                             start.sigma.prior = 1)
dataCOM$p.eff.all[1:length(p_eff_overall[[1]])] <- p_eff_overall[[1]]

# individual estimation
p_eff_individ <- c()
aic_eff_individ <- c()
mu_eff_indiv <- c()
sigma_eff_indiv <- c()
sigman_eff_indiv <- c()
for(i in 1:ns){
  individ <- subset(dataCOM, dataCOM$subject == i)
  individrep1 <- subset(individ, individ$rep != 2)
  p <- p_efficient(individrep1,
                   start.sigman = 0.1,
                   start.mu.prior = 0,
                   start.sigma.prior = 1)
  p_eff_individ <- c(p_eff_individ, p[[1]])
  aic_eff_individ <- c(aic_eff_individ, p[[2]])
  mu_eff_indiv <- c(mu_eff_indiv, p[[3]])
  sigma_eff_indiv <- c(sigma_eff_indiv, p[[4]])
  sigman_eff_indiv <- c(sigman_eff_indiv, p[[5]])
}
dataCOM$p.eff.ind[1:length(p_eff_individ)] <- p_eff_individ




# put all estimates of sigma; resp. mu, sigma and sigma_n into one table
sigma_constant <- as.data.frame(c(p_const_overall[[3]], sigma_const_ind))
sigma_constant <- cbind(c("{overall}", seq(1:34)), round(sigma_constant, 2))
colnames(sigma_constant) <- c("{subject}", "\\sigma")
estimates_const <- sigma_constant

mu_eff <- as.data.frame(c(p_eff_overall[[3]], mu_eff_indiv))
sigma_eff <- as.data.frame(c(p_eff_overall[[4]], sigma_eff_indiv))
sigman_eff <- as.data.frame(c(p_eff_overall[[5]], sigman_eff_indiv))
estimates_eff <- cbind(c("{overall}", seq(1:34)),
                       round(mu_eff, 2),
                       round(sigma_eff, 2),
                       round(sigman_eff, 2))
colnames(estimates_eff) <- c("{subject}", "\\mu", "\\sigma", "\\sigma_\\eta")


#####
# model comparison 7.2.1 and 7.2.2
#####
#   AIC --> smaller AIC is better, if difference > 10 then significantly better
# -> second model is better (the one with efficiency measure)
aic_overall <- as.data.frame(cbind("{constant}" = p_const_overall[[2]], "{efficient}" = p_eff_overall[[2]]))
aic_overall$`{difference}` <- aic_overall$`{constant}` - aic_overall$`{efficient}`
aic_individual <- as.data.frame(cbind("{constant}" = aic_const_individ, "{efficient}" = aic_eff_individ))
aic_individual$difference <- aic_individual$`{constant}` - aic_individual$`{efficient}`
aic_ind <- sapply(aic_individual, sum)
# put in one table (for latex)
aic_table <- rbind(aic_overall, aic_ind)
aic_table <- cbind("{estimation method}" = c("{overall}", "{individual}"), aic_table)




# scale probabilities
dataCOM$p.c.all.z <- scale(dataCOM$p.c.all)
dataCOM$p.c.ind.z <- scale(dataCOM$p.c.ind)
dataCOM$p.eff.all.z <- scale(dataCOM$p.eff.all)
dataCOM$p.eff.ind.z <- scale(dataCOM$p.eff.ind)

# add probabilities to second repetition 
dataCOM <- firstRoundAnswers(dataCOM, c("p.c.all", "p.c.ind", "p.eff.all", "p.eff.ind",
                                        "p.c.all.z", "p.c.ind.z", "p.eff.all.z", "p.eff.ind.z"))



#####
# 7.3 regressions with probabilities (p of Correct given two different models and each of them with two different calculation methods)
#####

#####
# 7.3.1 check working of probability data
#####

# COM ~ RT1 + Conf1
com_rtconf_glmer <- glmer(com ~ RT.z.rep1 + Conf.z.rep1 + 
                            (1 + RT.z.rep1 + Conf.z.rep1 | subject),
                          data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                          family = binomial(link = "logit"),
                          control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_rtconf_glmer_marginal <- glmermfx(com_rtconf_glmer)
summary(com_rtconf_glmer)

# COM ~ RT1 + p.c.all1
com_rtpcall_glmer <- glmer(com ~ RT.z.rep1 + p.c.all.z.rep1 +
                             (1 + RT.z.rep1 + p.c.all.z.rep1 | subject),
                           data = na.omit(dataCOM),
                           family = binomial(link = "logit"),
                           control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_rtpcall_glmer_marginal <- glmermfx(com_rtpcall_glmer)
summary(com_rtpcall_glmer)

# COM ~ RT1 + p.c.ind1
com_rtpcind_glmer <- glmer(com ~ RT.z.rep1 + p.c.ind.z.rep1 +
                             (1 + RT.z.rep1 + p.c.ind.z.rep1 | subject),
                           data = na.omit(dataCOM),
                           family = binomial(link = "logit"),
                           control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_rtpcind_glmer_marginal <- glmermfx(com_rtpcind_glmer)
summary(com_rtpcind_glmer)

# COM ~ RT1 + p.eff.all1
com_rtpeffall_glmer <- glmer(com ~ RT.z.rep1 + p.eff.all.z.rep1 +
                               (1 + RT.z.rep1 + p.eff.all.z.rep1 | subject),
                             data = na.omit(dataCOM),
                             family = binomial(link = "logit"),
                             control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_rtpeffall_glmer_marginal <- glmermfx(com_rtpeffall_glmer)
summary(com_rtpeffall_glmer)

# COM ~ RT1 + p.eff.ind1
com_rtpeffind_glmer <- glmer(com ~ RT.z.rep1 + p.eff.ind.z.rep1 +
                               (1 + RT.z.rep1 + p.eff.ind.z.rep1 | subject),
                             data = na.omit(dataCOM),
                             family = binomial(link = "logit"),
                             control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_rtpeffind_glmer_marginal <- glmermfx(com_rtpeffind_glmer)
summary(com_rtpeffind_glmer)



#####
# 7.3.1 whole regression with probability instead of Conf or sdrAll
#####

# COM ~ absVD.m + RT1 + SV + p.c.all1 (p.c.all = probability Correct, given constant noise model, estimated over all trials)
com_p.c.all_glmer <- glmer(com ~ absVD.m.z + RT.z.rep1 + mValtot.z + p.c.all.z.rep1 + 
                             (1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.c.all.z.rep1 | subject),
                           data = na.omit(dataCOM),
                           family = binomial(link = "logit"),
                           control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_p.c.all_glmer_marginal <- glmermfx(com_p.c.all_glmer)
summary(com_p.c.all_glmer)
#   plot estimtes
com_pcall_glmer_plot <- marginal.effects.plot(com_p.c.all_glmer, com_p.c.all_glmer_marginal,
                                              c("Intercept", "absVD", "RT", "SV", "Pc_all"), y = "COM")
#   generate table for latex with regression results
com_pcall_glmer_estimates <- glmerestimate.table(com_p.c.all_glmer, com_p.c.all_glmer_marginal)
colnames(com_pcall_glmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{p}")






# COM ~ absVD.m + RT1 + SV + p.c.ind1 (p.c.ind = probability Correct, given constant noise model, estimated within subjects)
com_p.c.ind_glmer <- glmer(com ~ absVD.m.z + RT.z.rep1 + mValtot.z + p.c.ind.z.rep1 + 
                             (1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.c.ind.z.rep1 | subject),
                           data = na.omit(dataCOM),
                           family = binomial(link = "logit"),
                           control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_p.c.ind_glmer_marginal <- glmermfx(com_p.c.ind_glmer)
summary(com_p.c.ind_glmer)
#   plot estimtes
com_pcind_glmer_plot <- marginal.effects.plot(com_p.c.ind_glmer, com_p.c.ind_glmer_marginal,
                                              c("Intercept", "absVD", "RT", "SV", "Pc_ind"), y = "COM")
#   generate table for latex with regression results
com_pcind_glmer_estimates <- glmerestimate.table(com_p.c.ind_glmer, com_p.c.ind_glmer_marginal)
colnames(com_pcind_glmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{p}")










# COM ~ absVD.m + RT1 + SV + p.eff.all1 (p.eff.all = probability Correct, given efficient measure model, estimated over all trials)
com_p.eff.all_glmer <- glmer(com ~ absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.all.z.rep1 + 
                               (1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.all.z.rep1 | subject),
                             data = na.omit(dataCOM),
                             family = binomial(link = "logit"),
                             control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_p.eff.all_glmer_marginal <- glmermfx(com_p.eff.all_glmer)
summary(com_p.eff.all_glmer)
#   plot estimtes
com_peffall_glmer_plot <- marginal.effects.plot(com_p.eff.all_glmer, com_p.eff.all_glmer_marginal,
                                                c("Intercept", "absVD", "RT", "SV", "Pe_all"), y = "COM")
#   generate table for latex with regression results
com_peffall_glmer_estimates <- glmerestimate.table(com_p.eff.all_glmer, com_p.eff.all_glmer_marginal)
colnames(com_peffall_glmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{p}")





# COM ~ absVD.m + RT1 + SV + p.eff.ind1 (p.eff.ind = probability Correct, given efficient measure model, estimated within subjects)
com_p.eff.ind_glmer <- glmer(com ~ absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.ind.z.rep1 + 
                               (1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.ind.z.rep1 | subject),
                             data = na.omit(dataCOM),
                             family = binomial(link = "logit"),
                             control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_p.eff.ind_glmer_marginal <- glmermfx(com_p.eff.ind_glmer)
summary(com_p.eff.ind_glmer)
#   plot estimtes
com_peffind_glmer_plot <- marginal.effects.plot(com_p.eff.ind_glmer, com_p.eff.ind_glmer_marginal,
                                                c("Intercept", "absVD", "RT", "SV", "Pe_ind"), y = "COM")
#   generate table for latex with regression results
com_peffind_glmer_estimates <- glmerestimate.table(com_p.eff.ind_glmer, com_p.eff.ind_glmer_marginal)
colnames(com_peffind_glmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{p}")





#####
# 7.3.2 whole regression with probability AND conf
#####

# COM ~ absVD.m + RT1 + SV + Conf1 + p.c.all1 (p.c.all = probability Correct, given constant noise model, estimated over all trials)
com_confp.c.all_glmer <- glmer(com ~ absVD.m.z + RT.z.rep1 + mValtot.z + p.c.all.z.rep1 + Conf.z.rep1 + 
                                 (1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.c.all.z.rep1 + Conf.z.rep1 | subject),
                               data = dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ],
                               family = binomial(link = "logit"),
                               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_confp.c.all_glmer_marginal <- glmermfx(com_confp.c.all_glmer)
summary(com_confp.c.all_glmer)
#   plot estimtes
com_confpcall_glmer_plot <- marginal.effects.plot(com_confp.c.all_glmer, com_confp.c.all_glmer_marginal,
                                                  c("Intercept", "absVD", "RT", "SV", "Pc_all", "Conf"), y = "COM")
#   generate table for latex with regression results
com_confpcall_glmer_estimates <- glmerestimate.table(com_confp.c.all_glmer, com_confp.c.all_glmer_marginal)
colnames(com_confpcall_glmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{p}", "{Conf}")






# COM ~ absVD.m + RT1 + SV + Conf1 + p.c.ind1 (p.c.ind = probability Correct, given constant noise model, estimated within subjects)
com_confp.c.ind_glmer <- glmer(com ~ absVD.m.z + RT.z.rep1 + mValtot.z + p.c.ind.z.rep1 + Conf.z.rep1 + 
                                 (1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.c.ind.z.rep1 + Conf.z.rep1 | subject),
                               data = dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ],
                               family = binomial(link = "logit"),
                               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_confp.c.ind_glmer_marginal <- glmermfx(com_confp.c.ind_glmer)
summary(com_confp.c.ind_glmer)
#   plot estimtes
com_confpcind_glmer_plot <- marginal.effects.plot(com_confp.c.ind_glmer, com_confp.c.ind_glmer_marginal,
                                                  c("Intercept", "absVD", "RT", "SV", "Pc_ind", "Conf"), y = "COM")
#   generate table for latex with regression results
com_confpcind_glmer_estimates <- glmerestimate.table(com_confp.c.ind_glmer, com_confp.c.ind_glmer_marginal)
colnames(com_confpcind_glmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{p}", "{Conf}")





# COM ~ absVD.m + RT1 + SV + Conf1 + p.eff.all1 (p.eff.all = probability Correct, given efficient measure model, estimated over all trials)
com_confp.eff.all_glmer <- glmer(com ~ absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.all.z.rep1 + Conf.z.rep1 + 
                                   (1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.all.z.rep1 + Conf.z.rep1 | subject),
                                 data = dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ],
                                 family = binomial(link = "logit"),
                                 control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_confp.eff.all_glmer_marginal <- glmermfx(com_confp.eff.all_glmer)
summary(com_confp.eff.all_glmer)
#   plot estimtes
com_confpeffall_glmer_plot <- marginal.effects.plot(com_confp.eff.all_glmer, com_confp.eff.all_glmer_marginal,
                                                    c("Intercept", "absVD", "RT", "SV", "Pe_all", "Conf"), y = "COM")
#   generate table for latex with regression results
com_confpeffall_glmer_estimates <- glmerestimate.table(com_confp.eff.all_glmer, com_confp.eff.all_glmer_marginal)
colnames(com_confpeffall_glmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{p}", "{Conf}")





# COM ~ absVD.m + RT1 + SV + Conf1 + p.eff.ind1 (p.eff.ind = probability Correct, given efficient measure model, estimated within subjects)
com_confp.eff.ind_glmer <- glmer(com ~ absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.ind.z.rep1 + Conf.z.rep1 + 
                                   (1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.ind.z.rep1 + Conf.z.rep1 | subject),
                                 data = dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ],
                                 family = binomial(link = "logit"),
                                 control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
com_confp.eff.ind_glmer_marginal <- glmermfx(com_confp.eff.ind_glmer)
summary(com_confp.eff.ind_glmer)
#   plot estimtes
com_confpeffind_glmer_plot <- marginal.effects.plot(com_confp.eff.ind_glmer, com_confp.eff.ind_glmer_marginal,
                                                    c("Intercept", "absVD", "RT", "SV", "Pe_ind", "Conf"), y = "COM")
#   generate table for latex with regression results
com_confpeffind_glmer_estimates <- glmerestimate.table(com_confp.eff.ind_glmer, com_confp.eff.ind_glmer_marginal)
colnames(com_confpeffind_glmer_estimates) <- c("{Intercept}", "{absVD}", "{RT}", "{SV}", "{p}", "{Conf}")








###############################################################################
# -----------------------------------------------------------------------------
# 8. Model Comparison
# -----------------------------------------------------------------------------
# Source comparison file (from wd)
source("./comparison.R")







###############################################################################
# -----------------------------------------------------------------------------
# save environment for further work (not to run everything each time)
#   . load it with load('myEnvironment.RData')
# -----------------------------------------------------------------------------
save.image(file = 'myEnvironment.RData')


