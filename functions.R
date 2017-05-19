# =============================================================================
# University of St.Gallen
# Course: Master's Thesis
# Author: Helena Aebersold
# Professors: Thomas Epper (FGN HSG), Rafael Polania (BLU UZH) 
# Date: 10.03.2017
# =============================================================================


# =============================================================================
# FUNCTIONS FILE
# - This file contains all functions and is sourced by the script file (MA_main.R)
# =============================================================================



# -----------------------------------------------------------------------------
# 0. Preliminaries
# -----------------------------------------------------------------------------

# Function to check for packages and load missing ones
GetPackages <- function(requiredPackages) {
  # checks if the given packages are installed and loads the missing ones
  #
  # Args: 
  #   requiredPackages = a vector of packages that should be loaded
  # Returns:
  #   loads packages
  newPackages <- requiredPackages[!(requiredPackages %in% 
                                      installed.packages()[ ,"Package"])]
  if (length(newPackages) > 0) {
    cat("Loading missing packages:", newPackages)
    install.packages(newPackages)
  }
  sapply(requiredPackages, require, character.only=TRUE)
}





###############################################################################
# -----------------------------------------------------------------------------
# 1. Load data, create matrix containing:
#   rating info: ratingsMat
#   choice info: choicesMat
# -----------------------------------------------------------------------------

# Function to return the number of repetition of the questions
repQuestion <- function(data){
  # looks at combinations of questions and returns for each the number of repetition
  #
  # Args: 
  #   data = data frame containing the numbers of Iup and Idown, which item is the correct one and which item was chosen
  # Returns:
  #   "rep":
  #     0 = no repetition
  #     1 = first repetition
  #     2 = second repetition
  #     > 2 = the nth repetition, but should not happen in this task
  #   "same": if subject has chosen the same image in the second repetition of the questions {1 = yes, 0 = no}
  #   "Chosenrep": if one of the repeated questions was not answered (no choice made), then set NA for both questions (/this question set)
  data <- as.data.frame(cbind(IUp = data$IUp,
                              IDown = data$IDown,
                              Correct = data$Correct,
                              ChosenI = data$ChosenI))
  data$rep <- 0
  data$com <- NA
  data$Chosenrep <- data$ChosenI
  for (i in 1:dim(data)[1]){
    z <- 2
    if(data[i, "rep"] == 0 & i != dim(data)[1]){ # check for each row except for last one if...
      for (j in (i+1):dim(data)[1]){
        if(data[j, "IUp"] == data[i, "IUp"] & # ... the image combination in any other row is the same
           data[j, "IDown"] == data[i, "IDown"] |
           data[j, "IUp"] == data[i, "IDown"] &
           data[j, "IDown"] == data[i, "IUp"]){
          data[i, "rep"] <- 1 # if yes, then 1st rep for first repetition
          data[j, "rep"] <- z # and z for zth repetition
          z <- z + 1
          if((data[i, "ChosenI"] == 78 | data[i, "ChosenI"] == 28) & # also check if subject has made decision in both repetition cases
             (data[j, "ChosenI"] == 78 | data[j, "ChosenI"] == 28)){
            if(data[i, "Correct"] == data[j, "Correct"]){ # if choice made in both repetitions, check if they have made same decision or have changed their mind
              data[j, "com"] <- 0
            } else {
              data[j, "com"] <- 1
            }
          } else { # if in one case there was no decision, remove decision in both cases, as we want to check for changes of mind
            data[i, "Chosenrep"] <- NA
            data[j, "Chosenrep"] <- NA
          }
        }
      }
    }
  }
  return(cbind(data$rep, data$com, data$Chosenrep))
}





# Function to return correct and incorrect answers according to the ratings
correctAnswers <- function(data){
  # is the choice made correct according to the different ratings
  #
  # Args: 
  #   data = data frame containing ChosenImages and VD of the different ratings
  # Returns:
  #   0 = no repetition
  #   1 = first repetition
  #   2 = second repetition
  #   > 2 = the nth repetition, but should not happen in this task
  data$Correct.r1 <- 0
  data$Correct.r2 <- 0
  idx1 <- which((data$ChosenI == 78 & data$VD.r1 > 0) | # chosen upper image and value difference is > 0
                  data$ChosenI == 28 & data$VD.r1 <= 0) # or chosen lower image and VD is <= 0
  idx2 <- which((data$ChosenI == 78 & data$VD.r2 > 0) | 
                  data$ChosenI == 28 & data$VD.r2 <= 0)
  data$Correct.r1[idx1] <- 1
  data$Correct.r2[idx2] <- 1
  return(cbind(data$Correct.r1, data$Correct.r2))
}




# Function to add columns with values from first repetition to second repetition
firstRoundAnswers <- function(data, variables){
  # rates from first repetition added to values from second
  #
  # Args: 
  #   data = data containing information about both repetitions
  #   variables = caracter-vector containing column names of which I want to have the first repeated values added to second repetition
  # Returns:
  #   the value of the first repetitions (in the columns of "variables"), put into the new columns where the questions was repeated the second time
  
  # add empty columns with column names = variables.rep1
  variables_new <- c()
  for(x in 1:length(variables)){
  data[, paste(variables[x], "rep1", sep = '.')] <- NA
  variables_new <- c(variables_new, paste(variables[x], "rep1", sep = '.'))
  }
  
  # paste the values from the first repetitions to the new columns according to the image set
  # do it for each subject individually
  data_new <- c()
  for(a in unique(data$subject)){
    data_indiv <- subset(data, data$subject == a)
      for(i in 1:dim(data_indiv)[1]){
      if(data_indiv[i, "rep"] == 2){
        for(j in which(data_indiv$rep == 1)){
          if(data_indiv[j, "IUp"] == data_indiv[i, "IUp"] & 
             data_indiv[j, "IDown"] == data_indiv[i, "IDown"] |
             data_indiv[j, "IUp"] == data_indiv[i, "IDown"] &
             data_indiv[j, "IDown"] == data_indiv[i, "IUp"]){
            for(f in 1:length(variables)){
              data_indiv[i, paste(variables_new[f])] <- data_indiv[j, variables[f]]
            }
          }
        }
      }
    }
    data_new <- rbind(data_new, data_indiv) # rbind all the individual matrices
  }
  return(data_new)
}





###############################################################################
# -----------------------------------------------------------------------------
# 2. descriptive statistics
# -----------------------------------------------------------------------------

#####
# Rating Part
###########################################


# Function returning value difference in the ratings of same image
ratingdiff <- function(data){
  # return the rating differnce of the same product
  #
  # Args:
  #   data = data frame containing image number, its rating values and subject number
  #
  # Returns:
  #   rating differences
  data$VD <- c()
  data$absVD <- c()
  data$VD.z <- c()
  data$absVD.z <- c()
  data_new <- c()
  for(a in unique(data$subject)){
    dataindiv <- subset(data, data$subject == a)
    for(i in 1:(0.5 * dim(dataindiv)[1])){
      for(j in (0.5 * dim(dataindiv)[1]):dim(dataindiv)[1]){
        if(dataindiv[i, "INr"] == dataindiv[j, "INr"]){
          dataindiv[i, "VD"] <- dataindiv[i, "Val"] - dataindiv[j, "Val"]
          dataindiv[i, "absVD"] <- abs(dataindiv[i, "VD"])
        }
      }
    }
    dataindiv[, "VD.z"] <- scale(dataindiv[, "VD"])
    dataindiv[, "absVD.z"] <- scale(dataindiv[, "absVD"])
    data_new <- rbind(data_new, dataindiv)
  }
  return(data_new)
}





# Function creating a pdf file containing histogram, N and kerney density for each inidividual
indivpdf <- function(data, variable){
  # creates pdf with individual histogram of variable, as well as N and kernel density
  #
  # Args:
  #   data = data frame containing subjectnumber, variable
  #   variable = variable which wants to be plotted as histogram
  #
  # Returns:
  #   a pdf file in the directory folder containing a histogram of variable for each individual
  pdf(paste0("individ", variable, ".pdf"))
  for(i in 1:ns){
    individ <- subset(data, data$subject == i)
    plot <- ggplot(individ, aes_string(variable)) +
      geom_histogram(aes(y = ..density..)) +
      stat_function(fun = dnorm, 
                    args = list(mean = mean(individ[, variable]), sd = sd(individ[, variable])), 
                    lwd = 0.5, 
                    col = 'red') + # add normal distribution with sample mean and sample sd
      geom_density(col = 'blue') + # add kernel density 
      theme_bw() + # make white background, only x and y axis lines left
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
}



# Function plotting each individual estimated N in one big plot
ratingN.oneplot <- function(data, variable){
  # returns plot of all individual N
  #
  # Args: 
  #   data = data frame containing subjectnumber, variable
  #   variable = variable which wants to be plotted (estimated N)
  #
  # Returns:
  # all individual N estimated out of variable in one plot
  
  # create the plots for each individual and access data of estimated N
  dens_long <- c()
  for(i in 1:ns){
    individ <- subset(data, data$subject == i)
    plot <- ggplot(individ, aes_string(variable)) +
      stat_function(fun = dtruncnorm, 
                    args = list(mean = mean(individ[, variable]), sd = sd(individ[, variable])), 
                    lwd = 0.5, 
                    col = 'red')
    build <- ggplot_build(plot)
    y_indiv <- build$data[[1]]$y
    x_indiv <- build$data[[1]]$x
    subjectnr <- rep(i, length(y_indiv))
    dens <- cbind(x_indiv, y_indiv, subjectnr)
    dens_long <- rbind(dens_long, dens)
  } 
  # one plot for all N (white theme)
  subject <- factor(as.data.frame(dens_long)$subjectnr)
  ggplot(data = as.data.frame(dens_long),
         aes(x = x_indiv, y = y_indiv, 
             col = subject,
             linetype = subject)) +
    geom_line() +
    scale_linetype_manual(values = c(rep(1:12, 3))) +
    scale_color_manual(values = c(rep("black", 12), rep("darkblue", 12),  rep("darkgreen", 12))) +
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + # force axis to start at 0
    theme_bw() + # make white background, only x and y axis lines left
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank() )+
    theme(panel.border= element_blank())+
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black")) +
    labs(#title = "individual ratings distribution",
         x = "rating value",
         y = "density")
}



# Function to return the mean of Correct for each number of bins of x and plot whole set with error bars
binned_mean <- function(data, x, ngroups){
  # return the mean of y, given the quantiles calculated from x
  #
  # Args: 
  #   data = data frame containing subject-number, Correct and x at least
  #   x = one character indicating which variable is taken for making number of groups
  #   ngroups = how many groups / bins shall be made
  # Returns:
  #   means of Correct given ngroups for x
  data$tile <- ntile(data[, paste(x)], ngroups)
  within_subject <- ddply(data, .(subject, tile), summarise, acc = mean(Correct))
  whole <- ddply(within_subject, .(tile), summarise, acc2 = mean(acc), se = sd(acc)/sqrt(ns)) # we need standard error for error bars
  plot <- ggplot(whole, aes(x = tile, y = acc2)) + 
    geom_line() +
    geom_errorbar(width = 0.1, aes(ymin = acc2 - se, ymax = acc2 + se)) +
    geom_point(shape = 21, size = 3, fill = "white") +
    theme_bw() + # make white background, only x and y axis lines left
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank() )+
    theme(panel.border= element_blank())+
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black")) +
    labs(#title = paste("mean", x),
         x = paste("binned", x),
         y = "Correct")
  print(whole)
  return(plot)
}

# Function same as binned_mean, only dependent variable is Conf instead of Correct
binned_mean_conf <- function(data, x, ngroups){
  data$tile <- ntile(data[, paste(x)], ngroups)
  within_subject <- ddply(data, .(subject, tile), summarise, acc = mean(Conf))
  whole <- ddply(within_subject, .(tile), summarise, acc2 = mean(acc), se = sd(acc)/sqrt(ns)) # we need standard error for error bars
  plot <- ggplot(whole, aes(x = tile, y = acc2)) + 
    geom_line() +
    geom_errorbar(width = 0.1, aes(ymin = acc2 - se, ymax = acc2 + se)) +
    geom_point(shape = 21, size = 3, fill = "white") +
    theme_bw() + # make white background, only x and y axis lines left
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank() )+
    theme(panel.border= element_blank())+
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black")) +
    labs(#title = paste("mean", x),
         x = paste("binned", x),
         y = "Conf")
  print(plot)
  return(whole)
}



# Function returning percentage of different rating values within a subject
ratingChange <- function(data){
  # returns % of different values for each subject
  #
  # Args:
  #   data = data containing the informations about absolute Value differences of the two ratings
  #
  # Returns:
  #   a vector of individual % of different rating values
  percent_different <- c()
  for(i in 1:ns){
    individ <- subset(data, data$subject == i)
    samevalues <- length(which(individ$absVD == 0))
    percent_different_indiv <- (1 - (samevalues / dim(individ)[[1]])) * 100
    percent_different <- c(percent_different, percent_different_indiv)
  }
  return(percent_different)
}




###############################################################################
# -----------------------------------------------------------------------------
# 4. Ranking effect on choice
#   for each individual and overall with GLMER
# -----------------------------------------------------------------------------

# Function returning logistic estimates and marginal effects in two different data frame (combined in a list) for each subject
LogitMarginEffect <- function(formula, data, Conf){
  # returning a data frame with logit estimates and one with marginal effects for regressions within each subject
  #
  # Args:
  #   formula = regression formula in form of logistic regression (y ~ x + z)
  #   data = data containing information for regression
  #   Conf = T if in regressino formula there is Conf, then we have to remove those subjects which are ignored in this kind of analysis
  #
  # Returns:
  #   list of two data frames containing individual betas, stdE, z-value, p-value for each covariate;
  #     in first data frame as logistic estimates, in the second as marginal effects
  coef_all_glm <- c()
  coef_all_margin <- c()

  if(Conf == T){
    for(i in 1:ns){
      # estimate for each individual separately but without those removed for conf
      if(i %in% rm_conf_subjects == F){
        individ <- subset(data, data$subject == i)
        
        # glm regression and estimates
        reg <- glm(formula, data = individ, family = binomial(link = "logit"))
        sumreg <- summary(reg)
        coef <- as.data.frame(sumreg$coefficients)
        coef$signif <- NA
        for(v in 1:dim(coef)[[1]]){
          if(coef[v, "Pr(>|z|)"] <= 0.0001){coef[v, "signif"] <- "****"} else 
            if(coef[v, "Pr(>|z|)"] > 0.0001 & coef[v, "Pr(>|z|)"] <= 0.001){coef[v, "signif"] <- "***"} else
              if(coef[v, "Pr(>|z|)"] > 0.001 & coef[v, "Pr(>|z|)"] <= 0.01){coef[v, "signif"] <- "**"} else
                if(coef[v, "Pr(>|z|)"] > 0.01 & coef[v, "Pr(>|z|)"] <= 0.05){coef[v, "signif"] <- "*"} else
                  if(coef[v, "Pr(>|z|)"] > 0.05){coef[v, "signif"] <- ""}
        }
        # glm with marginal effects
        regmfx <- logitmfx(formula, data = individ)
        coef_margin <- as.data.frame(regmfx$mfxest)
        coef_margin$signif <- NA
        for(v in 1:dim(coef_margin)[[1]]){
          if(coef_margin[v, "P>|z|"] <= 0.0001){coef_margin[v, "signif"] <- "****"} else 
            if(coef_margin[v, "P>|z|"] > 0.0001 & coef_margin[v, "P>|z|"] <= 0.001){coef_margin[v, "signif"] <- "***"} else
              if(coef_margin[v, "P>|z|"] > 0.001 & coef_margin[v, "P>|z|"] <= 0.01){coef_margin[v, "signif"] <- "**"} else
                if(coef_margin[v, "P>|z|"] > 0.01 & coef_margin[v, "P>|z|"] <= 0.05){coef_margin[v, "signif"] <- "*"} else
                  if(coef_margin[v, "P>|z|"] > 0.05){coef_margin[v, "signif"] <- ""}
        }
        
        # make wide format for better reading of betas for each subject
        coef_wide <- coef[1, ]
        coef_margin_wide <- coef_margin[1, ]
        for(x in 2:dim(coef)[[1]]){
          coef_wide <- cbind(coef_wide, coef[x, ])
        }
        for(x in 2:dim(coef_margin)[[1]]){
          coef_margin_wide <- cbind(coef_margin_wide, coef_margin[x, ])
        }
        covariates <- rownames(coef)
        covariates_margin <- rownames(coef_margin)
        estimations <- c("beta", "stdE", "z-val", "p-val", "signif")
        colname <- c()
        colname_margin <- c()
        for(a in 1:(length(covariates))){
          for(b in 1:(length(estimations))){
            colname <- c(colname, paste(covariates[a], estimations[b], sep = "."))
          }
        }    
        for(a in 1:(length(covariates_margin))){
          for(b in 1:(length(estimations))){
            colname_margin <- c(colname_margin, paste(covariates_margin[a], estimations[b], sep = "."))
          }
        }
        colnames(coef_wide) <- colname
        colnames(coef_margin_wide) <- colname_margin
        coef_wide <- cbind(subject = i, coef_wide)
        coef_margin_wide <- cbind(subject = i, coef_margin_wide)
        coef_all_glm <- rbind(coef_all_glm, coef_wide)
        coef_all_margin <- rbind(coef_all_margin, coef_margin_wide)
        
        # plot logistic curve
        #plot(individ$VD.m.z, individ$Up, main = i)
        #curve(predict(reg, data.frame(VD.m.z = x), type = "resp"), add = TRUE, col = "red")
      }
    }
  } else {
    for(i in 1:ns){
      individ <- subset(data, data$subject == i)
      
      # glm regression and estimates
      reg <- glm(formula, data = individ, family = binomial(link = "logit"))
      sumreg <- summary(reg)
      coef <- as.data.frame(sumreg$coefficients)
      coef$signif <- NA
      for(v in 1:dim(coef)[[1]]){
        if(coef[v, "Pr(>|z|)"] <= 0.0001){coef[v, "signif"] <- "****"} else 
          if(coef[v, "Pr(>|z|)"] > 0.0001 & coef[v, "Pr(>|z|)"] <= 0.001){coef[v, "signif"] <- "***"} else
            if(coef[v, "Pr(>|z|)"] > 0.001 & coef[v, "Pr(>|z|)"] <= 0.01){coef[v, "signif"] <- "**"} else
              if(coef[v, "Pr(>|z|)"] > 0.01 & coef[v, "Pr(>|z|)"] <= 0.05){coef[v, "signif"] <- "*"} else
                if(coef[v, "Pr(>|z|)"] > 0.05){coef[v, "signif"] <- ""}
      }
      # glm with marginal effects
      regmfx <- logitmfx(formula, data = individ)
      coef_margin <- as.data.frame(regmfx$mfxest)
      coef_margin$signif <- NA
      for(v in 1:dim(coef_margin)[[1]]){
        if(coef_margin[v, "P>|z|"] <= 0.0001){coef_margin[v, "signif"] <- "****"} else 
          if(coef_margin[v, "P>|z|"] > 0.0001 & coef_margin[v, "P>|z|"] <= 0.001){coef_margin[v, "signif"] <- "***"} else
            if(coef_margin[v, "P>|z|"] > 0.001 & coef_margin[v, "P>|z|"] <= 0.01){coef_margin[v, "signif"] <- "**"} else
              if(coef_margin[v, "P>|z|"] > 0.01 & coef_margin[v, "P>|z|"] <= 0.05){coef_margin[v, "signif"] <- "*"} else
                if(coef_margin[v, "P>|z|"] > 0.05){coef_margin[v, "signif"] <- ""}
      }
      
      # make wide format for better reading of betas for each subject
      coef_wide <- coef[1, ]
      coef_margin_wide <- coef_margin[1, ]
      for(x in 2:dim(coef)[[1]]){
        coef_wide <- cbind(coef_wide, coef[x, ])
      }
      for(x in 2:dim(coef_margin)[[1]]){
        coef_margin_wide <- cbind(coef_margin_wide, coef_margin[x, ])
      }
      covariates <- rownames(coef)
      covariates_margin <- rownames(coef_margin)
      estimations <- c("beta", "stdE", "z-val", "p-val", "signif")
      colname <- c()
      colname_margin <- c()
      for(a in 1:(length(covariates))){
        for(b in 1:(length(estimations))){
          colname <- c(colname, paste(covariates[a], estimations[b], sep = "."))
        }
      }    
      for(a in 1:(length(covariates_margin))){
        for(b in 1:(length(estimations))){
          colname_margin <- c(colname_margin, paste(covariates_margin[a], estimations[b], sep = "."))
        }
      }
      colnames(coef_wide) <- colname
      colnames(coef_margin_wide) <- colname_margin
      coef_wide <- cbind(subject = i, coef_wide)
      coef_margin_wide <- cbind(subject = i, coef_margin_wide)
      coef_all_glm <- rbind(coef_all_glm, coef_wide)
      coef_all_margin <- rbind(coef_all_margin, coef_margin_wide)
      
      # plot logistic curve
      #plot(individ$VD.m.z, individ$Up, main = i)
      #curve(predict(reg, data.frame(VD.m.z = x), type = "resp"), add = TRUE, col = "red")
    }
  }
  return(list(coef_all_glm, coef_all_margin))
}




# Function returning marginal effects of fixed effect estimates in a glmer estimation
glmermfx <- function(x, nsims = 1000){
  # returns marginal effects of glmer estimates
  #
  # Args:
  #   x = data in form of glmerMod (glmer estimates from package lme4)
  #
  # Returns:
  #   marginal effects of the fixed effect estimates
  set.seed(1984) # make random number draw reproducable
  pdf <- mean(dlogis(-log((1 - fitted(x)) / fitted(x)))) # density, distribution function of logistic distributions (of fitted values)
  pdfsd <- sd(dlogis(-log((1 - fitted(x)) / fitted(x))))
  marginal.effects <- pdf * fixef(x)
  sim <- matrix(rep(NA, nsims * length(fixef(x))), nrow = nsims) # simulate random draw for sd
  for(i in 1:length(fixef(x))){
    sim[, i] <- rnorm(nsims, fixef(x)[i], diag(vcov(x) ^ 0.5)[i])
  }
  pdfsim <- rnorm(nsims, pdf, pdfsd)
  sim.se <- pdfsim * sim
  res <- cbind(marginal.effects, apply(sim.se, 2, sd))
  colnames(res)[2] <- "standard.error"
  #ifelse(names(fixef(x))[1]=="(Intercept)",
   #      return(res[2:nrow(res), ]), return(res))
  return(res)
}





# Function returning a data frame for publication where sdE are in () and below the estimates with significance indication
estimate.table <- function(data, variables){
  # returns data frame for publication (estimates with significance and sdE in brackets)
  #
  # Args.:
  #   data = data containing regression estimates
  #   variables = variables which should be published with estimates and sdE
  #
  # Returns:
  #   data frame where each subject has an estimate and a sdE for each variable in a publication table form
  
  all_estimates <- c()
  for(i in unique(data$subject)){
    subset <- subset(data, data$subject == i)
    oneperson <- c()
    for(j in 1:length(variables)){
      name <- variables[j]
      beta <- paste0(name, ".beta")
      se <- paste0(name, ".stdE")
      signif <- paste0(name, ".signif")
      oneestimate <- rbind(paste0(round(subset[, beta], 4), subset[, signif]),
                           paste0("(", round(subset[, se], 4), ")"))
      oneperson <- cbind(oneperson, oneestimate)
    }
    oneperson <- cbind(c(i, ""), oneperson)
    all_estimates <- rbind(all_estimates, oneperson)
  }
  return(all_estimates)
}



# Function returning data frame with marginal effects and stdE as publication table
glmerestimate.table <- function(data, data.marginal){
  # retunrs data frame as a publication table
  #
  # Args.:
  #   data = data in formal class "glmerMod"
  #   data.marginal = data frame containing marginal effects calculated from data (see function "glmermfx")
  #
  # Returns:
  #   publication data frame containing marginal effects of the glmer regression as well as stdE of the marginal effects
  fixedeff <- as.data.frame(coef(summary(data)))
  fixedeff$signif <- NA
  for(v in 1:dim(fixedeff)[[1]]){
    if(fixedeff[v, "Pr(>|z|)"] <= 0.0001){fixedeff[v, "signif"] <- "****"} else 
      if(fixedeff[v, "Pr(>|z|)"] > 0.0001 & fixedeff[v, "Pr(>|z|)"] <= 0.001){fixedeff[v, "signif"] <- "***"} else
        if(fixedeff[v, "Pr(>|z|)"] > 0.001 & fixedeff[v, "Pr(>|z|)"] <= 0.01){fixedeff[v, "signif"] <- "**"} else
          if(fixedeff[v, "Pr(>|z|)"] > 0.01 & fixedeff[v, "Pr(>|z|)"] <= 0.05){fixedeff[v, "signif"] <- "*"} else
            if(fixedeff[v, "Pr(>|z|)"] > 0.05){fixedeff[v, "signif"] <- ""}
  }
  fixedeff <- cbind(fixedeff, data.marginal)
  fixedeff$beta <- paste0(round(fixedeff$marginal.effects, 4), fixedeff$signif)
  fixedeff$standard.error <- paste0("(", round(fixedeff$standard.error, 4), ")")
  estimates <- as.data.frame(t(fixedeff[, c("beta", "standard.error")]))
  rownames(estimates) <- c("fixed effects", "")
  
  return(estimates)
}



# Function returning a white plot with marginal effects and 95% Confidence Intervals
marginal.effects.plot <- function(data, data.marginal, variables, y){
  # returns ggplot of marginal effects
  #
  # Args.:
  #   data = glmer estimates
  #   data.marginal = data containing at least Estimates and Std.E of a regression; must be matrix (not data frame);
  #     names of columns should be "marginal.effects" and "standard.error"
  #   variables = how we want the coefficients be named --> make sure them to be in the correct order!!
  #   y = name of dependent variable to name y-axis as Marginal effect of y
  #
  # Returns:
  #   plot with marginal effects and error bars showing 95% confidence interval
  if(is.data.frame(data) == F){
    fixedeff <- as.data.frame(coef(summary(data)))
    fixedeff$signif <- NA
    for(v in 1:dim(fixedeff)[[1]]){
      if(fixedeff[v, "Pr(>|z|)"] <= 0.0001){fixedeff[v, "signif"] <- "****"} else 
        if(fixedeff[v, "Pr(>|z|)"] > 0.0001 & fixedeff[v, "Pr(>|z|)"] <= 0.001){fixedeff[v, "signif"] <- "***"} else
          if(fixedeff[v, "Pr(>|z|)"] > 0.001 & fixedeff[v, "Pr(>|z|)"] <= 0.01){fixedeff[v, "signif"] <- "**"} else
            if(fixedeff[v, "Pr(>|z|)"] > 0.01 & fixedeff[v, "Pr(>|z|)"] <= 0.05){fixedeff[v, "signif"] <- "*"} else
              if(fixedeff[v, "Pr(>|z|)"] > 0.05){fixedeff[v, "signif"] <- ""}
    }
  } else {
    fixedeff <- c()
    fixedeff$signif <- data$signif
  }
  
  dat <- as.data.frame(cbind(V1 = variables, data.marginal, sig = fixedeff$signif))
  dat$me <- as.numeric(as.character(dat$marginal.effects))
  dat$se <- as.numeric(as.character(dat$standard.error))
  plot <- ggplot(dat, aes(x = V1, y = marginal.effects, ymin = me - 1.96 * se, ymax = me + 1.96 * se)) + # 95% confidence interval
    scale_x_discrete(limits = variables) +
    scale_y_continuous(paste("Marginal Effect on", y), limits = c(-0.75, 0.75)) +
    geom_errorbar(aes(x = V1, y = me),size = .2, width = .1) + 
    geom_point(aes(x = V1, y = me)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "lightgrey") + 
    geom_text(aes(x = V1, y = me, label = sig), hjust = -1, vjust = 0) +
    theme_bw() + # make white background, only x and y axis lines left
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank() )+
    theme(panel.border= element_blank())+
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black")) +
    labs(x = "")
  return(plot)
}








###############################################################################
# -----------------------------------------------------------------------------
# 6. Effects on confidence
# -----------------------------------------------------------------------------

# Function returning data frame containing individual lm regression estimates 
conf.estimate <- function(data, formula){
  # returns data frame of lm estimates for each subject
  # 
  # Args.:
  #   data = data set containing all infos for regression
  #   formula = regression formula
  #
  # Returns:
  #   data frame with regression estimates of each subject
  
  coef_all_glm <- c()
  
  for(i in 1:ns){
    # estimate for each individual separately but without those removed for conf
    if(i %in% rm_conf_subjects == F){
      individ <- subset(data, data$subject == i)
      reg <- lm(formula, data = individ)
      sum <- summary(reg)
      coef <- as.data.frame(coef(sum))
      coef$signif <- NA
      for(v in 1:dim(coef)[[1]]){
        if(coef[v, "Pr(>|t|)"] <= 0.0001){coef[v, "signif"] <- "****"} else 
          if(coef[v, "Pr(>|t|)"] > 0.0001 & coef[v, "Pr(>|t|)"] <= 0.001){coef[v, "signif"] <- "***"} else
            if(coef[v, "Pr(>|t|)"] > 0.001 & coef[v, "Pr(>|t|)"] <= 0.01){coef[v, "signif"] <- "**"} else
              if(coef[v, "Pr(>|t|)"] > 0.01 & coef[v, "Pr(>|t|)"] <= 0.05){coef[v, "signif"] <- "*"} else
                if(coef[v, "Pr(>|t|)"] > 0.05){coef[v, "signif"] <- ""}
      }
      coef_wide <- coef[1, ]
      for(x in 2:dim(coef)[[1]]){
        coef_wide <- cbind(coef_wide, coef[x, ])
      }
      covariates <- rownames(coef)
      estimations <- c("beta", "stdE", "z-val", "p-val", "signif")
      colname <- c()
      for(a in 1:(length(covariates))){
        for(b in 1:(length(estimations))){
          colname <- c(colname, paste(covariates[a], estimations[b], sep = "."))
        }
      }    
      colnames(coef_wide) <- colname
      coef_wide <- cbind(subject = i, coef_wide)
      coef_all_glm <- rbind(coef_all_glm, coef_wide)
      
    }   
  }
  return(as.data.frame(coef_all_glm))
}



# Function returning data frame with lmer regression estimates incl. pvalue
conf.lmer.estimate <- function(reg){
  # data frame with all lmer estimates
  #
  # Args.:
  #   reg = regression results of a lmer regression format
  #
  # Returns:
  #   a data frame containing all regressino estimates as well as the p-value
  coef <- as.data.frame(coef(summary(reg)))
  coef$pvalue <- 2 * (1 - pnorm(abs(coef$`t value`))) # calculate p-value as only t-value given
  coef$signif <- NA
  for(v in 1:dim(coef)[[1]]){
    if(coef[v, "pvalue"] <= 0.0001){coef[v, "signif"] <- "****"} else 
      if(coef[v, "pvalue"] > 0.0001 & coef[v, "pvalue"] <= 0.001){coef[v, "signif"] <- "***"} else
        if(coef[v, "pvalue"] > 0.001 & coef[v, "pvalue"] <= 0.01){coef[v, "signif"] <- "**"} else
          if(coef[v, "pvalue"] > 0.01 & coef[v, "pvalue"] <= 0.05){coef[v, "signif"] <- "*"} else
            if(coef[v, "pvalue"] > 0.05){coef[v, "signif"] <- ""}
  }
  return(as.data.frame(coef))
}



# Function retruning publication table of lmer estimates after transformation with conf.lmer.estimate
lmerestimate.table <- function(estimates){
  # returns table for publication of lmer estimates
  #
  # Args.:
  #   estimates = estimation results of a lmer regression after transforming with conf.lmer.estimate
  #
  # Returns:
  #   data frame in publicational form for latex
  estimates$beta <- paste0(round(estimates$Estimate, 4), estimates$signif)
  estimates$stdE <- paste0("(", round(estimates$`Std. Error`, 4), ")")
  table <- as.data.frame(t(estimates[, c("beta", "stdE")]))
  rownames(table) <- c("fixed effects", "")
  return(table)
}


###############################################################################
# -----------------------------------------------------------------------------
# 7. Change of mind (COM) behavior
#   with Generalized Linear Mixed-Effects Model (glmer)
# -----------------------------------------------------------------------------
#####
# 7.2.1 simple model: constant noise (Normal distribution) accross scale
#####




# Function calculating probability of Correct, given estimated sigman for overall data, assuming constant noise
#   sources functions: llh_constant
p_constant <- function(data, start){
  # returns vector with trialwise probabilities of Correct
  #
  # Args.:
  #   data = data containing all information about mean ratings (Up and Down)
  #   start = where to start estimating sigman (for maximum likelihood function)
  #
  # Returns:
  #   vector containing probabilities of Correct, sigman estimated overall (!= within subject), assuming constant noise
    
  # get best sigman with log likelihood function:
 
      # Function returning the log likelihood assuming constant noise
      llh_constant <- function(sigman){
        # returns log likelihood
        #
        # Args.:
        #   sigman = what we want to estimate with this likelihood function
        #
        # Returns:
        #   log likelihood of sigman in data given constant noise (assuming normal distribution)
        sigman <- exp(sigman) # allow for negative sigma values
        prob <- pnorm( abs(data$mrUp - data$mrDown) / (sigman * sqrt(2)), # formula of our probability 
                       0, 1) # pnorm with mu = 0, sigma = 1
    
        #likelihood function for bernoulli process
        likelihoods <- ifelse(data$Correct == 1, prob, 1 - prob) # probability of being correct, regardles of which picture (up/down) was chosen
        ll <- -sum(log(likelihoods))
        #cat(ll, " ") # to see output of process for checking
        return(ll)
      }
  
  fit <- mle2(llh_constant, start = list(sigman = log(start)))
  sigmanfit <- exp(coef(fit)["sigman"])
  
  # check sigmanfit. If > 500 then take another starting value and recalculate
  while(sigmanfit > 500){
    start <- start + 50
    fit <- mle2(llh_constant, start = list(sigman = log(start)))
    sigmanfit <- exp(coef(fit)["sigman"])
#    print(sigmanfit)
  }
  
  # calculate probabilities with fitted sigman
  probfit <- pnorm(abs(data$mrUp - data$mrDown) / (sigmanfit * sqrt(2)),
                   0, 1) # same function as in llh_constant, now with known sigman
  
  return(list(probfit, AIC(fit), sigmanfit))
}






#####
# 7.2.2 extended model: efficient measure
#####



# Function calculating the logit
logit <- function(x){
  # calculate logit estimate
  # 
  # Args.:
  #   x = number which should be turned into logit value
  # 
  # Returns:
  #   logit value of input
  log(x / (1 - x))
}



# Function calculating probability of Correct, given estimated variables for overall data, assuming efficient measure (= a prior distirbution, where mu and sigma have to be estimated too)
#   sources functions: llh_efficient and logit
p_efficient <- function(data, start.sigman, start.mu.prior, start.sigma.prior){
  # returns vector with trialwise probabilities of Correct
  #
  # Args.:
  #   data = data containing all information about mean ratings (Up and Down)
  #   start.sigman = where to start estimating sigman (for maximum likelihood function)
  #   start.mu.prior = where to start estimating mu of prior distribution (for maximum likelihood function)
  #   start.sigma.prior = where to start estimating sigma of prior distribution (for maximum likelihood function)
  #
  # Returns:
  #   vector containing probabilities of Correct, variables estimated overall (!= within subject), assuming efficient measure
  
  # scale 0-922 to 0-1 for mean rating values of Up and Down
  data$mrUp.s <- data$mrUp/922 
  data$mrDown.s <- data$mrDown/922
  
  # scale 0-1 to -inf - +inf with logit
  #   avoid problems with logit (can't divide by 0, can't have log(0))
  data$mrUp.s[data$mrUP.s == 0] <- 0.001
  data$mrDown.s[data$mrDown.s == 0] <- 0.001
  data$mrUp.s[data$mrUP.s == 1] <- 1 - 0.001
  data$mrDown.s[data$mrDown.s == 1] <- 1 - 0.001
  #   expand
  data$mrUp.s.logit <- logit(data$mrUp.s)
  data$mrDown.s.logit <- logit(data$mrDown.s)
  
      # Function returning the log likelihood assuming constant noise
      llh_efficient <- function(mu.prior, sigma.prior, sigman){
        # returns log likelihood
        #
        # Args.:
        #   mu.prior, sigma.prior, sigman = variables we want to estimate with this likelihood function
        #
        # Returns:
        #   log likelihood of sigman in data given efficient measure (and a prior distribution)
        sigma.prior <- exp(sigma.prior) # allow for negative values
        sigman <- exp(sigman)
    
        prob <- pnorm( abs(pnorm(data$mrUp.s.logit, mu.prior, sigma.prior) - 
                            pnorm(data$mrDown.s.logit, mu.prior, sigma.prior)) /
                         (sigman * sqrt(2)), # formula of our probability
                       0, 1)
    
        #likelihood function for bernoulli process
        likelihoods <- ifelse(data$Correct == 1, prob, 1 - prob) # probability of being correct, regardles of which picture (up/down) was chosen
        ll <- -sum(log(likelihoods))
        #cat(ll, " ") # to see output of process for checking
        return(ll)
      }
  
  #   get best sigman, mu.prior and sigma.prior
  fit <- mle2(llh_efficient, start = list(sigman = log(start.sigman),
                                          mu.prior = start.mu.prior,
                                          sigma.prior = start.sigma.prior))
  sigman.fit <- exp(coef(fit)["sigman"])
  mu.prior.fit <- coef(fit)["mu.prior"] # don't have to take exp, is already what we want
  sigma.prior.fit <- exp(coef(fit)["sigma.prior"])
  
  # check fits.
  while(sigman.fit > 15){
    start.sigman <- start.sigman + 0.05
    fit <- mle2(llh_efficient, start = list(sigman = log(start.sigman),
                                            mu.prior = start.mu.prior,
                                            sigma.prior = start.sigma.prior))
    sigman.fit <- exp(coef(fit)["sigman"])
#    print(sigman.fit)
  }

#  while(mu.prior.fit > 500){
#    start.mu.prior <- start.mu.prior + 0.05
#    fit <- mle2(llh_efficient, start = list(sigman = log(start.sigman),
#                                            mu.prior = start.mu.prior,
#                                            sigma.prior = start.sigma.prior))
#    mu.prior.fit <- coef(fit)["mu.prior"]
#    print(mu.prior.fit)
#  }

  while(sigma.prior.fit > 15 & start.sigma.prior < 3){
    start.sigma.prior <- start.sigma.prior + 0.25
    fit <- mle2(llh_efficient, start = list(sigman = log(start.sigman),
                                            mu.prior = start.mu.prior,
                                            sigma.prior = start.sigma.prior))
    sigma.prior.fit <- exp(coef(fit)["sigma.prior"])
#    print(start.sigma.prior)
#    print(sigma.prior.fit)
  }
  
  # calculate probabilities with fitted sigman, mu.prior, sigma.prior
  probfit <- pnorm( abs(pnorm(data$mrUp.s.logit, mu.prior.fit, sigma.prior.fit) - 
                          pnorm(data$mrDown.s.logit, mu.prior.fit, sigma.prior.fit)) /
                      (sigman.fit * sqrt(2)), # formula of our probability
                    0, 1)
  
  return(list(probfit, AIC(fit), mu.prior.fit, sigma.prior.fit, sigman.fit))
}


