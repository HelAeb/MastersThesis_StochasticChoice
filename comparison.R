# =============================================================================
# University of St.Gallen
# Course: Master's Thesis
# Author: Helena Aebersold
# Professors: Thomas Epper (FGN HSG), Rafael Polania (BLU UZH) 
# Date: 30.04.2017
# =============================================================================


# =============================================================================
# MODEL COMPARISON
# - This file contains the code to compare the different models used in the
#     main script file (MA_main.R) and is sourced by it 
# =============================================================================



###############################################################################
# -----------------------------------------------------------------------------
# 4. Ranking Effect on Choice
# -----------------------------------------------------------------------------

##########
##### Up ~ 

up_0 <- glmer(Up ~ 1 +
                (1 | subject),
              data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
              family = binomial(link = "logit"),
              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

up_1 <- glmer(Up ~ 1 + VD.m.z +
                (1 + VD.m.z | subject),
              data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
              family = binomial(link = "logit"),
              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

up_2 <- glmer(Up ~ 1 + VD.m.z + RT.z +
                (1 + VD.m.z + RT.z | subject),
              data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
              family = binomial(link = "logit"),
              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

up_3 <- glmer(Up ~ 1 + VD.m.z + RT.z + mValtot.z +
                (1 + VD.m.z + RT.z + mValtot.z | subject),
              data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
              family = binomial(link = "logit"),
              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

up_3.1 <- glmer(Up ~ 1 + VD.m.z + RT.z + mValtot.z +
                (1 + VD.m.z + RT.z | subject),
              data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
              family = binomial(link = "logit"),
              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

up_4 <- glmer(Up ~ 1 + VD.m.z + RT.z + mValtot.z + Conf.z +
                (1 + VD.m.z + RT.z + Conf.z | subject),
              data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
              family = binomial(link = "logit"),
              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

up_4.1 <- glmer(Up ~ 1 + VD.m.z + RT.z + mValtot.z + Conf.z +
                (1 + VD.m.z + RT.z + Conf.z | subject),
              data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
              family = binomial(link = "logit"),
              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

up_5 <- glmer(Up ~ 1 + VD.m.z + RT.z + mValtot.z + Conf.z + (mValtot.z * VD.m.z) +
                (1 + VD.m.z + RT.z + mValtot.z + Conf.z + (mValtot.z * VD.m.z) | subject),
              data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
              family = binomial(link = "logit"),
              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

up_5.1 <- glmer(Up ~ 1 + VD.m.z + RT.z + mValtot.z + Conf.z + (mValtot.z * VD.m.z) +
                (1 + VD.m.z + RT.z + Conf.z + (mValtot.z * VD.m.z) | subject),
              data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
              family = binomial(link = "logit"),
              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

up_6 <- glmer(Up ~ 1 + VD.m.z + RT.z + mValtot.z + Conf.z + (mValtot.z * VD.m.z) +  (VD.m.z * Conf.z) +
                (1 + VD.m.z + RT.z + mValtot.z + Conf.z + (mValtot.z * VD.m.z) +  (VD.m.z * Conf.z) | subject),
              data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
              family = binomial(link = "logit"),
              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

up_6.1 <- glmer(Up ~ 1 + VD.m.z + RT.z + mValtot.z + Conf.z + (mValtot.z * VD.m.z) +  (VD.m.z * Conf.z) +
                (1 + VD.m.z + RT.z + Conf.z + (mValtot.z * VD.m.z) +  (VD.m.z * Conf.z) | subject),
              data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
              family = binomial(link = "logit"),
              control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))





comparison_up <- anova(up_0, up_1, up_2, up_3, up_3.1, up_4, up_4.1, up_5, up_5.1, up_6, up_6.1)
comparison_up$AIC
# CONCLUSION: Model 6.1 is best 
formula <- c("{Up $\\sim$ 1}", "{Up $\\sim$ VD}", "{Up $\\sim$ VD + RT}", "{Up $\\sim$ VD + RT + SV}", "{model 3 without SV in random effects}",
             "{Up $\\sim$ VD + RT + SV + Conf}", "{model 4 without SV in random effects}", "{Up $\\sim$ VD + RT + SV + Conf + (VD x SV)}",
             "{model 5 without SV in random effects}", "{Up $\\sim$ VD + RT + SV + Conf + (VD x SV) + (VD x Conf)}", "{model 6 without SV in random effects}")
negHessian <- c("{-}", "{-}", "{-}", "{x}", "{-}", "{-}", "{-}", "{x}", "{-}", "{x}", "{-}")
model <- c(0, 1, 2, 3, 3.1, 4, 4.1, 5, 5.1, 6, 6.1)
comp_up_table <- cbind(model, formula, round(comparison_up$AIC, 3), negHessian)
colnames(comp_up_table) <- c("{model}", "{formula}", "{AIC}", "{neg. Hessian}")






##########
##### Correct ~ 

correct_0 <- glmer(Correct ~ 1 + 
                     (1 | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_1 <- glmer(Correct ~ 1 + absVD.m.z +
                     (1 + absVD.m.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_2 <- glmer(Correct ~ 1 + absVD.m.z + RT.z +
                     (1 + absVD.m.z + RT.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_3 <- glmer(Correct ~ 1 + absVD.m.z + RT.z + mValtot.z +
                     (1 + absVD.m.z + RT.z + mValtot.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_3.1 <- glmer(Correct ~ 1 + absVD.m.z + RT.z + mValtot.z +
                     (1 + absVD.m.z + RT.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_4 <- glmer(Correct ~ 1 + absVD.m.z + RT.z + mValtot.z + Conf.z +
                     (1 + absVD.m.z + RT.z + mValtot.z + Conf.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_4.1 <- glmer(Correct ~ 1 + absVD.m.z + RT.z + mValtot.z + Conf.z +
                     (1 + absVD.m.z + RT.z + Conf.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_5 <- glmer(Correct ~ 1 + absVD.m.z + RT.z + mValtot.z + meanVD.z +
                     (1 + absVD.m.z + RT.z + mValtot.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_5.1 <- glmer(Correct ~ 1 + absVD.m.z + RT.z + mValtot.z + meanVD.z +
                     (1 + absVD.m.z + RT.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_6 <- glmer(Correct ~ 1 + absVD.m.z + RT.z + mValtot.z + sdrAll.z +
                     (1 + absVD.m.z + RT.z + mValtot.z + sdrAll.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_6.1 <- glmer(Correct ~ 1 + absVD.m.z + RT.z + mValtot.z + sdrAll.z +
                     (1 + absVD.m.z + RT.z + sdrAll.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_7 <- glmer(Correct ~ 1 + absVD.m.z + RT.z + mValtot.z + Conf.z + meanVD.z +
                     (1 + absVD.m.z + RT.z + mValtot.z + Conf.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_7.1 <- glmer(Correct ~ 1 + absVD.m.z + RT.z + mValtot.z + Conf.z + meanVD.z +
                     (1 + absVD.m.z + RT.z + Conf.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ],
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_8 <- glmer(Correct ~ 1 + absVD.m.z + RT.z + mValtot.z + Conf.z + sdrAll.z +
                     (1 + absVD.m.z + RT.z + mValtot.z + Conf.z + sdrAll.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

correct_8.1 <- glmer(Correct ~ 1 + absVD.m.z + RT.z + mValtot.z + Conf.z + sdrAll.z +
                     (1 + absVD.m.z + RT.z + Conf.z + sdrAll.z | subject),
                   data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ], 
                   family = binomial(link = "logit"),
                   control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))







comparison_correct <- anova(correct_0, correct_1, correct_2, correct_3, correct_3.1, correct_4,
                            correct_4.1, correct_5, correct_5.1, correct_6, correct_6.1,
                            correct_7, correct_7.1, correct_8, correct_8.1)
comparison_correct$AIC
# CONCLUSION: Model 7.1 is best 
formula <- c("{Correct $\\sim$ 1}", "{Correct $\\sim$ absVD}", "{Correct $\\sim$ absVD + RT}", "{Correct $\\sim$ absVD + RT + SV}", "{model 3 without SV in random effects}",
             "{Correct $\\sim$ absVD + RT + SV + Conf}", "{model 4 without SV in random effects}", "{Correct $\\sim$ absVD + RT + SV + mPD}",
             "{model 5 without SV in random effects}", "{Correct $\\sim$ absVD + RT + SV + SD}", "{model 6 without SV in random effects}",
             "{Correct $\\sim$ absVD + RT + SV + Conf + mPD}", "{model 7 without SV in random effects}", "{Correct $\\sim$ absVD + RT + SV + Conf + SD}",
             "{model 8 without SV in random effects}")
negHessian <- c("{-}", "{x}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{x}")
model <- c(0, 1, 2, 3, 3.1, 4, 4.1, 5, 5.1, 6, 6.1, 7, 7.1, 8, 8.1)
comp_corr_table <- cbind(model, formula, round(comparison_correct$AIC, 3), negHessian)
colnames(comp_corr_table) <- c("{model}", "{formula}", "{AIC}", "{neg. Hessian}")








###############################################################################
# -----------------------------------------------------------------------------
# 6. Effects on confidence
# -----------------------------------------------------------------------------
conf_0 <- lmer(Conf.z ~ 1 +
                    (1 | subject),
                  data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ])

conf_1 <- lmer(Conf.z ~ 1 + absVD.m.z +
                 (1 + absVD.m.z | subject),
               data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ])

conf_2 <- lmer(Conf.z ~ 1 + absVD.m.z + RT.z +
                 (1 + absVD.m.z + RT.z | subject),
               data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ])

conf_3 <- lmer(Conf.z ~ 1 + absVD.m.z + RT.z + mValtot.z +
                 (1 + absVD.m.z + RT.z + mValtot.z | subject),
               data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ])

conf_3.1 <- lmer(Conf.z ~ 1 + absVD.m.z + RT.z + mValtot.z +
                 (1 + absVD.m.z + RT.z | subject),
               data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ])

conf_4 <- lmer(Conf.z ~ 1 + absVD.m.z + RT.z + mValtot.z + meanVD.z +
                 (1 + absVD.m.z + RT.z + mValtot.z | subject),
               data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ])

conf_4.1 <- lmer(Conf.z ~ 1 + absVD.m.z + RT.z + mValtot.z + meanVD.z +
                 (1 + absVD.m.z + RT.z | subject),
               data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ])

conf_5 <- lmer(Conf.z ~ 1 + absVD.m.z + RT.z + mValtot.z + sdrAll.z +
                 (1 + absVD.m.z + RT.z + mValtot.z + sdrAll.z | subject),
               data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ])

conf_5.1 <- lmer(Conf.z ~ 1 + absVD.m.z + RT.z + mValtot.z + sdrAll.z +
                 (1 + absVD.m.z + RT.z + sdrAll.z | subject),
               data = dataChoice[-which(dataChoice$subject %in% rm_conf_subjects), ])






comparison_conf <- anova(conf_0, conf_1, conf_2, conf_3, conf_3.1, conf_4, conf_4.1, conf_5, conf_5.1)
comparison_conf$AIC
# CONCLUSION: Model 3 is best
formula <- c("{Conf $\\sim$ 1}", "{Conf $\\sim$ absVD}", "{Conf $\\sim$ absVD + RT}", "{Conf $\\sim$ absVD + RT + SV}", "{model 3 without SV in random effects}",
             "{Conf $\\sim$ absVD + RT + SV + mPD}", "{model 4 without SV in random effects}", "{Conf $\\sim$ absVD + RT + SV + SD}",
             "{model 5 without SV in random effects}")
negHessian <- c("{-}", "{-}", "{-}", "{-}", "{-}", "{x}", "{x}", "{x}", "{x}")
model <- c(0, 1, 2, 3, 3.1, 4, 4.1, 5, 5.1)
comp_conf_table <- cbind(model, formula, round(comparison_conf$AIC, 3), negHessian)
colnames(comp_conf_table) <- c("{model}", "{formula}", "{AIC}", "{neg. Hessian}")











###############################################################################
# -----------------------------------------------------------------------------
# 7. Change of mind (COM) behavior
# -----------------------------------------------------------------------------
com_0 <- glmer(com ~ 1 +
               (1 | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_1 <- glmer(com ~ 1 + absVD.m.z +
               (1 + absVD.m.z | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_2 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 +
                 (1 + absVD.m.z + RT.z.rep1 | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_3 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z +
                 (1 + absVD.m.z + RT.z.rep1 + mValtot.z | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_3.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z +
                 (1 + absVD.m.z + RT.z.rep1 | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_4 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 +
                 (1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_4.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 +
                 (1 + absVD.m.z + RT.z.rep1 + Conf.z.rep1 | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_5 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + meanVD.z +
                 (1 + absVD.m.z + RT.z.rep1 + mValtot.z | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_5.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + meanVD.z +
                 (1 + absVD.m.z + RT.z.rep1 | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_6 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + sdrAll.z +
                 (1 + absVD.m.z + RT.z.rep1 + mValtot.z + sdrAll.z | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_6.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + sdrAll.z +
                 (1 + absVD.m.z + RT.z.rep1 + sdrAll.z | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_7 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + meanVD.z +
                 (1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_7.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + meanVD.z +
                 (1 + absVD.m.z + RT.z.rep1 + Conf.z.rep1 | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_8 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + sdrAll.z +
                 (1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + sdrAll.z | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_8.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + sdrAll.z +
                 (1 + absVD.m.z + RT.z.rep1 + Conf.z.rep1 + sdrAll.z | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_9 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.c.all.z.rep1 +
                 (1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.c.all.z.rep1 | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_9.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.c.all.z.rep1 +
                 (1 + absVD.m.z + RT.z.rep1 + p.c.all.z.rep1 | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_10 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.c.ind.z.rep1 +
                 (1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.c.ind.z.rep1 | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_10.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.c.ind.z.rep1 +
                   (1 + absVD.m.z + RT.z.rep1 + p.c.ind.z.rep1 | subject),
                 data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                 family = binomial(link = "logit"),
                 control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_11 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.all.z.rep1 +
                 (1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.all.z.rep1 | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_11.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.all.z.rep1 +
                   (1 + absVD.m.z + RT.z.rep1 + p.eff.all.z.rep1 | subject),
                 data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                 family = binomial(link = "logit"),
                 control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_12 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.ind.z.rep1 +
                  (1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.ind.z.rep1 | subject),
                data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                family = binomial(link = "logit"),
                control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_12.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + p.eff.ind.z.rep1 +
                    (1 + absVD.m.z + RT.z.rep1 + p.eff.ind.z.rep1 | subject),
                  data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                  family = binomial(link = "logit"),
                  control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_13 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + p.c.all.z.rep1 +
                 (1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + p.c.all.z.rep1 | subject),
               data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
               family = binomial(link = "logit"),
               control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_13.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + p.c.all.z.rep1 +
                  (1 + absVD.m.z + RT.z.rep1 + Conf.z.rep1 + p.c.all.z.rep1 | subject),
                data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                family = binomial(link = "logit"),
                control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_14 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + p.c.ind.z.rep1 +
                  (1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + p.c.ind.z.rep1 | subject),
                data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                family = binomial(link = "logit"),
                control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_14.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + p.c.ind.z.rep1 +
                    (1 + absVD.m.z + RT.z.rep1 + Conf.z.rep1 + p.c.ind.z.rep1 | subject),
                  data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                  family = binomial(link = "logit"),
                  control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_15 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + p.eff.all.z.rep1 +
                  (1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + p.eff.all.z.rep1 | subject),
                data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                family = binomial(link = "logit"),
                control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_15.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + p.eff.all.z.rep1 +
                    (1 + absVD.m.z + RT.z.rep1 + Conf.z.rep1 + p.eff.all.z.rep1 | subject),
                  data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                  family = binomial(link = "logit"),
                  control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
 
com_16 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + p.eff.ind.z.rep1 +
                  (1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + p.eff.ind.z.rep1 | subject),
                data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                family = binomial(link = "logit"),
                control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))

com_16.1 <- glmer(com ~ 1 + absVD.m.z + RT.z.rep1 + mValtot.z + Conf.z.rep1 + p.eff.ind.z.rep1 +
                    (1 + absVD.m.z + RT.z.rep1 + Conf.z.rep1 + p.eff.ind.z.rep1 | subject),
                  data = na.omit(dataCOM[-which(dataCOM$subject %in% rm_conf_subjects), ]),
                  family = binomial(link = "logit"),
                  control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))






comparison_com <- anova(com_0, com_1, com_2, com_3, com_3.1, com_4, com_4.1, com_5, com_5.1,
                        com_6, com_6.1, com_7, com_7.1, com_8, com_8.1)
comparison_com2 <- anova(com_0, com_1, com_2, com_3, com_3.1, com_4, com_4.1, com_5, com_5.1,
                        com_6, com_6.1, com_7, com_7.1, com_8, com_8.1, com_9, com_9.1, com_10, com_10.1,
                        com_11, com_11.1, com_12, com_12.1, com_13, com_13.1, com_14, com_14.1, com_15, com_15.1,
                        com_16, com_16.1)
comparison_com_p <- anova(com_9, com_9.1, com_10, com_10.1,
                         com_11, com_11.1, com_12, com_12.1, com_13, com_13.1, com_14, com_14.1, com_15, com_15.1,
                         com_16, com_16.1)
comparison_com$AIC
# CONCLUSION: Model 7.1 is best
formula <- c("{COM $\\sim$ 1}", "{COM $\\sim$ absVD}", "{COM $\\sim$ absVD + RT}", "{COM $\\sim$ absVD + RT + SV}", "{model 3 without SV in random effects}",
             "{COM $\\sim$ absVD + RT + SV + Conf}", "{model 4 without SV in random effects}", "{COM $\\sim$ absVD + RT + SV + mPD}",
             "{model 5 without SV in random effects}", "{COM $\\sim$ absVD + RT + SV + SD}", "{model 6 without SV in random effects}",
             "{COM $\\sim$ absVD + RT + SV + Conf + mPD}", "{model 7 without SV in random effects}", "{COM $\\sim$ absVD + RT + SV + Conf + SD}",
             "{model 8 without SV in random effects}")
negHessian <- c("{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{x}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}")
model <- c(0, 1, 2, 3, 3.1, 4, 4.1, 5, 5.1, 6, 6.1, 7, 7.1, 8, 8.1)
comp_com_table <- cbind(model, formula, round(comparison_com$AIC, 3), negHessian)
colnames(comp_com_table) <- c("{model}", "{formula}", "{AIC}", "{neg. Hessian}")


# Table with p
formula <- c("{COM $\\sim$ absVD + RT + SV + p$^c_{overall}$}", "{model 9 without SV in random effects}",
             "{COM $\\sim$ absVD + RT + SV + p$^c_{individual}$}", "{model 10 without SV in random effects}",
             "{COM $\\sim$ absVD + RT + SV + p$^e_{overall}$}", "{model 11 without SV in random effects}",
             "{COM $\\sim$ absVD + RT + SV + p$^e_{individual}$}", "{model 12 without SV in random effects}",
             "{COM $\\sim$ absVD + RT + SV + Conf + p$^c_{overall}$}", "{model 13 without SV in random effects}",
             "{COM $\\sim$ absVD + RT + SV + Conf + p$^c_{individual}$}", "{model 14 without SV in random effects}",
             "{COM $\\sim$ absVD + RT + SV + Conf + p$^e_{overall}$}", "{model 15 without SV in random effects}",
             "{COM $\\sim$ absVD + RT + SV + Conf + p$^e_{individual}$}", "{model 16 without SV in random effects}")
negHessian <- c("{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}", "{x}", "{-}", "{-}", "{-}", "{-}", "{-}", "{-}")
model <- c(9, 9.1, 10, 10.1, 11, 11.1, 12, 12.1, 13, 13.1, 14, 14.1, 15, 15.1, 16, 16.1)
comp_com_p_table <- cbind(model, formula, round(comparison_com_p$AIC, 3), negHessian)
colnames(comp_com_p_table) <- c("{model}", "{formula}", "{AIC}", "{neg. Hessian}")











