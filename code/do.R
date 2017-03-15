rm(list=ls()) 
set.seed(100)
packages <- c("ggplot2", "Hmisc", "foreign", "plyr", "RMySQL", "xtable", 
              "data.table", "survey", "reshape2", "mfx", "numDeriv",
              "sandwich", "MASS", "gridExtra", "sampleSelection", "MASS",
              "arm", "speedglm", "boot")
lapply(packages, library, character.only = TRUE)
source("code/func.R")
load("data/mydata.RData")
stata.reg <- data.table(read.dta("output/reg_results.dta"))
stata.sumfit <- as.matrix(read.dta("output/sumfit.dta"))
mainvars <- c("black", "hispanic", "educyr")
x1 <- c("black", "hispanic", "educyr", "other", "female", "fage", "srh",
            "married", "widowed", "separated", "fyear")
x2 <- c(x1, "povcat", "ins")
theme_set(theme_bw())

# DISTRIBUTION OF RX EXPENDITURES ----------------------------------------------
# histogram of expenditures conditional on having a medical condition
p.condexphist <- ggplot(fyc[rxexp <=  3000 & d_mc == 1], aes(x = rxexp)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 100) + xlab("Rx Expenditures")+
  ylab("Count")
ggsave("figs/condexphist.pdf", p.condexphist, height = 4, width = 6.5)

# density of log expenditures conditional on positive expenditures
p.logexpden <- ggplot(fyc[rxuse == 1], aes(x = logrxexp)) + 
  geom_density(kernel = "gaussian", alpha = .2) + xlab("Rx Expenditures") + ylab("Density")
ggsave("figs/logexpden.pdf", p.logexpden, height = 4, width = 6.5)

# distribution of expenditures
p.exphist <- ggplot(fyc[rxexp <=  3000],  aes(x = rxexp)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 100) + xlab("Rx Expenditures")+
  ylab("Count")
ggsave("figs/exphist.pdf", p.exphist, height = 4, width = 6.5)

# SUMMARY STATISTICS ---------------------------------------------------------------
sumvars <- c("rxexp", "rxuse", "d_mc", "white", "other", "black", "hispanic", "educyr", 
             "srh1", "srh2", "srh3", "srh4", "srh5", "poor", "nearpoor", "lowincome",
             "midincome", "highincome", "ins", "age", "female")

# calculations
sumstats <- matrix(NA, nrow = length(sumvars), ncol = 6)
sumstats[, 1] <- as.numeric(fyc[, lapply(as.list(.SD)[sumvars], mean, na.rm = TRUE)])
sumstats[, 2] <- as.numeric(fyc[, lapply(as.list(.SD)[sumvars], sd, na.rm = TRUE)])
sumstats[, 3] <- as.numeric(fyc[, lapply(as.list(.SD)[sumvars], min, na.rm = TRUE)])
sumstats[, 4] <- as.numeric(fyc[, lapply(as.list(.SD)[sumvars], max, na.rm = TRUE)])
sumstats[, 5] <- as.numeric(fyc[, lapply(as.list(.SD)[sumvars], weighted.mean, 
                                         w = perwt, na.rm = TRUE)])
sumstats[, 6] <- as.numeric(fyc[, lapply(as.list(.SD)[sumvars], wtd.quantile, 
                                         weights = perwt, prob = .5)])
rownames(sumstats) <- c("Rx expenditures", "Purchased a prescription",
                        "Rx medical condition", "White", "Other race", "Black", "Hispanic",
                    "Years of education", "Health status excellent", 
                    "Heath status very good", "Health status good",
                    "Health status fair", "Health status poor", "Poor", "Near poor",
                    "Low income", "Middle income", "High income", "Insured",
                    "Age", "Female")

# making digits correct
sumstats[, 1] <- formatC(sumstats[, 1], format = "f", digits = 3)
sumstats[, 2] <- formatC(as.numeric(sumstats[, 2]), format = "f", digits = 3)
sumstats[, 4] <- formatC(as.numeric(sumstats[, 4]), format = "d", big.mark = ",")
sumstats[, 5] <- formatC(as.numeric(sumstats[, 5]), format = "f", digits = 3)
sumstats[, 6] <- formatC(as.numeric(sumstats[, 6]), format = "f", digits = 0)

# print table
print(xtable(sumstats), 
      include.rownames = TRUE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      hline.after = NULL,
      file = "tables/sumstats.txt")

# UTILIZATION BY DISEASE GROUP SUMMARY STATS -----------------------------------
util.means <- fyc[, lapply(.SD, mean, na.rm = TRUE), by = d_mc ,
                  .SDcols= c("rxuse","rxtot", "rxexp")]
util.means <- t(util.means)[-1, ]
rownames(util.means) <- c("Proportion using prescibed medications",
                          "Number of prescriptions filled",
                          "Rx expenditures")
print(xtable(util.means, digits = 3), 
      include.rownames = TRUE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      hline.after = NULL,
      file = "tables/utilbymc.txt")

# UNCONDITIONAL UTILIZATION BY RACE AND YEAR FIGURE ----------------------------
util.race <- fyc[, lapply(.SD, mean, na.rm = TRUE),
                    by = "myrace",
                    .SDcols = c("rxexp", "rxuse")]
util.raceyr <- fyc[, lapply(.SD, mean, na.rm = TRUE),
                          by = c("myrace", "year"),
                          .SDcols = c("rxexp", "rxuse")]
util.raceyr <- melt(util.raceyr, id = c("myrace", "year"),
                         variable.name = "rxtype", value.name = "util")
util.raceyr[, myrace := factor(util.raceyr$myrace)]
util.raceyr[, rxtype := revalue(util.raceyr$rxtype, 
                                   c("rxexp" = "Rx expenditures", 
                                   "rxuse" = "Purchased a prescription"))]
p.util.raceyr <- ggplot(data = util.raceyr[myrace != "Other"], 
                          aes(x = year, y = util, color = myrace)) +
                   geom_point() + geom_line() +
                   facet_wrap(~rxtype, scales = "free_y") +
                   xlab("Year")  + 
                   scale_colour_discrete(name = "") + theme_bw() +
                   theme(legend.position="bottom", axis.title.y=element_blank()) 
ggsave("figs/util_raceyr.pdf", p.util.raceyr, height = 4, width = 6.5)
outlier <- summary(fyc[fyc$other == 1 & fyc$year == 2011, rxexp])

# UNCONDITIONAL UTILIZATION BY EDUCATION AND YEAR FIGURE -----------------------
util.educ <- fyc[!is.na(educcat), lapply(.SD, mean, na.rm = TRUE),
                      by = "educcat",
                      .SDcols = c("rxexp", "rxuse")]
util.educbyyr <- fyc[!is.na(educcat), lapply(.SD, mean, na.rm = TRUE),
                        by = c("educcat", "year"),
                        .SDcols = c("rxexp", "rxuse")]
util.educbyyr <- melt(util.educbyyr, id = c("educcat", "year"),
                         variable.name = "rxtype", value.name = "util")
util.educbyyr$rxtype <- revalue(util.educbyyr$rxtype, 
                                   c("rxexp" = "Rx expenditures", 
                                     "rxuse" = "Purchased a prescription"))
p.util.educbyyr <- ggplot(data = util.educbyyr, 
                         aes(x = year, y = util, color = educcat)) +
                  geom_point() + geom_line() +
                  facet_wrap(~rxtype, scales = "free_y") +
                  xlab("Year")  + 
                  scale_colour_discrete(name = "Years of education") + theme_bw() +
                  theme(legend.position="bottom", 
                        axis.title.y=element_blank()) 
ggsave("figs/util_educbyyr.pdf", p.util.educbyyr, height = 4, width = 6.5)

# UNCONDITIONAL RX DISEASE GROUPS BY RACE AND EDUCATION ------------------------
mc.race <- fyc[, lapply(.SD, mean, na.rm = TRUE), by = myrace ,
                   .SDcols= c("d_mc","mcnum")]
mc.race <- mc.race[myrace != "Other"]
setnames(mc.race, c("myrace"), c("type"))
mc.educ <- fyc[, lapply(.SD, mean, na.rm = TRUE), by = educcat ,
               .SDcols= c("d_mc","mcnum")] 
mc.educ <- mc.educ[order(mc.educ$educcat),]
setnames(mc.educ, c("educcat"), c("type")) 
racelab <- data.frame(type = "\\textit{Race/Ethnicity}", d_mc = NA, mcnum = NA)
educlab <- data.frame(type = "\\textit{Years of education}", d_mc = NA, mcnum = NA)
blank <- data.frame(type = "", d_mc = NA, mcnum = NA)
mcsummary <- rbind(racelab, mc.race, blank, educlab, mc.educ[-5, ])
print(xtable(mcsummary, digits = 3), 
      include.rownames = FALSE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      hline.after = NULL,
      file = "tables/mcsummary.txt")

# DATA FOR MODELS --------------------------------------------------------------
fyc.m <- fyc[rxexp < 2e6, c("rxuse", "rxexp", "mcnum", "d_mc",
                            "rxuse_diab", "rxexp_diab", "rxuse_chol",
                            "rxexp_chol", "rxuse_depr", "rxexp_depr",
                            x2, rxcatnames), with = FALSE]
fyc.m <- fyc.m[complete.cases(fyc.m)]
fyc.m <- fyc.m[, logrxexp := ifelse(rxexp > 0, log(rxexp), NA)]
fyc.m[, logrxexp_diab := ifelse(rxexp_diab > 0, log(rxexp_diab), NA)]
fyc.m[, logrxexp_depr := ifelse(rxexp_depr > 0, log(rxexp_depr), NA)]
fyc.m[, logrxexp_chol := ifelse(rxexp_chol > 0, log(rxexp_chol), NA)]

# TWO PART MODEL PART 2: MODEL COMPARISONS -------------------------------------
# without disease group
exp.lm1 <- lm(Fm("rxexp", x1), data = fyc.m[rxuse == 1])
logexp.lm1 <- lm(Fm("logrxexp", x1), data = fyc.m[rxuse == 1])
logexp.lm1.diag <- LogolsDiagDT(logexp.lm1, group = c("fage"))

# controlling for disease group
c.exp.lm1 <- lm(Fm("rxexp", c(x1, rxcatnames)), data = fyc.m[rxuse == 1])
c.logexp.lm1 <- lm(Fm("logrxexp", c(x1, rxcatnames)), data = fyc.m[rxuse == 1])
c.logexp.lm1.diag <- LogolsDiagDT(c.logexp.lm1, group = c(rxcatnames))

# TWO PART MODEL PART 2: RESIDUALS ---------------------------------------------
# ols 
exp.lm1.diag <- data.table(res = exp.lm1$res, fit = exp.lm1$fitted.values)
p.exp.lm1.res <- ggplot(exp.lm1.diag[res < 250000], 
                        aes(y = res, x = fit)) + geom_jitter() + geom_smooth()+
  xlab("Fitted value") + ylab("Residual") # residual is actual - fitted
ggsave("figs/lm_y_res.pdf", p.exp.lm1.res, height = 4, width = 6)

# ols log scale 
logexp.lm1.stres <- studres(logexp.lm1) 
p.logexp.lm1.res <- ggplot(logexp.lm1.diag, 
                           aes(y = res, x = fit)) + geom_jitter() + 
  xlab("Fitted value") + ylab("Residual") + geom_smooth()
ggsave("figs/lm_logy_res.pdf", p.logexp.lm1.res, height = 4, width = 6)

p.logexp.lm1.stres.den <- ggplot(data.frame(stres = logexp.lm1.stres), aes(x = stres)) + 
  geom_density(kernel = "gaussian", alpha = .2) + xlab("Studentized residuals") + ylab("Density")
ggsave("figs/lm_logy_stres_den.pdf", p.logexp.lm1.stres.den, height = 4, width = 6.5)

p.logexp.lm1.qqplot <- ggplot(data.frame(stres = logexp.lm1.stres), aes(sample = stres)) + 
  stat_qq(size = .75, color="firebrick2") + geom_abline() +
  xlab("Quantiles of standard normal") + ylab("Quantiles of studentized residuals") 
ggsave("figs/lm_logy_qqplot.pdf", p.logexp.lm1.qqplot, height = 4, width = 6.5)

# ols conditional
c.exp.lm1.diag <- data.table(res = c.exp.lm1$res, fit = c.exp.lm1$fitted.values)
p.c.exp.lm1.res <- ggplot(c.exp.lm1.diag[res < 250000], 
                        aes(y = res, x = fit)) + geom_jitter() + geom_smooth()+
  xlab("Fitted value") + ylab("Residual") # residual is actual - fitted
ggsave("figs/lm_c_y_res.pdf", p.c.exp.lm1.res, height = 4, width = 6)

# ols conditional log scale
p.c.logexp.lm1.res <- ggplot(c.logexp.lm1.diag, 
                           aes(y = res, x = fit)) + geom_jitter() + 
  xlab("Fitted value") + ylab("Residual") + geom_smooth()
ggsave("figs/lm_c_logy_res.pdf", p.c.logexp.lm1.res, height = 4, width = 6)

# TWO PART MODEL PART 2: SIMULATED DATA ----------------------------------------
n <- nrow(logexp.lm1$model) 

# lognormal simulation
lognorm.sim <-  exp(rnorm(n, logexp.lm1$fitted.values, summary(logexp.lm1)$sigma))
c(mean(exp(logexp.lm1$model[, 1])), mean(lognorm.sim))
lognorm.simdat <- data.table(x = log(c(exp(logexp.lm1$model[, 1]), lognorm.sim)),
                             mod = rep(c("y", "ysim"), each = n))
p.lognorm.sim <- ggplot(lognorm.simdat, aes(x = x, linetype = mod)) +
  geom_density(alpha = .2) +
  xlab("Prescription Drug Expenditures ($2012)") + ylab("Density") +
  theme(legend.position="bottom") + labs(linetype="")
ggsave("figs/lognorm_sim.pdf", p.lognorm.sim, width = 5, height = 5)
rm(lognorm.simdat, lognorm.sim)

# TWO PART MODEL PART 2: MODEL COMPARISON SUMMARY ------------------------------
# predicted mean for log ols intercept model
logexp.lm.int <- lm(logrxexp ~ 1, data = fyc[rxuse == 1])
print(mean(exp(logexp.lm.int$model[, 1])))
print(mean(LogolsMean(logexp.lm.int)))
print(mean(LogolsMean(logexp.lm.int, error = "Duan"))) # matches y exactly

# predicted means
means <- c(mean(exp.lm1$fitted.values),
           apply(logexp.lm1.diag[, .(rawfit_lnhomo, rawfit_dhomo, rawfit_dhet)], 2, mean))
           
c.means <- c(mean(c.exp.lm1$fitted.values),
           apply(c.logexp.lm1.diag[, .(rawfit_lnhomo, rawfit_dhomo, rawfit_dhet)], 2, mean))

# r2
r2 <- c(summary(exp.lm1)$r.squared, 
        apply(logexp.lm1.diag[, .(rawfit_lnhomo, rawfit_dhomo, rawfit_dhet)], 2, 
              function (x) cor(logexp.lm1.diag$rxexp, x)^2))

c.r2 <-  c(summary(c.exp.lm1)$r.squared, 
           apply(c.logexp.lm1.diag[, .(rawfit_lnhomo, rawfit_dhomo, rawfit_dhet)], 2, 
                 function (x) cor(logexp.lm1.diag$rxexp, x)^2))

# combine results
sumfit <- t(rbind(c(means, stata.sumfit[, c("heck2stepmean", "heckmlmean")]),
               c(r2, stata.sumfit[, c("heck2stepr2", "heckmlr2")])))
rownames(sumfit) <- c("OLS on y", "OLS on ln(y) normal homoskedastic",
                      "OLS on ln(y) Duan homoskedastic", "OLS on ln(y) Duan heteroskedastic",
                      "Selection 2-Step on ln(y)", "Selection MLE on ln(y)")
print(xtable(sumfit), 
      include.rownames = TRUE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      hline.after = NULL,
      file = "tables/sumfit.txt")

# TWO PART MODEL PART 2: CROSS-VALIDATION --------------------------------------
# cross validation
if(!file.exists("output/cv.Rdata")){
  cv.exp.lm1 <- RCvLm(data = exp.lm1$model, f = formula(exp.lm1), k = 5, R = 10)
  cv.logexp.lm1 <- RCvLoglm(data = logexp.lm1$model, f = formula(logexp.lm1),
                            k = 5, group = "fage", R = 10)
  cv.c.exp.lm1 <- RCvLm(data = c.exp.lm1$model, f = formula(c.exp.lm1), k = 5, R = 10)
  #cv.c.logexp.lm1 <- RCvLoglm(data = c.logexp.lm1$model, f = formula(c.logexp.lm1),
  #                          k = 5, group = rxcatnames, R = 10)
  save(cv.exp.lm1, cv.logexp.lm1, cv.c.exp.lm1, file = "output/cv.Rdata")
}
load("output/cv.Rdata")

# plot
cv.dt <- data.table(rmse = c(cv.logexp.lm1$rmse), 
                    Model = factor(rep(c("Duan\n homoskedastic", "Normal\n homoskedastic",
                          "Duan\n heteroskedastic"), each = nrow(cv.logexp.lm1$rmse))))
p.cv <- ggplot(cv.dt, aes(y = rmse, x = Model)) + geom_boxplot() + ylab("RMSE")
ggsave("figs/cv.pdf", p.cv, width = 5, height = 5)

# TWO PART MODEL FOR OVERALL EXPENDITURES --------------------------------------
# logistic regression
dexp.logit1 <- glm(Fm("rxuse", x1), data = fyc.m, family = binomial())
dexp.logit2 <- glm(Fm("rxuse", x2), data = fyc.m, family = binomial())
dexp.logit1.ame <- MyAmeGlm(dexp.logit1)
dexp.me.mean <-  mean(fyc.m$rxuse) * (1 - mean(fyc.m$rxuse))

# linear regression 
logexp.lm0 <- speedlm(Fm("logrxexp", c(mainvars, "other", "female", "fage", "married",
                                  "widowed", "separated", "fyear")),
                 data = fyc.m[rxuse == 1]) # examining impact of self-reported health on results
logexp.lm2 <- lm(Fm("logrxexp", x2), data = fyc.m)

# table of results
coefutil <- RegTable(models = list(dexp.logit1, dexp.logit2, logexp.lm1, logexp.lm2),
                    robust = c(F, F, T, T))
coefutil[8, 4:5] <- formatC(rep(mean(fyc.m[rxuse ==1 , rxexp]), 2), 
                           format = "f", digits = 2)
tmp.est1 <- list(coef(dexp.logit1)[mainvars], coef(logexp.lm1)[mainvars])
tmp.est2 <- list(coef(dexp.logit2)[mainvars], coef(logexp.lm2)[mainvars])
chutil <- ChTable(est1 = tmp.est1, est2 = tmp.est2)
utilreg <- cbind(coefutil[, 1:3], chutil[, 1],
                         coefutil[, 4:5], chutil[, 2])
print(xtable(utilreg), 
      include.rownames = FALSE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      hline.after = NULL,
      file = "tables/utilreg.txt")

# reduction in coefficients due to access factors
1 - (coef(dexp.logit2)[mainvars]/coef(dexp.logit1)[mainvars])
1 - (coef(logexp.lm2)[mainvars]/coef(logexp.lm1)[mainvars])

# SELECTION MODELS vs. TWO PART MODEL FOR OVERALL EXPENDITURES -----------------
# two-part model
dexp.probit <- speedglm(Fm("rxuse", x1), data = fyc.m, family = binomial(link = probit))
dexp.probit.coef <- coeftest(dexp.probit)[mainvars, 1:2]
logexp.lm1.coef <- coeftest(logexp.lm1)[mainvars, 1:2]

# heckman 2-step
dexp.heck.2step.coef <- as.matrix(stata.reg[mod == "heck2step" & grepl("rxuse",var), 
                              .(coef, stderr)])
logexp.heck.2step.coef <- as.matrix(stata.reg[mod == "heck2step" & grepl("logrxexp",var), 
                              .(coef, stderr)])

# selection ml 
dexp.heck.ml.coef <- as.matrix(stata.reg[mod == "heckml" & grepl("rxuse",var), 
                              .(coef, stderr)])
logexp.heck.ml.coef <- as.matrix(stata.reg[mod == "heckml" & grepl("logrxexp",var), 
                                .(coef, stderr)])

# table
utilreg.heck <- EstTable(est = list(dexp.probit.coef, logexp.lm1.coef, 
                                    dexp.heck.2step.coef, logexp.heck.2step.coef,
                                    dexp.heck.ml.coef, logexp.heck.ml.coef))
print(xtable(utilreg.heck, digits = 3), 
      include.rownames = FALSE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      hline.after = NULL,
      file = "tables/utilreg_heck.txt")

# QUANTITIES OF INTEREST -------------------------------------------------------
## PREDICTIONS
# predictions at sample means
pred <- ExpAtMean(dexp.logit1, logexp.lm1, mean(fyc.m$rxuse),
          mean(fyc.m[rxuse == 1, rxexp]), mean(logexp.lm1.diag$ev_err))

# plot
p.pred <- ggplot(pred, aes(x = educyr, y = pred, col = race)) + geom_point() +
  geom_line() + xlab("Years of education") + ylab("Predicted expenditures") + 
  ggtitle("(a) Predicted expenditures \n with basic controls") + theme(legend.title=element_blank()) +
  theme(legend.position="bottom") + scale_x_continuous(breaks=c(9,12,15))

## AVERAGE MARGINAL EFFECTS
# effects
ame1 <- Ame2pFun(fyc.m, 1:nrow(fyc.m), f1 = Fm("rxuse", x1), f2 = Fm("logrxexp", x1))  
ame2 <- Ame2pFun(fyc.m, 1:nrow(fyc.m), f1 = Fm("rxuse", x2), f2 = Fm("logrxexp", x2)) 
if(!file.exists("output/boot.RData")){ # takes approximately 7 horus to run!
  ptm <- proc.time()
  ame1.boot <- Ame2pBoot(data = fyc.m, f1 = Fm("rxuse", x1), f2 = Fm("logrxexp", x1), B = 1100)
  ame2.boot <- Ame2pBoot(data = fyc.m, f1 = Fm("rxuse", x2), f2 = Fm("logrxexp", x2), B = 1100)
  proc.time() - ptm 
  save(ame1.boot, ame2.boot, file = "output/boot.RData")
}
load("output/boot.RData")
ame1.bias <- apply(ame1.boot, 2, mean) - ame1
ame2.bias <- apply(ame2.boot, 2, mean) - ame2

# plot
p.dat <- data.table(est = c(ame1, ame2),
                    lower = c(apply(ame1.boot, 2, quantile, probs = .025),
                              apply(ame2.boot, 2, quantile, probs = .025)),
                    upper = c(apply(ame1.boot, 2, quantile, probs = .975),
                              apply(ame2.boot, 2, quantile, probs = .975)),    
                    Variable = rep(c("Black", "Hispanic", "Education"), 2),
                    Controls = rep(c("Basic", "Adds access"), each = 3))
p.dat[Variable == "Education", ':=' (est = est * 4, lower = lower * 4, 
                                     upper = upper * 4)]
p.dat[, Variable := factor(Variable, levels = c("Black", "Hispanic", "Education"),
                           labels = c("Black", "Hispanic", "4 yrs of educ"))]
p.ame <- ggplot(p.dat, aes(x = Variable, y = est,
                           ymin = lower, ymax = upper)) + 
  geom_pointrange(aes(col = Controls), position = position_dodge(width = 0.25)) +
  scale_y_continuous(breaks = seq(round_any(max(p.dat$est), 100, f = ceiling), 
                                  round_any(min(p.dat$est), 100, f = floor), -50)) +
  ylab("AME/AIE") + theme(legend.position="bottom") +
  ggtitle("(b) Average marginal/incremental \n effects")

## TOTAL PLOT
p.qoi <- arrangeGrob(p.pred, p.ame, ncol = 2)
ggsave("figs/qoi.pdf", p.qoi, height = 4.25, width = 7)

# DISTRIBUTION OF MEDICAL CONDITIONS -------------------------------------------
p.mchist <- ggplot(fyc, aes(x = mcnum)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 1) +
  xlab("Number of Rx conditions") + ylab("Count") +
  theme_bw()
ggsave("figs/mchist.pdf", p.mchist, height = 4, width = 6.5)

mean.mcnum <- mean(fyc$mcnum)
var.mcnum <- var(fyc$mcnum)

simpois <- data.frame(mcnum = rpois(nrow(fyc), mean.mcnum))
p.simpois <- ggplot(simpois, aes(x = mcnum)) + 
  geom_histogram(color = "white", fill = "grey", binwidth = 1) +
  xlab("Number of Rx conditions") +
  theme_bw()

simnb <- data.frame(mcnum = rnbinom(nrow(fyc), mu = mean(fyc$mcnum), 
                 size = (var.mcnum - mean.mcnum)/mean.mcnum^2))
p.simnb <- ggplot(simnb, aes(x = mcnum)) + 
  geom_histogram(color = "white", fill = "grey", binwidth = 1) +
  xlab("Number of Rx conditions") +
  theme_bw()

# REGRESSION ESTIMATES OF AGGREGATE MEDICAL CONDITIONS -------------------------
# logit
mc.logit1 <- glm(Fm("d_mc", x1), data = fyc.m, family = binomial())
mc.logit2 <- glm(Fm("d_mc", x2), data = fyc.m, family = binomial())
mc.logit1.coef <- coef(mc.logit1)[mainvars]
mc.logit2.coef <- coef(mc.logit2)[mainvars]
mc.logit1.ame <- MyAmeGlm(mc.logit1, link= "logit")
mc.logit2.ame <- MyAmeGlm(mc.logit2, link= "logit")
mc.me.mean <- mean(fyc.m$rxuse) * (1 - mean(fyc.m$rxuse)) # marginal effect at sample mean

#poisson
mcnum.pois <- glm(Fm("mcnum", x1), data = fyc, family = poisson())
test.df <- data.frame(mcnum = mcnum.pois$model$mcnum,
                      mcnumhat = mcnum.pois$fitted.values)
test.df$ystar <- with(test.df, ((mcnum - mcnumhat)^2 - mcnum)/mcnumhat)
test.df$ystar2 <- with(test.df, ((mcnum - mcnumhat)^2 - mcnum))
test.disp <- lm(ystar~-1 + mcnumhat, test.df)

# negative binomial
mcnum.nb1 <- glm.nb(Fm("mcnum", x1), data = fyc.m)
mcnum.nb2 <- glm.nb(Fm("mcnum", x2), data = fyc.m)
mcnum.nb1.coef <- coef(mcnum.nb1)[mainvars]
mcnum.nb2.coef <- coef(mcnum.nb2)[mainvars]
#mcnum.nb1.amemc <- MyAmeMC(model = mcnum.nb1, x = model.matrix(mcnum.nb1), nsims = 100,
#                        link = "log")
mcnum.nb1.ame <- MyAmeGlm(mcnum.nb1, link= "log")
mcnum.nb2.ame <- MyAmeGlm(mcnum.nb2, link= "log")

## table of results
coefmc <- RegTable(models = list(mc.logit1, mc.logit2, mcnum.nb1, mcnum.nb2))
chmc <- ChTable(est1 = list(mc.logit1.coef, mcnum.nb1.coef),
                est2 = list(mc.logit2.coef, mcnum.nb2.coef))
mcreg <- cbind(coefmc[, 1:3], chmc[, 1],
                 coefmc[, 4:5], chmc[, 2])
print(xtable(mcreg), 
      include.rownames = FALSE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      hline.after = NULL,
      file = "tables/mcreg.txt")

# remove
rm(mc.logit1, mc.logit2, mcnum.pois, mcnum.nb1, mcnum.nb2)

# MOST COMMON RX CONDITIONS ----------------------------------------------------
mcprop <- t(fyc[, lapply(.SD, mean), .SDcols= rxcatnames])
mcprop.white <- t(fyc[fyc$white == 1, lapply(.SD, mean), .SDcols= rxcatnames])
mcprop.black <- t(fyc[fyc$black == 1, lapply(.SD, mean), .SDcols= rxcatnames])
mcprop.hisp <- t(fyc[fyc$hispanic == 1, lapply(.SD, mean), .SDcols= rxcatnames])
mc.table <- cbind(mcprop, mcprop.white, mcprop.black, mcprop.hisp)
mc.table <- cbind(seq(1, nrow(mc.table)), mc.table)
colnames(mc.table) <- c("rxcat", "prop", "prop.white", "prop.black", "prop.hisp")
mc.table <- merge(mc.table, lookup.rxcat, by = "rxcat")
mc.table <- mc.table[order(-mc.table$prop),]
mc.table <- MoveMe(mc.table, c("rxcatdesc"), "before", "prop")
print(xtable(mc.table[1:20, -1 ], digits = 3), 
      include.rownames = FALSE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      hline.after = NULL,
      file = "tables/mctable.txt")

# REGRESSION ESTIMATES OF SPECIFIC MEDICAL CONDITIONS --------------------------
beta2 <- matrix(NA, nrow = length(rxcatnames), ncol = 3)
se1 <- se2 <- beta1 <- beta2 
#ame <- beta
#ame.se <- beta
for (i in 1:length(rxcatnames)){
  rxcat.logit1 <- speedglm(Fm(rxcatnames[i], x1), data = fyc.m)
  rxcat.logit2 <- speedglm(Fm(rxcatnames[i], x2), data = fyc.m)
  beta1[i, ] <- coef(rxcat.logit1)[mainvars]
  beta2[i, ] <- coef(rxcat.logit2)[mainvars] 
  se1[i, ] <- sqrt(diag(vcov(rxcat.logit1))[mainvars])
  se2[i, ] <- sqrt(diag(vcov(rxcat.logit2))[mainvars])
  cat(i)
}

p.dat <- rbind(data.table(est = c(beta1), se = c(se1), Controls = "Basic controls",
                    var = rep(c("Black", "Hispanic", "Education"), each = 70)),
               data.table(est = c(beta2), se = c(se2), Controls = "Adds access controls",
                          var = rep(c("Black", "Hispanic", "Education"), each = 70)))
p.dat[, rxcat := rep(seq(1, 70), nrow(p.dat)/70)]
p.dat <- merge(p.dat, mc.table[, c("rxcat", "rxcatdesc", "prop")], by = "rxcat")
p.dat <- p.dat[order(prop), ]
p.dat[, id := .GRP, by = rxcat]
p.dat[, idlab:= factor(id, labels = unique(rxcatdesc))]
p.dat <- p.dat[order(-prop), ]
p.dat[var == "Education", est := est * 4]
p.dat[var == "Education", se := se * 4]
p.dat[, var := factor(var, levels = c("Black", "Hispanic", "Education"), 
                         labels = c("Black", "Hispanic", "Education"))]
p.rxcatreg <- ggplot(p.dat[id > 50], 
                     aes(x= idlab, y= est,
                        ymin = est - qnorm(.975) * se,
                        ymax = est + qnorm(.975) * se)) +  
   geom_pointrange(aes(col = Controls), position = position_dodge(width=0.50)) +
   coord_flip() + geom_hline(aes(x=0), lty=2)+ xlab("") + ylab("Coefficient") +
   facet_wrap(~var)+ theme_bw() + theme(axis.text.y  = element_text(size=8)) +
  theme(legend.position="bottom")
ggsave("figs/rxcatreg.pdf", p.rxcatreg, height = 6, width = 9)

# REGRESSION ESTIMATES OF CONDITIONAL UTILIZATION ------------------------------
# logit regression
c.dexp.logit1 <- glm(Fm("rxuse", c(x1, rxcatnames)), 
                    data = fyc.m, family = binomial())
c.dexp.logit2 <- glm(Fm("rxuse", c(x2, rxcatnames)), 
                    data = fyc.m, family = binomial())
c.dexp.logit1.coef <- coef(c.dexp.logit1)[mainvars]
c.dexp.logit2.coef <- coef(c.dexp.logit2)[mainvars]

# log expenitures ols
c.logexp.lm2 <- lm(Fm("logrxexp", c(x2, rxcatnames)), data = fyc.m[rxuse == 1])

## table of results
# panel a
coef1.rxcats <- RegTable(models = list(dexp.logit1, c.dexp.logit1,
                                       dexp.logit2, c.dexp.logit2))
tmp.est1 <- list(coef(dexp.logit1)[mainvars], coef(dexp.logit2)[mainvars])
tmp.est2 <- list(c.dexp.logit1.coef, c.dexp.logit2.coef)
ch1.rxcats <- ChTable(est1 = tmp.est1, est2 = tmp.est2)
utilreg1.rxcats <- cbind(coef1.rxcats[, 1:3], ch1.rxcats[, 1],
                         coef1.rxcats[, 4:5], ch1.rxcats[, 2])
print(xtable(utilreg1.rxcats, digits = 3), 
      include.rownames = FALSE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      hline.after = NULL,
      file = "tables/utilreg1_rxcats.txt")

# panel b
coef2.rxcats <- RegTable(models = list(logexp.lm1, c.logexp.lm1,
                                       logexp.lm2, c.logexp.lm2),
                            robust = rep(T, 4))
coef2.rxcats[8, 2:5] <- formatC(rep(mean(fyc.m[rxuse ==1 , rxexp]), 2), 
                           format = "f", digits = 2)
tmp.est1 <- list(coef(logexp.lm1)[mainvars], coef(logexp.lm2)[mainvars])
tmp.est2 <- list(coef(c.logexp.lm1)[mainvars], coef(c.logexp.lm2)[mainvars])
ch2.rxcats <- ChTable(est1 = tmp.est1, est2 = tmp.est2)
utilreg2.rxcats <- cbind(coef2.rxcats[, 1:3], ch2.rxcats[, 1],
                         coef2.rxcats[, 4:5], ch2.rxcats[, 2])
print(xtable(utilreg2.rxcats, digits = 3), 
      include.rownames = FALSE, include.colnames = FALSE,
      only.contents = TRUE, sanitize.text.function = identity,
      hline.after = NULL,
      file = "tables/utilreg2_rxcats.txt")

## REMOVE
rm(c.dexp.logit1, c.dexp.logit2)

# REGRESSION ESTIMATES OF UTILIZATION ON SPECIFIC DRUG CLASSES -----------------
fyc.diab <- fyc.m[rxcat05 == 1 | rxcat06 == 1]
fyc.depr <- fyc.m[rxcat27 == 1 | rxcat28 == 1 | rxcat29 == 1 | rxcat30 == 1]
fyc.chol <- fyc.m[rxcat09 == 1]
  
## LOGISTIC REGRESSIONS
# diabetes
print(mean(fyc.diab$rxuse_diab))
diab.logit1 <- glm(Fm("rxuse_diab", x1), data = fyc.diab, family = binomial())
diab.logit2 <- glm(Fm("rxuse_diab", x2), data = fyc.diab, family = binomial())
diab.logit1.coef <- coeftest(diab.logit1)[mainvars, 1:2]
diab.logit2.coef <- coeftest(diab.logit2)[mainvars, 1:2]

# depression
print(mean(fyc.depr$rxuse_depr))
depr.logit1 <- glm(Fm("rxuse_depr", x1), data = fyc.depr, family = binomial())
depr.logit2 <- glm(Fm("rxuse_depr", x2), data = fyc.depr, family = binomial())
depr.logit1.coef <- coeftest(depr.logit1)[mainvars, 1:2]
depr.logit2.coef <- coeftest(depr.logit2)[mainvars, 1:2]

# cholesterol
print(mean(fyc.chol$rxuse_chol))
chol.logit1 <- glm(Fm("rxuse_chol", x1), data = fyc.chol, family = binomial())
chol.logit2 <- glm(Fm("rxuse_chol", x2), data = fyc.chol, family = binomial())
chol.logit1.coef <- coeftest(chol.logit1)[mainvars, 1:2]
chol.logit2.coef <- coeftest(chol.logit2)[mainvars, 1:2]

## EXPENDITURE REGRESSIONS
# diabetes
print(mean(fyc.diab[fyc.diab$rxexp_diab > 0, rxexp_diab]))
diab.lm1 <- lm(Fm("logrxexp_diab", x1), data = fyc.diab)
diab.lm2 <- lm(Fm("logrxexp_diab", x2), data = fyc.diab)
diab.lm1.coef <- coeftest(diab.lm1)[mainvars, 1:2]
diab.lm2.coef <- coeftest(diab.lm2)[mainvars, 1:2]

# depression
print(mean(fyc.depr[fyc.depr$rxexp_depr > 0, rxexp_depr]))
depr.lm1 <- lm(Fm("logrxexp_depr", x1), data = fyc.depr)
depr.lm2 <- lm(Fm("logrxexp_depr", x2), data = fyc.depr)
depr.lm1.coef <- coeftest(depr.lm1)[mainvars, 1:2]
depr.lm2.coef <- coeftest(depr.lm2)[mainvars, 1:2]

# cholesterol
print(mean(fyc.chol[fyc.chol$rxexp_chol > 0, rxexp_chol]))
chol.lm1 <- lm(Fm("logrxexp_chol", x1), data = fyc.chol)
chol.lm2 <- lm(Fm("logrxexp_chol", x2), data = fyc.chol)
chol.lm1.coef <- coeftest(chol.lm1)[mainvars, 1:2]
chol.lm2.coef <- coeftest(chol.lm2)[mainvars, 1:2]

## COEFFICIENT PLOT
p.dat <- data.table(rbind(diab.logit1.coef, depr.logit1.coef, chol.logit1.coef,
               diab.lm1.coef, depr.lm1.coef, chol.lm1.coef,
               diab.logit2.coef, depr.logit2.coef, chol.logit2.coef,
               diab.lm2.coef, depr.lm2.coef, chol.lm2.coef))
setnames(p.dat, c("est", "se"))
p.dat[, var := rep(c("Black", "Hispanic", "Education"), nrow(p.dat)/3)]
p.dat[, var := factor(var, labels = c("Education", "Hispanic", "Black"),
                      levels = c("Education", "Hispanic", "Black"))]
p.dat[, disease := rep(rep(c("Anti-diabetics", "Antidepressants", "Anti-cholesterol"),
                           each = 3), 2)]
p.dat[, Model := rep(rep(c("Logit", "OLS"), each = nrow(p.dat)/4), 2)]
p.dat[, Controls := rep(c("Basic controls", "Adds access controls"), each = nrow(p.dat)/2)]
p.classreg1 <- ggplot(p.dat[Model == "Logit"], 
                     aes(x= var, y= est,
                         ymin = est - qnorm(.975) * se,
                         ymax = est + qnorm(.975) * se)) +  
  geom_pointrange(aes(col = Controls), position = position_dodge(width=0.50)) +
  coord_flip() + geom_hline(aes(x=0), lty=2)+ xlab("") + ylab("Coefficient") +
  facet_wrap(~disease)+ theme_bw() + theme(axis.text.y  = element_text(size=8)) +
  theme(legend.position="bottom") + ggtitle(paste0("(a) 1*(expenditures > 0) \n"))
p.classreg2 <- ggplot(p.dat[Model == "OLS"], 
                      aes(x= var, y= est,
                          ymin = est - qnorm(.975) * se,
                          ymax = est + qnorm(.975) * se)) +  
  geom_pointrange(aes(col = Controls), position = position_dodge(width=0.50)) +
  coord_flip() + geom_hline(aes(x=0), lty=2)+ xlab("") + ylab("Coefficient") +
  facet_wrap(~disease)+ theme_bw() + theme(axis.text.y  = element_text(size=8)) +
  theme(legend.position="bottom") + ggtitle("(b) ln(expenditures)|expenditures > 0 \n")

# combine plots
pdf("figs/classreg.pdf", height = 6, width = 7, onefile=FALSE)
print(grid_arrange_shared_legend(p.classreg1, p.classreg2))
dev.off()

# MEAN PREDICTED EXPENDITURES FOR SPECIFIC DRUG CLASSES ------------------------
# predicted means and smearing factors
diab.lm1.diag <- LogolsDiagDT(diab.lm1, group = c("fage"), dv = "logrxexp_diab")
c(mean(exp(diab.lm1.diag$logy)), mean(diab.lm1.diag$rawfit_dhet))
depr.lm1.diag <- LogolsDiagDT(depr.lm1, group = c("fage"), dv = "logrxexp_depr")
c(mean(exp(depr.lm1.diag$logy)), mean(depr.lm1.diag$rawfit_dhet))
chol.lm1.diag <- LogolsDiagDT(chol.lm1, group = c("fage"), dv = "logrxexp_chol")
c(mean(exp(chol.lm1.diag$logy)), mean(chol.lm1.diag$rawfit_dhet))

# predictions by education and race/ethnicity
pred.diab <- ExpAtMean(diab.logit1, diab.lm1, mean(fyc.diab$rxuse_diab),
                        mean(fyc.diab[rxuse_diab == 1, rxexp_diab]), 
                       mean(diab.lm1.diag$ev_err))
pred.depr <- ExpAtMean(depr.logit1, depr.lm1, mean(fyc.depr$rxuse_depr),
                       mean(fyc.depr[rxuse_depr == 1, rxexp_depr]), 
                       mean(depr.lm1.diag$ev_err))
pred.chol <- ExpAtMean(chol.logit1, chol.lm1, mean(fyc.chol$rxuse_chol),
                       mean(fyc.chol[rxuse_chol == 1, rxexp_chol]), 
                       mean(chol.lm1.diag$ev_err))
pred.class <- data.table(rbind(pred.diab, pred.depr, pred.chol), 
                   class = rep(c("Anti-diabetics", "Antidepressants", "Anti-cholesterol"),
                               each = nrow(pred.diab)))

# plot
p.pred.class <- ggplot(pred.class, aes(x = educyr, y = pred, col = race)) + geom_point() +
  geom_line() + facet_wrap(~class) +
  xlab("Years of education") + ylab("Predicted expenditures") + 
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom") + scale_x_continuous(breaks=c(9,12,15))
ggsave("figs/class_meanexp.pdf", p.pred.class, height = 4, width = 6.5)
