# MODE FUNCTION ----------------------------------------------------------------
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# MOVE ME FUNCTION -------------------------------------------------------------
MoveMe <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  x
}

# FYC TABLE NAME ---------------------------------------------------------------
FycName <- function(year, type){
  name <- paste0("fyc", year, "_", type)
  return(name)
}

# DOWNLOAD SQL TABLES FUNCTION -------------------------------------------------
MyQuery <- function(conn, vars, varnames, year, sql_table){
  # Queries variables from sql table
  #
  # Args:
  #   conn: sql connection
  #   vars: variables to query
  #   varnames: rename variables from sql table
  #   year: download table from this year and add variable equal to this year
  #   sql_table: identifier for sql table
  #
  # Returns:
  #   data frame from sql query
  for (i in 1:length(vars)){
    if (vars[i] == "NA"){
      vars[i] <- "dupersid"
    }
  }
  df <- dbGetQuery(conn = conn, paste0("SELECT ", paste(vars, collapse = ", "), 
                                         " FROM ", sql_table)) 
  for (i in 1:length(varnames)){
    if (i > 1 & vars[i] == "dupersid"){
        df[, i] <- NA
    }
  }
  names(df) <- varnames
  df$year <- year
  return(df)
}

# GGPLOT MULTIPLE PLOTS SHARED LEGEND ------------------------------------------
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

# FORMULA FOR MODEL ------------------------------------------------------------
Fm <- function(y, xvars){
  return(as.formula(paste(y, "~", paste(xvars, collapse = "+"))))
}

# INVERSE LOGIT FUNCTION -------------------------------------------------------
InvLogit <- function(a){
  # Inverse logit function  transforms continous variable to (0, 1) scale
  #
  # Args:
  #   a: input to logistic function
  #
  # Returns:
  #   Value given input a
  exp(a)/(1 + exp(a))
}

# MODEL DIAGNOSTIC DATA TABLE --------------------------------------------------
LogolsDiagDT <- function(mod, group, dv = "logrxexp"){
  x <-  data.table(fit = mod$fitted.values, res = mod$res, 
                   mod$model)
  setnames(x, dv, "logy")
  x[, rxexp := exp(logy)]
  x[, ev_err := mean(exp(res)), by = group]
  x[, hsigma := sqrt(mean(res^2)), by = group]
  x[, rawfit_dhomo := exp(fit) * mean(exp(res))]
  x[, rawfit_dhet := exp(fit) * ev_err]
  x[, rawfit_lnhomo := exp(fit + summary(mod)$sigma^2/2)]
  x[, rawfit_lnhet:= exp(fit + hsigma^2/2)]
  return(x)
}

# MEAN OF LOGLINEAR MODEL ------------------------------------------------------
LogolsMean <- function(mod, error = "lognormal"){
  if (error == "lognormal"){
    return(exp(mod$fitted.values + summary(mod)$sigma^2/2))
  } else if (error == "Duan"){
      res <- mean(exp(mod$res))
      return(exp(mod$fitted.values) * res)
  }
}

# K-FOLD CROSS VALIDATION LINEAR MODEL -----------------------------------------
CvLm <- function(data, f, k){
  data <- data.table(data)
  data[, id := sample(1:k, nrow(data), replace = TRUE)] 
  folds <- 1:k
  rmse <- rep(NA, k)
  mpe <- rep(NA, k)
  progress.bar <- create_progress_bar("text")
  progress.bar$init(k)
  for (i in 1:k){
    trainingset <- data[id %in% folds[-i]]
    testset <- data[id %in% c(i)]
    lm <- lm(f, trainingset)
    pred <- data.table(k = i, predict(lm,  testset), 
                       testset[, all.vars(f)[1], with = FALSE])
    setnames(pred, c("k", "yhat", "y"))
    rmse[i] <- sqrt(mean((pred$y - pred$yhat)^2))
    mpe[i] <- mean(pred$y - pred$yhat)
    progress.bar$step()
  }  
  return(list(rmse = rmse, mpe = mpe))
}

# REPEATED K-FOLD CROSS VALIDATION LINEAR MODEL --------------------------------
RCvLm <- function(data, f, k, R = 1){
  rmse <- matrix(NA, nrow = R, ncol = k)
  mpe <- matrix(NA, nrow = R, ncol = k)
  for (i in 1:R){
    cv <- CvLm(data, f, k)
    rmse[i, ] <- cv$rmse
    mpe[i, ] <- cv$mpe
    cat(i)
  }
  return(list(rmse = rmse, mpe = mpe))
}

# K-FOLD CROSS VALIDATION LOG OLS MODEL ----------------------------------------
CvLoglm <- function(data, f, k, group){
  data <- data.table(data)
  data[, id := sample(1:k, nrow(data), replace = TRUE)] 
  folds <- 1:k
  rmse <- matrix(NA, nrow = k, ncol = 3)
  mpe <- matrix(NA, nrow = k, ncol = 3)
  colnames(rmse) <- colnames(mpe) <- c("dhomo", "lnhomo", "dhet")
  progress.bar <- create_progress_bar("text")
  progress.bar$init(k)
  for (i in 1:k){
    trainingset <- data[id %in% folds[-i]]
    testset <- data[id %in% c(i)]
    lm <- lm(f, trainingset)
    err <- data.table(res = lm$res, trainingset[, group, with = FALSE])
    err <- err[, .(ev_err = mean(exp(res))), by = group]
    testset <- merge(testset, err, by = group)
    pred <- data.table(k = i, predict(lm,  testset), 
                      testset[, all.vars(f)[1], with = FALSE])
    setnames(pred, c("k", "logyhat", "logy"))
    pred[, y := exp(logy)]
    pred[, yhat_dhomo := exp(logyhat) * mean(exp(lm$res))]
    pred[, yhat_dhet := exp(logyhat) * testset[, ev_err]]
    pred[, yhat_lnhomo := exp(logyhat + summary(lm)$sigma^2/2)]
    rmse[i, ] <- apply(pred[, .(yhat_dhomo, yhat_lnhomo, yhat_dhet)], 2, 
                       function (x) sqrt(mean((pred$y - x)^2)))
    mpe[i, ] <- apply(pred[, .(yhat_dhomo, yhat_lnhomo, yhat_dhet)], 2, 
                       function (x) mean(pred$y - x))
    progress.bar$step()
  }  
  return(list(rmse = rmse, mpe = mpe))
}

# REPEAT K-FOLD CROSS VALIDATION LOG OLS MODEL ---------------------------------
RCvLoglm <- function(data, f, k, group, R = 1){
  rmse <-  list()
  mpe <- list()
  for (i in 1:R){
    cv <- CvLoglm(data, f, k, group)
    rmse[[i]] <- cv$rmse
    mpe[[i]] <- cv$mpe
    cat(i)
  }
  rmse <- do.call(rbind, rmse)
  mpe <- do.call(rbind, mpe)
  return(list(rmse = rmse, mpe = mpe))
}

# INCREMENTAL EFFECT AT MEAN ---------------------------------------------------
IeAtMean <- function(coef, mean){
  InvLogit(logit(mean) + coef) - InvLogit(logit(mean)) 
}

# CALCULATE AVERAGE MARGINAL EFFECT --------------------------------------------
AmeGlm <- function(mod, link = "logit", var){
  b <- coef(mod)[var]
  if (link == "logit"){
    return(mean(mod$fit * (1 - mod$fit)) * b) 
  } else if (link == "log") {
    return(mean(mod$fit) * b) 
  }
}

# CALCULATE AVERAGE INCREMENTAL EFFECT -----------------------------------------
AieGlm <- function(mod, var){
  x <- mod$model
  x1 <- x
  x1[, var] <- 1
  x0 <- x
  x0[, var] <- 0
  aie <- mean(predict(mod, x1, type = "response") - predict(mod, x0, type = "response"))
  names(aie) <- var
  return(aie)
}

# MY AME/AIE -------------------------------------------------------------------
MyAmeGlm <- function(mod, link = "logit"){
  aie.black <- AieGlm(mod, "black")
  aie.hisp <- AieGlm(mod, "hispanic")
  ame.educ <- AmeGlm(mod, link = link, "educyr")
  return(c(aie.black, aie.hisp, ame.educ))
}

# AIE  (MONTE CARLO SES)  ------------------------------------------------------
AieMC <- function(beta, v, x, varnum, nsims = 10000, link = "logit"){
  b.sims <- t(mvrnorm(n = nsims, mu = beta, Sigma = v))
  x1 <- x
  x1[, varnum] <- 1
  x0 <- x
  x0[, varnum] <- 0
  ame.sims <- rep(NA, nsims)
  for (i in 1:length(ame.sims)){
    if (link == "logit"){
      ame.sims[i] <- mean(InvLogit(x1 %*% b.sims[, i]) - InvLogit(x0 %*% b.sims[, i])) 
    } else if(link == "log"){
      ame.sims[i] <- mean(exp(x1 %*% b.sims[, i]) - exp(x0 %*% b.sims[, i]))
    }
  }
  ame <- mean(ame.sims)
  se <- sd(ame.sims) 
  results <- cbind(ame, se)
  return(results)
}

# AME (MONTE CARLO SES)  -------------------------------------------------------
AmeMC <- function(beta, v, x, varnum, nsims = 10000, link = "logit"){
  b.sims <- t(mvrnorm(n = nsims, mu = beta, Sigma = v))
  ame.sims <- rep(NA, nsims)
  for (i in 1:nsims){
    if (link == "logit"){
      ame.sims[i] <- mean(InvLogit(x %*% b.sims[, i]) * (1 - InvLogit(x %*% b.sims[, i])) * b.sims[varnum, i])
    } else if(link == "log"){
      ame.sims[i] <- mean(exp(x %*% b.sims[, i]) %*% b.sims[varnum, i])
    } 
  }
  ame <- mean(ame.sims)
  se <- sd(ame.sims) 
  results <- cbind(ame, se)
  return(results)
}


# AVERAGE MARGINAL EFFECT WITH DELTA METHOD SE'S -------------------------------
AmeDM <- function(beta, v, x, varnums, link = "logit"){
  # AME for continous variable in logistic regression. SEs calculated 
  # with delta method
  #
  # Args:
  #   model: what type of model was used
  #   beta: vector of k x 1 regression coefficients
  #   v: variance-covariance matrix
  #   x: matrix of data (must include 1's)
  #   varnums: variables to calculate AME for (row numbers in beta)
  #
  # Returns:
  #   Average marginal effect and standard errors
  if (link == "logit"){
    g <- function(b){
      return(apply(as.matrix(InvLogit(x %*% b) * (1 - InvLogit(x %*% b))) %*% b[varnums], 2, mean))
    }
  } else if (link == "log"){
    g <- function(b){
      return(apply(exp(x %*% b) %*% b[varnums], 2, mean))
    }
  }
  ame <- g(b = beta)
  grad_g <- jacobian(g, beta)
  var <- grad_g %*% v %*% t(grad_g)
  se <- sqrt(diag(var))
  results <- cbind(ame, se)
  return(results)
}

# AVERAGE INCREMENTAL EFFECT WITH DELTA METHOD SE'S ----------------------------
AieDM <- function(beta, v, x, varnums, link = "logit"){
  # AME for discrete variable in logistic regression. SEs calculated 
  # with delta method
  #
  # Args:
  #   model: what type of model was used
  #   beta: vector of k x 1 regression coefficients
  #   v: variance-covariance matrix
  #   x: matrix of data (must include 1's)
  #   varnums: variables to calculate AME for (row numbers in beta)
  #
  # Returns:
  #   Average marginal effect and standard errors
  x1 <- x
  x1[, varnums] <- 1
  x0 <- x
  x0[, varnums] <- 0
  if (link == "logit"){
    g <- function(b){
      return(mean(InvLogit(x1 %*% b) - InvLogit(x0 %*% b)))
    } 
  } else if(link == "logl"){
    g <- function(b){
      return(mean(exp(x1 %*% b) - exp(x0 %*% b)))
    }
  } else {
    print("Model choice unavailable")
  }
  ame <- g(b = beta)
  grad_g <- jacobian(g, beta)
  var <- grad_g %*% v %*% t(grad_g)
  se <- sqrt(diag(var))
  results <- cbind(ame, se)
  return(results)
}

# MY AVERAGE MARGINAL EFFECTS (DELTA METHOD SE'S) ------------------------------
MyAmeDM <- function(model, x = xnew, link = "logit"){
  # AME for black, hispanic and education variable (note: they must be in the
  # 2nd, 3rd, and 4th spots in the beta vector)
  #
  # Args:
  #   model: name of logit regression model
  #   x: data of x variables for marginal effects
  #
  # Returns:
  #   Average marginal effect and standard errors for black, hispanic, and 
  #   education variables
  beta <- as.matrix(coef(model))
  v <- vcov(model)
  ame.black <- AieDM(beta = beta, v = v, x = x, varnums = 2, link = link)
  ame.hisp <- AieDM(beta = beta, v = v, x = x, varnums = 3, link = link)
  ame.educ <- AmeDM(beta = beta, v = v, x = x, varnums = 4, link = link)
  ame <- rbind(ame.black, ame.hisp, ame.educ)
  return(ame)
}

# MY AVERAGE MARGINAL EFFECTS MONTE CARLO SIMULATION----------------------------
MyAmeMC <- function(model, x, nsims, link = "logit"){
  # AME for black, hispanic and education variable (note: they must be in the
  # 2nd, 3rd, and 4th spots in the beta vector) and monte carlo standard errors
  #
  # Args:
  #   model: name of logit regression model
  #   x: data of x variables for marginal effects
  #   modtype: model type
  #
  # Returns:
  #   Average marginal effect and  monte carlo standard errors for black, hispanic, 
  #   and education variables
  beta <- as.matrix(coef(model))
  v <- vcov(model)
  ame.black <- AieMC(beta = beta, v = v, x = x, nsims = nsims, varnum = 2, link = link)
  ame.hisp <- AieMC(beta = beta, v = v, x = x, nsims = nsims, varnum = 3, link = link)
  ame.educ <- AmeMC(beta = beta, v = v, x = x, nsims = nsims, varnum = 4, link = link)
  ame <- rbind(ame.black, ame.hisp, ame.educ)
  return(ame)
}

# PREDICTION FOR TWO PART MODEL ------------------------------------------------
Pred2p <- function(mod1, mod2, data, group){
  err <- data.table(res = mod2$model[, 1] - mod2$fit, mod2$model[, colnames(group), drop = FALSE])
  err <- err[, .(ev_err = mean(exp(res))), by = c(colnames(group))] 
  x <- data.table(group, fit1 = InvLogit(data %*% coef(mod1))[, 1],
                  fit2 = (data %*% coef(mod2))[, 1])
  x[, id := seq(1, .N)]
  x <- merge(x, err, by = colnames(group))
  x[, pred := fit1 *  exp(fit2) * ev_err]
  x <- x[order(id)]
  return(x[, pred])
}

# AME FOR TWO PART MODEL -------------------------------------------------------
Ame2p <- function(mod1, mod2, data, group, var){
  yhat <- Pred2p(mod1, mod2, data = data, group = group)
  phat <- InvLogit(data %*% coef(mod1))
  me <- (coef(mod2)[var] + coef(mod1)[var] * (1 - phat)) * yhat
  return(mean(me))
}

# AME FOR TWO PART MODEL -------------------------------------------------------
Aie2p <- function(mod1, mod2, data, group, var){
  x1 <- x0 <- data
  x1[, var] <- 1
  x0[, var] <- 0
  ie <- Pred2p(mod1, mod2, x1, group = group) - Pred2p(mod1, mod2, x0, group = group)
  return(mean(ie))
}

# AME/AIE FUNCTION -------------------------------------------------------------
Ame2pFun <- function(data, inds, f1, f2){
  dat <- data[inds]
  m.logit <-  speedglm(f1, data = dat, family = binomial())
  m.lm <- speedlm(f2, data = dat[rxuse == 1], model = TRUE, fitted = TRUE)
  grp <- dat[, "fage", with = FALSE]
  f.x <- as.formula(paste("~", paste(all.vars(f1)[-1], collapse = "+")))
  dat.m <- model.matrix(f.x, dat)
  aie.black <- Aie2p(m.logit, m.lm, dat.m, grp, "black")
  aie.hisp <- Aie2p(m.logit, m.lm, dat.m, grp, "hispanic")
  ame.educyr <- Ame2p(m.logit, m.lm, dat.m, grp, "educyr")
  return(c(aie.black, aie.hisp, ame.educyr))
}

# AME/AIE BOOTSTRAP ------------------------------------------------------------
Ame2pBoot <- function(data, f1, f2, B){
   ame <- matrix(NA, nrow = B, ncol = 3)
   progress.bar <- create_progress_bar("text")
   progress.bar$init(B)
   for (i in 1:B){
     indices <- sample(1:nrow(data), size = nrow(data), replace = T)
     try(ame[i, ] <- Ame2pFun(data, indices, f1, f2))
     progress.bar$step()
   }  
  return(ame)
}  

# MEAN EXPENDITURES ------------------------------------------------------------
MeanExp <- function(alpha, beta, sigma, x, res = NULL, method = "normal"){
  if (method == "normal"){
    return(InvLogit(x %*% alpha) * exp(x %*% beta + sigma^2/2)) 
  } else if (method == "smearing"){
    return(InvLogit(x %*% alpha) * exp(x %*% beta) * mean(exp(res))) 
  }
}

# PREDICT EXPENDITURES VIA SIMULATION ------------------------------------------
SimPred <- function(model1, model2, sim1, sim2, x,
                    educ = 12, black = 0, hisp = 0){
  n.sims <- nrow(sim1)
  N <- nrow(x)
  x[, "educyr"] <- educ
  x[, "black"] <-  black
  x[, "hispanic"] <- hisp
  exp.sim <- matrix(NA, nrow = N, n.sims)
  for (s in 1:n.sims){
    p.use <- InvLogit(x %*% sim1[s, ]) 
    use.sim <- rbinom(N, 1, p.use)
    exp.sim[, s] <- ifelse(use.sim == 0, 0, 
                      exp(rnorm(N, x %*% coef(sim2)[s, ], sigma.hat(sim2)[s])))
  }
  return(exp.sim)
}

# PREDICT EXPENDITURES VIA SIMULATION ------------------------------------------
ExpAtMean <- function(mod1, mod2, prob, cmean, smear.mean){
  xb1 <- logit(prob)
  xb2 <- log(cmean/smear.mean)
  educyr <- seq(8, 17)
  educyrb1 <- (educyr - 12) * coef(mod1)["educyr"]
  educyrb2 <- (educyr - 12) * coef(mod2)["educyr"]
  pred <- matrix(NA, length(educyr), 3)
  pred[, 1] <- InvLogit(xb1 + educyrb1) * exp(xb2 + educyrb2) * smear.mean
  pred[, 2] <- InvLogit(xb1 + educyrb1 + coef(mod1)["black"]) *
    exp(xb2 + educyrb2 + coef(mod2)["black"]) * smear.mean
  pred[, 3] <- InvLogit(xb1 + educyrb1 + coef(mod1)["hispanic"]) *
    exp(xb2 + educyrb2 + coef(mod2)["hispanic"]) * smear.mean
  pred <- data.table(pred = c(pred), 
                     race = rep(c("White", "Black", "Hispanic"), each = length(educyr)),
                     educyr = rep(educyr, 3))
  return(pred)
}

# SELECTION MODEL COEFFICIENTS AND STANDARD ERRORS -----------------------------
MyHeckCoef <- function(model, part = "outcome"){
  # Returns coefficients and robust standard errors from heckman selection model
  #
  # Args:
  #   model: name of regression model
  #
  # Returns:
  #   Matrix with column of coefficients and column of standard errors
  coef <- coef(model, part = part)[c("black", "hispanic", "educyr")]
  se <- sqrt(diag(vcov(model, part = part))[c("black", "hispanic", "educyr")])
  return(cbind(coef, se))
  return(coef)
}

# TABLE of REGRESSION ESTIMATES WITH STANDARD ERRORS IN PARENTEHSES ------------
EstTable <- function(est){
  # Regression table with standard errors in parentheses
  #
  # Args:
  #   coef: matrix of estimates (i.e. coefficients)
  #   se: matrix of se's
  #
  # Returns:
  #   Table with coefficients and se's in parentheses
  n.est <- length(est)
  coef <- matrix(NA, nrow = nrow(est[[1]]), ncol = n.est)
  se <- coef
  for (i in 1:n.est){
    coef[, i] <- est[[i]][, 1]
    se[, i] <- est[[i]][, 2]
  }
  coef <- formatC(coef , format="f", digits=3)
  se <- formatC(se , format="f", digits=3)
  names <- c("Black", "Hispanic", "Years of education")
  names <- as.vector(rbind(names, ""))
  table <-  matrix(as.vector(rbind(as.vector(coef),
                                   paste("(",as.vector(se),")", sep=""))), 
                   nrow=2*nrow(coef))
  table <- cbind(names, table)
  return(table)
}

# REGRESSION TABLE WITH STANDARD ERRORS IN PARENTEHSES -------------------------
RegTable <- function(models, vars = c("black", "hispanic", "educyr"),
                     varnames = c("Black", "Hispanic", "Years of education"),
                     modsums = c("meandv", "obs"),
                     modsums.names = c("Mean of dv", "Observations"),
                     robust = NULL, digits = 3){
  # Regression table with standard errors in parentheses
  #
  # Args:
  #   mod: list of models
  #   varnames: names of variables to report in table
  #   modsums: list of model summary measures to report
  #   modsums.names: user chosen names for model summaries
  #   robust: list of same length as number of models for whether to use
  #           stata robust standard errrors
  #  digits: number of digits to report 
  #
  # Returns:
  #   Table with coefficients and se's in parentheses
  nmodels <- length(models)
  if(is.null(robust)){
    robust <- rep(F, nmodels)
  } 
  coef <- se <- matrix(NA, nrow = length(vars), ncol = nmodels)
  ms <- list()
  for (i in 1:nmodels){
    ms[["meandv"]][i] <- mean(models[[i]]$model[, 1])
    ms[["obs"]][i] <- nobs(models[[i]])
    if (robust[i] == T){
      est <- coeftest(models[[i]], vcov = vcovHC(models[[i]], "HC1"))[vars, 1:2]  
    } else{
      est <- coeftest(models[[i]])[vars, 1:2]  
    }
    coef[, i] <- est[, 1]
    se[, i] <- est[, 2]
  }
  coef <- formatC(coef , format="f", digits = digits)
  se <- formatC(se , format="f", digits = digits)
  varnames <- as.vector(rbind(varnames, ""))
  table <-  matrix(as.vector(rbind(as.vector(coef),
                                   paste("(",as.vector(se),")", sep=""))), 
                   nrow=2*nrow(coef))
  ms[["meandv"]] <- formatC(ms[["meandv"]], format = "f", digits = digits)
  #ms[["meandv"]] <- paste0("\\multicolumn{1}{c}{", ms[["meandv"]], "}")
  ms[["obs"]] <- formatC(ms[["obs"]], format = "d", big.mark = ",")
  ms[["obs"]] <- paste0("\\multicolumn{1}{c}{", ms[["obs"]], "}")
  ms <- do.call(rbind, ms)
  ms.order <- match(rownames(ms), modsums)
  ms <- ms[ms.order[!is.na(ms.order)], ]
  table <- rbind(table, rep("", nmodels), ms)
  table <- cbind(c(varnames, "", modsums.names), table)
  row.names(table) <- seq(1, nrow(table))
  return(table)
}

# PERCENT CHANGE IN REGRESSION COEFFICIENTS TABLE ------------------------------
ChTable <- function(est1, est2){
  est1 <- do.call(rbind, est1) 
  est2 <- do.call(rbind, est2) 
  ch <- t(100*(1 - est2/est1))
  ch <- formatC(ch, format = "f", digits = 0)
  ch <- matrix(paste0(ch, "\\%"), nrow = ncol(est1))
  ch <-  matrix(as.vector(rbind(as.vector(ch), NA)), 
                nrow=2*nrow(ch))
  na.m <- matrix(NA, nrow = 3, ncol = nrow(est1))
  ch <- rbind(ch, na.m)
  return(ch)
}

