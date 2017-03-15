# MEDICAL CONDITION FILES ----------------------------------------------------------

# reshape wide
mc <- mc[!is.na(mc$rxcat), ]
setkey(mc, year, dupersid, panel, rxcat)
mc.rxcat <- mc[, .N, by= key(mc)]
mc.wide <- mc.rxcat
mc.wide$N <- 1
mc.wide$rxcat <- paste0("rxcat", sprintf("%02d", mc.wide$rxcat))
mc.wide <- dcast.data.table(mc.wide, year + dupersid + panel ~ rxcat, value.var = "N")
mc.wide[is.na(mc.wide)] <- 0

# count of rxcat's
setkey(mc.rxcat, year, dupersid, panel)
mc.count <- mc.rxcat[, .N, by= key(mc.rxcat)]
mc.count <- rename(mc.count, c("N" = "mcnum"))

# PRESCRIBED MEDICINE FILES --------------------------------------------------------
pm <- data.table(pm)
pm.count <- pm[, .N, by= c("dupersid", "year")]

# check that pm file matches fyc
fyc.temp <- fyc[, c("year","dupersid", "rxtot", "age"), with = FALSE]
temp <- merge(fyc.temp, pm.count, by = c("dupersid", "year"), all.x = TRUE)
temp$N <- ifelse(is.na(temp$N), 0, temp$N)
nrow(temp[temp$rxtot != temp$N, ])

# diabetes sample
pm.diabetes <- subset(pm, tc1s1 == 99)
setkey(pm.diabetes, year, dupersid)
pm.diabetes$rxtot <- 1
pm.diabetes.sum <- pm.diabetes[, lapply(.SD, sum, na.rm = TRUE),
                     by = key(pm.diabetes),
                     .SDcols = c("rxtot", "rxxpx")]
pm.diabetes.sum <- rename(pm.diabetes.sum, c("rxtot" = "rxtot_diab", "rxxpx" = "rxexp_diab")) 

# depression sample
pm.depr <- subset(pm, tc1s1 == 249 | tc2s1 == 249 | tc3s1 == 249)
setkey(pm.depr, year, dupersid)
pm.depr$rxtot <- 1
pm.depr.sum <- pm.depr[, lapply(.SD, sum, na.rm = TRUE),
                               by = key(pm.depr),
                               .SDcols = c("rxtot", "rxxpx")]
pm.depr.sum <- rename(pm.depr.sum, c("rxtot" = "rxtot_depr", "rxxpx" = "rxexp_depr")) 

# cholesterol sample
pm.chol <- subset(pm, (year <= 2004 & tc1 == 19) | 
                    (year > 2004 & (tc1s1 == 19 | tc2s1 == 19 | tc3s1 == 19)))
setkey(pm.chol, year, dupersid)
pm.chol$rxtot <- 1
pm.chol.sum <- pm.chol[, lapply(.SD, sum, na.rm = TRUE),
                       by = key(pm.chol),
                       .SDcols = c("rxtot", "rxxpx")]
pm.chol.sum <- rename(pm.chol.sum, c("rxtot" = "rxtot_chol", "rxxpx" = "rxexp_chol")) 

# CREATE FINALIZED FYC DATA FRAME (I.E. INDIVIDUAL-YEAR DATA) ----------------------
fyc <- subset(fyc, age >= 25)

# pooled linkage estimation file (taylor series linearization)
fyc <- merge(fyc, vartsl, by = c("dupersid", "panel"))

# cpi data
fyc <- merge(fyc, cpi, by = c("year"))
fyc$cpi2012 <- cpi$cpi[nrow(cpi)]

# medical conditions
fyc <- merge(fyc, mc.count, by = c("year", "dupersid", "panel"), all.x = TRUE)
fyc$mcnum <- ifelse(is.na(fyc$mcnum), 0, fyc$mcnum)
fyc$d_mc <- as.numeric(fyc$mcnum > 0)
fyc <- merge(fyc, mc.wide, by = c("year", "dupersid", "panel"), all.x = TRUE)
rxcatnames <- names(fyc)[grep("^rxcat", names(fyc))]
for (col in rxcatnames) fyc[is.na(get(col)), (col) := 0]

# prescribed medications
fyc <- merge(fyc, pm.diabetes.sum, by = c("dupersid", "year"), all.x = TRUE)
fyc <- merge(fyc, pm.depr.sum, by = c("dupersid", "year"), all.x = TRUE)
fyc <- merge(fyc, pm.chol.sum, by = c("dupersid", "year"), all.x = TRUE)
for (i in c("rxtot_diab", "rxexp_diab", "rxtot_depr", "rxexp_depr",
            "rxtot_chol", "rxexp_chol")){
  fyc[, i]  <- ifelse(is.na(fyc[, get(i)]), 0, fyc[, get(i)])
}

## CREATING/EDITING VARIABLES
# utilization
fyc$rxexp <- fyc$rxexp * (fyc$cpi2012/fyc$cpi)
fyc$rxexp_diab <- fyc$rxexp_diab * (fyc$cpi2012/fyc$cpi)
fyc$rxexp_depr <- fyc$rxexp_depr * (fyc$cpi2012/fyc$cpi)
fyc$rxexp_chol <- fyc$rxexp_chol * (fyc$cpi2012/fyc$cpi)

fyc[, logrxexp := ifelse(rxexp > 0, log(rxexp), NA)]
fyc[, sqrtrxexp := sqrt(rxexp)]

fyc$rxuse <- as.numeric(fyc$rxexp > 0)
fyc$rxuse_diab <- ifelse(fyc$rxexp_diab > 0, 1, 0)
fyc$rxuse_depr <- ifelse(fyc$rxexp_depr > 0, 1, 0)
fyc$rxuse_chol <- ifelse(fyc$rxexp_chol > 0, 1, 0)

# hispanic
fyc$hispanic <- as.numeric(fyc$hispanic == 1)

# non-hispanic black
fyc$black <- as.numeric(fyc$year < 2002 & fyc$race == 4)
fyc$black <- ifelse(fyc$year >= 2002 & fyc$race == 2, 1, fyc$black)
fyc$black <- ifelse(fyc$hispanic == 1, 0, fyc$black)

# whites
fyc$white <- as.numeric(fyc$year < 2002 & fyc$race == 5)
fyc$white <- ifelse(fyc$year >= 2002 & fyc$race == 1, 1, fyc$white)
fyc$white <- ifelse(fyc$hispanic == 1, 0, fyc$white)

# other
fyc$other <- as.numeric(fyc$black == 0 & fyc$hispanic == 0 & fyc$white == 0)

# race/ethnicity
fyc$myrace <- ifelse(fyc$black == 1, "Black", "White")
fyc$myrace <- ifelse(fyc$hispanic == 1, "Hispanic", fyc$myrace)
fyc$myrace <- ifelse(fyc$other == 1, "Other", fyc$myrace)

# years of education
fyc$educyr <- as.numeric(fyc$educyr)
fyc$educyr <- ifelse(fyc$educyr < 0, NA, fyc$educyr)

# years of education categories
fyc$educcat <- NA
fyc$educcat <- ifelse(fyc$educyr < 12, 1, fyc$educcat)
fyc$educcat <- ifelse(fyc$educyr == 12, 2 , fyc$educcat)
fyc$educcat <- ifelse(fyc$educyr >= 13 & fyc$educyr <= 15 , 3, fyc$educcat)
fyc$educcat <- ifelse(fyc$educyr >= 16, 4, fyc$educcat)
fyc$educcat <- ordered(fyc$educcat, levels = c(1, 2, 3, 4),
                       labels = c("Less than 12", "12 Exactly",
                                  "13 to 15", "16 to 17+"))

# poverty categories
fyc$poor <- as.numeric(fyc$povcat == 1)
fyc$nearpoor <- as.numeric(fyc$povcat == 2)
fyc$lowincome <- as.numeric(fyc$povcat == 3)
fyc$midincome <- as.numeric(fyc$povcat == 4)
fyc$highincome <- as.numeric(fyc$povcat == 5)
fyc$povcat <- factor(fyc$povcat)

# health insurance
fyc$ins <- as.numeric(fyc$inscov == 1 | fyc$inscov == 2)

# sex
fyc$female <- as.numeric(fyc$sex == 2)

# age
fyc$agesq <- fyc$age^2
fyc$agecube <- fyc$age^3
fyc$fage <- as.factor(fyc$age)
agedums <- model.matrix( ~ -1 + fage, fyc) 
colnames(agedums) <- sub("fage", "age", colnames(agedums))
fyc <- cbind(fyc, agedums)

# marital status
fyc$married[fyc$marry >=0] <- as.numeric(fyc$marry[fyc$marry >=0] == 1)
fyc$widowed[fyc$marry >=0] <- as.numeric(fyc$marry[fyc$marry >=0] == 2)
fyc$separated[fyc$marry >=0] <- as.numeric(fyc$marry[fyc$marry >=0] == 3 |
                                             fyc$marry[fyc$marry >=0] == 4)
fyc$nevermarried[fyc$marry >=0] <- as.numeric(fyc$marry[fyc$marry >=0] == 5)

# self-reported health status
fyc$srh <- ifelse(fyc$year == 1996, fyc$rthlth42, fyc$rthlth53)
fyc$srh <- ifelse(fyc$srh <0, fyc$rthlth42, fyc$srh)
fyc$srh <- ifelse(fyc$srh <0, fyc$rthlth31, fyc$srh)
fyc$srh <- ifelse(fyc$srh <0, NA, fyc$srh)
for (i in sort(unique(fyc$srh[!is.na(fyc$srh)]))){
  fyc[, paste0("srh", i)] <- as.numeric(fyc$srh == i)
}
fyc$srh <- factor(fyc$srh)

# year
fyc[, fyear := factor(year)]

# SAVE DESIRED OBJECTS -------------------------------------------------------------
rm(list= ls()[!(ls() %in% c("pm", "fyc", "mc", "mc.count", "varts", "rxcatnames", "xdata", 
                            "lookup.rxcat", lsf.str()))])
save(list = ls(), file = "output/mydata.RData")
write.dta(fyc, "output/fyc.dta")