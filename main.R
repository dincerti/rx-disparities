rm(list=ls()) 
packages <- c("ggplot2", "Hmisc", "foreign", "plyr", "RMySQL", "xtable", 
              "data.table", "survey", "reshape2", "mfx", "numDeriv",
              "sandwich", "MASS", "gridExtra", "sampleSelection", "MASS",
              "arm", "speedglm", "boot")
lapply(packages, library, character.only = TRUE)
source("code/func.R")
source("code/do.R")
source("code/text.R")