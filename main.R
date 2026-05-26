# main file to run

if (!dir.exists("plots")) {
  dir.create("plots")
}

source("preprocessing.R")
source("anomalyIF.R")
source("bayes.R")
source("bayesEvol.R")
source("mcmc.R")
source("comparison.R")
