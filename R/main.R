# main file to run

if (!dir.exists("plots")) {
  dir.create("plots")
}

source(file.path("R", "preprocessing.R"))
source(file.path("R", "anomalyIF.R"))
source(file.path("R", "bayes.R"))
source(file.path("R", "bayesEvol.R"))
source(file.path("R", "mcmc.R"))
source(file.path("R", "comparison.R"))
