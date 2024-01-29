requiredPackages <- c("boot", "rstatix", "tibble", "psych", "ltm", "stringr", "ggplot2", "rjson", "reticulate", "ggpubr", "lme4", "lmerTest", "pracma", "lattice", "MASS", "apcluster", "blme", "smacof","cluster","factoextra", "pwr", "magrittr", "tidyr", "dplyr", "ggbiplot")
new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
lapply(requiredPackages, require, character.only = TRUE)