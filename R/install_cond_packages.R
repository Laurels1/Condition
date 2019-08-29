# run this function to install all packages required for condition factor analysis
#
#

install_cond_packages <- function(){
  pkg <- c("dplyr", "tidyr", "data.table", "rgdal", "here", "RODBC","readr","remotes")
  gitHub <- c("Survdat")
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE,repos='http://cran.us.r-project.org')
  #sapply(pkg, require, character.only = TRUE)
  # github
  new.gitHub <- "Survdat" %in% installed.packages()[,"Package"]
  if (new.gitHub == F) remotes::install_github("slucey/RSurvey/Survdat")
}

