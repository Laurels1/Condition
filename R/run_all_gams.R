out.dir="output"
data.dir <- "data"
gis.dir  <- "gis"

# source all files/functions
# Reduces clutter in global environment
source('C:/Users/andrew.beet/Documents/MyWork/gitHub_repos/condition/R/RelConditionEPU.R')
source('C:/Users/andrew.beet/Documents/MyWork/gitHub_repos/condition/R/StomFullnessData_allfh.R')
source('C:/Users/andrew.beet/Documents/MyWork/gitHub_repos/condition/R/gam_calcs_strata.R')

# run each one
cond.epu <- RelConditionEPU(out.dir,data.dir,gis.dir)
stom <- StomFullnessData_llfh(out.dir,data.dir,gis.dir)
gam_calcs_strata(cond.epu=cond.epu,stom=stom,out.dir,data.dir,gis.dir)

# source automation
# ultimately a function also
source('C:/Users/andrew.beet/Documents/MyWork/gitHub_repos/condition/R/automate_select_variables.r')
