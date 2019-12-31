
source("R/RelConditionEPU.R")

condition <- RelConditionEPU(pullNewData = FALSE, out.dir="output")
# plot results
plot_condition(condition$condGOM, filename = "GOMcondition_2018", out.dir="output")
