
source("R/RelConditionEPU.R")
condition <- RelConditionEPU(pullNewData=T,out.dir="output")

condition <- RelConditionEPU(pullNewData = FALSE, out.dir="output")
# plot results
plot_condition(condition$condGOM, filename = "GOMcondition_2019", out.dir="output")
plot_condition(condition$condGB, filename = "GBcondition_2019", out.dir="output")
plot_condition(condition$condMAB, filename = "MABcondition_2019", out.dir="output")
plot_condition(condition$condSS, filename = "SScondition_2019", out.dir="output")
