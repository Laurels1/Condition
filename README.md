# Condtition
NEFSC fall survey relative condition using standard length-weight regressions to compare individual weights at a given length. Code parses condition by species, sex and EPU.

# Files to run automated condition analyses:
RelConditionEPU.R (calculates relative condition)
StomFullnessData_allfh.R (stomach fullness index)
gam_calcs_strata.R (brings in environmental covariates)
automate_select_variables.R (runs automation to select best fitting variable for each category and runs full model for each species)
