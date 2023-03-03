# This repository is a scientific product and is not official communication of the National Oceanic and
Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is
provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of
Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed
by all applicable Federal law. Any reference to specific commercial products, processes, or services by service
mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or
favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a
DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by
DOC or the United States Government.”

# Condtition
NEFSC fall survey relative condition using standard length-weight regressions to compare individual weights at a given length. Code parses condition by species, sex and EPU.

# Files to run automated condition analyses:
RelConditionEPU.R (calculates relative condition)
StomFullnessData_allfh.R (stomach fullness index)
gam_calcs_strata.R (brings in environmental covariates)
automate_select_variables.R (runs automation to select best fitting variable for each category and runs full model for each species)
