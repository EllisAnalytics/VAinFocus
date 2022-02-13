
## This function packages all enrollment data gathered from the Virginia 
## Department of Education's Fall Membership Build-A-Table web application.                                                           
##                                                                            
## The data set includes also subgroup categories for students with 
## disabilities, English language learners, federal ethnicity groups, 
## and economically disadvantaged students.
##
## EXTERNAL DEPENDENCIES:
##   - tidyverse
##      - stringr 
##      - readr 
##      - tidyr 
##      - dplyr 
##      - purrr 
##
## INTERNAL DEPENDENCIES:
##   - ./R/tidy/enrollment/tidy_state_enrollment.R
##   - ./R/tidy/enrollment/tidy_state_grade_enrollment.R
##   - ./R/tidy/enrollment/tidy_div_enrollment.R
##   - ./R/tidy/enrollment/tidy_div_grade_enrollment.R
##   - ./R/tidy/enrollment/tidy_div_sch_enrollment.R
##   - ./R/tidy/enrollment/tidy_div_sch_grade_enrollment.R
##   - ./R/tidy/tidy_helpers.R
##      - set_subgroup_category()
##      - set_subgroup()

################################################################################

tidy_enrollment <- function() {
    return(
        bind_rows(
            tidy_state_enrollment(),
            tidy_state_grade_enrollment(),
            tidy_div_enrollment(),
            tidy_div_grade_enrollment(),
            tidy_div_sch_enrollment(),
            tidy_div_sch_grade_enrollment()
        )
    )
}
