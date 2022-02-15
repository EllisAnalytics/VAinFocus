
library(tidyverse)
library(glue)

source("./R/tidy/enrollment/tidy_state_enrollment.R")
source("./R/tidy/enrollment/tidy_state_grade_enrollment.R")
source("./R/tidy/enrollment/tidy_div_enrollment.R")
source("./R/tidy/enrollment/tidy_div_grade_enrollment.R")
source("./R/tidy/enrollment/tidy_div_sch_enrollment.R")
source("./R/tidy/enrollment/tidy_div_sch_grade_enrollment.R")

source("./R/tidy/tidy_helpers.R")
source("./R/tidy/tidy_enrollment.R")

enrollment <- tidy_enrollment()

test <- list(
  Enrollment = list(
    State = list(
      Summary = list(
        CurrentEnrollment = enrollment %>%
          filter(DivisionNumber == -1 & 
                 SchoolYear == "2020-2021" &
                 Subgroup == "All Students" &
                 Grade == "All Grades"),
        HistoricalEnrollment = enrollment %>%
          filter(DivisionNumber == -1 &
                 Subgroup == "All Students" &
                 Grade == "All Grades")
      ),
      Detail = enrollment %>%
        filter(DivisionNumber == -1)
    ),
    Divisions = list(
      Summary = list(
        CurrentEnrollment = enrollment %>%
          filter(DivisionNumber != -1 & 
                 SchoolNumber == -1 &
                 SchoolYear == "2020-2021" &
                 Subgroup == "All Students" &
                 Grade == "All Grades"),
        HistoricalEnrollment = enrollment %>%
          filter(DivisionNumber != -1 &
                 SchoolNumber == -1 &
                 Subgroup == "All Students" &
                 Grade == "All Grades")
      ),
      Detail = enrollment %>%
        filter(DivisionNumber != -1)
    )
  )
)

saveRDS(test, "app_data.rds")
