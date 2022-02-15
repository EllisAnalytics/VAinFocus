
## This function formats enrollment data gathered from the Virginia Department 
## of Education's Fall Membership Build-A-Table web application aggregated by 
## division, school, and grade level.                                                           
##                                                                            
## The data set includes subgroup categories for students with disabilities,  
## English language learners, federal ethnicity groups, and economically      
## disadvantaged students.
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
##   - ./R/tidy/tidy_helpers.R
##      - set_subgroup_category()
##      - set_subgroup()

################################################################################

tidy_div_sch_grade_enrollment <- function() {

  # vector of field names for the file with all students
  fields22 <- c(
    "SchoolYear", "DivisionNumber", "DivisionName", "SchoolNumber",
    "SchoolName", "Pre-KG", "KG", "1st Grade", "2nd Grade",
    "3rd Grade", "4th Grade", "5th Grade", "6th Grade", "7th Grade",
    "8th Grade", "9th Grade", "10th Grade", "11th Grade", "12th Grade",
    "FullTime", "PartTime", "Total"
  )

  # vector of field names for the files with subgroups
  fields23 <- c(
    "SchoolYear", "DivisionNumber", "DivisionName", "SchoolNumber",
    "SchoolName", "Subgroup", "Pre-KG", "KG", "1st Grade", "2nd Grade",
    "3rd Grade", "4th Grade", "5th Grade", "6th Grade", "7th Grade",
    "8th Grade", "9th Grade", "10th Grade", "11th Grade", "12th Grade",
    "FullTime", "PartTime", "Total"
  )

  # FUNCTION - processes the individual enrollment files
  get_enrollment <- function(.path) {
    if (str_detect(.path, "all")) {
      df <- read_csv(
        file = .path,
        col_types = "cdcdcccccccccccccccccc",
        trim_ws = TRUE,
        name_repair = make.names,
        show_col_types = FALSE
      ) %>%
        set_names(fields22) %>%
        pivot_longer(
          cols = !c(1:5, 20:22),
          names_to = "Grade",
          values_to = "Enrollment"
        ) %>%
        mutate(
          DivisionName = glue("{DivisionName} Public Schools"),
          SubgroupCategory = set_subgroup_category(.path),
          Subgroup = set_subgroup(.path),
        ) %>%
        select(
          c(
            SchoolYear:SchoolName,
            SubgroupCategory,
            Subgroup,
            Grade:Enrollment
          )
        )

      df$Enrollment <- df$Enrollment %>%
        replace_na(0)

      return(df)
    } else {
      df <- read_csv(
        file = .path,
        col_types = "cdcdccccccccccccccccccc",
        trim_ws = TRUE,
        name_repair = make.names,
        show_col_types = FALSE
      ) %>%
        set_names(fields23) %>%
        pivot_longer(
          cols = !c(1:6, 21:23),
          names_to = "Grade",
          values_to = "Enrollment"
        ) %>%
        mutate(
          DivisionName = glue("{DivisionName} Public Schools"),
          SubgroupCategory = set_subgroup_category(.path),
          Subgroup = set_subgroup(.path, Subgroup),
        ) %>%
        select(
          c(
            SchoolYear:SchoolName,
            SubgroupCategory,
            Subgroup,
            Grade:Enrollment
          )
        )

      df$Enrollment <- df$Enrollment %>%
        replace_na(0)

      return(df)
    }
  }

  # process all enrollment files and write to a data frame
  enrollment_div_sch_grade <- list.files(
    path = "./data/raw/enrollment/div_sch_grade",
    pattern = ".csv",
    full.names = TRUE
  ) %>%
    set_names() %>%
    map_dfr(
      .f = get_enrollment
    )

  return(enrollment_div_sch_grade)
}
