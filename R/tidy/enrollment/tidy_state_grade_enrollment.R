
## This function formats enrollment data gathered from the Virginia Department 
## of Education's Fall Membership Build-A-Table web application aggregated by 
## grade level.                                                           
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

tidy_state_grade_enrollment <- function() {
    
    # vector of field names for the file with all students
    fields18 <- c(
        "SchoolYear", "Pre-KG", "KG", "1st Grade", "2nd Grade", "3rd Grade", 
        "4th Grade", "5th Grade", "6th Grade", "7th Grade", "8th Grade", 
        "9th Grade", "10th Grade", "11th Grade", "12th Grade", "FullTime", 
        "PartTime", "Total"
    )
    
    # vector of field names for the files with subgroups
    fields19 <- c(
        "SchoolYear", "Subgroup", "Pre-KG", "KG", "1st Grade", "2nd Grade", 
        "3rd Grade", "4th Grade", "5th Grade", "6th Grade", "7th Grade", 
        "8th Grade", "9th Grade", "10th Grade", "11th Grade", "12th Grade", 
        "FullTime", "PartTime", "Total"
    )
    
    # FUNCTION - processes the individual enrollment files
    get_enrollment <- function(.path) {
        if (str_detect(.path, "all")) {
            df <- read_csv(
                file = .path,
                col_types = "cccccccccccccccccc",
                trim_ws = TRUE,
                name_repair = make.names,
                show_col_types = FALSE
            ) %>%
                set_names(fields18) %>%
                pivot_longer(
                    cols = !c(1, 16:18),
                    names_to = "Grade",
                    values_to = "Enrollment"
                ) %>%
                mutate(
                    DivisionNumber = -1,
                    DivisionName = "All Divisions",
                    SchoolNumber = -1,
                    SchoolName = "All Schools",
                    SubgroupCategory = set_subgroup_category(.path),
                    Subgroup = set_subgroup(.path),
                ) %>%
                select(
                    c(
                        SchoolYear,
                        DivisionNumber:DivisionName,
                        SchoolNumber:SchoolName,
                        SubgroupCategory,
                        Subgroup,
                        Grade:Enrollment
                    )
                )
            
            return(df)
        } else {
            df <- read_csv(
                file = .path,
                col_types = "ccccccccccccccccccc",
                trim_ws = TRUE,
                name_repair = make.names,
                show_col_types = FALSE
            ) %>%
                set_names(fields19) %>%
                pivot_longer(
                    cols = !c(1:2, 17:19),
                    names_to = "Grade",
                    values_to = "Enrollment"
                ) %>%
                mutate(
                    DivisionNumber = -1,
                    DivisionName = "All Divisions",
                    SchoolNumber = -1,
                    SchoolName = "All Schools",
                    SubgroupCategory = set_subgroup_category(.path),
                    Subgroup = set_subgroup(.path, Subgroup),
                ) %>%
                select(
                    c(
                        SchoolYear,
                        DivisionNumber:DivisionName,
                        SchoolNumber:SchoolName,
                        SubgroupCategory,
                        Subgroup,
                        Grade:Enrollment
                    )
                )
            
            return(df)
        }
    }
    
    # process all enrollment files and write to a data frame
    enrollment_state_grade <- list.files(
        path = "./data/raw/enrollment/state_grade",
        pattern = ".csv",
        full.names = TRUE
    ) %>%
        set_names() %>%
        map_dfr(
            .f = get_enrollment
        )
    
    return(enrollment_state_grade)
}
