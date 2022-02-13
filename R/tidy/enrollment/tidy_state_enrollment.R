
## This function formats enrollment data gathered from the Virginia Department 
## of Education's Fall Membership Build-A-Table web application at the state     
## level.
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

tidy_state_enrollment <- function() {
    
    # vector of field names for the file with all students
    fields4 <- c(
        "SchoolYear", "FullTime", "PartTime", "Total"
    )
    
    # vector of field names for the files with subgroups
    fields5 <- c(
        "SchoolYear", "Subgroup", "FullTime", "PartTime", "Total"
    )
    
    # FUNCTION - processes the individual enrollment files
    get_enrollment <- function(.path) {
        if (str_detect(.path, "all")) {
            df <- read_csv(
                file = .path,
                col_types = "cccc",
                trim_ws = TRUE,
                name_repair = make.names,
                show_col_types = FALSE
            ) %>%
                set_names(fields4) %>%
                mutate(
                    DivisionNumber = -1,
                    DivisionName = "All Divisions",
                    SchoolNumber = -1,
                    SchoolName = "All Schools",
                    SubgroupCategory = set_subgroup_category(.path),
                    Subgroup = set_subgroup(.path),
                    Grade = "All Grades"
                ) %>%
                select(
                    c(
                        SchoolYear,
                        DivisionNumber:DivisionName,
                        SchoolNumber:SchoolName,
                        SubgroupCategory,
                        Subgroup,
                        Grade,
                        "Enrollment" = Total
                    )
                )
            
            return(df)
        } else {
            df <- read_csv(
                file = .path,
                col_types = "ccccc",
                trim_ws = TRUE,
                name_repair = make.names,
                show_col_types = FALSE
            ) %>%
                set_names(fields5) %>%
                mutate(
                    DivisionNumber = -1,
                    DivisionName = "All Divisions",
                    SchoolNumber = -1,
                    SchoolName = "All Schools",
                    SubgroupCategory = set_subgroup_category(.path),
                    Subgroup = set_subgroup(.path, Subgroup),
                    Grade = "All Grades"
                ) %>%
                select(
                    c(
                        SchoolYear,
                        DivisionNumber:DivisionName,
                        SchoolNumber:SchoolName,
                        SubgroupCategory,
                        Subgroup,
                        Grade,
                        "Enrollment" = Total
                    )
                )
            
            return(df)
        }
    }
    
    # process all enrollment files and write to a data frame
    enrollment_state <- list.files(
        path = "./data/raw/enrollment/state",
        pattern = ".csv",
        full.names = TRUE
    ) %>%
        set_names() %>%
        map_dfr(
            .f = get_enrollment
        )
    
    return(enrollment_state)
}

