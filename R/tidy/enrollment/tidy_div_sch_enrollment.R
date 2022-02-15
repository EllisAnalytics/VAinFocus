
## This function formats enrollment data gathered from the Virginia Department 
## of Education's Fall Membership Build-A-Table web application aggregated by 
## division and school.                                                           
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

tidy_div_sch_enrollment <- function() {
    
    # vector of field names for the file with all students
    fields8 <- c(
        "SchoolYear", "DivisionNumber", "DivisionName", "SchoolNumber",
        "SchoolName", "FullTime", "PartTime", "Total"
    )
    
    # vector of field names for the files with subgroups
    fields9 <- c(
        "SchoolYear", "DivisionNumber", "DivisionName", "SchoolNumber",
        "SchoolName", "Subgroup", "FullTime", "PartTime", "Total"
    )
    
    # FUNCTION - processes the individual enrollment files
    get_enrollment <- function(.path) {
        if (str_detect(.path, "all")) {
            df <- read_csv(
                file = .path,
                col_types = "cdcdcccc",
                trim_ws = TRUE,
                name_repair = make.names,
                show_col_types = FALSE
            ) %>%
                set_names(fields8) %>%
                mutate(
                    DivisionName = glue("{DivisionName} Public Schools"),
                    Grade = "All Grades",
                    SubgroupCategory = set_subgroup_category(.path),
                    Subgroup = set_subgroup(.path),
                ) %>%
                select(
                    c(
                        SchoolYear:SchoolName,
                        SubgroupCategory,
                        Subgroup,
                        Grade,
                        "Enrollment" = Total
                    )
                )
            
            df$Enrollment <- df$Enrollment %>%
                replace_na(0)
            
            return(df)
        } else {
            df <- read_csv(
                file = .path,
                col_types = "cdcdccccc",
                trim_ws = TRUE,
                name_repair = make.names,
                show_col_types = FALSE
            ) %>%
                set_names(fields9) %>%
                mutate(
                    DivisionName = glue("{DivisionName} Public Schools"),
                    Grade = "All Grades",
                    SubgroupCategory = set_subgroup_category(.path),
                    Subgroup = set_subgroup(.path, Subgroup),
                ) %>%
                select(
                    c(
                        SchoolYear:SchoolName,
                        SubgroupCategory,
                        Subgroup,
                        Grade,
                        "Enrollment" = Total
                    )
                )
            
            df$Enrollment <- df$Enrollment %>%
                replace_na(0)
            
            return(df)
        }
    }
    
    # process all enrollment files and write to a data frame
    enrollment_div_sch <- list.files(
        path = "./data/raw/enrollment/div_sch",
        pattern = ".csv",
        full.names = TRUE
    ) %>%
        set_names() %>%
        map_dfr(
            .f = get_enrollment
        )
    
    return(enrollment_div_sch)
}
