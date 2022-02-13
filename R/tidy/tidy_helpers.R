
# FUNCTION - sets the subgroup category based on the file being processed
set_subgroup_category <- function(.path) {
    if (str_detect(.path, "dis")) {
        return("Students with Disabilities")
    } else if (str_detect(.path, "ell")) {
        return("English Language Learner")
    } else if (str_detect(.path, "race")) {
        return("Federal Ethnicity")
    } else if (str_detect(.path, "ses")) {
        return("Economically Disadvantaged")
    } else {
        return("All Students")
    }
}

# FUNCTION - formats the subgroup value based on the file being processed
set_subgroup <- function(.path, .subgroup = NULL) {
    if (is.null(.subgroup)) {
        return("All Students")
    }
    
    return(
        case_when(
            str_detect(.path, "dis") ~ if_else({{ .subgroup }} == "Y",
                                               true = "SWD",
                                               false = "Not SWD"
            ),
            str_detect(.path, "ell") ~ if_else({{ .subgroup }} == "Y",
                                               true = "ELL",
                                               false = "Not ELL"
            ),
            str_detect(.path, "ses") ~ if_else({{ .subgroup }} == "Y",
                                               true = "Economically Disadvantaged",
                                               false = "Not Economically Disadvantaged"
            ),
            str_detect(.path, "race") ~ {{ .subgroup }}
        )
    )
}