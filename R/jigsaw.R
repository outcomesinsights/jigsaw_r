#' Load Data from ZIP File
#'
#' This function takes a zipfile, identifies all files in the zipfile, and unzips them all into a temp file.
#' @param zipfile The path to the ZIP file to load as a character string.
#' @param load_files Should the files be loaded to the global environment?  Defaults to TRUE.
#' @param save_dir The path to the directory where the file should be saved.  Defaults to working directory.
#'
#' @return  Returns a list of the filenames for later loading.
#' @examples
#' \dontrun{# nothing here yet}
#' @export
load_data <- function(zipfile = NULL, load_files = TRUE, save_dir = ".") {
    message(paste0("Unzipping files from ", zipfile))
    unzippedfiles <-
        unzip(
            zipfile,
            overwrite = TRUE,
            exdir = tempdir())
    message("Files unzipped to temporary directory.")
    metadata <-
        jsonlite::fromJSON(
            grep("dataset_metadata.json", unzippedfiles, value = TRUE))
    message("Metadata file is created.")
    data_dict <-
        data.table::fread(
            grep(
                "dataset_labels.csv",
                unzippedfiles,
                value = TRUE),
            verbose = TRUE)
    data_dict$data_type <- gsub("date", "Date", data_dict$data_type)
    data_dict$data_type <- gsub("string", "character", data_dict$data_type)
    message("Data dictionary file is created.")
    cohort <-
        data.table::fread(
            grep(
                "dataset.csv",
                unzippedfiles,
                value = TRUE),
            colClasses = as.vector(data_dict$data_type),
            verbose = TRUE)
    message("Cohort file is created. Converting dates to IDate class.")
    cohort <- make_dates(cohort, data_dict)
    message("Cohort file date conversion finished.")
    events <-
        data.table::fread(
            grep("events.csv", unzippedfiles, value = TRUE),
            colClasses = list(
                character = c("criterion_type", "value_as_string", "units_source_value", "event_name"),
                numeric = "value_as_number"),
            verbose = TRUE)
    message("Events file is created. Converting dates to IDate class.")
    events <- make_dates(events, data_dict)
    message("Events file date conversion finished.")
    if(load_files){
        events <<- events
        cohort <<- cohort
        data_dict <<- data_dict
        metadata <<- metadata
    } else {
        save(
            list = c("events", "cohort", "data_dict", "metadata"),
            file = save_dir)
        message(paste0("Files saved to ", save_dir))
    }
    message("Finished load process.")
}

#' Converts Variable to Date Type
#'
#' converts all date columns to date format (in place via data.table).  using data.table IDate class -- much faster
#'
#' @param dt data table
#' @param dd The data dictionary with variable types.
#'
#' @return Variable conveted to an R date type
#' @import data.table
#' @examples
#' \dontrun{# nothing here yet}
#' @export
make_dates <- function(dt = NULL, dd = data_dict){
    if (names(dt)[length(names(dt))] == "event_name") {
        datecols <- grep("date$", names(dt), ignore.case = TRUE)
    } else {
        datecols <- dd[, grep("Date", data_type)] # col numbers for dates
    }
    dt[, (datecols) := lapply(.SD, as.IDate, format = "%Y-%m-%d"), .SDcols = datecols]
}

#' Replace NA Values
#'
#' replaces NA value with value in new (default = 0)
#'
#' @param original column from data table
#' @param new replacement value (default = 0)
#'
#' @return Vector with NA replaced
#'
#' @examples
#' \dontrun{# nothing here yet}
#' @export
change_NA <- function(original, new = 0){
    ifelse(is.na(original), new, original)
}

#' Fix Counts to Make 0
#'
#' @param dt data table
#' @param dd data dictionary file
#'
#' @return Corrected counts that have 0 for NA.  Calls change_NA
#' @import data.table
#' @examples
#' \dontrun{# nothing here yet}
#' @export
fix_counts  <- function(dt = cohort, dd = data_dict){
    # replaces NA values in integer (count) variables to make them 0
    countcols <- grep("integer", dd$data_type) # col numbers for counts (integer columns)
    dt[, (countcols) := lapply(.SD, change_NA), .SDcols = countcols] # changes all NA to 0
}


#' Add Event to Cohort Using Function of Numeric Variable in Time Window
#'
#' Takes any numeric data in events file (e.g., laboratory value) and estimates a function of it (e.g., mean)
#' using all available values.  Values can be limited to a window before the person's index date (from cohort file).
#' Windows must be negative (i.e., before index date).  Function parameters can be passed in ... to the function
#' (e.g., na.rm = TRUE).  Default function is mean.  Other options include median, max, and min.

#' @param varname variable name
#' @param days size of window in days
#' @param func function to apply to values (e.g., mean)
#' @param ... passed to function (e.g., na.rm = TRUE)
#'
#' @return see above
#' @import  magrittr
#' @import  data.table
#' @examples
#' \dontrun{# nothing here yet}
#' @export
add_numerical_event <- function(varname = NULL, days = -365, func = mean, ...){
    setkey(cohort, person_id)
    earliest_date <- cohort[, .(person_id, check_date = index_date + days)] %>% setkey(., person_id)
    dt <- events[event_name == varname, .(person_id, start_date, value = value_as_number)] %>%
        setkey(., person_id, start_date) %>%
        merge(., earliest_date, all.x = TRUE) %>%
        .[start_date >= check_date, .(result = func(value, ...)), by = person_id] %>%
        setkey(., person_id)
    merge(cohort, dt, all.x = TRUE) %>%
        .[, .(result)]
}

#' Clean Numeric Event
#'
#' takes an event type and makes all values <= lower cut or >= upper cut "NA".  Also limits to specific unit type (which can be a character vector of types)
#' note -- need to add function to deduplicate by day and allow user to over-ride
#'
#' @param event_group event type to clean
#' @param lower_cut value below which data will be considered NA (inclusive)
#' @param upper_cut value above which data will be considered NA (inclusive)
#' @param unit_type limit to specific unit type (e.g., mmol/L)
#'
#' @return see above
#' @import data.table
#' @examples
#' \dontrun{# nothing here yet}
#' @export
clean_numeric_event <- function(event_group = "", lower_cut = -Inf, upper_cut = Inf, unit_type = NULL) {
    if (!(event_group %in% events[, unique(event_name)])) stop(paste("there is no event name variable called", event_group))
    if (!(is.numeric(lower_cut))) stop(paste("lower_cut is not numeric:", lower_cut))
    if (!(is.numeric(upper_cut))) stop(paste("upper_cut is not numeric:", upper_cut))
    if (!(is.character(unit_type)) & !is.null(unit_type)) stop(paste("unit_type is not a character:", upper_cut))
    setkey(events, event_name, person_id, start_date)
    events[event_group, value_as_number := ifelse(is.na(value_as_number) | value_as_number <= lower_cut | value_as_number >= upper_cut, NA, value_as_number)]
    if (!is.null(unit_type)) {
        events[event_group, value_as_number := ifelse(!is.na(units_source_value) & units_source_value %in% unit_type, value_as_number, NA)]
    }
    return(events)
}

#' Create Classification Variable Based On Window
#'
#' takes categorical (string) data in events file and classifies person based on all available values in the window
#' before the person's index date (from cohort file).  Windows must be negative (i.e., before index date).  NA = FALSE

#' @param varname variable name
#' @param check_value ???
#' @param days size of window in days
#'
#' @return see above
#' @import  magrittr
#' @import  data.table
#' @examples
#' \dontrun{# nothing here yet}
#' @export
add_categorical_event <- function(varname = NULL, check_value = NULL, days = -365){
    setkey(cohort, person_id)
    earliest_date <- cohort[, .(person_id, check_date = index_date + days)] %>%
        setkey(., person_id)
    cat_dt <- events[event_name == varname, .(person_id, start_date, cat_variable = value_as_string)] %>%
        setkey(., person_id, start_date) %>%
        merge(., earliest_date, all.x = TRUE) %>%
        .[start_date >= check_date, .(cat_result = sum(cat_variable %in% check_value, na.rm = TRUE) %>% as.logical), by = person_id] %>%
        setkey(., person_id)
    merge(cohort, cat_dt, all.x = TRUE) %>%
        .[, cat_result := ifelse(is.na(cat_result), FALSE, cat_result)] %>%
        .[, .(cat_result)]
}
