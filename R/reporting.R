#' Round Numeric Variable
#'
#' This was taken from this StackOverflow answer:  http://stackoverflow.com/questions/21328047/round-multiple-vectors-in-dataframe-with-plyr/21328269#21328269
#'
#' @param x Vector to round
#' @param n Number of digits for rounding (default =)
#'
#' @return Vector with  numeric variable rounded
#'
#' @examples
#' \dontrun{# nothing here yet}
#' @export
round_if_numeric <- function(x, n = 3) {
    y <- if(is.numeric(x)) round(x, n) else x
    return(y)
}

#' Round All Numeric Columns in Data Frame
#'
#' This was taken from this StackOverflow answer:  http://stackoverflow.com/questions/21328047/round-multiple-vectors-in-dataframe-with-plyr/21328269#21328269
#'
#' @param df Data frame
#' @param n Digits to round to (to right of decimal)
#'
#' @return Data frame
#'
#' @examples
#' \dontrun{# nothing here yet}
#' @export
round_df <- function(df, n = 3) {
    y <- lapply(df, round_if_numeric(), n = n)
    y <- as.data.frame(y)
    return(x)
}

#' Format Numeric Value as Character
#'
#' @param x vector
#' @param digits digits to round
#'
#' @return The original numeric vector rounded, formatted and converted to character.
#'
#' @examples
#' \dontrun{# nothing here yet}
#' @export
fmt <- function(x, digits = 2) {
    if(all(is.na(x))){
        return(NA_real_)
    } else {
    y <-
        round(x, digits) %>%
        format(x, big.mark = ",", format = "f", scientific = FALSE, nsmall = digits)
    return(y)
    }
}

#' Make Word File Using Template
#'
#' This simply uses the read_docx() function in officer package with the template file embedded
#' within this package.  An external template file can also be used.
#'
#' @param template_file File path to a .docx file that will be used as template.  Default is
#' to use the included template file.
#'
#' @return Returns a document that can be used with officer package
#' @examples
#' \dontrun{# nothing here yet}
#' @export
create_word_doc <- function(template_file = NULL){
    if(is.null(template_file)){
        template_file <- paste0(system.file("extdata", package = "jigsaw.r"), "/template.docx") # normalize path for windows
    } else {
        if(!file.exists(template_file)){
            stop("The template_file does not seem to exist.  Please check it.")
        }
    }
    officer::read_docx(path = template_file)
}


#' Create a Table in Word
#'
#' @param df Data frame that will be the table
#' @param footer Footer text for the table
#' @param header Header text for the table
#'
#' @return Returns a FlexTable object.  To add this to a document, use the addtodoc() function
#' @import officer
#' @import flextable
#' @examples
#' \dontrun{# nothing here yet}
#' @export
wordtable <- function(df, footer = NULL, header = NULL, autofit_table = FALSE){
    # flextable requires the df names to be valid data.frame names
    df_copy <- copy(df)
    old_names <- copy(names(df_copy)) # Need to copy otherwise just a pointer to the df_copy names vector
    new_names <- make.names(old_names)
    setnames(df_copy, old_names, new_names)

    ft <- flextable(df_copy)

    if(!is.null(header)) {
        ft <- do.call( add_header, c( list( x = ft, top = FALSE), as.list(header) ) )
    }
    if(!is.null(footer)) {
        ft <-  do.call( add_footer, c( list( x = ft, top = FALSE), as.list(footer) ) )
        ft <- italic(ft, part="footer")
    }

    first_col_name <- names(df_copy)[1]

    names(old_names) <- new_names

    ft <- do.call( set_header_labels, c(list(ft),as.list(old_names)))
    ft <- bold(ft, bold = TRUE, part = "header")
    ft <- align(ft, align = "center", part = "all")
    ft <- padding(ft, padding = 3, part = "all")
    #ft <- merge_v(ft, j = first_col_name, part = "body")

    if( autofit_table ) {
        ft <- autofit(ft)
    }

    return(ft)
}

#' Add To Word Document
#'
#' Adds a officer object to an existing document.  Generally, this will be a figure or a table, but it
#' could be anything supported by the officer package.
#'
#' @param docu Document object to which the object will be added
#' @param item Object from officer package to add
#' @param caption Text for the caption
#' @param depth Depth the table caption should be set
#' @param pagebreak Should there be a pagebreak added (default is TRUE)
#'
#' @return Adds object to document
#' @import officer
#' @examples
#' \dontrun{# nothing here yet}
#' @export
addtodoc <- function(docu, item, caption = NULL, caption_depth = 1, pagebreak = TRUE) {

    if(!is.null(caption)){
        docu <- body_add_par(docu, value = caption) %>%
            add_table_caption(depth = caption_depth)
    }

    docu <- flextable::body_add_flextable(docu, item)

    if(pagebreak){
        docu <- body_add_break(docu)
    }

    return(docu)
}


#' Fix Tidyr "Tidy" Names
#'
#' @param df Data frame from running tidyr tidy()
#'
#' @return Data frame with proper column names
#'
#' @examples
#' \dontrun{# nothing here yet}
#' @export
tidynames <- function(df) {
    names(df) <- c("Variable", "Estimate", "Standard Error", "Statistic", "P-value")
    return(df)
}

#' Fix Tidyr "Glance" Names
#'
#' @param df Data frame from running tidyr glance()
#'
#' @return Data frame with proper column names
#'
#' @examples
#' \dontrun{# nothing here yet}
#' @export
glancenames <- function(df) {
    names(df) <- c("Null Deviance", "Null DF", "Log Likelihood", "AIC", "BIC")
    return(df)
}

#' Make a Single Table Row (internal)
#'
#' uses fmt for digits to report, and find_class to identify type of data and choose formatting.
#' creates a single row for a table based on a single variable.  need to use rbind and then setnames to name columns
#' need to add function to handle factors and/or report multiple rows per variable
#' factor uses upper level (of two level factor) as numerator (e.g., levels = c("m", "f") uses f)

#' @param dt data table
#' @param variable variable
#' @param label label
#' @param digits digits for rounding
#' @param levsymbol level symbol
#' @param ... future?
#'
#' @return Single row for summary table
#' @import  magrittr
#' @import  data.table
#' @examples
#' \dontrun{# nothing here yet}
#' @export
.table_row <- function(dt, variable, label = NULL, scaling = 100, digits, levsymbol = "", ...){
    fmean <- function(x) mean(x, na.rm = TRUE) %>%
        fmt(digits)
    fsd   <- function(x) sqrt(var(x, na.rm = TRUE)) %>%
        fmt(digits)
    fpct  <- function(x) (scaling * sum(x, na.rm = TRUE) / sum(!is.na(x))) %>%
        fmt(digits) %>%
        paste0(., "%")
    fnum  <- function(x) sum(x, na.rm = TRUE) %>%
        fmt(0)
    fpct_f  <- function(x) (scaling * sum(x == levels(x)[[2]], na.rm = TRUE) / sum(!is.na(x))) %>%
        fmt(digits)  %>%
        paste0(., "%")
    fnum_f  <- function(x) sum(x == levels(x)[[2]], na.rm = TRUE) %>%
        fmt(0)
    ftot  <- function(x) sum(!is.na(x)) %>%
        fmt(0)
    label <- ifelse(is.null(label), variable, label)
    check <- .find_class(dt, variable)
    if(check$type == "empty") { # all NA -- no values
        data.table(
            varname = label,
            levels = levsymbol,
            col1 = NA,
            col2 = NA,
            col3 = NA)
    } else if(check$type == "cont") { # continuous
        .make_5cols(
            dt = dt,
            variable = variable,
            label = label,
            levsymbol = levsymbol,
            digits = digits,
            f1 = fmean,
            f2 = fsd,
            f3 = ftot)
    } else if(check$type == "2level") { # 2 level variable reported as one row using 0,1 (needs to handle char, other integer ranges, and numeric)
        .make_5cols(
            dt = dt,
            variable = variable,
            label = label,
            levsymbol = levsymbol,
            digits = digits,
            f1 = fpct,
            f2 = fnum,
            f3 = ftot)
    } else if(check$type == "2factor") { # 2 level factor reported as one row using 0,1
        .make_5cols(dt = dt,
                    variable = variable,
                    label = label,
                    levsymbol = levsymbol,
                    digits = digits,
                    f1 = fpct_f,
                    f2 = fnum_f,
                    f3 = ftot)
    } else if(check$type == "factor") { # factor - may want to coerce everything categorical to factor and use this
        # make sure variable is a factor
        dt_up <- copy(dt)

        if( !is.factor(dt[[variable]] )){
            dt[[variable]] <- as.factor(dt[[variable]])
        }
        .summarize_cats(dt = dt_up, variable = variable, label = label )
    } else{
        warning(variable, " of type ", check$type, "can't handle this data type yet")
        NULL
    }
}
#' Make 5 Columns (internal)
#'
#' @param dt data table
#' @param variable variable
#' @param label label
#' @param levsymbol level symbol
#' @param f1 function 1
#' @param f2 function 2
#' @param f3 function 3
#' @param ... for future?
#' @return tbd
#' @import  magrittr
#' @import  data.table
#' @examples
#' \dontrun{# nothing here yet}#'
#' @export
.make_5cols <- function(dt, variable, label, levsymbol, f1, f2, f3, ...){
    data.table(
        varname = c(label, rep("",length(levsymbol)-1)),
        levels = levsymbol,
        col1 = dt[, lapply(.SD, f1), .SDcols = variable] %>%
            unlist(),
        col2 = dt[, lapply(.SD, f2), .SDcols = variable] %>%
            unlist(),
        col3 = dt[, lapply(.SD, f3), .SDcols = variable] %>%
            unlist())
}

.summarize_cats <- function(dt, variable, label, digits = 2, scaling = 100, useNA = "no") {
    res_dt <- as.data.table(table(dt[[variable]], useNA = useNA ))

    res_dt[, .( varname = c(label, rep("",length(V1)-1)),
                levels = V1,
                col1 = paste0( fmt( N / sum(N) * scaling, digits), "%"),
                col2 = fmt(N, 0),
                col3 = fmt(sum(N),0) )]
}

#' Determine Data Type and/or Class
#'
#' @param dt data table
#' @param variable variable
#'
#' @return Type of data
#' @import  data.table
#' @examples
#' \dontrun{# nothing here yet}
#' @export
.find_class <- function(dt, variable){
    cl <- class(dt[, get(variable)])
    un <- dt[!is.na(get(variable)), length(unique(get(variable)))]
    d <-
        data.table(
            class = cl,
            unique_levels = un,
            type = "other")
    d[,
      type := ifelse(
        class %in% c("numeric", "integer", "difftime") & unique_levels > 2,
        "cont",
        type)]
    d[,
      type := ifelse(
          class %in% c("integer", "numeric") & unique_levels <= 2,
          "2level",
          type)]
    d[,
      type := ifelse(
          class == "logical" & unique_levels %in% 1:2,
          "2level",
          type)]
    d[,
      type := ifelse(
          unique_levels == 0,
          "empty",
          type)]
    d[,
      type := ifelse(
          class == "factor" & unique_levels == 2,
          "factor",
          type)]
    d[,
      type := ifelse(
          class == "factor" & unique_levels > 2,
          "factor",
          type)]

    d[,
      type := ifelse(
          class == "character" & unique_levels < 20,
          "factor",
          type)]
    return(d)
}

#' Make "Table 1" to Summarize Variables in Data Frame
#'
#' @param varlist Data frame containing 2 columns.  Column 1 is the names of the variables.
#' Column 2 is a name to be used as the variable name (typically longer than the variable name).
#' @param dt Data table with variables of interest in it.
#' @param digits Number of digits for continuous variables
#' @param verbose Boolean When TRUE display debug information
#'
#' @return Returns Table 1 as a data.table
#'
#' @examples
#' \dontrun{# nothing here yet}
#' @export
make_table1 <- function(varlist, dt, digits = 2, verbose = FALSE){
    output <- vector("list", nrow(varlist))

    if(verbose) message("Generating Table 1")

    for(i in 1:nrow(varlist)) {

        if(verbose) message("...", varlist[[1]][i])

        res <- .table_row(dt, varlist[[1]][i], varlist[[2]][i], digits = digits)

        if( !is.null(res) ) {
            output[[i]] <- res
        }
    }

    res <- data.table::rbindlist(output)
    setnames(res, c("Variable","Category","Mean or Pct", "SD or Count", "Sample Size"))

    return(res)
}

#' Add a caption before a table
#'
#' @param x officer doc object
#' @param style character word style
#' @param depth Depth of sequence number
#'
#' @return Returns officer doc object
#'
#' @export
add_table_caption <- function (x, style = NULL, depth)
{
    x <- slip_in_text(x, str = ": ", style = style, pos = "before")
    x <- slip_in_seqfield(x, str = "SEQ table \\* Arabic \\s 1 \\* MERGEFORMAT",
                          style = style, pos = "before")
    x <- slip_in_text(x, str = "Table ", style = style, pos = "before")
    x
}
# test data set for table 1 functions:
# options(stringsAsFactors = FALSE)
# # Test data set
# t <- data.table(a=1:10, b=as.factor(letters[1:10]), c=c(TRUE, FALSE), d=runif(10, 0.5, 100), e=c(0,1), f=as.integer(c(0,1)), g=as.numeric(1:10), h=c("cat1", "cat2", "cat3", "cat4", "cat5"), i = as.factor(c("m", "f")))
# # Test variable names and labels
# w <- data.frame(u = c("a", "b", "c", "d"), v = c("variable 1", "variable 2", "variable 3", "variable 4"))
# # Run the function
# make_table1(w, t)
