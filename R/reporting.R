#' Round Numeric Variable
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
    # rounds and/or uses 1,000 separator.
    round(x, digits) %>% format(x, big.mark = ",", format = "f", scientific = FALSE, nsmall = digits)
}

#' Make Word File Using Template
#'
#' This simply uses the docx() function in ReporterRs package with the template file embedded
#' within this package.  An external template file can also be used.
#'
#' @param template_file File path to a .docx file that will be used as template.  Default is
#' to use the included template file.
#'
#' @return Returns a document that can be used with ReporteRs package
#' @examples
#' \dontrun{# nothing here yet}
#' @export
create_word_doc <- function(template_file = NULL){
    if(is.NULL(template)){
        t <- paste0(system.file("extdata", package = "jigsaw"), "template.docx")
        docx(template = t)
    } else {
        if(file.exists(template_file)){
            docx(template = template_file)
        } else {
            stop("The template_file does not seem to exist.  Please check it.")
        }
    }
}


#' Create a Table in Word
#'
#' @param df Data frame that will be the table
#' @param footer Footer text for the table
#' @param header Header text for the table
#'
#' @return Returns a FlexTable object from the ReporteRs package.  To add this to a document, use the addtodoc() function
#' @import ReporteRs
#' @import ReporteRsjars
#' @import rJava
#' @examples
#' \dontrun{# nothing here yet}
#' @export
wordtable <- function(df, footer = NULL, header = NULL){
    ft <- FlexTable(df, header.columns = is.null(header), body.par.props = parProperties(padding = 3, text.align = "center"),
        header.par.props = parProperties(padding = 3, text.align = "center"))
    if(!is.null(header)) {
        ft <- addHeaderRow(ft, value = names(df), text.properties = textBold())
        ft <- addHeaderRow(ft, value = header, colspan = length(names(df)), first = TRUE, text.properties = textBold())
    }
    if(!is.null(footer)) {
        ft <- addFooterRow(ft, value = footer, colspan = length(names(df)), text.properties = textItalic())
    }
    return(ft)
}

#' Add To Word Document
#'
#' Adds a ReporteRs object to an existing document.  Generally, this will be a figure or a table, but it
#' could be anything supported by the ReporteRs package.
#'
#' @param docu Document object to which the object will be added
#' @param item Object from ReporteRs package to add
#' @param caption Text for the caption
#' @param pagebreak Should there be a pagebreak added (default is TRUE)
#'
#' @return Adds object to document
#' @import ReporteRs
#' @import ReporteRsjars
#' @import rJava
#' @examples
#' \dontrun{# nothing here yet}
#' @export
addtodoc <- function(docu, item, caption = NULL, pagebreak = TRUE) {
    if(!is.null(caption)){
        docu <- addParagraph(docu, value = caption, stylename = "Caption")
    }
    docu <- addFlexTable(docu, item)
    if(pagebreak){
        docu <- addPageBreak(docu)
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
.table_row <- function(dt, variable, label = NULL, digits = 2, levsymbol = "", ...){
    fmean <- function(x) mean(x, na.rm = TRUE) %>%
        fmt(digits)
    fsd   <- function(x) sqrt(var(x, na.rm = TRUE)) %>%
        fmt(digits)
    fpct  <- function(x) (sum(x, na.rm = TRUE) / sum(!is.na(x))) %>%
        fmt(digits)
    fnum  <- function(x) sum(x, na.rm = TRUE) %>%
        fmt(0)
    fpct_f  <- function(x) (sum(x == levels(x)[[2]], na.rm = TRUE) / sum(!is.na(x))) %>%
        fmt(digits)
    fnum_f  <- function(x) sum(x == levels(x)[[2]], na.rm = TRUE) %>%
        fmt(0)
    ftot  <- function(x) sum(!is.na(x)) %>%
        fmt(0)
    label <- ifelse(is.null(label), variable, label)
    check <- .find_class(dt, variable)
    if(check$type == "cont") { # continuous
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
        l <- length(levels(dt[[variable]]))
        varname <- c(label, rep("", (l - 1)))
        q <-
            data.table(
                varname,
                count = dt[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = variable, by = variable],
                total = dt[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = variable]
            ) %>%
                setnames(., 1:4, c("varname", "levels", "col2", "col3")) %>%
                .[, col1 := (col2 / col3)] %>%
                setcolorder(., c(1, 2, 5, 3, 4))
        q$col1 <-
            q$col1 %>%
            round(2) %>%
            format(., big.mark = ",", format = "f", scientific = FALSE, nsmall = 2)
        q$col2 <-
            q$col2 %>%
            round(0) %>%
            format(., big.mark = ",", format = "f", scientific = FALSE, nsmall = 0)
        q$col3 <-
            q$col3 %>%
            round(0) %>%
            format(., big.mark = ",", format = "f", scientific = FALSE, nsmall = 0)
        return(q)
    } else
        stop("can't handle this data type yet")
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
        varname = label,
        levels = levsymbol,
        col1 = dt[, lapply(.SD, f1), .SDcols = variable] %>%
            unlist(),
        col2 = dt[, lapply(.SD, f2), .SDcols = variable] %>%
            unlist(),
        col3 = dt[, lapply(.SD, f3), .SDcols = variable] %>%
            unlist())
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
          class == "logical",
          "2level",
          type)]
    d[,
      type := ifelse(
          class == "factor" & unique_levels == 2,
          "2factor",
          type)]
    d[,
      type := ifelse(
          class == "factor" & unique_levels > 2,
          "factor",
          type)]
    return(d)
}

#' Make "Table 1" to Summarize Variables in Data Frame
#'
#' @param varlist Data frame containing 2 columns.  Column 1 is the names of the variables.
#' Column 2 is a name to be used as the variable name (typically longer than the variable name).
#' @param dt Data table with variables of interest in it.
#'
#' @return
#'
#' @examples
#' \dontrun{# nothing here yet}
#' @export
make_table1 <- function(varlist, dt){
    output <- vector("list", nrow(varlist))
    for(i in seq_along(varlist[, 1])) {
        output[[i]] <- .table_row(dt, varlist[i, 1], varlist[i, 2])
    }
    return(data.table::rbindlist(output))
}

# test data set for table 1 functions:
# options(stringsAsFactors = FALSE)
# # Test data set
# t <- data.table(a=1:10, b=as.factor(letters[1:10]), c=c(TRUE, FALSE), d=runif(10, 0.5, 100), e=c(0,1), f=as.integer(c(0,1)), g=as.numeric(1:10), h=c("cat1", "cat2", "cat3", "cat4", "cat5"), i = as.factor(c("m", "f")))
# # Test variable names and labels
# w <- data.frame(u = c("a", "b", "c", "d"), v = c("variable 1", "variable 2", "variable 3", "variable 4"))
# # Run the function
# make_table1(w, t)
