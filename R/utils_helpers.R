#' Create a SQLite DB for `shinycurate` results
#'
#' @description Create (or replace) a `"table1"` results table in a SQLite file.
#' If `template` is a data frame, the table is created and populated; if it's a
#' named vector of SQLite types, an empty table is created.
#'
#' @param path      Path to the `.sqlite` file (created if missing).
#' @param template  Data frame of rows **or** named character vector of SQLite types. If a vector, template values can include "TEXT", "REAL", "INTEGER", or "BOOLEAN"
#'  (converted to INTEGER because sqlite doesn't have logical type.
#' @param overwrite Logical; drop existing `"table1"` before (re)creating. Default `TRUE`.
#'
#' @return A `DBIConnection`. Disconnect with `DBI::dbDisconnect()`.
#'
#' @examples
#' \dontrun{
#' con <- create_database(tempfile(fileext = ".sqlite"),
#'                        template = example_template(), overwrite = TRUE)
#' DBI::dbDisconnect(con)
#'
#' con <- create_database(tempfile(fileext = ".sqlite"),
#'                        template = mtcars, overwrite = TRUE)
#' DBI::dbDisconnect(con)
#' }
#'
#' @export
create_database <- function(path, template = default_template(), overwrite=TRUE){


  con <- RSQLite::dbConnect(RSQLite::SQLite(), path)

  if((is.data.frame(template) & !"id" %in% colnames(template)) | !"id" %in% names(template)) stop("Template MUST include an `id` column")
  if(is.vector(template) & template[names(template) == "id"] != "TEXT") stop("`id` column in template must define a TEXT column")
  name = "table1"

  # Automatically turn BOOLEAN into REAL
  if(is.vector(template) & "BOOLEAN" %in% template) {template[template %in% "BOOLEAN"] <- "INTEGER"}

  # Overwrite = drop so we can create again
  if (overwrite && RSQLite::dbExistsTable(con, name)) {
    RSQLite::dbRemoveTable(con, name)  # equivalent to DROP TABLE IF EXISTS with quoting
  }

  if (is.data.frame(template)) {
    template$id <- as.character(template$id)
    # create + insert
    df <- normalize_for_sqlite(template)
    RSQLite::dbWriteTable(con, name, df, overwrite = overwrite)
    # add_id_column(con)
    return(con)
  }

  RSQLite::dbCreateTable(conn = con, name, fields = template)
  # add_id_column(con)

  # Also write the template to the sampe file
  path_template <- paste0(path, ".template.csv")
  df_template <- data.frame(
    names = names(template),
    types = template
  )
  write.csv(df_template, file = path_template, row.names = FALSE)

  return(con)
}


read_database_template_file <- function(path, drop_id = TRUE){
  df = read.csv(path)

  types <-  df[[2]]
  names(types) <-  df[[1]]

  if(drop_id){
    types <- types[!names(types) %in% "id"]
  }

  return(types)
}

# add_id_column <- function(conn){
#   DBI::dbExecute(conn = conn, statement = "ALTER TABLE table1 ADD COLUMN id TEXT");
#   DBI::dbExecute(conn = conn, statement = "UPDATE table1 SET id = rowid WHERE id IS NULL;");
#   DBI::dbExecute(conn = conn, statement = "CREATE UNIQUE INDEX idx_item_id ON table1(id) WHERE id IS NOT NULL;");
# }

# example_template_df <- function(){
#   data.frame(
#     col1 = numeric(0),
#     col2 = character(0),
#     col3 = logical(0)
#   )
# }
#

default_template <- function(){
  c(
    id = "TEXT",
    numeric_feature = "REAL",
    text_feature = "TEXT",
    logical_feature = "BOOLEAN"
  )
}

# query_from_template <- function(conn, id, template){
#
# }


# helper: coerce R types to SQLite-friendly ones
normalize_for_sqlite <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.logical(x)) as.integer(x)        # 0/1
    else if (is.factor(x)) as.character(x)  # store labels
    else if (inherits(x, "Date")) as.character(x)                # ISO date
    else if (inherits(x, "POSIXt")) format(x, "%Y-%m-%d %H:%M:%S") # timestamp
    else x
  })
  df
}

# generate_example_dataframe <- function(){
#   data.frame(
#     id = as.character(1:10),
#     text = keyToEnglish::generate_random_sentences(n = 10, punctuate = TRUE, fast = TRUE)
#   )
# }

highlight_text <- function(text, pattern=NULL, ignore_case = FALSE, add_span = TRUE){
  if(is.null(pattern)) return(text)
  if(is.na(pattern)) return(text)
  if(nchar(pattern) == 0) return(text)

  text_highlighted <- gsub(
    x = text,
    pattern = paste0("(",pattern, ")"),
    replacement = "<mark>\\1</mark>",
    ignore.case = ignore_case,
    perl = TRUE
  )

  paste0("<span>", text_highlighted, "</span>")
}

# Utilities ---------------------------------------------------------------

to_title <- function(x) {
  x <- gsub("_", " ", x)
  x <- paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "")
  x
}

is_booleanish <- function(vals) {
  if (is.null(vals)) return(FALSE)
  vals <- unique(na.omit(vals))
  if (!length(vals)) return(FALSE)
  truthy <- c("true","false","yes","no","y","n","t","f","0","1",0,1,TRUE,FALSE)
  all(tolower(as.character(vals)) %in% truthy)
}

# Map a column to a UI input
make_input_for_column <- function(id, label, type, distinct_vals) {
  input_id <- paste0("in_prop_", id)

  # Type hints
  is_num  <- grepl("INT|REAL|NUM|DOUBLE|DECIMAL", type, ignore.case = TRUE)
  is_date <- grepl("DATE|TIME", type, ignore.case = TRUE)

  # Use distinct values to decide between select vs free entry
  n_levels <- if (is.null(distinct_vals)) Inf else length(distinct_vals)

  if (is_booleanish(distinct_vals) || grepl("^BOOL", type, ignore.case = TRUE)) {
    numericInput(inputId = input_id, label = label, value = NA, width = "100%")

  } else if (is_date) {
    # If dates present, set a sensible default
    dv <- suppressWarnings(as.Date(distinct_vals))
    default <- if (any(!is.na(dv))) max(dv, na.rm = TRUE) else Sys.Date()
    dateInput(inputId = input_id, label = label, value = default, width = "100%")

  } else if (is_num) {
    numericInput(inputId = input_id, label = label, value = NA, width = "100%")

  } else if (is.finite(n_levels) && n_levels >= 2 && n_levels <= 50) {
    selectizeInput(
      inputId = input_id, label = label,
      choices = sort(unique(distinct_vals)),
      multiple = TRUE, width = "100%",
      options = list(placeholder = "Select…")
    )

  } else {
    textInput(inputId = input_id, label = label, value = "", width = "100%")
  }
}

# Collapse multi-selects, turn "" into NA, logical -> 1/0
coerce_for_sql <- function(x) {
  if (is.null(x)) return(NA)
  if (is.logical(x)) return(as.integer(x))         # TRUE/FALSE -> 1/0
  if (length(x) > 1) x <- paste(x, collapse = ", ")# multi -> "a, b"
  if (is.character(x) && !nzchar(x)) return(NA)    # "" -> NA (→ NULL)
  x
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

split_commas <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NULL)
  unlist(strsplit(as.character(x), "\\s*,\\s*"))
}

safe_id <- function(x) {
  # Replace anything not alnum/underscore with underscore; ensure starts with letter
  out <- gsub("[^A-Za-z0-9_]", "_", x)
  out <- ifelse(grepl("^[A-Za-z]", out), out, paste0("x_", out))
  # Make unique if duplicates
  make.unique(out, sep = "_")
}
