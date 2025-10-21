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

  # Write the template file
  path_template <- paste0(path, ".template.csv")
  df_template <- data.frame(
    names = names(template),
    types = template
  )
  write.csv(df_template, file = path_template, row.names = FALSE)

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
    return(con)
  }

  RSQLite::dbCreateTable(conn = con, name, fields = template)

  return(con)
}

#' Read a database template file
#'
#' @description Read a `.template.csv` file created by `create_database()` and
#' return a named character vector of SQLite types.
#'
#' @param path Path to the `.template.csv` file.
#' @param drop_id Logical; if `TRUE` (default), remove the `"id"` column.
#'
#' @return Named character vector of SQLite column types.
#' @noRd
read_database_template_file <- function(path, drop_id = TRUE){
  df = read.csv(path)

  types <-  df[[2]]
  names(types) <-  df[[1]]

  if(drop_id){
    types <- types[!names(types) %in% "id"]
  }

  return(types)
}

#' Default database template
#'
#' @description Return a minimal example template defining four columns and
#' their SQLite types.
#'
#' @return Named character vector of SQLite column types.
#' @noRd
default_template <- function(){
  c(
    id = "TEXT",
    numeric_feature = "REAL",
    text_feature = "TEXT",
    logical_feature = "BOOLEAN"
  )
}


#' Query a single row by id using a template
#'
#' @description Given a DBI connection, an `id`, and a template (named character
#' vector of SQLite types, e.g. "TEXT", "REAL", "INTEGER", "BOOLEAN"), fetch the
#' corresponding row from the table and return a **named list of scalars**
#' coerced to R types per the template. If no row is found, returns a named list
#' of `NA` values (typed sensibly) unless `on_missing = "NULL"`.
#'
#' @param conn       A `DBIConnection` to the SQLite DB.
#' @param id         Row identifier (character scalar).
#' @param template   Named character vector of SQLite types. Names are columns.
#'                   Types can include "TEXT", "REAL", "INTEGER", "BOOLEAN".
#' @param table      Table name. Default "table1".
#' @param include_id Logical; if `TRUE` and "id" not in `names(template)`, it is
#'                   prepended to the query result (as character).
#' @param on_missing What to return when no row is found: `"NA"` (default; a
#'                   named list of typed NAs) or `"NULL"`.
#'
#' @return A named list with names matching `template` (and `id` if requested).
#' @export
query_from_template <- function(conn, id, template, table = "table1",
                                include_id = TRUE, on_missing = c("NA", "NULL")) {
  stopifnot(DBI::dbIsValid(conn))
  on_missing <- match.arg(on_missing)

  if (is.null(names(template)) || any(!nzchar(names(template)))) {
    stop("`template` must be a named character vector of SQLite types.")
  }

  # Normalize type labels once
  tupper <- toupper(as.character(template))
  names(tupper) <- names(template)

  # Determine columns to fetch
  cols <- names(template)
  if (include_id && !"id" %in% cols) {
    cols <- c("id", cols)
  }

  # Quote identifiers safely
  qcols <- vapply(cols, DBI::dbQuoteIdentifier, conn = conn, FUN.VALUE = character(1))
  qtable <- DBI::dbQuoteIdentifier(conn, table)

  sql <- sprintf("SELECT %s FROM %s WHERE %s = ? LIMIT 1",
                 paste(qcols, collapse = ", "),
                 qtable,
                 DBI::dbQuoteIdentifier(conn, "id"))

  res <- DBI::dbGetQuery(conn, sql, params = list(id))

  # Helper: typed NA for each declared type
  typed_na <- function(dtype) {
    dtype <- toupper(dtype)
    if (identical(dtype, "BOOLEAN")) return(NA)              # will coerce to logical below
    if (grepl("REAL|DOUBLE|NUMERIC|DECIMAL", dtype)) return(NA_real_)
    if (grepl("INT", dtype)) return(NA_integer_)
    NA_character_
  }

  # Build default (for missing row OR missing columns)
  default_vec <- lapply(names(template), function(nm) typed_na(tupper[[nm]]))
  names(default_vec) <- names(template)
  if (include_id && !"id" %in% names(default_vec)) {
    default_vec <- c(list(id = NA_character_), default_vec)
  }

  if (!nrow(res)) {
    return(if (on_missing == "NULL") NULL else default_vec)
  }

  row <- res[1, , drop = FALSE]

  # Coercion per template
  coerce_one <- function(value, colname) {
    if (identical(colname, "id")) {
      return(as.character(value))
    }
    dtype <- tupper[[colname]]
    if (is.null(dtype)) {
      # If dtype isn't in template (e.g., when include_id=TRUE and id is in template already)
      return(value)
    }
    dtype <- toupper(dtype)

    if (identical(dtype, "BOOLEAN")) {
      # stored as 0/1 (INTEGER) in SQLite
      if (is.null(value) || length(value) == 0 || is.na(value)) return(NA)
      return(as.logical(as.integer(value)))
    }
    if (grepl("REAL|DOUBLE|NUMERIC|DECIMAL", dtype)) {
      v <- suppressWarnings(as.numeric(value))
      return(if (length(v)) v else NA_real_)
    }
    if (grepl("INT", dtype)) {
      v <- suppressWarnings(as.integer(value))
      return(if (length(v)) v else NA_integer_)
    }
    # TEXT and everything else
    v <- as.character(value)
    if (length(v) == 0) v <- NA_character_
    v
  }

  # Ensure all requested columns exist in the result (fill with NA when absent)
  missing_cols <- setdiff(cols, colnames(row))
  if (length(missing_cols)) {
    for (mc in missing_cols) row[[mc]] <- NA
  }
  row <- row[ , cols, drop = FALSE]

  out <- mapply(coerce_one, value = as.list(row[1, , drop = TRUE]), colname = names(row),
                SIMPLIFY = FALSE, USE.NAMES = TRUE)
  out
}

#' Write a row to SQLite using a template (typed, safe, upsert)
#'
#' @description
#' Given a DBI connection, an `id`, a named list of values, and a template
#' (named character vector of SQLite types), write the row into `table`.
#' Values are coerced to the appropriate SQLite-compatible representations:
#'   - "BOOLEAN" -> 0/1 (logical)
#'   - "REAL"/numeric types -> numeric
#'   - "INTEGER" -> integer
#'   - "TEXT" -> character
#' Missing entries in `values` become `NULL` (SQLite) for that column.
#'
#' Uses an "upsert" implemented as UPDATE-then-INSERT inside a transaction,
#' so it does **not** require a UNIQUE constraint on `id`.
#'
#' @param conn     A `DBIConnection`.
#' @param id       Row identifier (character scalar). If `values[["id"]]`
#'                 is present, it is ignored in favor of this argument.
#' @param values   Named list of scalars for columns in the template (excluding `id`).
#'                 Extra names are ignored; missing names become NULL.
#' @param template Named character vector of SQLite types ("TEXT", "REAL",
#'                 "INTEGER", "BOOLEAN"). Names are column names.
#' @param table    Table name. Default "table1".
#'
#' @return Invisibly, a list with elements:
#'         `action` = "update" or "insert", and `rows` = rows affected (integer).
#' @export
write_to_database <- function(conn, id, values, template, table = "table1") {
  stopifnot(DBI::dbIsValid(conn))
  if(is.numeric(id)) id <- as.character(id)
  if (!is.character(id) || length(id) != 1L || !nzchar(id)) {
    stop("`id` must be a non-empty character scalar.")
  }
  if (is.null(names(template)) || any(!nzchar(names(template)))) {
    stop("`template` must be a named character vector of SQLite types.")
  }
  # if (!"id" %in% names(template)) {
  #   stop("`template` must include an 'id' column.")
  # }

  # Normalize declared types
  decl <- toupper(as.character(template))
  names(decl) <- names(template)

  # Columns to write (exclude id; it's a separate parameter)
  write_cols <- setdiff(names(template), "id")

  # Coercion: R -> DB-friendly (NA -> NULL by DBI; BOOLEAN -> 0/1; etc.)
  coerce_for_db <- function(val, dtype) {
    dtype <- toupper(dtype)
    if (identical(dtype, "BOOLEAN")) {
      if (is.null(val) || (length(val) == 1 && is.na(val))) return(NA_integer_)
      return(as.integer(isTRUE(val)))
    }
    if (grepl("REAL|DOUBLE|NUMERIC|DECIMAL", dtype)) {
      v <- suppressWarnings(as.numeric(val))
      if (!length(v)) return(NA_real_)
      return(v)
    }
    if (grepl("INT", dtype)) {
      v <- suppressWarnings(as.integer(val))
      if (!length(v)) return(NA_integer_)
      return(v)
    }
    # TEXT & fallback
    v <- if (is.null(val)) NA_character_ else as.character(val)
    if (!length(v)) v <- NA_character_
    v
  }

  # Build a complete row list in template order (excluding id)
  row_vals <- lapply(write_cols, function(nm) {
    coerce_for_db(values[[nm]], decl[[nm]])
  })
  names(row_vals) <- write_cols

  # Safety: verify table exists and columns are present
  if (!DBI::dbExistsTable(conn, table)) {
    stop(sprintf("Table '%s' does not exist.", table))
  }
  tbl_cols <- DBI::dbListFields(conn, table)
  missing_tbl_cols <- setdiff(c("id", write_cols), tbl_cols)
  if (length(missing_tbl_cols)) {
    stop(sprintf("Table '%s' is missing columns: %s",
                 table, paste(missing_tbl_cols, collapse = ", ")))
  }

  # Quoted identifiers
  qtable <- DBI::dbQuoteIdentifier(conn, table)
  qset   <- paste(sprintf("%s = ?", vapply(write_cols, DBI::dbQuoteIdentifier,
                                           conn = conn, FUN.VALUE = character(1))),
                  collapse = ", ")
  qid    <- DBI::dbQuoteIdentifier(conn, "id")
  qcols  <- paste(c(DBI::dbQuoteIdentifier(conn, "id"),
                    vapply(write_cols, DBI::dbQuoteIdentifier,
                           conn = conn, FUN.VALUE = character(1))),
                  collapse = ", ")
  qmarks <- paste(rep("?", length(write_cols) + 1L), collapse = ", ")

  # Parameter lists
  params_update <- c(unname(row_vals), list(id))          # SET ... WHERE id = ?
  params_insert <- c(list(id), unname(row_vals))          # (id, ...) VALUES (?, ?, ...)

  DBI::dbBegin(conn)
  on.exit(try(DBI::dbRollback(conn), silent = TRUE), add = TRUE)

  # 1) Try UPDATE
  sql_update <- sprintf("UPDATE %s SET %s WHERE %s = ?", qtable, qset, qid)
  n_upd <- DBI::dbExecute(conn, sql_update, params = params_update)

  if (n_upd > 0L) {
    DBI::dbCommit(conn); on.exit(NULL, add = FALSE)
    return(invisible(list(action = "update", rows = n_upd)))
  }

  # 2) No row updated → INSERT
  sql_insert <- sprintf("INSERT INTO %s (%s) VALUES (%s)", qtable, qcols, qmarks)
  n_ins <- DBI::dbExecute(conn, sql_insert, params = params_insert)

  DBI::dbCommit(conn); on.exit(NULL, add = FALSE)
  invisible(list(action = "insert", rows = n_ins))
}


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


.ui_to_sqlite <- function(x) {
  x <- tolower(x)
  out <- ifelse(x %in% c("boolean","bool"), "BOOLEAN",
                ifelse(x %in% c("numeric","real","integer"), "REAL",  # use REAL as common numeric
                       "TEXT"))
  stats::setNames(out, names(x))
}

#' Fetch all case IDs from a SQLite database table
#'
#' @description
#' Return all values from the `"id"` column of a specified table.
#'
#' @param conn  A valid `DBIConnection` to the SQLite database.
#' @param table Character scalar; table name. Default `"table1"`.
#'
#' @return A character vector of IDs (possibly empty if the table has no rows).
#' @export
fetch_all_ids_from_database <- function(conn, table = "table1") {
  stopifnot(DBI::dbIsValid(conn))
  if (!DBI::dbExistsTable(conn, table)) {
    stop(sprintf("Table '%s' does not exist in this database.", table))
  }

  qtable <- DBI::dbQuoteIdentifier(conn, table)
  qid    <- DBI::dbQuoteIdentifier(conn, "id")

  sql <- sprintf("SELECT %s FROM %s", qid, qtable)
  res <- DBI::dbGetQuery(conn, sql)

  # Return as character vector (even if id column was numeric)
  if (!nrow(res)) {
    return(character(0))
  }
  as.character(res[[1]])
}
