#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


# Read Inputs -------------------------------------------------------------



  # Fetch Curation Dataframe
  curation_df <- reactive({
    req(input$in_file_curate)
    info <- input$in_file_curate
    validate(
      need(nrow(info) == 1, "Please upload exactly one CSV file."),
      need(grepl("\\.csv$", info$name, ignore.case = TRUE), "File must be a .csv")
    )

    path <- info$datapath
    # read it (pick your reader)
    df <- utils::read.csv(path, stringsAsFactors = FALSE)   # or readr::read_csv(path, show_col_types = FALSE)

    # minimal schema checks
    required <- c("id", "text")
    missing  <- setdiff(required, names(df))
    validate(need(length(missing) == 0, paste("Missing column(s):", paste(missing, collapse = ", "))))

    # mutate based on regex search
    input$in_bttn_search_regex # Trigger reload
    df$..regex_hit = isolate(grepl(x=df$text, pattern = pattern(), ignore.case = input$in_regex_ignore_case, perl = TRUE))

    df
  })


  # Get list of all ID values
  all_ids <- eventReactive(curation_df(), {
    df  <- curation_df()
    ids <- unique(as.character(df$id))
    ids <- ids[!is.na(ids) & nzchar(ids)]

    # keep the current selection if it's still valid; otherwise pick the first
    old <- isolate(input$in_current_id)
    sel <- if (length(old) && !is.null(old) && old %in% ids) old else ids[1]
    names(ids) <- paste0(ids, ifelse(df[["..regex_hit"]], "  ✓", ""))
    ids
  })

  # update id choices whenever the data changes
  observeEvent(all_ids(), {
    updateSelectInput(session, "in_current_id",
                      choices = all_ids())
  }, ignoreInit = FALSE)



  # Get REGEX (for highlighting cetain
  pattern <- reactive({input$in_regex})

  # Update current text
  current_text <- eventReactive(input$in_current_id, {
    df <- curation_df()
    text <- df[["text"]][df$id == input$in_current_id][1]
  })

  # Database Connection
  vols <- c(Home = fs::path_home(), WD = getwd())
  shinyFiles::shinyFileChoose(input, "in_pick_db", roots = vols, filetypes = c("sqlite","db",""))

  picked_db_path <- reactiveVal(NULL)

  observeEvent(input$in_pick_db, {
    pf <- shinyFiles::parseFilePaths(vols, input$in_pick_db)
    if (nrow(pf)) picked_db_path(normalizePath(pf$datapath[1], winslash = "/", mustWork = FALSE))
  })

  db_conn <- reactive({
    req(picked_db_path())
    DBI::dbConnect(RSQLite::SQLite(), picked_db_path())
  })


  # Database Table
  db_table <- reactive({
    validate(need(db_conn(), message = "Please upload sqlite database"))
    input$in_refresh_db # Trigger new table when refresh action link is clicked
    DBI::dbGetQuery(db_conn(), "SELECT * FROM table1")
  })

  # Read in Database Template (used to generate property insights)
  template <- reactive({
    req(picked_db_path())
    expected_template_path <- paste0(picked_db_path(), ".template.csv")
    message("Searching for template.csv at", expected_template_path)
    tmp <- read_database_template_file(expected_template_path)
    print(tmp)
    return(tmp)
  })

  # Create UI for filling in missing info ------------------------------------------------
  case_inputs <- mod_template_to_inputs_server(id = "mod_db_column_inputs", template = template)

  # Downloads / Exports ---------------------------------------------------------------

  # Download Default SQLITE database
  output$dl_default_db <- downloadHandler(
    filename = function() "default.sqlite",
    content = function(file) {
      p <- system.file("databases", "default.sqlite", package = "shinycurate")
      # if (p == "") p <- golem::app_sys("databases/default.sqlite")
      file.copy(p, file, overwrite = TRUE)
    })

  # Download CurationText
  output$dl_example_sentences <- downloadHandler(
    filename = function() "example.csv",
    content = function(file) {
      p <- system.file("example.csv", package = "shinycurate")
      # if (p == "") p <- golem::app_sys("example.csv")
      file.copy(p, file, overwrite = TRUE)
    })


  # Rendering ---------------------------------------------------------------
  # Render/Re-render text (whenever data changes / bttn search is clicked)
  output$out_current_text_html <- renderText({
    highlight_text(current_text(), pattern = pattern(), ignore_case = input$in_regex_ignore_case)
  })  |> bindEvent(input$in_bttn_search_regex, current_text())

  output$out_dt_curation <- DT::renderDataTable({ curation_df() })
  output$out_dt_database <- DT::renderDataTable({ db_table() })

  output$out_text_rawdata_summary <- renderText({
      paste0(
        "Regex Hits: ", sum(curation_df()$..regex_hit), "/",nrow(curation_df())
        )
    })

  output$out_picked_db_path <- renderText({
    validate(need(picked_db_path(), message = "Please Select Database"))
    picked_db_path()
    })


# # Hydrate dynamic UI based on selected entry ------------------------------
  # # When the selected ID or the DB table changes, hydrate controls from DB
  # observeEvent(list(input$in_current_id, db_table()), {
  #   conn <- db_conn(); req(conn)
  #   id_chr <- input$in_current_id; req(nzchar(id_chr))
  #
  #   # Try to cast to integer if your DB id is INTEGER, else keep as char
  #   id_val <- suppressWarnings(as.integer(id_chr))
  #   if (is.na(id_val)) id_val <- id_chr
  #
  #   cols <- non_id_cols(); req(nrow(cols) > 0)
  #
  #   # Build SELECT only for the dynamic columns
  #   qcols <- DBI::dbQuoteIdentifier(conn, cols$name)
  #   sql   <- sprintf("SELECT %s FROM %s WHERE %s = ? LIMIT 1",
  #                    paste(qcols, collapse = ", "),
  #                    DBI::dbQuoteIdentifier(conn, "table1"),
  #                    DBI::dbQuoteIdentifier(conn, "id"))
  #
  #   row <- tryCatch(DBI::dbGetQuery(conn, sql, params = list(id_val)), error = function(e) NULL)
  #
  #   # If row not found, clear the controls and return
  #   if (is.null(row) || nrow(row) == 0) {
  #     lapply(cols$name, function(nm) {
  #       inputId <- paste0("in_prop_", nm)
  #       # Try each updater; whichever exists will succeed
  #       try(updateCheckboxInput(session, inputId, value = FALSE), silent = TRUE)
  #       try(updateNumericInput(session, inputId, value = NA), silent = TRUE)
  #       try(updateDateInput(session, inputId, value = NULL), silent = TRUE)
  #       try(updateSelectizeInput(session, inputId, selected = NULL), silent = TRUE)
  #       try(updateTextInput(session, inputId, value = ""), silent = TRUE)
  #     })
  #     return(invisible())
  #   }
  #
  #   # Populate controls with DB values
  #   row <- row[1, , drop = FALSE]
  #   for (i in seq_len(nrow(cols))) {
  #     nm   <- cols$name[i]
  #     typ  <- cols$type[i] %||% ""
  #     val  <- row[[nm]]
  #     inputId <- paste0("in_prop_", nm)
  #
  #     # Choose updater based on SQLite type hints; fall back gracefully
  #     if (grepl("BOOL", typ, ignore.case = TRUE)) {
  #       try(updateCheckboxInput(session, inputId, value = as.logical(as.integer(val %||% 0))), silent = TRUE)
  #
  #     } else if (grepl("DATE|TIME", typ, ignore.case = TRUE)) {
  #       v <- suppressWarnings(as.Date(val))
  #       try(updateDateInput(session, inputId, value = v), silent = TRUE)
  #
  #     } else if (grepl("INT|REAL|NUM|DOUBLE|DECIMAL", typ, ignore.case = TRUE)) {
  #       v <- suppressWarnings(as.numeric(val))
  #       try(updateNumericInput(session, inputId, value = v), silent = TRUE)
  #
  #     } else {
  #       # Prefer selectize (multi) if present; otherwise text
  #       sel <- if (is.character(val) && grepl(",", val)) split_commas(val) else val
  #       ok  <- try(updateSelectizeInput(session, inputId, selected = sel, server = TRUE), silent = TRUE)
  #       if (inherits(ok, "try-error")) {
  #         try(updateTextInput(session, inputId, value = as.character(val %||% "")), silent = TRUE)
  #       }
  #     }
  #   }
  # }, ignoreInit = FALSE)



# Update Database ---------------------------------------------------------

  observeEvent(input$in_bttn_updatedb, {
    conn <- db_conn()
    validate(need(conn, "Upload a SQLite database first"))
    case_id_chr <- input$in_current_id
    validate(need(nzchar(case_id_chr), "Choose a Case ID first"))

    # If your DB id is INTEGER, cast; else we’ll keep as character
    case_id <- suppressWarnings(as.integer(case_id_chr))
    if (is.na(case_id)) case_id <- case_id_chr

    # Collect sidebar values
    props <- case_inputs()                          # named list col -> value
    cols  <- names(props)
    vals  <- lapply(props, coerce_for_sql)       # your helper: logical->1/0, ""->NA, multi->"a, b"
    keep  <- vapply(vals, function(v) !(is.null(v) || (length(v) == 1 && is.na(v))), logical(1))
    cols_to_update <- cols[keep]
    vals_to_update <- unname(vals[keep])

    if (!length(cols_to_update)) {
      showNotification("Nothing to write: fill at least one property.", type = "warning")
      return(invisible())
    }

    q_table <- DBI::dbQuoteIdentifier(conn, "table1")
    q_id    <- DBI::dbQuoteIdentifier(conn, "id")

    # UPDATE ... WHERE id = ?
    set_clause <- paste(
      sprintf("%s = ?", DBI::dbQuoteIdentifier(conn, cols_to_update)),
      collapse = ", "
    )
    sql_update <- sprintf("UPDATE %s SET %s WHERE %s = ?", q_table, set_clause, q_id)
    params_update <- c(as.list(vals_to_update), list(case_id))

    # INSERT (id, <cols...>) VALUES (?, ?, ...)
    insert_cols <- c("id", cols_to_update)
    q_insert_cols <- DBI::dbQuoteIdentifier(conn, insert_cols)
    ph <- paste(rep("?", length(insert_cols)), collapse = ", ")
    sql_insert <- sprintf(
      "INSERT INTO %s (%s) VALUES (%s)",
      q_table, paste(q_insert_cols, collapse = ", "), ph
    )
    params_insert <- c(list(case_id), as.list(vals_to_update))

    # Try UPSERT form first (works if id is UNIQUE/PK and SQLite >= 3.24)
    upsert_supported <- TRUE
    sql_upsert <- sprintf(
      "INSERT INTO %s (%s) VALUES (%s) ON CONFLICT(%s) DO UPDATE SET %s",
      q_table,
      paste(q_insert_cols, collapse = ", "),
      ph,
      DBI::dbQuoteIdentifier(conn, "id"),
      paste(sprintf("%s = excluded.%s",
                    DBI::dbQuoteIdentifier(conn, cols_to_update),
                    DBI::dbQuoteIdentifier(conn, cols_to_update)),
            collapse = ", ")
    )

    res <- tryCatch({
      DBI::dbWithTransaction(conn, {
        # Prefer UPSERT (atomic in one statement)
        if (upsert_supported) {
          DBI::dbExecute(conn, sql_upsert, params = params_insert)
        } else {
          # Fallback: UPDATE then (if 0) INSERT
          n <- DBI::dbExecute(conn, sql_update, params = params_update)
          if (isTRUE(n == 0)) {
            DBI::dbExecute(conn, sql_insert, params = params_insert)
          }
        }
      })
    }, error = function(e) e)

    # If UPSERT threw because SQLite is old or id not unique, fallback once
    if (inherits(res, "error") && upsert_supported) {
      res <- tryCatch({
        DBI::dbWithTransaction(conn, {
          n <- DBI::dbExecute(conn, sql_update, params = params_update)
          if (isTRUE(n == 0)) {
            DBI::dbExecute(conn, sql_insert, params = params_insert)
          }
        })
      }, error = function(e) e)
    }

    if (inherits(res, "error")) {
      showNotification(paste("Write failed:", conditionMessage(res)), type = "error", duration = 8)
    } else {
      showNotification("Row written ✅ (inserted or updated)", type = "message")
      # If you’ve got a manual refresh tick, bump it here:
      # db_refresh_tick(db_refresh_tick() + 1)
    }
  })


}
