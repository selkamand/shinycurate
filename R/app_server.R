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
    old <- isolate(current_case_id())
    sel <- if (length(old) && !is.null(old) && old %in% ids) old else ids[1]
    names(ids) <- paste0(ids, ifelse(df[["..regex_hit"]], "  ✓", ""))
    ids
  })

  # update id choices whenever the data changes
  observeEvent(all_ids(), {
    updateSelectInput(session, "in_current_id",
                      choices = all_ids())
  }, ignoreInit = FALSE)

  # Scroll to the next regex hit not already described in database
  observeEvent(input$in_next_hit, {
    df <- curation_df()
    df$id <- as.character(df$id)

    current_rownumber <- match(current_case_id(), df$id, nomatch = NA_integer_)
    if (is.na(current_rownumber)) return(invisible(NULL))

    # IDs already stored in DB
    all_ids_in_db <- fetch_all_ids_from_database(conn = db_conn(), table = "table1")

    df$in_db <- df$id %in% all_ids_in_db
    df$regex_hit <- df$..regex_hit

    # 1) Try to find the next uncurated row *below* current
    idx_pool  <- which(!df$in_db & df$regex_hit)
    idx_below <- idx_pool[idx_pool > current_rownumber]

    if (length(idx_below)) {
      next_rownumber <- idx_below[1]
    } else {
      # 2) Wrap to the top: first uncurated row (excluding current row)
      idx_wrap <- setdiff(idx_pool, current_rownumber)
      next_rownumber <- if (length(idx_wrap)) idx_wrap[1] else NA_integer_
    }

    if (is.na(next_rownumber)) {
      # Everything is curated; nothing to move to
      showNotification("All cases are already in the database.", type = "message")
      return(invisible(NULL))
    }

    new_id <- df$id[next_rownumber]
    if (length(new_id) != 1L || !nzchar(new_id)) return(invisible(NULL))

    updateSelectInput(session = session, inputId = "in_current_id", selected = new_id)
  })


  # Get Currently Selected ID
  current_case_id <- reactive({input$in_current_id})


  # Get REGEX (for highlighting cetain
  pattern <- reactive({input$in_regex})

  # Update current text
  current_text <- eventReactive(current_case_id(), {
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
    input$in_bttn_updatedb # Trigger new table after new data is submitted
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
  case_inputs <- mod_template_to_inputs_server(
    id = "mod_db_column_inputs",
    template = template,
    case_id = current_case_id,
    conn = db_conn,
    table = "table1"
  )


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
  output$out_dt_database <- DT::renderDataTable({ db_table() }) |> bindEvent(input$in_bttn_updatedb, db_table())

  output$out_text_rawdata_summary <- renderText({
      paste0(
        "Regex Hits: ", sum(curation_df()$..regex_hit), "/",nrow(curation_df())
        )
    })

  #
  output$out_text_regex_summary <- renderText({
    df <- curation_df()
    df$id <- as.character(df$id)

    total_regex_hits <- sum(df$..regex_hit, na.rm = TRUE)
    total_entries    <- nrow(df)

    # Safely get IDs currently in DB; if DB not ready, treat as none present
    ids_in_db <- tryCatch(
      fetch_all_ids_from_database(conn = db_conn(), table = "table1"),
      error = function(e) character(0)
    )
    ids_in_db <- as.character(ids_in_db)

    n_missing_entries <- sum(df$..regex_hit & !(df$id %in% ids_in_db), na.rm = TRUE)

    glue::glue(
      "Regex Hits: {total_regex_hits} / {total_entries}<br/>",
      "Regex Hits not described in database: {n_missing_entries}"
    )
  }) |> bindEvent(input$in_bttn_search_regex, input$in_bttn_updatedb, input$in_refresh_db, curation_df(), db_table())

  output$out_picked_db_path <- renderText({
    validate(need(picked_db_path(), message = "Please Select Database"))
    picked_db_path()
    })

  observeEvent(input$in_bttn_updatedb, {
    write_to_database(conn = db_conn(), id = current_case_id(), values = case_inputs(), template = template(), table = "table1")
  })

}
