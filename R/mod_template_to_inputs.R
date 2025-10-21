# Map SQLite types -> UI input types
.sqlite_to_ui <- function(x) {
  x <- toupper(x)
  ifelse(grepl("^BOOL", x), "boolean",
         ifelse(grepl("INT|REAL|NUMERIC|DOUBLE|DECIMAL", x), "numeric",
                "string"))
}

# Null defaults for inputs when DB has no value
.null_default_for_type <- function(ui_type) {
  switch(ui_type,
         "numeric" = NA_real_,
         "boolean" = FALSE,     # checkboxInput is binary; NA not supported in widget
         "string"  = "",
         NA
  )
}

#' template_to_inputs UI
#' @noRd
mod_template_to_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("inputs"))
  )
}

#' template_to_inputs Server
#'
#' @param id Module id
#' @param template Named character vector OR reactive returning one.
#'        Values should be from {"numeric","boolean","string"} **OR**
#'        you can pass a SQLite template and it will be mapped automatically.
#' @param case_id reactive() character scalar indicating the selected case id
#' @param conn DBIConnection to the SQLite DB
#' @param table table name (default "table1")
#' @param db_template Named character vector of SQLite types (including "id").
#'        If `template` is already a UI-type template, `db_template` is only used to fetch values.
#'
#' @return reactive() named list of current input values, names match original template names
#' @noRd
mod_template_to_inputs_server <- function(
    id,
    template,
    case_id,
    conn,
    table = "table1"
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Normalize template -> reactive + UI types
    # assertions::assert_reactive(case_id)
    template_rx <- if (is.reactive(template)) template else reactive(template)

    # Decide if user passed SQLite types; if so, map to UI types
    ui_template_rx <- reactive({
      t <- template_rx()
      # if any looks like SQLite types, map
      looks_sql <- any(grepl("TEXT|REAL|INT|BOOL|DOUBLE|DECIMAL|NUMERIC", t, ignore.case = TRUE))
      if (looks_sql) {
        stats::setNames(.sqlite_to_ui(t), names(t))
      } else {
        # assume already in {"numeric","boolean","string"}
        tolower(t)
      }
    })

    # Render inputs any time the (UI) template changes
    output$inputs <- renderUI({
      types  <- ui_template_rx()
      labels <- names(types)
      ids    <- setNames(safe_id(labels), labels)

      ui <- Map(function(var, type){
        id <- ids[[var]]
        switch(
          type,
          "numeric" = numericInput(ns(id), label = var, value = NA, width = "100%"),
          "boolean" = checkboxInput(ns(id), label = var, value = FALSE, width = "100%"),
          "string"  = textInput(ns(id), label = var, value = "", width = "100%"),
          NULL
        )
      }, labels, types)

      tagList(ui)
    })

    # When case_id , pull values from DB and update inputs
    observeEvent(list(case_id(), template(), conn()), {
      conn_ <- conn()
      req(DBI::dbIsValid(conn_))
      cid <- case_id()
      if (is.null(cid) || !nzchar(cid)) return()

      # Decide if user passed SQLite or UI types; derive SQLite types accordingly
      t_now <- template()
      looks_sql <- any(grepl("TEXT|REAL|INT|BOOL|DOUBLE|DECIMAL|NUMERIC", t_now, ignore.case = TRUE))
      sqlite_tmpl <- if (looks_sql) t_now else .ui_to_sqlite(t_now)

      # Drop id for fetch
      tmpl_db_no_id <- sqlite_tmpl[names(sqlite_tmpl) != "id"]

      row_vals <- query_from_template(
        conn       = conn_,
        id         = cid,
        template   = tmpl_db_no_id,
        table      = table,
        include_id = FALSE,
        on_missing = "NA"
      )

      types_ui  <- ui_template_rx()
      labels    <- names(types_ui)
      ids       <- setNames(safe_id(labels), labels)

      for (var in labels) {
        ui_type <- types_ui[[var]]
        val <- row_vals[[var]]
        if (is.null(val) || (length(val) == 1 && is.na(val))) {
          val <- .null_default_for_type(ui_type)
        }
        if (identical(ui_type, "numeric")) {
          updateNumericInput(session, ids[[var]], value = val)
        } else if (identical(ui_type, "boolean")) {
          updateCheckboxInput(session, ids[[var]], value = isTRUE(val))
        } else {
          updateTextInput(session, ids[[var]], value = if (is.null(val)) "" else as.character(val))
        }
      }
    }, priority = 100)

    # Expose current values as a named list (names = original template names)
    values <- reactive({
      types  <- ui_template_rx()
      labels <- names(types)
      ids    <- setNames(safe_id(labels), labels)

      out <- lapply(labels, function(var) input[[ ids[[var]] ]])
      names(out) <- labels

      # Coerce to scalars per UI type (ensures stable typing)
      out <- Map(function(v, tp){
        if (identical(tp, "numeric")) {
          v <- suppressWarnings(as.numeric(v)); if (!length(v)) NA_real_ else v
        } else if (identical(tp, "boolean")) {
          isTRUE(v)
        } else {
          if (is.null(v)) "" else as.character(v)
        }
      }, out, types)

      out
    })

    return(values)
  })
}
