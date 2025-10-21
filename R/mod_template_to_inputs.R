#' template_to_inputs UI Function
#'
#' From a named vector (names = variables, values = "numeric"/"boolean"/"string"),
#' render a matching set of inputs.
#'
#' @param id Module id.
#'
#' @noRd
#' @importFrom shiny NS tagList uiOutput
mod_template_to_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("inputs"))
  )
}

#' template_to_inputs Server Function
#'
#' @param id Module id.
#' @param template Named character vector OR reactive that returns one.
#'   Values must be one of: "numeric", "boolean", "string".
#' @param initial Optional named list/vector of initial values (same names as template).
#' @param labels Optional named character vector of labels (same names as template).
#'
#' @return A reactive() that yields a named list of scalar values cast to the
#'         requested type (numeric / logical / character). Missing blank inputs
#'         become NA for numeric, FALSE for boolean (checkbox default), and "" for string.
#'
#' @noRd
#' @import shiny
mod_template_to_inputs_server <- function(id, template, initial = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    template_reactive <- if(is.reactive(template)) template else reactive({template})
    # assertions::assert_reactive(template)


    # render the inputs whenever the template changes
    output$inputs <- renderUI({
      types <- template_reactive()
      labels <- names(types)
      ids  <- safe_id(labels) # sanitize IDs (preserve map to original names)
      # build inputs
      ui <- Map(function(id, type){
        switch(
          tolower(type),
          "numeric" = numericInput(ns(id), label = id, value = NULL, width = "100%"),
          "real" = numericInput(ns(id), label = id, value = NULL, width = "100%"),
          "integer" = numericInput(ns(id), label = id, value = NULL, width = "100%", step = 1),
          "boolean" = checkboxInput(ns(id), label = id, value = NULL, width = "100%"),
          "string"  = textInput(ns(id), label = id, value = NULL, width = "100%"),
          NA
        )
      }, ids, types)

      na_status <-  vapply(X = ui, FUN = anyNA, FUN.VALUE = logical(1))
      # if(anyNA(ui)) stop("Template type was not understood. Please ensure all types are either numeric, boolean, or string, not: ", toString(types[na_status]))
      tagList(ui)
    })

    # expose values as a reactive named list, cast per template
    values <- reactive({
      types <- template_reactive()
      labels <- names(types)
      ids  <- safe_id(labels)

      out <- lapply(ids, function(id){
        input[[id]]
      })

      names(out) <- ids
      return(out)
    })

    return(values)
  })
}
