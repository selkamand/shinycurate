
#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # fluidPage(
    #   golem::golem_welcome_page() # Remove this line to start building your UI
    # )
    bslib::page_navbar(
      title = "Curation",
      theme = bslib::bs_theme(bootswatch = "minty"),
      sidebar = bslib::sidebar(
        open = "always",
        width = "400px",
        bslib::card(bslib::card_header("Text to curate"),
                    shiny::fileInput(inputId = "in_file_curate", label = "Curation File (csv)", width="100%"),
                    tags$small(class = "text-muted",
                               "(Or ", downloadLink("dl_example_sentences", "download example.csv"), ")"),
                    class = "secondary"
        ),

        bslib::card(
          bslib::card_header("Database"),
          shinyFiles::shinyFilesButton("in_pick_db", "Choose DB file", "Select a .sqlite", multiple = FALSE),
          tags$small(class = "text-muted",
                     "(Or ", downloadLink("dl_default_db", "download the default"), ")"),
          class = "secondary",
          textOutput(outputId = "out_picked_db_path")
        ),
        bslib::card(
          style = bslib::css(overflow = "visible"),
          bslib::card_header("Navigate Cases"),
          bslib::card_body(
            style = bslib::css(overflow = "visible"),
            shiny::selectInput(inputId = "in_current_id", label = "Case ID", choices = NULL, width = "100%"),
            actionButton(inputId = "in_next_hit", label = "Go to next case not already in DB but regex-hit")
          )

        ),
        bslib::card(
          bslib::card_header("Search (regex)"),
          shiny::textInput(inputId = "in_regex", label = "Search (Regex)", value = NA_character_, width = "100%"),
          shiny::checkboxInput(inputId = "in_regex_ignore_case", label = "Ignore Case", value = FALSE, width = "100%"),
          shiny::actionButton(inputId = "in_bttn_search_regex", label = "Search"),
          # shiny::checkboxInput(inputId = "in_filter_", label = "Ignore Case", value = FALSE, width = "100%")
        ),
        bslib::card(
          bslib::card_header("Database Properties"),
          mod_template_to_inputs_ui(id = "mod_db_column_inputs"),
          # uiOutput("out_db_column_controls"),
          shiny::actionButton(inputId = "in_bttn_updatedb", label = "Update Database")
        )

      ),

      bslib::nav_panel(
        title="Curation",
        bslib::card(
          bslib::card_header("Text"),
          htmlOutput(outputId = "out_current_text_html"),
        ),
      ),
      bslib::nav_panel(
        title = "Raw Data",
        bslib::card(
          bslib::card_header("Text Curation"),
          textOutput(outputId = "out_text_rawdata_summary"),
          DT::DTOutput("out_dt_curation"),
      )),
      bslib::nav_panel(
        title="Database",
        bslib::card(
          bslib::card_header(span("Database ", actionLink(inputId = "in_refresh_db", label = "(refresh)"))),
          textOutput(outputId = "out_text_dbpath"),
          DT::DTOutput("out_dt_database"),
        )
      )
      # dl_example_sentences
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinycurate"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
