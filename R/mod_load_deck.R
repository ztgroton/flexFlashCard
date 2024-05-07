#' 'Load Deck' Module - UI
#'
#' @param id character - module id
#'
#' @return shiny tags
#' @export
#'
#' @examples
#' \dontrun{
#'  module_load_deck_ui('load_deck')
#' }
mod_load_deck_ui <- function(id) {

  ns <- NS(id)

  div(
    id = id,
    tagList(
      fileInput(
        inputId = ns("file"),
        label = "Upload Card Deck",
        multiple = FALSE,
        accept = c("excel", "excel", ".xls",".xlsx")
      )
    )
  )

}

#' 'Load Deck' Module - Server
#'
#' @param id character - module id
#'
#' @import shiny
#'
#' @return reactive data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'  load_deck <- module_load_deck_server('load_deck')
#' }
mod_load_deck_server <- function(id) {

  moduleServer(id, function(input, output, session){

    ns <- session$ns

    # ____________ ----
    # reactive values ----
    deck_data <- reactiveVal(NULL)

    # ________ ----
    # observers ----

    # * input$file ----
    observeEvent(input$file, {

      ext <- tools::file_ext(input$file$name)

      if (ext == "xls" || ext == 'xlsx'){

        tmp <- readxl::read_excel(input$file$datapath)
        deck_data(tmp)

      } else {
        shinyWidgets::sendSweetAlert(
          session  = session, title = "Error - File Type",
          text = tags$span(
            tags$h4(
              HTML(
                paste(
                  "Hi", '<br>', "You have selected '", ext, "' file type.", '<br>',
                  "Should only be xlsx or xls file"
                )
              ),
              style = "color: steelblue;"
            )
          ),
          type = 'warning'
        )
      }

    })

    # __________ ----
    # return value ----
    reactive({deck_data()})

  })

}
