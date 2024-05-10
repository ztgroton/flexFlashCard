#' 'Load Deck' Module - UI
#'
#' @importFrom utils read.csv write.csv
#'
#' @param id character - module id
#'
#' @return shiny tags
#' @export
#'
#' @examples
#' \dontrun{
#'  mod_load_deck_ui('load_deck')
#' }
mod_load_deck_ui <- function(id) {

  ns <- NS(id)

  tagList(
    downloadButton(
      outputId = ns("template"),
      label = "Download CSV Template",
      style = "height: 25% !important;"
    ),
    fileInput(
      inputId = ns("file"),
      label = NULL,
      width = '100%',
      multiple = FALSE,
      accept = c(".csv", ".xls",".xlsx"),
      buttonLabel = "Browse",
      placeholder = "Load from Excel File"
    ),
    selectizeInput(
      inputId = ns("topic"), label = "Select Topic(s) to Review",
      choices = '(no data loaded)', multiple = FALSE,
      options = list(dropdownParent = 'body'), width = '100%'
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
#'  load_deck <- mod_load_deck_server('load_deck')
#' }
mod_load_deck_server <- function(id) {

  moduleServer(id, function(input, output, session){

    ns <- session$ns

    # ____________ ----
    # reactive values ----
    deck_data <- reactiveVal(NULL)

    # __________________ ----
    # reactive expressions ----

    # * topic_data ----
    topic_data <- reactive({

      req(deck_data())
      req(input$topic)

      if ("Select All" %in% input$topic){
        deck_data()
      } else {
        ind <- deck_data()$Topic %in% input$topic
        deck_data()[ind,,drop=F]
      }

    })

    # * topic_cols ----
    topic_cols <- reactive({topic_data()[, 1:2]})

    # ________ ----
    # observers ----

    # * input$file ----
    observeEvent(input$file, {

      ext <- tools::file_ext(input$file$name)

      if (ext == "xls" || ext == 'xlsx'){
        deck_data(readxl::read_excel(input$file$datapath))
      } else if (ext == 'csv') {
        deck_data(read.csv(input$file$datapath))
      } else {
        shinyWidgets::sendSweetAlert(
          session  = session, title = "Error - File Type",
          text = tags$span(
            tags$h4(
              HTML(
                paste(
                  "Hi", '<br>',
                  "You have selected '", ext, "' file type.",
                  '<br>', "Should only be xlsx or xls file"
                )
              ),
              style = "color: steelblue;"
            )
          ),
          type = 'warning'
        )
      }

    })

    # * deck_data() ----
    observeEvent(deck_data(), {

      updateSelectInput(
        session = session, inputId = "topic",
        choices = c(unique(deck_data()$Topic), "Select All"),
        selected = 'Select All'
      )

    })

    # ______ ----
    # outputs ----

    # * output$template ----
    output$template <- downloadHandler(
      filename = function() {
        paste0("FlashCardTemplate.csv")
      },
      content = function(file) {

        data <- data.frame(
          Topic = character(),
          Question = character(),
          Definition = character(),
          Examples = character(),
          Comments = character()
        )

        write.csv(data, file, row.names = FALSE)

      }
    )

    # __________ ----
    # return value ----
    list(data = list(all = reactive({deck_data()}), topic = reactive({topic_data()})))

  })

}
