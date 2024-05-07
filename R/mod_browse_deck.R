
#' 'Browse Deck' Module - UI
#'
#' @param id character - module id
#'
#' @return shiny tags
#' @export
#'
#' @examples
#' \dontrun{
#'  mod_browse_deck_ui('browse_deck')
#' }
mod_browse_deck_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("card_tbl"))
  )
}

#' 'Browse Deck' Module - Server
#'
#' @param id character - module id
#' @param topic reactive data.frame - selected topic data
#'
#' @import shiny
#'
#' @return reactive list
#' @export
#'
#' @examples
#' \dontrun{
#'  selected_card <- mod_browse_deck_server('browse_deck')
#' }
mod_browse_deck_server <- function(id, topic) {

  stopifnot(is.reactive(topic))

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # __________________ ----
    # reactive expressions ----

    # * topic_cols ----
    topic_cols <- reactive({topic()[, 1:2]})

    # ______ ----
    # outputs ----

    # * output$card_tbl ----
    output$card_tbl <- DT::renderDataTable({
      DT::datatable(
        data = topic_cols(),
        selection = list(mode = "single", target = 'row'),
        rownames = TRUE,
        #class ='cell-border stripe compact white-space: nowrap',
        escape= FALSE,
        editable = FALSE,
        options = list(
          dom = 'lftip',
          scrollX = TRUE,
          scrollY = '75vh'
        ),
        fillContainer = getOption("DT.fillContainer", TRUE)
      )
    })

  })

  # __________ ----
  # return value ----
  reactive({list()})

}
