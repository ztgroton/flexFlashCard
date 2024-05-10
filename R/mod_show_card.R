
#' 'Show Card' Module - UI
#'
#' @param id character - module id
#'
#' @return shiny tags
#' @export
#'
#' @examples
#' \dontrun{
#'  mod_show_card_ui('show_card')
#' }
mod_show_card_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns('tbl'))
  )
}

#' 'Show Card' Module - Server
#'
#' @param id character - module id
#' @param card reactive data.frame - selected card data
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  mod_show_card_server('show_card')
#' }
mod_show_card_server <- function(id, card) {

  stopifnot(is.reactive(card))

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    data <- reactive({card()})

    # ______ ----
    # outputs ----

    # * output$tbl ----
    output$tbl <- DT::renderDataTable({
      req(data())
      DT::datatable(
        data = data(),
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

}
