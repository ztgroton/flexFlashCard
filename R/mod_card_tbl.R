
#' 'Card Table' Module - UI
#'
#' @param id character - module id
#'
#' @return shiny tags
#' @export
#'
#' @examples
#' \dontrun{
#'  mod_card_tbl_ui('show_card')
#' }
mod_card_tbl_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns('tbl'))
  )
}

#' 'Card Table' Module - Server
#'
#' @param id character - module id
#' @param card reactive data.frame - selected card data
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  mod_card_tbl_server('show_card', card)
#' }
mod_card_tbl_server <- function(id, card) {

  stopifnot(is.reactive(card))

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ______ ----
    # outputs ----

    # * output$tbl ----
    output$tbl <- DT::renderDataTable({
      req(card())
      DT::datatable(
        data = card(),
        selection = list(mode = "single", target = 'row'),
        rownames = TRUE,
        #class ='cell-border stripe compact white-space: nowrap',
        escape= FALSE,
        editable = FALSE,
        options = list(
          dom = 't',
          scrollX = TRUE,
          scrollY = '75vh'
        ),
        fillContainer = getOption("DT.fillContainer", TRUE)
      )
    })

  })

}
