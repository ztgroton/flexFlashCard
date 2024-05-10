
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
    uiOutput(ns('card_ui'))
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
#'  mod_show_card_server('show_card', card)
#' }
mod_show_card_server <- function(id, card) {

  stopifnot(is.reactive(card))

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # __________________ ----
    # reactive expressions ----

    # * card_html ----
    card_html <- reactive({build_card_html(card())})

    # _____________ ----
    # inline functions ----

    # * show_new_card ----
    show_new_card <- function(data) {

      output$card_ui <- renderUI({
        flashCard::flashCardOutput(ns("card"), width = 450, height = 350)
      })

      output$card <- flashCard::renderFlashCard({
        flashCard::flashCard(
          data = data,
          frontColor = "#1a6ecc",
          backColor = "#1a6ecc",
          front_text_color = "white",
          back_text_color = "white",
          elementId = NULL
        )
      })

    }

    # ______ ----
    # observers ----

    # * card_html() ----
    observeEvent(card_html(), {

      show_new_card(card_html())

    })

  })

}
