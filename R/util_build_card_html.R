
#' Build HTML Tags for Flash Card
#'
#' @param df data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'  html <- build_card_html(card_data)
#' }
build_card_html <- function(df) {

  stopifnot(is.data.frame(df))
  stopifnot(isTRUE(nrow(df) == 1))

  front <- c(
    paste(c(rep('<br>', 7), df$Topic)),
    paste('<h1>', df$Question,'<h1>')
  )

  back <- c(
    paste('<h2><b><i>',"Definition",'</i></b></h2> <h3>', paste(df$Definition), '</h3>'),
    paste('<h2><b><i>',"Example(s)",'</i></b></h2> <h3>', paste(df$Examples), '</h3>'),
    paste('<h2><b><i>',"Comments / Strategies",'</i></b></h2> <h3>', paste(df$Comments), '</h3>')
  )

  res <- list(front = front, back = back)
  return(res)

}
