#uiElements.R

#' @export
#' @rdname uiElements
shopLevelKpis <- function() {
  fluidRow(
    h4("Key Performance Indicators"),
    box(width =  12,
        infoBoxOutput('revenueKpi', width = 4),
        infoBoxOutput('customersKpi', width = 4),
        infoBoxOutput('numProductsKpi', width = 4)
    ))
}
