# #library(nutterb/shinydust)
# library(shiny)
# library(pixiedust)
# library(shinydust)
# checkboxTable(tbl = mtcars,
#               inputId = paste0("carChoice", 1:nrow(mtcars)),
#               label = rownames(mtcars),
#               value = FALSE,
#               table_label = "Select Vehicles",
#               display_table=TRUE,
#               pixie = . %>% sprinkle(bg_pattern_by = "rows"))
#
# ## Not run:
# library(shiny)
# library(pixiedust)
# library(shinydust)
#
# server <- shinyServer(function(input, output) {
#   output$table <-
#     renderText({
#       cbind(rownames(mtcars), mtcars) %>%
#         checkboxTable(inputId = paste0("chooseCar", 1:nrow(mtcars)),
#                       label = "",
#                       value = FALSE,
#                       table_label = "Select a Vehicle",
#                       pixie = . %>%
#                         sprinkle(bg_pattern_by = "rows") %>%
#                         sprinkle_table(pad = 7) %>%
#                         sprinkle_colnames("rownames(mtcars)" = "",
#                                           control = ""))
#     })
#
#   output$chooseCar1 <- renderText(paste0("Mazda RX4: ", input$chooseCar1))
#   output$chooseCar2 <- renderText(paste0("Mazda RX4 Wag: ", input$chooseCar2))
# })
#
# ui <- shinyUI(fluidPage(
#   wellPanel(
#     verbatimTextOutput("chooseCar1"),
#     verbatimTextOutput("chooseCar2"),
#     uiOutput("table")
#   )
# ))
#
# shinyApp(ui = ui, server = server)
#
# ## End(Not run)
#



#****************************
#* Checkbox Group Table

checkboxGroupTable(tbl = mtcars,
                   inputId = "carChoice",
                   label = rownames(mtcars),
                   choices = paste0("car", 1:nrow(mtcars)),
                   table_label = "Select Vehicles",
                   display_table=TRUE,
                   pixie = . %>% sprinkle(bg_pattern_by = "rows"))
## Not run:
library(shiny)
library(pixiedust)
library(shinydust)

server <- shinyServer(function(input, output) {
  output$table <-
    renderText({
      cbind(rownames(mtcars), mtcars) %>%
        checkboxGroupTable(inputId = "chooseCar",
                           label = "",
                           choices = paste0("car", 1:nrow(mtcars)),
                           table_label = "Select a Vehicle",
                           pixie = . %>%
                             sprinkle(bg_pattern_by = "rows") %>%
                             sprinkle_table(pad = 7) %>%
                             sprinkle_colnames("rownames(mtcars)" = "",
                                               control = ""))
    })

  output$choice <- renderText(input$chooseCar)
})

ui <- shinyUI(fluidPage(
  wellPanel(
    verbatimTextOutput("choice"),
    uiOutput("table")
  )
))

shinyApp(ui = ui, server = server)

## End(Not run)

#
# #***********************************
# #* Radio Button Table
# radioTable(tbl = mtcars,
#            inputId = "chooseCar",
#            label = rownames(mtcars),
#            choices = paste0("car", 1:nrow(mtcars)),
#            table_label = "Select a Vehicle",
#            display_table=TRUE,
#            pixie = . %>% sprinkle(bg_pattern_by = "rows"))
#
# ## Not run:
# library(shiny)
# library(pixiedust)
# library(shinydust)
#
# server <- shinyServer(function(input, output) {
#   output$table <-
#     renderText({
#       cbind(rownames(mtcars), mtcars) %>%
#         radioTable(inputId = "chooseCar",
#                    label = "",
#                    choices = paste0("car", 1:nrow(mtcars)),
#                    table_label = "Select a Vehicle",
#                    pixie = . %>%
#                      sprinkle(bg_pattern_by = "rows") %>%
#                      sprinkle_table(pad = 7) %>%
#                      sprinkle_colnames("rownames(mtcars)" = "",
#                                        control = ""))
#     })
#
#   output$choice <- renderText(input$chooseCar)
# })
#
# ui <- shinyUI(fluidPage(
#   wellPanel(
#     verbatimTextOutput("choice"),
#     uiOutput("table")
#   )
# ))
#
# shinyApp(ui = ui, server = server)

## End(Not run)
