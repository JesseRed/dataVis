#UI.R
cat(getwd())
setwd("..")
#setwd("D:/Programming/dataVIS")
#source("./app/module_test.R")
source("./modules/compare_trials_stats.R")

source("./modules/overview_plot.R")
source("./modules/compare_diff_of_diff_stats.R")
source("./modules/compare_trials_plot.R")
source("./modules/compare_group_stats.R")
source("./functions/map2colors.R")
source("./functions/get_data.R")

source("./modules/regression_stats.R")
#source("./modules/anova_stats.R")

#source("./app/preprocess_data.R")
#source("./app/preprocessing.R")

#source("./app/module_test.R")
source("./functions/preprocess_data.R")
source("./modules/preprocessing.R")

ui <- dashboardPage(
  dashboardHeader(title = "Biomag Visualizer"),
  dashboardSidebar(
    sidebarMenu(id ="mySidebarMenu",
      menuItem("preprocess data", tabName = "preprocessdata", icon = icon("file-upload")),
      menuItem("load data", tabName = "loaddata", icon = icon("file-export"), startExpanded = TRUE,
               menuSubItem(icon = NULL, uiOutput("selectDirCoh")),
               menuSubItem(icon = NULL, uiOutput("selectDirTra")),
               menuSubItem(icon = NULL, uiOutput("selectDirFre")),
               menuSubItem(icon = NULL, uiOutput("selectDirGra"))
      ),
      menuItem(id = "xxx1", "show data", tabName = "showdata", icon = icon("th"), startExpanded = TRUE,
               menuSubItem("Coherence",
                           tabName = "CoherenceTab",
                           icon = icon('line-chart')),
               menuSubItem("Transferentropy",
                           tabName = "TransferentropyTab",
                           icon = icon('line-chart')),
               menuSubItem("Frequency",
                           tabName = "FrequencyTab",
                           icon = icon('line-chart')),
               menuSubItem("Granger",
                           tabName = "GrangerTab",
                           icon = icon('line-chart'))
      ),
      menuItem("global Inputs", tabName = "xxx", icon = icon("globe"), startExpanded = TRUE,
               menuSubItem(icon = NULL, uiOutput("freq")),
               menuSubItem(icon = NULL, uiOutput("glob_sig"))
      ),
      menuItem("Image Save", tabName = "xxx", icon = icon("save"), startExpanded = FALSE,
               menuSubItem(icon = NULL, uiOutput("saveimageButton")),
               menuSubItem(icon = NULL, uiOutput("saveimagefilename")),
               menuSubItem(icon = NULL, uiOutput("saveimageheight")),
               menuSubItem(icon = NULL, uiOutput("saveimagewidth")),
               menuSubItem(icon = NULL, uiOutput("saveimagedpi")),
               menuSubItem(icon = NULL, uiOutput("saveimagefontsize")),
               menuSubItem(icon = NULL, uiOutput("saveimagefileformat"))
      ),
      menuItem("Options", tabName = "OptionsTab", icon = icon("wrench")),

      # menuItem("load data", tabName = "loaddata1", icon = icon("th"),
      #          menuSubItem(icon = NULL, uiOutput("freq")),
      #          menuSubItem(icon = NULL, uiOutput("glob_sig"))
      # ),
      #selectInput("HumanTrafficking", "Choose a trafficking type: ", list("Victim", "Trafficker")),

      textOutput("res")
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),

                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )

              )
      ),

      # Second tab content
      tabItem(tabName = "OptionsTab",
              uiOutput("tabsOpt")
      ),
      tabItem(tabName = "preprocessdata",
              #h2("preprocess data about here"),
              box(
                title = "preprocessing",
                width = 12,
                preprocessingUI("preprocessing")
              )
      ),
      tabItem(tabName = "CoherenceTab",
              uiOutput("tabsCoh")
      ),
      tabItem(tabName = "TransferentropyTab",
              uiOutput("tabsTra")
      ),
      tabItem(tabName = "FrequencyTab",
              uiOutput("tabsFre")
      ),
      tabItem(tabName = "GrangerTab",
              uiOutput("tabsGra")
      )


    )
)

)
