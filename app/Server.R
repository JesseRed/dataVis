#Server.R
# source("./modules/compare_trials_stats.R")
library(shiny)
library(shinydashboard)
library(shinyalert)
library("gplots")
library("circlize")
library(RColorBrewer)
library("GISTools")
library("corrplot")
library("pheatmap")
library(viridis)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(ggm)
library(compute.es)
library(multcomp)
#library(WRS) # install.packages("WRS", repos= "http://R-Forge.R-project.org")
library(car)
library(pastecs)
library(effects)
library(pastecs)
library(compute.es)
library(sortable)
#library(shinyWidgets)

#setwd("..")
#setwd("D:/Programming/dataVIS")

source("./modules/compare_trials_stats.R")
source("./functions/get_data.R")
source("./modules/overview_plot.R")
source("./modules/compare_diff_of_diff_stats.R")
source("./modules/compare_trials_plot.R")
source("./modules/compare_group_stats.R")
source("./modules/behavioral_data_stats.R")
source("./modules/regression_stats.R")
#source("./modules/anova_stats.R")
source("./modules/ancova_stats.R")
source("./modules/options_mod_01.R")

source("./functions/map2colors.R")
source("./functions/plots.R")

source("./functions/preprocess_data.R")
source("./modules/preprocessing.R")
#
options(shiny.maxRequestSize=300*1024^2)

server <- function(input, output) {

  ######################
   ### reactive val ###
  ######################
  initialized = FALSE
  dir_listCoh <- reactiveVal(value = dir(path = "./data", pattern = "^Coherence", full.names = F, recursive = F))
  dir_listTra <- reactiveVal(value = dir(path = "./data", pattern = "^Transferentropy", full.names = F, recursive = F))
  dir_listFre <- reactiveVal(value = dir(path = "./data", pattern = "^Frequency", full.names = F, recursive = F))
  dir_listGra <- reactiveVal(value = dir(path = "./data", pattern = "^Granger", full.names = F, recursive = F))

 # method <- reactive({})
  method <- reactiveVal(value = "Coherence")
  directory <- reactiveVal(value = "Coherence")

  # globale Variablen
  # das laesst sich nur schwer vermeiden da sonst die immer gleichen unzaehligen
  # variablen durch jede einzelne Funktion geschleift werden muessen und das
  # am Ende extrem unuebersichtlich ist und immer noch Informationsbeduerfnisse
  # dazu kommen
  # Prinzip ist, dass Frequenz, significanz, die Daten im Verzeichnis und die
  # Methode globale Variablen sind die in der Sidebar reaktiv geaendert werden koennen
  g_act_method <<- reactive({
    #cat(file = stderr(), paste0("g_act_method call with : input$mySidebarMenu=", input$mySidebarMenu, "\n"))
    if        (input$mySidebarMenu == "CoherenceTab")      { return("Coherence")
    } else if (input$mySidebarMenu == "TransferentropyTab"){ return("Transferentropy")
    } else if (input$mySidebarMenu == "FrequencyTab")      { return("Frequency")
    } else if (input$mySidebarMenu == "GrangerTab")        { return("Granger")
    } else if (input$mySidebarMenu == "OptionsTab")        { return("Options")
    } else {   return("CoherenceDDD")  }
  })

  g_act_data_dir <<- reactive({
    if (g_act_method()=="Coherence"){       return(input$dataDirCoh)}
    if (g_act_method()=="Transferentropy"){ return(input$dataDirTra)}
    if (g_act_method()=="Frequency"){       return(input$dataDirFre)}
    if (g_act_method()=="Granger"){         return(input$dataDirGra)}
    if (g_act_method()=="Options"){         return(input$dataDirCoh)}

    return("CoherenceDDDD")
  })
  mydir <- eventReactive(input$mySidebarMenu,{input$dataDirCoh}, ignoreNULL = FALSE)
  #freq <- eventReactive(input$freq,{input$freq}, ignoreNULL = FALSE)
  #directory <- reactiveVal(value = "Coherence")

  g_data      <<- reactive({ get_data(g_act_data_dir())                })
  g_beh      <<- reactive({ get_global_tbl_beh(g_act_data_dir())      })
  g_regions <<- reactive({ get_global_uregion_list(g_act_data_dir()) })
  g_trials  <<- reactive({ get_global_utrial_list(g_act_data_dir())  })
  g_groups  <<- reactive({ get_global_group_names(g_act_data_dir())  })
  g_freqs   <<- reactive({ get_global_ufreq_list(g_act_data_dir())   })
  g_regions_named <<- reactive({ get_global_region_names(g_act_data_dir()) })
  g_trials_named  <<- reactive({ get_global_trial_names(g_act_data_dir())  })
  g_sel_freqs<<- reactive({ get_selected_freq_list(g_freqs(), input$freq) })
  g_sig<<- reactive({input$glob_sig})

  g_saveImage_button <<- reactive({input$saveimageButton})
  g_saveImage_width <<- reactive({input$saveimagewidth})
  g_saveImage_height <<- reactive({input$saveimageheight})
  g_saveImage_filename <<- reactive({input$saveimagefilename})
  g_saveImage_fileext <<- reactive({input$saveimagefileformat})
  g_saveImage_dpi <<- reactive({input$saveimagedpi})
  g_saveImage_fontsize <<- reactive({input$saveimagefontsize})

  set.seed(122)
  histdata <- rnorm(500)




  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

  ##################
   #### Sidebar ###
  ##################
  output$res <- renderText({
    paste("You've selected:", input$mySidebarMenu)
  })
  output$selectDirCoh <- renderUI({
    selectInput("dataDirCoh", "Coherence",choices = dir_listCoh(), selected = dir_listCoh()[2])
    })
  output$selectDirTra <- renderUI({
    selectInput("dataDirTra", "Transferentropy",
                choices = dir_listTra(),
                selected = dir_listTra()[2])})
  output$selectDirFre <- renderUI({
    selectInput("dataDirFre", "Frequency",choices = dir_listFre(), selected = dir_listFre()[2])})
  output$selectDirGra <- renderUI({
    selectInput("dataDirGra", "Granger",choices = dir_listGra(),selected = dir_listGra()[2])})



  # # Show modal when button is clicked.
  # observeEvent(input$mySidebarMenu, {
  #   #dir_list(list.dirs(path = "./data", full.names = F, recursive = F))
  #   cat(file = stderr(), paste0("observeEvent:", input$mySidebarMenu, "\n"))
  #   cat(file = stderr(), paste0("method:", method(), "\n"))
  #   cat(file = stderr(), paste0("directory:", directory(), "\n"))
  #   cat(file = stderr(), paste0("input$dataDirTra:", input$dataDirTra, "\n"))
  #   cat(file = stderr(), paste0("g_act_method:", g_act_method(), "\n"))
  #   cat(file = stderr(), paste0("g_act_data_dir:", g_act_data_dir(), "\n"))
  #
  #   # if (input$mySidebarMenu == "CoherenceTab"){
  #   #   method("Coherence")
  #   #   directory(input$dataDirCoh)
  #   # } else if (input$mySidebarMenu == "TranferentropyTab"){
  #   #   method("Transferentropy")
  #   #   directory(input$dataDirTra)
  #   #
  #   # } else if (input$mySidebarMenu == "FrequencyTab"){
  #   #   method("Frequency")
  #   #   directory(input$dataDirFre)
  #   #
  #   # } else if (input$mySidebarMenu == "GrangerTab"){
  #   #   method("Granger")
  #   #   directory(input$dataDirGra)
  #   #
  #   # } else if (input$mySidebarMenu == "OptionsTab"){
  #   #   method("Options")
  #   #   directory(input$dataDirCoh)
  #   #
  #   # } else {
  #   #   # default ... initially called
  #   #   method("Coherence")
  #   #   directory("Coherence")
  #   #
  #   # }
  #   cat(file = stderr(), paste0("directory:", directory(), "\n"))
  #   cat(file = stderr(), paste0("input$dataDirTra:", input$dataDirTra, "\n"))
  #   cat(file = stderr(), paste0("g_act_method:", g_act_method(), "\n"))
  #   cat(file = stderr(), paste0("g_act_data_dir:", g_act_data_dir(), "\n"))
  #   cat(file = stderr(), paste0("_________________________\n"))
  # })

  output$freq <- renderUI({
    sliderInput(inputId = "freq",
                label = "Frequency range",
                value = c(8,14), min = 0, max = 40)
  })
  output$glob_sig <- renderUI({
    numericInput("glob_sig", h4("sig threshold"), min =0, max = 1, value = 0.05, step = 0.00001)
    #sliderInput("glob_sig", h4("sig threshold"), min =0 , max = 1, value = 0.05, step = 0.01)
  })

  output$saveimageButton <- renderUI({
    actionButton("saveimageButton", "save Image")
  })
  output$saveimagefilename <- renderUI({
    textInput("saveimagefilename","filename", "ImageOutput")
  })
  output$saveimageheight <- renderUI({
    numericInput("saveimageheight", "height (cm)", 10)
  })
  output$saveimagewidth <- renderUI({
    numericInput("saveimagewidth","width (cm)", 10)
  })
  output$saveimagedpi <- renderUI({
    numericInput("saveimagedpi", "dpi", 600)
  })
  output$saveimagefontsize <- renderUI({
    numericInput("saveimagefontsize", "fontsize", 18)
  })
  output$saveimagefileformat <- renderUI({
    selectInput( "saveimagefileformat", "file format", choices = c("tiff", "pdf", "png"))
  })





  #####################
  #### Tabs Options ###
  #####################
  output$tabsOpt <- renderUI({
    fluidRow(
      tabBox(
        title = NULL, width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",

        tabPanel("Regions", options_mod_01UI("Options_01"))

      )
    )
  })


  ##################
  #### Tabs Coh ###
  ##################
  output$tabsCoh <- renderUI({
    fluidRow(
      tabBox(
        title = NULL, width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",

        tabPanel("Plot", overviewPlotUI("CohOverviewPlot")),
        tabPanel("Comp Plot", compareTrialsPlotUI("CohPlot")),
        tabPanel("Trials Stat", compareTrialsStatsUI("CohTrialsStat")),
        tabPanel("Groups Stat", compareGroupsStatsUI("CohGroupsStats")),
        tabPanel("Diff Stat", compareDiffOfDiffStatsUI("CohDiffOfDiffStats")),
        tabPanel("Regression", regressionStatsUI("CohRegStats")),
        tabPanel("ANCOVA", ancovaStatsUI("CohAncovaStats"))
      )
    )
  })


  ##################
  #### Tabs TRA ###
  ##################
  output$tabsTra <- renderUI({
    fluidRow(
      tabBox(
        title = NULL, width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset2", height = "250px",
        tabPanel("Plot", overviewPlotUI("TraOverviewPlot")),
        tabPanel("Comp Plot", compareTrialsPlotUI("TraPlot")),
        tabPanel("Trials Stat", compareTrialsStatsUI("TraTrialsStat")),
        tabPanel("Groups Stat", compareGroupsStatsUI("TraGroupsStats")),
        tabPanel("Diff Stat", compareDiffOfDiffStatsUI("TraDiffOfDiffStats")),
        tabPanel("Regression", regressionStatsUI("TraRegStats")),
        tabPanel("ANCOVA", ancovaStatsUI("TraAncovaStats"))
      )
    )
  })


  ##################
  #### Tabs Fre ###
  ##################
  output$tabsFre <- renderUI({
    fluidRow(
      tabBox(
        title = NULL, width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",

      )
    )
  })


  ##################
  #### Tabs Gra ###
  ##################
  output$tabsGra <- renderUI({
    fluidRow(
      tabBox(
        title = NULL, width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",
        tabPanel("Victimwww", "Victim tab"),
        tabPanel("Trafficker", "Trafficker tab")
      )
    )
  })

  options_mod_01Server("Options_01", reactive(input$glob_sig), reactive(input$freq))

#  compareTrialsStatsServer("CohTrialsStat", g_data(), reactive(glob_sig), reactive(freq), utrial_list(), uregion_list(), group_names())
  overviewPlotServer("CohOverviewPlot", "Coherence", reactive(input$glob_sig), reactive(input$freq))
  compareTrialsStatsServer("CohTrialsStat", reactive(input$glob_sig), reactive(input$freq))
  compareTrialsPlotServer("CohPlot", reactive(input$freq))
  compareGroupsStatsServer("CohGroupsStats",  reactive(input$glob_sig), reactive(input$freq))
  compareDiffOfDiffStatsServer("CohDiffOfDiffStats", reactive(input$glob_sig), reactive(input$freq))
  behavioralDataStatsServer("CohBehDataStats", reactive(input$glob_sig), reactive(input$freq))
  regressionStatsServer("CohRegStats", reactive(input$glob_sig), reactive(input$freq))
  ancovaStatsServer("CohAncovaStats", reactive(input$glob_sig), reactive(input$freq))

  overviewPlotServer("TraOverviewPlot", "Transferentropy", reactive(input$glob_sig), reactive(input$freq))
  compareTrialsStatsServer("TraTrialsStat", reactive(input$glob_sig), reactive(input$freq))
  compareTrialsPlotServer("TraPlot", reactive(input$freq))
  compareGroupsStatsServer("TraGroupsStats", reactive(input$glob_sig), reactive(input$freq))
  compareDiffOfDiffStatsServer("TraDiffOfDiffStats", reactive(input$glob_sig), reactive(input$freq))
  behavioralDataStatsServer("TraBehDataStats", reactive(input$glob_sig), reactive(input$freq))
  regressionStatsServer("TraRegStats", reactive(input$glob_sig), reactive(input$freq))
  ancovaStatsServer("CohAncovaStats", reactive(input$glob_sig), reactive(input$freq))

  preprocessingServer("preprocessing")

}
