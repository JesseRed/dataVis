#Server.R
# source("./modules/compare_trials_stats.R")
library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)
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
library(reshape2)
library(tidyverse)
library(shinyjqui)
library(optimbase)
library(rlang)
library(gdata)
library(shinyBS)
<<<<<<< HEAD
library(DT)
library(cowplot)
library(patchwork)
#library(compare)
=======
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
#library(shinyWidgets)

#setwd("..")
#setwd("D:/Programming/dataVIS")

# for quickcor
#devtools::install_github("hannet91/ggcor")

<<<<<<< HEAD
options(browser = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
=======

>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
#source("./functions/get_data.R")
pathnames <- list.files(pattern="[.]R$", path="functions", full.names=TRUE);
sapply(pathnames, FUN=source);
pathnames <- list.files(pattern="[.]R$", path="modules", full.names=TRUE);
sapply(pathnames, FUN=source);
#
# source("./modules/compare_trials_stats.R")
# source("./modules/overview_plot.R")
# source("./modules/compare_diff_of_diff_stats.R")
# source("./modules/compare_trials_plot.R")
# source("./modules/compare_group_stats.R")
# source("./modules/behavioral_data_stats.R")
# source("./modules/regression_stats.R")
# #source("./modules/anova_stats.R")
# source("./modules/ancova_stats.R")
# source("./modules/options_mod_order.R")
# source("./modules/options_mod_name.R")
#
# source("./functions/map2colors.R")
# source("./functions/plots.R")
#
# source("./functions/preprocess_data.R")
# source("./modules/preprocessing.R")
#
options(shiny.maxRequestSize=300*1024^2)

server <- function(input, output, session) {

  ######################
   ### reactive val ###
  ######################
  selecteddatadir<-datarootdirServer("datarootdir")
  g_datarootpath <<-reactiveVal(getwd())
  observeEvent(selecteddatadir(),{
    #cat(file = stderr(),paste0("change in selecteddatadir detected","\n"))
    val <- selecteddatadir()
    g_datarootpath(val)
  })
  initialized = FALSE
<<<<<<< HEAD
  dir_listCon <- reactive({input$dirRefreshButton; dir(path = g_datarootpath(), pattern = "^Conn", full.names = F, recursive = F)})
  dir_listCoh <- reactive({input$dirRefreshButton; dir(path = g_datarootpath(), pattern = "^Coherence", full.names = F, recursive = F)})
  dir_listTra <- reactive({input$dirRefreshButton; dir(path = g_datarootpath(), pattern = "^Transferentropy", full.names = F, recursive = F)})
  dir_listFre <- reactive({input$dirRefreshButton; dir(path = g_datarootpath(), pattern = "^Frequency", full.names = F, recursive = F)})
  dir_listGra <- reactive({input$dirRefreshButton; dir(path = g_datarootpath(), pattern = "^Granger", full.names = F, recursive = F)})
  dir_listERP <- reactive({input$dirRefreshButton; dir(path = g_datarootpath(), pattern = "^ERP", full.names = F, recursive = F)})
  dir_listRS  <- reactive({input$dirRefreshButton; dir(path = g_datarootpath(), pattern = "^RS", full.names = F, recursive = F)})
  dir_listBeh <- reactive({input$dirRefreshButton; list.files(path = file.path(g_datarootpath(), "Behavioral"), pattern = ".csv$", full.names = F, recursive = F)})
=======
  dir_listCoh <- reactive({dir(path = g_datarootpath(), pattern = "^Coherence", full.names = F, recursive = F)})
  dir_listTra <- reactive({dir(path = g_datarootpath(), pattern = "^Transferentropy", full.names = F, recursive = F)})
  dir_listFre <- reactive({dir(path = g_datarootpath(), pattern = "^Frequency", full.names = F, recursive = F)})
  dir_listGra <- reactive({dir(path = g_datarootpath(), pattern = "^Granger", full.names = F, recursive = F)})
  dir_listERP <- reactive({dir(path = g_datarootpath(), pattern = "^ERP", full.names = F, recursive = F)})
  dir_listRS  <- reactive({dir(path = g_datarootpath(), pattern = "^RS", full.names = F, recursive = F)})
  dir_listBeh <- reactive({list.files(path = file.path(g_datarootpath(), "Behavioral"), pattern = ".csv$", full.names = F, recursive = F)})
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc

  # dir_listCoh <- reactiveVal(value = dir(path = "../data", pattern = "^Coherence", full.names = F, recursive = F))
  # dir_listTra <- reactiveVal(value = dir(path = "../data", pattern = "^Transferentropy", full.names = F, recursive = F))
  # dir_listFre <- reactiveVal(value = dir(path = "../data", pattern = "^Frequency", full.names = F, recursive = F))
  # dir_listGra <- reactiveVal(value = dir(path = "../data", pattern = "^Granger", full.names = F, recursive = F))
  #
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
    if        (input$mySidebarMenu == "ConnectivityTab")   { return("Connectivity")
    } else if (input$mySidebarMenu == "CoherenceTab")      { return("Coherence")
    } else if (input$mySidebarMenu == "TransferentropyTab"){ return("Transferentropy")
    } else if (input$mySidebarMenu == "FrequencyTab")      { return("Frequency")
    } else if (input$mySidebarMenu == "GrangerTab")        { return("Granger")
    } else if (input$mySidebarMenu == "ERPTab")            { return("ERP")
    } else if (input$mySidebarMenu == "RSTab")             {
      cat(file=stderr(),"updateSliderInput\n")
      updateSliderInput(session,"freq", value = c(0,5))
      return("RS")
    #} else if (input$mySidebarMenu == "OptionsTab")        { return("Options")
    } else if (input$mySidebarMenu == "BehTab")            { return("Beh")
    } else {   return("Coherence")  }
  })

  g_act_data_dir <<- reactive({
<<<<<<< HEAD
    if (g_act_method()=="Connectivity"){       return(file.path(g_datarootpath(),input$dataDirCon))}
=======
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
    if (g_act_method()=="Coherence"){       return(file.path(g_datarootpath(),input$dataDirCoh))}
    if (g_act_method()=="Transferentropy"){ return(file.path(g_datarootpath(),input$dataDirTra))}
    if (g_act_method()=="Frequency"){       return(file.path(g_datarootpath(),input$dataDirFre))}
    if (g_act_method()=="Granger"){         return(file.path(g_datarootpath(),input$dataDirGra))}
    if (g_act_method()=="ERP"){             return(file.path(g_datarootpath(),input$dataDirERP))}
    if (g_act_method()=="RS"){              return(file.path(g_datarootpath(),input$dataDirRS))}
    if (g_act_method()=="Beh"){              return(file.path(g_datarootpath(),"Behavioral",input$dataDirBeh))}
    #if (g_act_method()=="Options"){         return(file.path(g_datarootpath(),input$dataDirCoh))}

    return("Coherence")
  })
  mydir <- eventReactive(input$mySidebarMenu,{input$dataDirCoh}, ignoreNULL = FALSE)
  #freq <- eventReactive(input$freq,{input$freq}, ignoreNULL = FALSE)
  #directory <- reactiveVal(value = "Coherence")

  # this reload variable triggers the manual reload of other global variables
  # best option to change this is g_reload = g_reload(g_reload()+1)

  g_reload_rVal   <<- reactiveVal(0)
  g_D             <<- reactive({g_reload_rVal(); get_global_D(g_act_data_dir())                })
  g_data          <<- reactive({ g_D()$mdat                                   })
  g_beh           <<- reactive({ g_D()$df_BD                                  })
  g_regions       <<- reactive({ g_D()$uregion_list                           })
  g_trials        <<- reactive({ g_D()$utrial_list                            })
  g_groups        <<- reactive({ c("all_groups",g_D()$ugroup_list)            })
  g_freqs         <<- reactive({ g_D()$ufreq_list                             })
  g_regions_named <<- reactive({ g_D()$uregion_list_named                     })
  g_trials_named  <<- reactive({ g_D()$utrial_list_named                      })
  g_sel_freqs     <<- reactive({ get_selected_freq_list(g_freqs(),input$freq) })
  g_sig           <<- reactive({ input$glob_sig                               })

  # g_data      <<- reactive({ get_data(g_act_data_dir())                })
  # g_beh      <<- reactive({ get_global_tbl_beh(g_act_data_dir())      })
  # g_regions <<- reactive({ get_global_uregion_list(g_act_data_dir()) })
  # g_trials  <<- reactive({ get_global_utrial_list(g_act_data_dir())  })
  # g_groups  <<- reactive({ get_global_group_names(g_act_data_dir())  })
  # g_freqs   <<- reactive({ get_global_ufreq_list(g_act_data_dir())   })
  # g_regions_named <<- reactive({ get_global_region_names(g_act_data_dir()) })
  # g_trials_named  <<- reactive({ get_global_trial_names(g_act_data_dir())  })
  # g_sel_freqs<<- reactive({ get_selected_freq_list(g_freqs(), input$freq) })
  # g_sig<<- reactive({input$glob_sig})
  #
  g_saveImage_button <<- reactive({input$saveimageButton})
  g_saveImage_width <<- reactive({input$saveimagewidth})
  g_saveImage_height <<- reactive({input$saveimageheight})
  g_saveImage_filename <<- reactive({file.path(g_act_data_dir(),input$saveimagefilename)})
  g_saveImage_fileext <<- reactive({input$saveimagefileformat})
  g_saveImage_dpi <<- reactive({input$saveimagedpi})
  g_saveImage_fontsize <<- reactive({input$saveimagefontsize})

  g_visprop_onlysig         <<- reactive({input$visprop_onlysig})
  g_visprop_inlinenumbers   <<- reactive({input$visprop_inlinenumbers})
<<<<<<< HEAD


  # observeEvent(input$dirRefreshButton,{
  #
  # })
=======
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc

  ##################
   #### Sidebar ###
  ##################
  output$res <- renderText({
    paste("You've selected:", input$mySidebarMenu)
  })
  output$RefreshButton <- renderUI({
    actionButton("dirRefreshButton", "refresh")
  })
  output$selectDirCon <- renderUI({
    selectInput("dataDirCon", "Connectivity",choices = dir_listCon(), selected = dir_listCon()[2])
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
  output$selectDirERP <- renderUI({
    selectInput("dataDirERP", "ERP",choices = dir_listERP(),selected = dir_listERP()[2])})
  output$selectDirRS <- renderUI({
    selectInput("dataDirRS", "RS",choices = dir_listRS(),selected = dir_listRS()[1])})
  output$selectDirBeh <- renderUI({
    selectInput("dataDirBeh", "Beh",choices = dir_listBeh(),selected = dir_listBeh()[1])})



  output$freq <- renderUI({
    sliderInput(inputId = "freq",
                label = "Frequency range",
                value = c(8,14), min = 0, max = 40)
  })
  output$glob_sig <- renderUI({
    numericInput("glob_sig", h4("sig threshold"), min =0, max = 1, value = 0.05, step = 0.00001)
    #sliderInput("glob_sig", h4("sig threshold"), min =0 , max = 1, value = 0.05, step = 0.01)
  })
<<<<<<< HEAD
=======

>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
  output$visprop_onlysig <- renderUI({
    checkboxInput("visprop_onlysig", "show only sig.", value = FALSE)
  })
  output$visprop_inlinenumbers <- renderUI({
    checkboxInput("visprop_inlinenumbers", "show nums in graph", value = TRUE)
  })
<<<<<<< HEAD
=======

>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
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




#
#   #####################
#   #### Tabs Options ###
#   #####################
#   output$tabsOpt <- renderUI({
#     fluidRow(
#       tabBox(
#         title = NULL, width = 12,
#         # The id lets us use input$tabset1 on the server to find the current tab
#         id = "tabset1", height = "250px",
#         tabPanel("Regions order", options_mod_orderUI("Options_order")),
#         tabPanel("Regions name", options_mod_nameUI("Options_name"))
#       )
#     )
#   })
<<<<<<< HEAD


  ##################
  #### Tabs Con ###
  ##################
  output$tabsCon <- renderUI({
    cat(file = stderr(), "into output$tabsCon \n")
    updateSliderInput(session,"freq", value = c(0,5))
    fluidRow(
      tabBox(
        title = NULL, width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",
        tabPanel("Plot", overviewPlotUI("ConOverviewPlot")),
        tabPanel("Comp Plot",  compareTrialsPlotUI("ConnPlot2")),
        tabPanel("Long Plot",  longitudinalPlotUI("Conn")),
#        tabPanel("Comp Plot", compareTrialsPlotUI("ConPlot")),
        tabPanel("Trials Stat", compareTrialsStatsLongUI("ConTrialsStat")),
        tabPanel("Groups Stat", compareGroupsStatsUI("ConGroupsStats")),
        tabPanel("Diff Stat", compareDiffOfDiffStatsUI("ConDiffOfDiffStats")),
        tabPanel("Regression", regressionStatsUI("ConRegStats")),
        tabPanel("ANCOVA", ancovaStatsUI("ConAncovaStats")),
        tabPanel("Options Regions", optionsUI("Options"))
        #tabPanel("Regions order", options_mod_orderUI("Options_order")),
        #tabPanel("Regions name", options_mod_nameUI("Options_name"))
        #tabPanel("Plot", RSPlotUI("ConnPlot")),

      )
    )
  })
=======
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc


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
        tabPanel("ANCOVA", ancovaStatsUI("CohAncovaStats")),
        tabPanel("Options Regions", optionsUI("Options")),
        tabPanel("Regions order", options_mod_orderUI("Options_order")),
        tabPanel("Regions name", options_mod_nameUI("Options_name"))

      )
    )
  })


  ##################
  #### Tabs TRA ###
  ##################
  output$tabsTra <- renderUI({
    cat(file = stderr(), "into output$tabsTra \n")
    updateSliderInput(session,"freq", value = c(0,5))
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
        tabPanel("ANCOVA", ancovaStatsUI("TraAncovaStats")),
        tabPanel("Options Regions", optionsUI("Options"))
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


  ##################
  #### Tabs ERP ###
  ##################
  output$tabsERP <- renderUI({
    fluidRow(
      tabBox(
        title = NULL, width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",

        tabPanel("Plot", ERPPlotUI("ERPPlot"))

      )
    )
  })

  #ERPPlotUI("ERPPlot")
  ERPPlotServer("ERPPlot")


  ##################
  #### Tabs RS ###
  ##################
  output$tabsRS <- renderUI({
    cat(file = stderr(), "into output$tabsRS \n")
    updateSliderInput(session,"freq", value = c(0,5))
    fluidRow(
      tabBox(
        title = NULL, width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",

        tabPanel("Plot", RSPlotUI("RSPlot")),
        tabPanel("Comp Plot",  compareTrialsPlotUI("RSPlot2")),
        tabPanel("Long Plot",  longitudinalPlotUI("RS"))
        # tabPanel("Comp Plot", compareTrialsPlotUI("CohPlot")),
        # tabPanel("Trials Stat", compareTrialsStatsUI("CohTrialsStat")),
        # tabPanel("Groups Stat", compareGroupsStatsUI("CohGroupsStats")),
        # tabPanel("Diff Stat", compareDiffOfDiffStatsUI("CohDiffOfDiffStats")),
        # tabPanel("Regression", regressionStatsUI("CohRegStats")),
        # tabPanel("ANCOVA", ancovaStatsUI("CohAncovaStats"))
      )
    )
  })

  dfb <- reactive({
    cat(file = stderr(), paste0("datafile", g_act_data_dir(),"\n"))
    dfx = read.csv(file = g_act_data_dir(), header = TRUE, sep = input$sep, check.names = FALSE)
    return(dfx)
  })
  ##################
  #### Tabs Beh ####
  ##################
  output$tabsBeh <- renderUI({
    cat(file = stderr(), "into output$tabsBeh \n")
    updateSliderInput(session,"freq", value = c(0,5))
    fluidRow(
      tabBox(
        title = NULL, width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",

        #tabPanel("Plot", RSPlotUI("BehPlot")),
        tabPanel("Beh Plot",  behavioralPlotUI("BehPlot2"))
        #tabPanel("Long Beh Plot",  longitudinalPlotUI("RS"))

      )
    )
  })

<<<<<<< HEAD
  ##############################################
  ### The new Connectivity Entity ##############
  #overviewPlotServer("ConOverviewPlot", "Connectivity", reactive(input$glob_sig), reactive(input$freq))
  compareTrialsStatsLongServer("ConTrialsStat", reactive(input$glob_sig), reactive(input$freq))
  compareTrialsPlotServer("ConPlot")
  compareGroupsStatsServer("ConGroupsStats",  reactive(input$glob_sig), reactive(input$freq))
  compareDiffOfDiffStatsServer("ConDiffOfDiffStats", reactive(input$glob_sig), reactive(input$freq))
  behavioralDataStatsServer("ConBehDataStats", reactive(input$glob_sig), reactive(input$freq))
  regressionStatsServer("ConRegStats", reactive(input$glob_sig), reactive(input$freq))
  ancovaStatsServer("ConAncovaStats", reactive(input$glob_sig), reactive(input$freq))
  compareTrialsPlotServer("ConnPlot2")
  longitudinalPlotServer("Conn", dir_listCon())
  overviewPlotServer("ConOverviewPlot", "Coherence", reactive(input$glob_sig), reactive(input$freq))
  ##############################################

=======
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
  #RSPlotUI("ERPPlot")
  #RSPlotServer("RSPlot")
  compareTrialsPlotServer("RSPlot2")
  longitudinalPlotServer("RS", dir_listRS())

  optionsServer("Options")
  options_mod_orderServer("Options_order")
  options_mod_nameServer("Options_name")

#  compareTrialsStatsServer("CohTrialsStat", g_data(), reactive(glob_sig), reactive(freq), utrial_list(), uregion_list(), group_names())
  overviewPlotServer("CohOverviewPlot", "Coherence", reactive(input$glob_sig), reactive(input$freq))
  compareTrialsStatsServer("CohTrialsStat", reactive(input$glob_sig), reactive(input$freq))
  compareTrialsPlotServer("CohPlot")
  compareGroupsStatsServer("CohGroupsStats",  reactive(input$glob_sig), reactive(input$freq))
  compareDiffOfDiffStatsServer("CohDiffOfDiffStats", reactive(input$glob_sig), reactive(input$freq))
  behavioralDataStatsServer("CohBehDataStats", reactive(input$glob_sig), reactive(input$freq))
  regressionStatsServer("CohRegStats", reactive(input$glob_sig), reactive(input$freq))
  ancovaStatsServer("CohAncovaStats", reactive(input$glob_sig), reactive(input$freq))

  overviewPlotServer("TraOverviewPlot", "Transferentropy", reactive(input$glob_sig), reactive(input$freq))
  compareTrialsStatsServer("TraTrialsStat", reactive(input$glob_sig), reactive(input$freq))
  compareTrialsPlotServer("TraPlot")
  compareGroupsStatsServer("TraGroupsStats", reactive(input$glob_sig), reactive(input$freq))
  compareDiffOfDiffStatsServer("TraDiffOfDiffStats", reactive(input$glob_sig), reactive(input$freq))
  behavioralDataStatsServer("TraBehDataStats", reactive(input$glob_sig), reactive(input$freq))
  regressionStatsServer("TraRegStats", reactive(input$glob_sig), reactive(input$freq))
  ancovaStatsServer("CohAncovaStats", reactive(input$glob_sig), reactive(input$freq))

  preprocessingServer("preprocessing")
  mergedataServer("mergedata")

  behavioralPlotServer("BehPlot2") #,reactive(read.csv(file = g_act_data_dir(), header = TRUE, sep = input$sep, check.names = FALSE)))


}
