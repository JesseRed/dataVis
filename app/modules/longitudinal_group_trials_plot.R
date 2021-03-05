library(shiny)
library(markdown)
library(corrr)
library(GGally)
library(ggcorrplot)
library(ggplot2)
library(plotly)

longitudinalPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("fluidRow_oben")),

    # absolutePanel(
    #   bottom = 20, right = 20, width = 200,
    #   draggable = TRUE,
    #   wellPanel(
    #     htmlOutput(ns("html_text")),
    #     sliderInput("n", "", min=3, max=20, value=5),
    #     plotOutput("plot2", height="200px")
    #
    #   ),
    #   style = "opacity: 0.92"
    # ),

  )
}

longitudinalPlotServer <- function(id, dir_listRS) {
  moduleServer(
    id,
    function(input, output, session) {
      ns<-session$ns

      output$fluidRow_oben <- renderUI({
        fluidPage(
        fluidRow(
          column(2,
                 style = "background-color: #fcfcfc;",
                 style = 'border-right: 2px solid gray',
                 h4("trial comparison", align = "center"),
                 fluidRow(
                   column(6,
                          selectInput(ns("trial1"), h5("Select Trial 1", align = "center"),
                                      choices = g_trials_named(), selected =g_trials_named()[1])
                   ),
                   column(6,
                          selectInput(ns("trial2"), h5("Select Trial 2", align = "center"),
                                      choices = g_trials_named(), selected = g_trials_named()[2])
                   )
                 )
          ),
          column(2,

                 style = "background-color: #fcfcfc;",
                 #style = 'border-bottom: 2px solid gray',
                 style = "border-right: 2px solid black",
                 h4("group comparison", align = "center"),
                 fluidRow(
                   column(6,
                          selectInput(ns("group1"), h5("Select Group1", align = "center"),
                                      choices = g_groups(), selected = g_groups()[2])
                          ),
                   column(6,
                          selectInput(ns("group2"), h5("Select Group 2", align = "center"),
                                      choices = g_groups(), selected = g_groups()[2])
                          )
                 )
          ),

          column(2,
                 style = "background-color: #fcfcfc;",
                 style = 'border-right: 2px solid gray',
                 h4("longitudinal data", align = "center"),
                 fluidRow(
                   column(6,
                          textInput(ns("ld_1"), h5("long data 1", align = "center"), value = "1")
                         ),
                   column(6,
                          textInput(ns("ld_2"), h5("long data 2", align = "center"), value = "2, 3")
                   )
                ),
                checkboxInput(ns("longtimefirst"), "estimate time first", value = TRUE),
                checkboxInput(ns("averagelong"), "average same long subj(1 vs. av(2,3))", value = TRUE),
                checkboxInput(ns("cb_same_subjects"), "include only reoccuring subj", value = TRUE)
          ),
          # column(1,
          #        style = "background-color: #fcfcfc;",
          #        style = 'border-right: 2px solid gray',
          #        h4("Visualize", align = "center"),
          #        checkboxInput(ns("longtimefirst"), "estimate time first", value = TRUE),
          #        checkboxInput(ns("cb_same_subjects"), "include only reoccuring subj", value = TRUE)
          # ),
          column(2,
                 style = "background-color: #fcfcfc;",
                 style = 'border-right: 2px solid gray',
                 h4("Filter", align = "center"),
                 textInput(ns("filterg1"), h5("filter G1", align = "center"), value = "Zeichen__1>0"),
                 textInput(ns("filterg2"), h5("filter G2", align = "center"), value = "Zeichen__1>0"),

          ),
          column(2,
                 style = "background-color: #fcfcfc;",
                 style = 'border-right: 2px solid gray',
                 h4("Visualize", align = "center"),
                 selectInput(ns("method"), h5("method"),
                             choices = c("Corrplot", "Corrplot_mixed", "Corrplot_clustered", "ggcorr", "Circle", "Pheatmap"), selected = 1),
                 fluidRow(
                   column(6,
                         selectInput(ns("clustering"), h5("method"),
                             choices = c("original", "FPC","PCA", "hclust"), selected = 1)
                   ),
                   column(6,
                         numericInput(ns("num_hclust"),h5("num hclust"), 3)
                   )
                 )


          ),
          # column(2,
          #        style = "background-color: #fcfcfc;",
          #        style = 'border-right: 2px solid gray',
          #        h4("Clustering", align = "center"),
          #        fluidRow(
          #           column(6,
          #                 selectInput(ns("clustering"), h5("method"),
          #                    choices = c("original", "FPC","PCA", "hclust"), selected = 1)
          #           ),
          #           column(6,
          #                numericInput(ns("num_hclust"),h5("num hclust"), 3)
          #           )
          #        )
          # ),
          column(2,
                 fluidRow(
                   column(6,
                          numericInput(ns("plot_height"),"plot height",800)
                   ),
                   column(6,
                          numericInput(ns("plot_width"),"plot width",0)
                   ),
                 ),
                 fluidRow(
                   column(6,
                          numericInput(ns("plot_res"),"res",96),
                   ),
                   column(6,
                         actionButton(ns("ExportData"), "export Data"),

                 ),
                 ),
          )
        ),
        fluidRow(
          column(12,
                 box(title = "Was wurde berechnet?...", width = 12, collapsible = TRUE, collapsed = TRUE, verbatimTextOutput(ns("text_explanation"))),
          )
        ),

        fluidRow(
          #  plotOutput(ns("plot"), width = "auto", height = "800px", click = ns("plot_click"))
          plotOutput(ns("plot"), width = "auto", height = "auto", click = ns("plot_click")),
          br(),
          br(),
          br(),
        ),

        fluidRow(
          column(9,
                 plotOutput(ns("hist"), width = "auto", height = "300px", click = ns("plot_click_hist")),
          ),
          column(3,
                 verbatimTextOutput(ns("text_bottom")),
                 fluidRow(
                   column(6,
                          numericInput(ns("nif_click_x"),"x=",1),
                   ),
                   column(6,
                          numericInput(ns("nif_click_y"),"y=",2),
                   ),
                 ),
                 fluidRow(
                   column(6,
                          textInput(ns("nif_click_x_region"),"x=","no selection"),
                   ),
                   column(6,
                          textInput(ns("nif_click_y_region"),"y=","no selection"),
                   ),
                 ),
          )
        ),
        fluidRow(
          column(12,
                 box(title = "Included subjects", width = 12, collapsible = TRUE, collapsed = TRUE,
                     #uiOutput(ns("includedSubjects"))
                     actionButton(ns("testexclude"), "update"),

                     checkboxGroupInput(ns("Subjects"), label = h3("Subjects"), inline = T,
                                        choices = g_D()$df_BD$ID,
                                        selected =  g_D()$df_BD$ID),

                     checkboxGroupInput(ns("Group1"), label = h3("Group 1"), inline = T,
                                        choices = c()), #, #curdata()$df_data1$ID,
                     #                     selected = c()), #curdata()$df_data1$ID[my_included_subjects_g1()]),

                     checkboxGroupInput(ns("Group2"), label = h3("Group 2"), inline = T,
                                        choices = c()) #, #curdata()$df_data2$ID,
                     #                      selected = c()) #curdata()$df_data2$ID[my_included_subjects_g2()])
                 ),
          )
        ),
        fluidRow(
          column(12,
                 box(title = "... need skippy mode?...", width = 12, collapsible = TRUE, collapsed = TRUE,
                     fluidRow(
                       column(6,
                              numericInput(ns("xx"),"xxx",0)
                       ),
                       column(6,
                              numericInput(ns("own cluster algo"),"yyy",0)
                       )
                     ),
                 )
          )

        ),
        fluidRow(
          #HTML("<div class='col-sm-4' style='min-width: 350px !important;'>"),
          column(12, box(title = "Behavioral data.frame", width = 12, collapsible = TRUE, collapsed = TRUE,
                         tableOutput(ns("head_beha")))),
          column(12, box(title = "Behavioral data.frame Group1", width = 12, collapsible = TRUE, collapsed = TRUE,
                         tableOutput(ns("head_beha_g1")))),
          column(12, box(title = "Behavioral data.frame Group2", width = 12, collapsible = TRUE, collapsed = TRUE,
                         tableOutput(ns("head_beha_g2")))),
        ),
        fluidRow(

          #HTML("<div class='col-sm-4' style='min-width: 350px !important;'>"),
          column(12, box(title = "Network configuration", width = 12, collapsible = TRUE, collapsed = TRUE,
                         actionButton(ns("usenewnetwork"), "use new network"),

                         actionButton(ns("useoriginalnetwork"), "use original network"),
                         materialSwitch(inputId = "idxxx", label = "Primary switch", status = "info"),
                         prettyRadioButtons(
                           inputId = "choosenetwork",
                           label = "Which network to use for analysis",
                           choices = c("original network", "new network defined here"),
                           shape = "round",
                           status = "danger",
                           fill = TRUE,
                           inline = TRUE
                         ),
                         DT::dataTableOutput('mycheckboxtable'),
                         uiOutput(ns("network"))
          ),
        )
        ),
        fluidRow(
          column(12,
                 box(title = "Plot ..........expand for help (comp_plot_markdown.md)", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp_Comp_Plot"))),
          )
        ),
        fluidRow(
          plotlyOutput(ns("myplotly"), width = "1000", height = "800px")
        ),

)
      })
      subjects_to_exclude = reactive({
        # list of subjects that are not marked
        to_exclude = setdiff( g_D()$df_BD$ID, input$Subjects)
        #cat(file = stderr(), paste0("XXX subjects_to_exclude reactive = ", to_exclude, "\n"))
        return(to_exclude)
        })

      my_included_subjects = reactive({get_included_subjects( g_D()$df_BD$ID, subjects_to_exclude())})
      my_included_subjects_g1 = reactive({ req(input$Subjects); get_included_subjects( curdata()$df_data1$ID, subjects_to_exclude())})
      my_included_subjects_g2 = reactive({get_included_subjects( curdata()$df_data2$ID, subjects_to_exclude())})

      observeEvent(input$testexclude, {
        cat(file = stderr(), paste0("included Subjects = \n"))
        #cat(file = stderr(), paste0("included Subjects = ", input$Subjects, "\n"))
        #cat(file = stderr(), paste0("class(subjects_to_exclude = ", class(subjects_to_exclude()), "\n"))
        #cat(file = stderr(), paste0("length(subjects_to_exclude = ", length(subjects_to_exclude()), "\n"))
        #cat(file = stderr(), paste0("my_included_subjects() = ", my_included_subjects(), "\n"))

        updateCheckboxGroupInput(session, "Subjects",
                                 choices = g_D()$df_BD$ID, inline = T,
                                 selected =  g_D()$df_BD$ID[my_included_subjects()])

        updateCheckboxGroupInput(session, "Group1",
                                 choices = curdata()$df_data1$ID, inline = T,
                                 selected =  curdata()$df_data1$ID[my_included_subjects_g1()]

        )

        updateCheckboxGroupInput(session, "Group2",
                                 choices = curdata()$df_data2$ID, inline = T,
                                 selected =  curdata()$df_data2$ID[my_included_subjects_g2()]
                                 )
      })

      output$head_beha <- renderTable({
        g_D()$df_BD
      })
      output$head_beha_g1 <- renderTable({
        curdata()$df_data1
      })
      output$head_beha_g2 <- renderTable({
        curdata()$df_data2
      })

      x1<<- NULL
      x2<<- NULL
      myTabPlots <<- list()

      # filter data by group
      data_freqmean <- reactive({
        get_data_freqmean(g_data(), g_sel_freqs())
      })
#
#       click_x_num <- reactive({ round(input$plot_click$x)})
#       click_y_num <- reactive({ abs(round(input$plot_click$y)-length(g_regions())-1)})
#       click_x_reg <- reactive({ regions()[click_x_num]})
#       click_y_reg <- reactive({ regions()[click_y_num]})

      ####################################################################################
      ####################################################################################
      # The following section handels the selection of the x and y coordinate in the plot
      # a reactiveVal is needed because i need a common variable that can be changed
      # bei either the click or the change in the input field
      level_x_rval <- reactiveVal(1)
      level_y_rval <- reactiveVal(2)
      region_x_rval <- reactiveVal("not_selected")
      region_y_rval <- reactiveVal("not_selected")
      # update the textInput and numericInput depending on the reactiveVals
      observe({
          updateTextInput(session, "nif_click_x_region", value = region_x_rval())
          updateTextInput(session, "nif_click_y_region", value = region_y_rval())
      })
      # wenn die x, y Koordinate manuell verstellt wird
      observeEvent(c(input$nif_click_x, input$nif_click_y),{
        #cat(file = stderr(), "plot_text observeEvent")
        level_x_rval(input$nif_click_x)
        level_y_rval(input$nif_click_y)
        region_x_rval(g_regions()[input$nif_click_x])
        region_y_rval(g_regions()[input$nif_click_y])
      })
      # wenn in den plot geklickt wird (funktioniert nur fuer den Corplot)
      observeEvent(input$plot_click,{
        cat(file = stderr(), "plot_click observeEvent")
        level_x = round(input$plot_click$x)
        level_y = abs(round(input$plot_click$y)-length(g_regions())-1)
        region_x = (g_regions()[level_x])
        region_y = (g_regions()[level_y])
        level_x_rval(level_x)
        level_y_rval(level_y)
        region_x_rval(region_x)
        region_y_rval(region_y)
        updateNumericInput(session, "nif_click_x", value = level_x)
        updateNumericInput(session, "nif_click_y", value = level_y)

      })
      ####################################################################################
      ####################################################################################


      # get the data for the second time point
      # die longitudinalen Daten sind kodiert als nummern hinter den IDs der Subjects XY001_1
      # daher teilen wir hier die Subjects einfach entsprechend auf

      curdata <- reactive({
        cat(file = stderr(), paste0("curdata with dim(g_D()$mat)=", dim(g_D()$mat),"\n"))
        req(input$group1)
        req(input$group2)
        req(input$trial1)
        req(input$trial2)
        req(input$ld_1)
        req(input$ld_2)
        # req(input$cb_same_subjects)
        # req(input$averagelong)
        # req(input$longtimefirst)
        #gD1 <<- D1()
        #gD2 <<- D2()
        cat(file = stderr(), paste0("curdata with dim(g_D()$mat)=", dim(g_D()$mat),"\n"))
        cat(file = stderr(), paste0("curdata with length(g_D())=", length(g_D()),"\n"))

        M <- get_currently_selected_data_long3(g_D(),
                                              input$group1,
                                              input$group2,
                                              as.numeric(input$trial1),
                                              as.numeric(input$trial2),
                                              g_sel_freqs(),
                                              tbl_beh = g_D()$df_BD,
                                              long_def1 = as.numeric(unlist(strsplit(input$ld_1, split=","))),
                                              long_def2 = as.numeric(unlist(strsplit(input$ld_2, split=","))),
                                              is_exclude_not_reoccuring_subj = input$cb_same_subjects,
                                              averagelong = input$averagelong,
#                                              datalong = D2()$mdat,
#                                              tbl_beh_long = D2()$df_BD,
                                              estimate_time_first = input$longtimefirst,
                                              filter_g1 = input$filterg1,
                                              filter_g2 = input$filterg2,
                                              subjects_to_exclude = subjects_to_exclude()
                                            )
            gM <<- M

        return(M)
      })

      plotwidth <- reactive({
        if (input$plot_width == 0){ return("auto")            }
        else{                       return(input$plot_width)  }
      })

      plotheight <- reactive({
        if (input$plot_height == 0){ return("auto")}
        else{ return(input$plot_height) }
      })


      # description of what is shown
      output$text_explanation<- renderPrint({
        d<-curdata()
        out <- d$explanation
        cat(out)
      })

      ###########################################################
      ### RENDERPLOT
      output$myplotly<-renderPlotly({
        start_time = Sys.time()
        cat(file=stderr(), "before curdata() in myplotly\n")

        # d <- curdata()
        # mat_t <<- d$mat_t
        # mat_p <<- d$mat_p
        # p <-generate_plot_ggplot_corrplot_handmade(mat_p, mat_t)
        # p
        # cat(file = stderr(),paste0("renderPlotly duration =",Sys.time()-start_time,"\n"))

      })


      ###########################################################
      ### RENDERPLOT
      output$plot<-renderPlot(
        width = function() plotwidth(),
        height = function() plotheight(),
        #res = input$plot_res,
        {
          start_time <- Sys.time()

        req(input$trial1)
        req(input$trial2)
        req(input$group1)
        req(input$group2)
        req(input$method)
#        dev.off()
        cur_dev <- dev.cur()
        cat(file = stderr(), cur_dev)
        cat(file=stderr(), "before curdata() in plot\n")
        d <- curdata()
        mat_t <<- d$mat_t
        mat_p <<- d$mat_p
        ###################
        # CORRPLOT
        if (input$method=="Corrplot"){

          generate_plot_Corrplot(d$mat_p, d$mat_t, regions = colnames(d$mat_p),
                                 clustering_method = input$clustering,
                                 num_hclust = input$num_hclust) #D$uregion_list)

        }


        if (input$method=="Corrplot_mixed"){
          #png("mypng.png")
          #x1 <<- plot(d$mat_t)
          mat_p_sig <- mat_p
          mat_p_sig[mat_p>g_sig()]<-g_sig()+0.0000000001

          #dev.off()
          rownames(mat_p) = vector(mode="character", length=length(g_regions()))
          x1 <<- corrplot(mat_p_sig, method="circle", tl.cex = 0.9, type = "upper", is.corr = FALSE,
                          p.mat = mat_p_sig, sig.level = g_sig(),
                          diag=FALSE,
                          insig = "blank",
                          tl.srt = 45,
                          col=colorRampPalette(c("blue","red","green"))(200)
                          #cl.lim = c(0,g_sig())
          )
                          #non_corr.method = "pch",
                          #col=colorRampPalette(c("blue","red","green"))(200))
          colnames(mat_t) = vector(mode="character", length=length(g_regions()))

         # myplot_corr <<- corrplot(mat_t, add = TRUE, method="number", tl.cex = 0.9, type = "lower", is.corr = FALSE,
        #                           p.mat = mat_p, sig.level = g_sig())


        }
        if (input$method=="Corrplot_clustered"){
          #png("mypng.png")
          #x1 <<- plot(d$mat_t)
          #https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html order = "AOE"
          rownames(mat_p) = vector(mode="character", length=length(g_regions()))
          x1 <<- corrplot(mat_p, method="circle", tl.cex = 0.9, type = "upper", is.corr = FALSE,
                          p.mat = mat_p, sig.level = g_sig(),
                          col=colorRampPalette(c("blue","red","green"))(200))
          colnames(mat_t) = vector(mode="character", length=length(g_regions()))

          myplot_corr <<- corrplot(mat_t, add = TRUE, method="number", tl.cex = 0.9, type = "lower", is.corr = FALSE,
                                   p.mat = mat_p, sig.level = g_sig())

          #,
          #               col=colorRampPalette(c("blue","red","green"))(200))
          #dev.off()


        }
        if (input$method=="ggcorr"){
          #png("mypng.png")
          #x1 <<- plot(d$mat_t)
          #https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html order = "AOE"
          #df <- as.data.frame(mat_p)
          #ggplot(data = df) + geom_point()
          #df <- as.data.frame(mat_p)
          #x <-ggplot(data = df, aes(x=frontopolar_A, y = central_A)) + geom_point()
          #x
          p <-generate_plot_ggplot_corrplot_handmade(mat_p, mat_t)



         #xx<- generate_histogram_plot_facet(input$group1, input$group2, input$trial1, input$trial2, freq(), level_x_rval(), level_y_rval())
         return(p)
        }
        if (input$method=="Circle"){
          myplotcircle = generate_plot_Circle(d$mat_p, d$mat_t, d$data1, d$data2)


        }
        if (input$method=="Pheatmap"){

          cur_dev <- dev.cur()
          # Pheatmat setzt das dev.cur() um ... daher manuelles zuruck setzen
          myplotpheatmap = generate_plot_Pheatmap(d$mat_p, d$mat_t, myfontsize = 18)
          #cur_dev <- dev.cur()
          #cat(file = stderr(), paste0("cur_dev=", cur_dev,"\n"))
          dev.set(cur_dev)
          return(myplotpheatmap)

        }
        cat(file = stderr(),paste0("plot duration =",Sys.time()-start_time,"\n"))

      }
      )

      output$text_bottom <- renderPrint({
        cat(file = stderr(),paste0("level_y_rval()=",level_y_rval(),"\n"))
        cat(file = stderr(),paste0("level_x_rval()=",level_x_rval(),"\n"))
        glob_text_d <<- curdata()
        x = curdata()$data1[, level_y_rval(), level_x_rval()]
        y = curdata()$data2[, level_y_rval(), level_x_rval()]
        z = t.test(x,y, paired = curdata()$my_paired)
        out <- create_my_ttest_string(z, paired = curdata()$my_paired, mean1 = mean(x, na.rm = T), mean2 = mean(y, na.rm = T))
        cat(out)
      })

      output$hist <- renderPlot({
        glob_hist_d <<- curdata()
        generate_histogram_plot_facet_long(1,2,
                                      input$trial1, input$trial2,
                                      g_sel_freqs(),
                                      level_x_rval(), level_y_rval(),
                                      data = curdata())

      })


      output$facet <- renderPlot({
        df = tbl_beh
        d = data_freqmean()
        df$data1 = d[,level_x_rval, level_y_rval, input$trial1]
        df$num <- ave(df$data1, df$Gruppe, FUN = seq_along)

      })

      output$htmlhelp_Comp_Plot <- renderUI({
        # if (showhtml()){
        includeMarkdown(rmarkdown::render("./documentation/longitudinal_group_trials_plot_markdown.md"))
        # }
      })


      output$network <- renderUI({
        fluidRow(
        column(1, style = "background-color: #fcfcfc;",
               style = 'border-right: 2px solid gray',
               for (i in 1:length(g_regions())){
                 h4(g_regions()[i], align = "center")

               }
        ),
        column(11,
               radioGroupButtons(inputId = ns("head_beha"),
                                 choices = rep("X", 4),
                                 direction = "horizontal",
                                 individual = TRUE,
                                 width = '200px'
                                 )
        ),
        )
      })

      output$my


          #                   selected = "2013")
        # fluidRow(
        #   tabBox(
        #     title = NULL, width = 12,
        #     # The id lets us use input$tabset1 on the server to find the current tab
        #     id = "tabset1", height = "250px",
        #     tabPanel("Plot", overviewPlotUI("ConOverviewPlot")),
        #     tabPanel("Comp Plot", compareTrialsPlotUI("ConPlot")),
        #     tabPanel("Trials Stat", compareTrialsStatsUI("ConTrialsStat")),
        #     tabPanel("Groups Stat", compareGroupsStatsUI("ConGroupsStats")),
        #     tabPanel("Diff Stat", compareDiffOfDiffStatsUI("ConDiffOfDiffStats")),
        #     tabPanel("Regression", regressionStatsUI("ConRegStats")),
        #     tabPanel("ANCOVA", ancovaStatsUI("ConAncovaStats")),
        #     tabPanel("Options Regions", optionsUI("Options")),
        #     tabPanel("Regions order", options_mod_orderUI("Options_order")),
        #     tabPanel("Regions name", options_mod_nameUI("Options_name")),
        #     tabPanel("Plot", RSPlotUI("ConnPlot")),
        #     tabPanel("Comp Plot",  compareTrialsPlotUI("ConnPlot2")),
        #     tabPanel("Long Plot",  longitudinalPlotUI("Conn"))
        #   )
        # )
      #})



      # Observe Funktion fuer den zentralen Specherbutton
      observeEvent(g_saveImage_button(),{
        req(input$group1)
        req(input$group1)
        req(input$trial1)
        req(input$trial2)

        cat(file = stderr(), "observeEvent(g_save_Image_button(), with input$method =", input$method,"\n")
        cat(file = stderr(), "dpi=",g_saveImage_dpi(),"\n")
        cat(file=stderr(), "before curdata() in g_saveImage_button\n")

        d <- curdata()
        #if (g_saveImage_button()>0){
          filename = paste0(g_saveImage_filename(),"_hist", format(Sys.time(), "%Y-%m-%d-%H-%M-%S."), g_saveImage_fileext())
          myplot<-generate_histogram_plot_facet(input$group1, input$group2, input$trial1, input$trial2, g_sel_freqs(), level_x_rval(), level_y_rval())
          ggsave(file = filename, width = g_saveImage_width(), height =g_saveImage_height(), units = "cm", plot = myplot, type = "cairo", dpi = g_saveImage_dpi())
        #}


          filename2 = paste0(g_saveImage_filename(),"_",input$method, format(Sys.time(), "%Y-%m-%d-%H-%M-%S."), g_saveImage_fileext())
          open_device_for_save(filename2)
          myplot = switch(
            input$method,
            "Corrplot"     = generate_plot_Corrplot(d$mat_p, d$mat_t),
            "Circle"       = generate_plot_Circle(d$mat_p, d$mat_t, d$data1, d$data2),
            "Pheatmap"     = generate_plot_Pheatmap(d$mat_p, d$mat_t)
          )
          dev.off()
      }
      )

      observeEvent(input$ExportData, { export_selected_tab_data(data = curdata()) })


    }
  )
}



# https://yihui.shinyapps.io/DT-radio/
# library(shiny)
# library(DT)
# shinyApp(
#   ui = fluidPage(
#     title = 'Radio buttons in a table',
#     DT::dataTableOutput('foo'),
#     verbatimTextOutput('sel')
#   ),
#   server = function(input, output, session) {
#     m = matrix(
#       as.character(1:5), nrow = 12, ncol = 5, byrow = TRUE,
#       dimnames = list(month.abb, LETTERS[1:5])
#     )
#     for (i in seq_len(nrow(m))) {
#       m[i, ] = sprintf(
#         '<input type="radio" name="%s" value="%s"/>',
#         month.abb[i], m[i, ]
#       )
#     }
#     m
#     output$foo = DT::renderDataTable(
#       m, escape = FALSE, selection = 'none', server = FALSE,
#       options = list(dom = 't', paging = FALSE, ordering = FALSE),
#       callback = JS("table.rows().every(function(i, tab, row) {
#           var $this = $(this.node());
#           $this.attr('id', this.data()[0]);
#           $this.addClass('shiny-input-radiogroup');
#         });
#         Shiny.unbindAll(table.table().node());
#         Shiny.bindAll(table.table().node());")
#     )
#     output$sel = renderPrint({
#       str(sapply(month.abb, function(i) input[[i]]))
#     })
#   }
# )





















