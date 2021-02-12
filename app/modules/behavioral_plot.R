library(shiny)
library(plotly)
library(hrbrthemes)
library(dplyr)

behavioralPlotUI <- function(id){
  ns <- NS(id)
  # Thanks to the namespacing, we only need to make sure that the IDs
  # are unique within this function, rather than unique across the entire app.
  tagList(
    uiOutput(ns("uiANOVA")),
    #verbatimTextOutput(ns("textANOVAStats")),


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

behavioralPlotServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
      ns<-session$ns
      df <- reactive({
        #cat(file = stderr(), paste0("datafile ", g_act_data_dir(),"\n"))
        #dfx = read.csv(file = g_act_data_dir(), header = TRUE, sep = ";", check.names = FALSE)
        dfx = fread(file = g_act_data_dir())
        #cat(file = stderr(), paste0("the colnames are: ", colnames(dfx),"\n"))
        return(dfx)
      })

      dfw <- reactive({
        wide2long(df())
      })
      # unique Colnames without the number after "__"


      ucolnames <- reactive({
        # cat(file = stderr(), paste(colnames(dfb)))
        ucolnames <- unlist(unique(lapply(strsplit(colnames(df()),'__'),`[[`,1)), use.names=FALSE)
        ucolnames <- c("Measurement_num","Measurement_str",ucolnames)
      })

      #f_utrial_list_all <- reactive({c("all", g_trials())})
      output$uiANOVA <- renderUI({

        fluidPage(
          fluidRow(

            column(5,

                   style = "background-color: #fcfcfc;",
                   #style = 'border-bottom: 2px solid gray',
                   style = "border-right: 2px solid black",
                   h4("Method", align = "center"),
                   fluidRow(
                     column(6,
                            selectInput(ns("group1"), h5("Select Group1", align = "center"),
                                        choices = c("data across time", "one vs. one"), selected = 1)
                     ),
                     # column(6,
                     #        selectInput(ns("group2"), h5("Select Group 2", align = "center"),
                     #                    choices = g_groups(), selected = g_groups()[3])
                     # )

                   )

            ),
            column(5,
                   style = "background-color: #fcfcfc;",
                   style = 'border-right: 2px solid gray',
                   h4("trial comparison", align = "center"),
                   fluidRow(
                     column(6,
                            selectInput(ns("column1"), h5("Select Variable", align = "center"),
                                        choices = ucolnames(), selected = 1),
                            #                                        choices = c("A","B"), selected = 1)#ucolnames(), selected = 1)
                     ),
                     column(6,
                            selectInput(ns("column2"), h5("Select Variable", align = "center"),
                                          choices = ucolnames(), selected = ucolnames()[8]),
                              #                                        choices = c("A","B"), selected = 1)#ucolnames(), selected = 1)
                       )
                     # column(6,
                     #        selectInput(ns("trial2"), h5("Select Trial 2", align = "center"),
                     #                    choices = g_trials_named(), selected = g_trials_named()[2])

                   )
            ),
            column(1,
                   style = "background-color: #fcfcfc;",
                   h4("Visualize", align = "center"),
                   selectInput(ns("statsMethod"), h5("method"),
                               choices = c("Regression","ANOVA"), selected = 1)

            ),
            column(1,
                   style = "background-color: #fcfcfc;",
                   h4("sep", align = "center"),
                   textInput(ns("sep"), h5("sep"),";")
            )
          ),
          fluidRow(
            # column(9,
            #        plotOutput(ns("plot"), width = "auto", height = "700px", click = ns("plot_click")),
            # ),
            # column(3,
            #        selectInput(ns("mainregressor"), h4("main regressor"),
            #                    choices = colnames(g_beh())),
            #        selectInput(ns("regressors"), h4("potential regressors"),
            #                    multiple = TRUE, selectize = FALSE,
            #                    size = 35,
            #                    choices = colnames(g_beh()),
            #                    selected = 3)
            # )
          ),

          fluidRow(align = "center", h4("the showing"),
                   column(9,
                          plotlyOutput(ns("data_time"), width = "auto", height = "600px"
                                       ),
                          # plotOutput(ns("data_time"), width = "auto", height = "600px",
                          #            #click = ns("plot_click"),
                          #            dblclick = dblclickOpts(id = ns("plot_dblclick")),
                          #            hover = hoverOpts(id = ns("plot_hover")),
                          #            brush = brushOpts(id = ns("plot_brush"))
                          # ),
                   ),
                   column(3, align = "left",
                          verbatimTextOutput(ns("text_data_time")),
                   )),

          fluidRow(align = "center", h4("the showing2"),
                   column(9,
                          plotlyOutput(ns("data_time2"), width = "auto", height = "600px"),
                   ),
                   column(3, align = "left",
                          verbatimTextOutput(ns("text_data_time2")),
                   )),

          fluidRow(align = "center", h4("the showing3"),
                   column(9,
                          plotOutput(ns("data_time3"), width = "auto", height = "600px", click = ns("plot_click_hist")),
                   ),
                   column(3, align = "left",
                          verbatimTextOutput(ns("text_data_time3")),
                   )),
#
#           fluidRow(align = "center", h4("comparison of Trial1 vs. Trial 2 of selected group 1"),
#                    column(9,
#                           plotOutput(ns("hist_compare_diffTrial_sameGroup1"), width = "auto", height = "300px", click = ns("plot_click_hist")),
#                    ),
#                    column(3, align = "left",
#                           verbatimTextOutput(ns("text_stats_compare_diffTrial_sameGroup1")),
#                    )),
#
#           fluidRow(align = "center", h4("comparison of Trial1 vs. Trial 2 of selected group 2"),
#                    column(9,
#                           plotOutput(ns("hist_compare_diffTrial_sameGroup2"), width = "auto", height = "300px", click = ns("plot_click_hist")),
#                    ),
#                    column(3, align = "left",
#                           verbatimTextOutput(ns("text_stats_compare_diffTrial_sameGroup2")),
#                    )),
#           fluidRow(align = "center", h4("comparison of Group 1 vs. Group 2 of selected trial 1"),
#                    column(9,
#                           plotOutput(ns("hist_compare_diffGroup_sameTrial1"), width = "auto", height = "300px", click = ns("plot_click_hist")),
#                    ),
#                    column(3, align = "left",
#                           verbatimTextOutput(ns("text_stats_compare_diffGroup_sameTrial1")),
#                    )),
#           fluidRow(align = "center", h4("comparison of Group 1 vs. Group 2 of selected trial 2"),
#                    column(9,
#                           plotOutput(ns("hist_compare_diffGroup_sameTrial2"), width = "auto", height = "300px", click = ns("plot_click_hist")),
#                    ),
#                    column(3, align = "left",
#                           verbatimTextOutput(ns("text_stats_compare_diffGroup_sameTrial2")),
#                    )),

          # fluidRow(
          #   column(12,
          #          verbatimTextOutput(ns("text_stats")),
          #   )),
          #   fluidRow(
          #     column(12,
          #            verbatimTextOutput(ns("simple_correlation")),
          #     )
          #   ),
          fluidRow(
            column(12,
                   box(title = "simple correlation ..........expand for help", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp_simple_correlation"))),
            )
          ),
          fluidRow(
            column(12,
                   box(title = "show orgigninal table ..........", width = 12, collapsible = TRUE, collapsed = TRUE, tableOutput(ns("showtable"))),
            )
          ),
          fluidRow(
            column(12,
                   tableOutput(ns("tab_simple_group_correlation")),
            )
          ),
          fluidRow(
            column(12,
                   tableOutput(ns("tab_simple_trial_correlation")),
            )
          ),
          fluidRow(
            column(12,
                   box(title = "help for partial correlation below", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp_partial_correlation"))),
            )
          ),
          fluidRow(
            column(12,
                   box(title = "partial correlation ", width = 12, collapsible = TRUE, collapsed = FALSE,
                       fluidRow(
                         column(3,
                                verbatimTextOutput(ns("partial_correlationG1T1"))
                         ),
                         column(3,
                                verbatimTextOutput(ns("partial_correlationG1T2"))
                         ),
                         column(3,
                                verbatimTextOutput(ns("partial_correlationG2T1"))
                         ),
                         column(3,
                                verbatimTextOutput(ns("partial_correlationG2T2"))
                         ),
                       )),
            )
          ),
          # fluidRow(
          #   column(12,
          #          box(title = "partial correlation ", width = 12, collapsible = TRUE, collapsed = FALSE, verbatimTextOutput(ns("partial_correlation"))),          )
          # )


        )
      })

      selColData <- reactive({
        req(input$column)

      })

      output$data_time <-renderPlotly({
        cat(file = stderr(), paste0("input$column2=",input$column2,"\n"))
        options(viewer=NULL)
        windowsFonts(Times=windowsFont("TT Times New Roman"))
        bd2x <<- dfw()
#        ggplot(data = dfw(), aes(x = input$column1, y = input$column2)) +
         p<-ggplot(data = dfw(), aes(x = Measurement_num, y = Zeichen, group = ID, colour = ID, shape = ID)) +
           geom_line()+
           geom_point(size = 2)+ # shape = 21, color="black", fill="#69b3a2", size=6) +
           ggtitle(paste0(input$column1," vs. ", input$column2))

         ggplotly(p)
         #geom_line( color="grey") +
          #geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
          #theme_ipsum() +
          #ggtitle("Evolution of bitcoin price")

        #cat(file = stderr(), "renderPlot\n")
#        plot(c(1,2,3), c(1,2,3))
      })
      output$text_data_time <- renderPrint({
        cat("statistic text about here")
        cat("input$plot_click:\n")
        str(input$plot_click)
        cat("input$plot_hover:\n")
        str(input$plot_hover)
        cat("input$plot_dblclick:\n")
        str(input$plot_dblclick)
        cat("input$plot_brush:\n")
        str(input$plot_brush)
      })

      addPopover(session, id = "data_time", title = "Data", content = paste0("asdfsda"),
                 placement = "bottom",trigger = "hover")

      output$data_time2 <-renderPlotly({
        cat(file = stderr(), "renderPlotly\n")
        data(diamonds, package = "ggplot2")
        p<-plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")
        p
        #

        # dfw() %>%
        #   plot_ly(x = ~Measurement, y = ~Zeichen) %>%
        #   layout(title = "My histos")
        #p<-ggplot(data = dfw(), aes(x = Measurement, y = Zeichen)) +
        #  geom_point()
        #return(p)
      })













      ################################################################
      # THE histogram plots of individual subjects
      ###########################################
      #
      #       output$hist_compare_diffTrial_sameGroup1 <- renderPlot({
      #         req(input$plot_click$x)
      #         create_df_for_histplot(compare = "trials", group=1, trial = 1)
      #       })
      #
      #
      #       output$hist_compare_diffTrial_sameGroup2 <- renderPlot({
      #         req(input$plot_click$x)
      #         create_df_for_histplot(compare = "trials", group=2, trial = 1)
      #       })
      #
      #
      #       output$hist_compare_diffGroup_sameTrial1 <- renderPlot({
      #         req(input$plot_click$x)
      #         create_df_for_histplot(compare = "groups", group=1, trial = 1)
      #       })
      #
      #
      #       output$hist_compare_diffGroup_sameTrial2 <- renderPlot({
      #         req(input$plot_click$x)
      #         create_df_for_histplot(compare = "groups", group=1, trial = 2)
      #       })



      #
      #       ################################################################
      #       # THE text about t-statistics
      #       ###########################################
      #       output$text_stats_compare_diffGroup_sameTrial1 <- renderPrint({
      #         req(input$plot_click)
      #         z = ttest_estimation(compare = "groups", group = 1, trial = 1)
      #         cat(z$mydescription)
      #       })
      #
      #       output$text_stats_compare_diffGroup_sameTrial2 <- renderPrint({
      #         req(input$plot_click)
      #         z = ttest_estimation(compare = "groups", group = 1, trial = 2)
      #         cat(z$mydescription)
      #       })
      #
      #       output$text_stats_compare_diffTrial_sameGroup1 <- renderPrint({
      #         req(input$plot_click)
      #         z = ttest_estimation(compare = "trials", group = 1, trial = 1)
      #         cat(z$mydescription)
      #       })
      #
      #       output$text_stats_compare_diffTrial_sameGroup2 <- renderPrint({
      #         req(input$plot_click)
      #         z = ttest_estimation(compare = "trials", group = 2, trial = 1)
      #         cat(z$mydescription)
      #       })
      #       ####
      #       #################################################################
      #



      output$htmlhelp_simple_correlation <- renderUI({
        # if (showhtml()){
        includeMarkdown(rmarkdown::render("./documentation/simple_correlation_markdown.md"))
        # }
      })

      output$htmlhelp_parial_correlation <- renderUI({
        # if (showhtml()){
        includeMarkdown("./documentation/partial_correlation_markdown.md")
        # }
      })

      output$showtable <- renderTable({
        #req(input$plot_click)
        return(df())
      })

      ###########################################
      # the newly created statistics section
      output$tab_simple_group_correlation <- renderTable({
        req(input$plot_click)

        # berechne Werte fuer den main regessor
        #reg_name = get_beh_tbl_data_by_group(input$group1, input$mainregressor)
        b1 = get_beh_tbl_data_by_group(input$group1, input$mainregressor)
        b2 = get_beh_tbl_data_by_group(input$group2, input$mainregressor)
        #cat(file = stderr(), "now create")
        df <- append_correlation_row(x1 = xg1t1, b1 = b1, x2 = xg2t1, b2 = b2,
                                     method = "pearson",
                                     t = g_trials()[input$trial1],
                                     g1 = input$group1,
                                     g2 = input$group2,
                                     reg_name = input$mainregressor)
        df <- append_correlation_row(x1 = xg1t2, b1 = b1, x2 = xg2t2, b2 = b2,
                                     method = "pearson",
                                     t = g_trials()[input$trial2],
                                     g1 = input$group1,
                                     g2 = input$group2,
                                     reg_name = input$mainregressor)

        #cat(file = stderr(), "now for loop")
        for ( i in 1:length(input$regressors)){
          b1 = get_beh_tbl_data_by_group(input$group1, input$regressors[i])
          b2 = get_beh_tbl_data_by_group(input$group2, input$regressors[i])
          df <- append_correlation_row(x1 = xg1t1, b1 = b1, x2 = xg2t1, b2 = b2,
                                       method = "pearson",
                                       t = g_trials()[input$trial1],
                                       g1 = input$group1,
                                       g2 = input$group2,
                                       reg_name = input$regressors[i], df=df)



          df <- append_correlation_row(x1 = xg1t2, b1 = b1, x2 = xg2t2, b2 = b2,
                                       method = "pearson",
                                       t = g_trials()[input$trial2],
                                       g1 = input$group1,
                                       g2 = input$group2,
                                       reg_name = input$regressors[i], df=df)


        }
        return(df)

      })
      ####
      #################################################################

      ###########################################
      # the newly created statistics section for different trials
      output$tab_simple_trial_correlation <- renderTable({
        req(input$plot_click)

        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]

        xg1t1 = data_g1_t1()[,level_y(),level_x()]
        xg1t2 = data_g1_t2()[,level_y(),level_x()]
        xg2t1 = data_g2_t1()[,level_y(),level_x()]
        xg2t2 = data_g2_t2()[,level_y(),level_x()]


        # berechne Werte fuer den main regessor
        #reg_name = get_beh_tbl_data_by_group(input$group1, input$mainregressor)
        b1 = get_beh_tbl_data_by_group(input$group1, input$mainregressor)
        b2 = get_beh_tbl_data_by_group(input$group2, input$mainregressor)

        df <- append_correlation_row_trials(x1 = xg1t1, b1 = b1, x2 = xg1t2,
                                            method = "pearson",
                                            g = input$group1,
                                            t1 = input$trial1,
                                            t2 = input$trial2,
                                            reg_name = input$mainregressor)
        df <- append_correlation_row_trials(x1 = xg2t1, b1 = b2, x2 = xg2t2,
                                            method = "pearson",
                                            g = input$group2,
                                            t1 = input$trial1,
                                            t2 = input$trial2,
                                            reg_name = input$mainregressor, df = df)

        for ( i in 1:length(input$regressors)){
          b1 = get_beh_tbl_data_by_group(input$group1, input$regressors[i])

          df <- append_correlation_row_trials(x1 = xg1t1, b1 = b1, x2 = xg1t2,
                                              method = "pearson",
                                              g = input$group1,
                                              t1 = input$trial1,
                                              t2 = input$trial2,
                                              reg_name = input$regressors[i], df = df)

          b2 = get_beh_tbl_data_by_group(input$group2, input$regressors[i])

          df <- append_correlation_row_trials(x1 = xg2t1, b1 = b2, x2 = xg2t2,
                                              method = "pearson",
                                              g = input$group2,
                                              t1 = input$trial1,
                                              t2 = input$trial2,
                                              reg_name = input$regressors[i], df = df)

        }
        return(df)

      })


      ###########################################
      # the newly created statistics section for different trials
      output$partial_correlationG1T1 <- renderPrint({
        req(input$plot_click)

        cat(create_partial_correlation_string(group=1,trial=1))

      })

      ###########################################
      # the newly created statistics section for different trials
      output$partial_correlationG1T2 <- renderPrint({
        req(input$plot_click)

        cat(create_partial_correlation_string(group=1,trial=2))

      })

      ###########################################
      # the newly created statistics section for different trials
      output$partial_correlationG2T1 <- renderPrint({
        req(input$plot_click)

        cat(create_partial_correlation_string(group=2,trial=1))

      })

      ###########################################
      # the newly created statistics section for different trials
      output$partial_correlationG2T2 <- renderPrint({
        #req(input$plot_click)
        #df()
        cat('something')
        #cat(create_partial_correlation_string(group=2,trial=2))

      })









      ####
      #################################################################

      ## general function specific for this tab
      #################################################################

      create_partial_correlation_string <- function( group = 1, trial = 1){
        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]



        if (group==1){
          gin = input$group1
          b = get_beh_tbl_data_by_group(input$group1, input$mainregressor)
          if (trial == 1){
            tin = g_trials()[input$trial1]
            x_in = data_g1_t1()[,level_y(),level_x()]
          }
          if (trial == 2){
            tin = g_trials()[input$trial2]
            x_in = data_g1_t2()[,level_y(),level_x()]
          }
        }
        if (group==2){
          gin = input$group2
          b = get_beh_tbl_data_by_group(input$group2, input$mainregressor)
          if (trial == 1){
            tin = g_trials()[input$trial1]

            x_in = data_g2_t1()[,level_y(),level_x()]
          }
          if (trial == 2){
            tin = g_trials()[input$trial2]
            x_in = data_g2_t2()[,level_y(),level_x()]
          }
        }




        df <- data.frame(x = x_in, y = b)
        n = c("x", "y")

        for ( i in 1:length(input$regressors)){

          b = get_beh_tbl_data_by_group(gin, input$regressors[i])
          df<-cbind(df, b)
          #names(df)[names(df)=="V1"]<-input$regressors[i]
          n <- c(n,input$regressors[i])
        }
        names(df)<-n
        pc <-pcor(n, var(df))
        tmptest <- pcor.test(pc, length(n)-2, length(b))
        out <- paste0("group = ", gin, "  trial = ",trial, "\n",
                      "r = ", pc, "\n",
                      "r^2 =", pc^2, "\n",
                      "t = ", tmptest[1],"\n",
                      "df = ", tmptest[2], "\n",
                      "p = ", tmptest[3]
        )
        return(out)

      }





      ttest_estimation <- function(compare = "groups",
                                   group = 1, trial = 1){
        xg1t1 = data_g1_t1()[,level_y(),level_x()]
        xg1t2 = data_g1_t2()[,level_y(),level_x()]
        xg2t1 = data_g2_t1()[,level_y(),level_x()]
        xg2t2 = data_g2_t2()[,level_y(),level_x()]
        mystring = ""
        ispaired = FALSE
        m1 = 0
        m2 = 0
        # vergleiche 2 Gruppen mit einem Trial
        if (compare == "groups"){
          mystring = paste0(mystring, input$group1, " vs. ", input$group2)
          ispaired = FALSE
          if (input$group1 == input$group2){
            cat("no output in case of same groups")
            return()
          }

          if (trial==1){
            mystring = paste0(mystring, " of trial ", g_trials()[input$trial1], "\n")
            z = t.test(xg1t1,xg2t1, paired = ispaired)
            m1 = mean(xg1t1)
            m2 = mean(xg2t1)
          }
          if (trial == 2){
            mystring = paste0(mystring, " of trial ", g_trials()[input$trial2], "\n")
            z = t.test(xg1t2,xg2t2, paired = ispaired)
            m1 = mean(xg1t2)
            m2 = mean(xg2t2)
          }
        }
        # if comparing 2 trails of the same group
        if (compare == "trials"){
          mystring = paste0(mystring, g_trials()[as.numeric(input$trial1)], " vs. ", g_trials()[as.numeric(input$trial2)])
          ispaired = TRUE
          if (group==1){
            z = t.test(xg1t1,xg1t2, paired = ispaired)
            mystring = paste0(mystring, " of group ", input$group1, "\n")
            m1 = mean(xg1t1)
            m2 = mean(xg1t2)

          }
          if (group == 2){
            z = t.test(xg2t1,xg2t2, paired = ispaired)
            mystring = paste0(mystring, " of group ", input$group2, "\n")
            m1 = mean(xg2t1)
            m2 = mean(xg2t2)

          }
        }
        z$mydescription <- paste0(mystring, create_my_ttest_string(z, paired = ispaired, mean1 = m1, mean2 = m2))

        return(z)

      }


      create_df_for_histplot <- function(compare = "groups",
                                         group = 1, trial = 1){
        xg1t1 = data_g1_t1()[,level_y(),level_x()]
        xg1t2 = data_g1_t2()[,level_y(),level_x()]
        xg2t1 = data_g2_t1()[,level_y(),level_x()]
        xg2t2 = data_g2_t2()[,level_y(),level_x()]
        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]

        #cat(file = stderr(), "create_df_for_histplot\n")
        #string1 = paste0(input$trial1," vs ", input$trial2, "in group ", input$group1, "\n")
        if (compare == "groups"){
          if (input$group1 == input$group2){
            cat("no output in case of same groups")
            return()
          }
          if (trial==1){
            x = xg1t1
            y = xg2t1
          }
          if (trial == 2){
            x = xg1t2
            y = xg2t2
          }
          df <- data.frame(Gruppe=c(rep(input$group1, times=length(x)),
                                    rep(input$group2, times=length(y))),
                           val=c(x, y))
          df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
          # means for geomline
          df_hline = data.frame(Gruppe = c(input$group1,input$group2), Means=c(mean(x), mean(y)))

        }
        # if comparing 2 trails of the same group
        if (compare == "trials"){
          if (group==1){
            x = xg1t1
            y = xg1t2
          }
          if (group == 2){
            x = xg2t1
            y = xg2t2
          }
          df <- data.frame(Gruppe=c(rep(g_trials()[as.numeric(input$trial1)], times=length(x)),
                                    rep(g_trials()[as.numeric(input$trial2)], times=length(y))),
                           val=c(x, y))

          df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
          # means for geomline
          df_hline = data.frame(Gruppe = c(g_trials()[as.numeric(input$trial1)],
                                           g_trials()[as.numeric(input$trial2)]),
                                Means=c(mean(xg1t1), mean(xg1t2)))
        }



        ggplot(df, aes(num, val, fill=Gruppe)) +
          geom_bar(stat="identity") +
          facet_wrap(~Gruppe) +
          geom_hline(data = df_hline, aes(yintercept = Means))


      }


    }
  )
}


append_correlation_row <- function(x1 = NULL, b1 = NULL, x2 = NULL, b2 = NULL,
                                   t = "not known",
                                   method = "pearson", reg_name = "no_reg_name",
                                   g1 = "not known", g2 = "not known",
                                   df = NULL) {
  m1 = cor.test(x1,b1, method = method)
  m2 = cor.test(x2,b2, method = method)
  #cat(file = stderr(), m1$estimate)
  #cat(file = stderr(), m2$estimate)
  #cat(file = stderr(), length(x1))
  #cat(file = stderr(), length(x2))
  r_ind = comparing_independent_rs(m1$estimate, m2$estimate, length(x1),length(x2))
  df2 <- data.frame(regname = reg_name,
                    cor_method = method,
                    trial  = t,
                    group1 = g1,
                    r1     = m1$estimate,
                    p1     = m1$p.value,
                    t1      = m1$statistic,
                    df1    = m1$parameter,
                    CI1_l  = m1$conf.int[1],
                    CI1_h  = m1$conf.int[2],
                    group2 = g2,
                    r2     = m2$estimate,
                    p2     = m2$p.value,
                    t2     = m2$statistic,
                    df2    = m2$parameter,
                    CI2_l  = m2$conf.int[1],
                    CI2_h  = m2$conf.int[2],
                    z_dif  = r_ind[1],
                    p_dif  = r_ind[2],
                    stringsAsFactors = FALSE

  )
  if (is.null(df)){
    return(df2)
    #    df <- create_empty_df_for_correlation(num_groups = 2)
  }
  df_new <- rbind(df, df2)
  return(df_new)

}

append_correlation_row_trials <- function(x1 = NULL, b1 = NULL, x2 = NULL,
                                          method = "pearson", g = "not known", reg_name = "no_reg_name",
                                          t1 = "not known", t2 = "not known",
                                          df = NULL) {
  x = x1
  y = b1
  z = x2
  mxy = cor.test(x,y, method = method)
  mzy = cor.test(z,y, method = method)
  mxz = cor.test(x,z, method = method)
  #comparing_independent_rs <-function(rxy, rxz, rzy, n)
  r_dep = comparing_dependent_rs(mxy$estimate, mxz$estimate, mzy$estimate, length(x))
  df2 <- data.frame(regname = reg_name,
                    cor_method = method,
                    group  = g,
                    trial1 = t1,
                    r1     = mxy$estimate,
                    p1     = mxy$p.value,
                    t1      = mxy$statistic,
                    df1    = mxy$parameter,
                    CI1_l  = mxy$conf.int[1],
                    CI1_h  = mxy$conf.int[2],
                    trial2 = t2,
                    r2     = mzy$estimate,
                    p2     = mzy$p.value,
                    t2     = mzy$statistic,
                    df2    = mzy$parameter,
                    CI2_l  = mzy$conf.int[1],
                    CI2_h  = mzy$conf.int[2],
                    t_dif  = r_dep[1],
                    p_dif  = r_dep[2],
                    stringsAsFactors = FALSE

  )
  if (is.null(df)){
    return(df2)
    #    df <- create_empty_df_for_correlation(num_groups = 2)
  }
  df_new <- rbind(df, df2)
  return(df_new)

}





wide2long<- function(df){
  # get a list of all measurement timepoints (the numbers after the __ )
  mytimemarkers = c()
  x = strsplit(colnames(df),'__')
  for (i in 1:length(x)) {
    if (length(x[[i]])==2){
      #cat(file = stderr(), paste0("i=",i,"\n"))

      mytimemarkers=c(mytimemarkers,x[[i]][2])
    }
  }
  umytimemarkers = unique(mytimemarkers)
  ucolnames = unlist(unique(lapply(strsplit(colnames(df),'__'),`[[`,1)), use.names=FALSE)
  ucolnames = c("Measurement_num","Measurement_str",ucolnames)
  #cat(file = stderr(), paste0('\n',ucolnames,'\n'))
  df_new = data.frame(matrix(ncol=length(ucolnames), nrow=0))
  names(df_new) <- ucolnames
  #cat(file = stderr(), paste0("\n Mycolnames = \n",colnames(df_new),"\n"))

  df_new_row_idx = 1
  df_row_idx = 1
  # gehe ueber jedes Feld
  # bestimme den colname und entsprechend das neue Feld
  var_colnames <- get_var_colnames(colnames(df))
  non_var_colnames <- get_non_var_colnames(colnames(df))
  #non_var_colnames <- c("Measurement", non_var_colnames)
  max_measurement_num =0

  for (col_num in 1:length(colnames(df))){ #ncol(df)){
    new_colname = get_new_colname(colnames(df)[col_num])
    num = get_measurement_number(colnames(df)[col_num])
    if (num>max_measurement_num){max_measurement_num = num}
    for (i in 1:nrow(df)){
      df_new[[i+((num-1)*nrow(df)), new_colname]] = df[[i,col_num]]
    }
  }
#fuelle noch die nicht variablen Spalten
#cat(file= stderr(), paste0("length(umyteimmarkers)=", umytimemarkers,"\n"))
    for (i in 1:nrow(df)){
      for (k in 1:length(umytimemarkers)){
        df_new[[i+((k-1)*nrow(df)), "Measurement_num"]] = strtoi(umytimemarkers[k])
        df_new[[i+((k-1)*nrow(df)), "Measurement_str"]] = umytimemarkers[k]
        for (col_num in 1:length(non_var_colnames)){
          df_new[[i+((k-1)*nrow(df)), non_var_colnames[col_num]]] = df[[i,non_var_colnames[col_num]]]
        }
      }
    }

  return(df_new)
}


get_var_colnames<-function(mycolnames){
  var_colnames <- c()
  for (i in 1:length(mycolnames)){
    x = strsplit(mycolnames[i],'__')
    if (length(x[[1]])==2){
      var_colnames <- c(var_colnames, mycolnames[i])
    }
  }
  return(var_colnames)
}


get_non_var_colnames<-function(mycolnames){
  non_var_colnames <- c()
  for (i in 1:length(mycolnames)){
    x = strsplit(mycolnames[i],'__')
    if (length(x[[1]])==1){
      non_var_colnames <- c(non_var_colnames, mycolnames[i])
    }
  }
  return(non_var_colnames)
}

get_new_colname<-function(oldcolname){
  x = strsplit(oldcolname,'__')
  if (length(x[[1]])==2){
    num = as.numeric(x[[1]][2])
    new_colname = str_trim(x[[1]][1])
  }else{
    num = 1
    new_colname = str_trim(x[[1]][1])
  }
  return(new_colname)
}

get_measurement_number<-function(oldcolname){
  x = strsplit(oldcolname,'__')
  if (length(x[[1]])==2){
    num = as.numeric(x[[1]][2])
    new_colname = str_trim(x[[1]][1])
  }else{
    num = 1
    new_colname = str_trim(x[[1]][1])
  }
  return(num)
}



