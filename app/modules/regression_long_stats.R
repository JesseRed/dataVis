library(shiny)


regressionLongStatsUI <- function(id){
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

regressionLongStatsServer <- function(id, input_glob_sig, freq) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
      ns<-session$ns
      #f_utrial_list_all <- reactive({c("all", g_trials())})
      output$uiANOVA <- renderUI({

        fluidPage(


          fluidRow(
            column(4,
                   fluidRow(
                     column(6,
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
                     column(6,

                            style = "background-color: #fcfcfc;",
                            #style = 'border-bottom: 2px solid gray',
                            style = "border-right: 2px solid black",
                            h4("group comparison", align = "center"),
                            fluidRow(
                              column(6,
                                     selectInput(ns("group1"), h5("Select Group 1", align = "center"),
                                                 choices = g_groups(), selected = g_groups()[2])
                              ),
                              column(6,
                                     selectInput(ns("group2"), h5("Select Group 2", align = "center"),
                                                 choices = g_groups(), selected = g_groups()[2])
                              )
                            )
                     ),
                   ),
                   # fluidRow(
                   #   style = "background-color: #fcfcfc;",
                   #   #style = "border-top: 2px solid black",
                   #   h4("is the analysis directed?", align = "left"),
                   #   column(12,
                   #          prettyRadioButtons(
                   #            inputId = ns("causal"),
                   #            label = "",
                   #            choices = c("non-directed", "directed"),
                   #            shape = "round",
                   #            status = "danger",
                   #            fill = TRUE,
                   #            inline = TRUE
                   #          ),
                   #          ),
                   # ),
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
                   selectInput(ns("statsMethod"), h5("method"),
                               choices = c("Regression","ANOVA"), selected = 1)

            ),
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




          #######################

          # fluidRow(
          #
          #   column(5,
          #
          #          style = "background-color: #fcfcfc;",
          #          #style = 'border-bottom: 2px solid gray',
          #          style = "border-right: 2px solid black",
          #          h4("group comparison", align = "center"),
          #          fluidRow(
          #            column(6,
          #                   selectInput(ns("group1"), h5("Select Group 1 Long", align = "center"),
          #                               choices = g_groups(), selected = g_groups()[2])
          #            ),
          #            column(6,
          #                   selectInput(ns("group2"), h5("Select Group 2", align = "center"),
          #                               choices = g_groups(), selected = g_groups()[3])
          #            )
          #
          #          )
          #
          #   ),
          #   column(5,
          #          style = "background-color: #fcfcfc;",
          #          style = 'border-right: 2px solid gray',
          #          h4("trial comparison", align = "center"),
          #          fluidRow(
          #            column(6,
          #                   selectInput(ns("trial1"), h5("Select Trial 1", align = "center"),
          #                               choices = g_trials_named(), selected = g_trials_named()[1])
          #            ),
          #            column(6,
          #                   selectInput(ns("trial2"), h5("Select Trial 2", align = "center"),
          #                               choices = g_trials_named(), selected = g_trials_named()[2])
          #            )
          #
          #          )
          #   ),
          #
          # ),
        fluidRow(
          style = 'border-top: 2px solid gray',
          column(9,
                 plotOutput(ns("plot"), width = "auto", height = "700px", click = ns("plot_click")),
          ),
          column(3,
                 selectInput(ns("mainregressor"), h4("main regressor"),
                             choices = colnames(g_beh())),
                 selectInput(ns("regressors"), h4("potential regressors"),
                             multiple = TRUE, selectize = FALSE,
                             size = 35,
                             choices = colnames(g_beh()),
                             selected = 3)
                 )
        ),


        fluidRow(align = "center", h4("comparison of Time 1 vs. Time 2"),
                 column(9,
                        plotOutput(ns("hist_compare_diffTime"), width = "auto", height = "300px", click = ns("plot_click_hist")),
                 ),
                 column(3, align = "left",
                        verbatimTextOutput(ns("text_stats_compare_diffTime")),
                 )),

        fluidRow(align = "center", h4("comparison of Trial1 vs. Trial 2 of selected group 1"),
                 column(9,
                        plotOutput(ns("hist_compare_diffTrial_sameGroup1"), width = "auto", height = "300px", click = ns("plot_click_hist")),
                 ),
                 column(3, align = "left",
                        verbatimTextOutput(ns("text_stats_compare_diffTrial_sameGroup1")),
                 )),

        fluidRow(align = "center", h4("comparison of Trial1 vs. Trial 2 of selected group 2"),
          column(9,
                 plotOutput(ns("hist_compare_diffTrial_sameGroup2"), width = "auto", height = "300px", click = ns("plot_click_hist")),
          ),
          column(3, align = "left",
                 verbatimTextOutput(ns("text_stats_compare_diffTrial_sameGroup2")),
          )),
        fluidRow(align = "center", h4("comparison of Group 1 vs. Group 2 of selected trial 1"),
          column(9,
                 plotOutput(ns("hist_compare_diffGroup_sameTrial1"), width = "auto", height = "300px", click = ns("plot_click_hist")),
          ),
          column(3, align = "left",
                 verbatimTextOutput(ns("text_stats_compare_diffGroup_sameTrial1")),
          )),
        fluidRow(align = "center", h4("comparison of Group 1 vs. Group 2 of selected trial 2"),
          column(9,
                 plotOutput(ns("hist_compare_diffGroup_sameTrial2"), width = "auto", height = "300px", click = ns("plot_click_hist")),
          ),
          column(3, align = "left",
                 verbatimTextOutput(ns("text_stats_compare_diffGroup_sameTrial2")),
          )),

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
                 box(title = "simple correlation ..........expand for help", width = 12, collapsible = TRUE, collapsed = TRUE, verbatimTextOutput(ns("help_simple_correlation"))),
          )
        ),
        fluidRow(
          column(12,
                 tableOutput(ns("tab_simple_time_correlation")),
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
        fluidRow(
          column(12,
                 box(title = "Included subjects", width = 12, collapsible = TRUE, collapsed = TRUE,
                     #uiOutput(ns("includedSubjects"))
                     actionButton(ns("testexclude"), "update"),

                     checkboxGroupInput(ns("Subjects"), label = h3("Subjects"), inline = T,
                                        choices = g_D()$df_BD$ID,
                                        selected =  g_D()$df_BD$ID),

                     style = "background-color: #fcfcfc;",
                     style = 'border-bottom: 2px solid gray',
                     checkboxGroupInput(ns("Group1"), label = h3("Group 1"), inline = T,
                                        choices = c()), #, #curdata()$df_data1$ID,
                     #                     selected = c()), #curdata()$df_data1$ID[my_included_subjects_g1()]),

                     style = "background-color: #fcfcfc;",
                     style = 'border-bottom: 2px solid gray',
                     checkboxGroupInput(ns("Group2"), label = h3("Group 2"), inline = T,
                                        choices = c()) #, #curdata()$df_data2$ID,
                 ),
          )
        ),

        # fluidRow(
        #   column(12,
        #          box(title = "partial correlation ", width = 12, collapsible = TRUE, collapsed = FALSE, verbatimTextOutput(ns("partial_correlation"))),          )
        # )


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

        numbered_IDs_all <- get_included_subjects_with_numbers(g_D()$df_BD$ID, my_included_subjects())
        numbered_IDs_g1 <- get_included_subjects_with_numbers(curdata()$df_data1$ID, my_included_subjects_g1())
        numbered_IDs_g2 <- get_included_subjects_with_numbers(curdata()$df_data2$ID, my_included_subjects_g2())

        # updateCheckboxGroupInput(session, "Subjects",
        #                          choices = numbered_IDs_all, inline = T,
        #                          selected =  numbered_IDs_all[my_included_subjects()])


        updateCheckboxGroupInput(session, "Group1",
                                 choices = numbered_IDs_g1, inline = T,
                                 selected =  numbered_IDs_g1[my_included_subjects_g1()]

        )

        updateCheckboxGroupInput(session, "Group2",
                                 choices = numbered_IDs_g2, inline = T,
                                 selected =  numbered_IDs_g2[my_included_subjects_g2()]
        )
        updateCheckboxGroupInput(session, "Subjects",
                                 choices = g_D()$df_BD$ID, inline = T,
                                 selected =  g_D()$df_BD$ID[my_included_subjects()])


        # updateCheckboxGroupInput(session, "Group1",
        #                          choices = curdata()$df_data1$ID, inline = T,
        #                          selected =  curdata()$df_data1$ID[my_included_subjects_g1()]
        #
        # )
        #
        # updateCheckboxGroupInput(session, "Group2",
        #                          choices = curdata()$df_data2$ID, inline = T,
        #                          selected =  curdata()$df_data2$ID[my_included_subjects_g2()]
        # )
      })

      # Funktion um an die ausgewaehlten Subjects Numbern zu schreiben damit
      # die Auswahl in der GUI einfacher wird
      get_included_subjects_with_numbers <- function(IDs, is_included){
        # nummern duerfen nur die Subjects erhalten die selectiert sind
        idx = 1
        for (i in 1:length(IDs)){
          if (is_included[i]){
            IDs[i] <- paste0(idx,". ",IDs[i])
            idx <- idx +1
          }
        }
        return(IDs)
      }



      # filter data by group
      data_freqmean <- reactive({
        get_data_freqmean(g_data(), freq())
      })


      # data_1 <- reactive({
      #   get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial1), freq())
      # })
      # data_2 <- reactive({
      #   get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial2), freq())
      # })
      # data_g1_t1 <- reactive({
      #   get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial1), freq())
      # })
      # data_g1_t2 <- reactive({
      #   get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial2), freq())
      # })
      # data_g2_t1 <- reactive({
      #   get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial1), freq())
      # })
      # data_g2_t2 <- reactive({
      #   get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial2), freq())
      # })
      #

      data_1 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial1), g_sel_freqs())
      })
      data_2 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial2), g_sel_freqs())
      })
      data_g1_t1 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial1), g_sel_freqs())
      })
      data_g1_t2 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial2), g_sel_freqs())
      })
      data_g2_t1 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial1), g_sel_freqs())
      })
      data_g2_t2 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial2), g_sel_freqs())
      })

      level_x <- reactive({round(input$plot_click$x)})
      level_y <- reactive({abs(round(input$plot_click$y)-length(g_regions())-1)})

      # curdata <- reactive({
      #   get_currently_selected_data_long3(g_D(), input$group1, input$group2, as.numeric(input$trial1), as.numeric(input$trial2), g_sel_freqs())
      # #  get_currently_selected_data(g_data(), input$group1, input$group2, as.numeric(input$trial1), as.numeric(input$trial2), freq())
      # })



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
                                               subjects_to_exclude = subjects_to_exclude()#,
                                               #iscausal = iscausal(),
#                                               network = network_new()


        )
        gM <<- M

        return(M)
      })

      ###########################################################
      ### RENDERPLOT
      output$plot<-renderPlot({

          req(input$trial1)
          req(input$trial2)
          req(input$group1)
          req(input$group2)
          d <- curdata()
          mat_t <<- d$mat_t
          mat_p <<- d$mat_p
          ###################
          # CORRPLOT
          generate_plot_Corrplot(d$mat_p, d$mat_t)

      })

#
#       output$hist <- renderPlot({
#         req(input$plot_click$x)
#         req(input$plot_click$y)
#         region_x = g_regions()[level_x()]
#         #cat(file = stderr(), region_x)
#         #level_x = round(input$plot_click$x)
#         #level_y = abs(round(input$plot_click$y)-length(g_regions())-1)
#         region_x = g_regions()[level_x()]
#         region_y = g_regions()[level_y()]
#         #        level_x = 1
#         #        level_y = 2
#         #df = g_beh()
#         d = data_freqmean()
#
#         if (input$trial1 == input$trial2) {
#           cat(file = stderr(), "trial1 == trial2\n")
#           string1 = paste0(input$group1," vs ", input$group2, " in trial ", names(g_trials_named())[input$trial1], "\n") #utrial_list[input$trial1], "\n")
#           d1 = get_data_group_freqmean(g_data(), input$group1, freq())
#           d2 = get_data_group_freqmean(g_data(), input$group2, freq())
#           x = d1[,level_x(), level_y(), as.numeric(input$trial1)]
#           y = d2[,level_x(), level_y(), as.numeric(input$trial1)]
#           df <- data.frame(Gruppe=c(rep(input$group1, times=length(x)),
#                                     rep(input$group2, times=length(y))),
#                            val=c(x, y))
#           df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
#           # means for geomline
#           df_hline = data.frame(Gruppe = c(input$group1,input$group2), Means=c(mean(x), mean(y)))
#           # df$val = d[,level_x, level_y, as.numeric(input$trial1)]
#           # df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
#           # dummy2 = data.frame(Gruppe = c(0,1), Means=c(0.4, 0.5))
#
#         }
#         if (input$group1 == input$group2){
#           string1 = paste0(g_trials()[input$trial1]," vs ", g_trials()[input$trial2], "in group ", input$group1, "\n")
#           data1 = data_1()
#           data2 = data_2()
#           x = data1[,level_x(), level_y()]
#           y = data2[,level_x(), level_y()]
#           df <- data.frame(Gruppe=c(rep(g_trials()[as.numeric(input$trial1)], times=length(x)),
#                                     rep(g_trials()[as.numeric(input$trial2)], times=length(y))),
#                            val=c(x, y))
#           df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
#           # means for geomline
#           df_hline = data.frame(Gruppe = c(g_trials()[as.numeric(input$trial1)],
#                                            g_trials()[as.numeric(input$trial2)]),
#                                 Means=c(mean(x), mean(y)))
#
#           #p<-ggplot(df, aes(num, val, fill=Gruppe))
#           #p + geom_bar(stat="identity") + facet_wrap(~Gruppe)
#         }
#
#         ##########later delete
#         # temporary
#         string1 = paste0(g_trials()[input$trial1]," vs ", g_trials()[input$trial2], "in group ", input$group1, "\n")
#         data1 = data_1()
#         data2 = data_2()
#         x = data1[,level_x(), level_y()]
#         y = data2[,level_x(), level_y()]
#         df <- data.frame(Gruppe=c(rep(g_trials()[as.numeric(input$trial1)], times=length(x)),
#                                   rep(g_trials()[as.numeric(input$trial2)], times=length(y))),
#                          val=c(x, y))
#         df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
#         # means for geomline
#         df_hline = data.frame(Gruppe = c(g_trials()[as.numeric(input$trial1)],
#                                          g_trials()[as.numeric(input$trial2)]),
#                               Means=c(mean(x), mean(y)))
#
#
#         ###################################
#
#         ggplot(df, aes(num, val, fill=Gruppe)) +
#           geom_bar(stat="identity") +
#           facet_wrap(~Gruppe) +
#           geom_hline(data = df_hline, aes(yintercept = Means))
#
#       })



      ################################################################
      # THE histogram plots of individual subjects
      ###########################################

      output$hist_compare_diffTime <- renderPlot({
        req(input$plot_click$x)
        create_df_for_histplot2(compare = "time", group=1, trial = 1)
      })

      output$hist_compare_diffTrial_sameGroup1 <- renderPlot({
        req(input$plot_click$x)
        create_df_for_histplot(compare = "trials", group=1, trial = 1)
      })


      output$hist_compare_diffTrial_sameGroup2 <- renderPlot({
        req(input$plot_click$x)
        create_df_for_histplot(compare = "trials", group=2, trial = 1)
      })


      output$hist_compare_diffGroup_sameTrial1 <- renderPlot({
        req(input$plot_click$x)
        create_df_for_histplot(compare = "groups", group=1, trial = 1)
      })


      output$hist_compare_diffGroup_sameTrial2 <- renderPlot({
        req(input$plot_click$x)
        create_df_for_histplot(compare = "groups", group=1, trial = 2)
      })




      ################################################################
      # THE text about t-statistics
      ###########################################
      output$text_stats_compare_diffTime <- renderPrint({
        req(input$plot_click)
        z = ttest_estimation2(compare = "time")
        cat(z$mydescription)
      })

      output$text_stats_compare_diffGroup_sameTrial1 <- renderPrint({
        req(input$plot_click)
        z = ttest_estimation(compare = "groups", group = 1, trial = 1)
        cat(z$mydescription)
      })

      output$text_stats_compare_diffGroup_sameTrial2 <- renderPrint({
        req(input$plot_click)
        z = ttest_estimation(compare = "groups", group = 1, trial = 2)
        cat(z$mydescription)
      })

      output$text_stats_compare_diffTrial_sameGroup1 <- renderPrint({
        req(input$plot_click)
        z = ttest_estimation(compare = "trials", group = 1, trial = 1)
        cat(z$mydescription)
      })

      output$text_stats_compare_diffTrial_sameGroup2 <- renderPrint({
        req(input$plot_click)
        z = ttest_estimation(compare = "trials", group = 2, trial = 1)
        cat(z$mydescription)
      })
      ####
      #################################################################





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


      output$help_simple_correlation <- renderPrint({
        text = "in der obersten Tabelle stehen Werte unter einbeziehung der excludeten subjects\n"
        text = paste0(text, " in den unteren beiden Tabellen dann die Correlationen zu allen Subjects\n")
        text = paste0(text, " oberste Zeile zeigt die Zeitabhaengigkeit\n")
        text = paste0(text, " oberste Zeile zeigt die Zeitabhaengigkeit\n")
        cat(paste0("output$tab_simpple_time_correlation"))
        cat(paste0("mainregressor = ", input$mainregressor,"\n"))
      })

      ###########################################
      # the newly created statistics section
      output$tab_simple_time_correlation <- renderTable({
        req(input$plot_click)
        cat(file = stderr(), paste0("output$tab_simpple_time_correlation"))
        cat(file = stderr(), paste0("mainregressor = ", input$mainregressor,"\n"))
        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]

        x_con = curdata()$data1[,level_y(),level_x()]
        x_beh = curdata()$df_data1
        y_con = curdata()$data2[,level_y(),level_x()]
        y_beh = curdata()$df_data2


        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]


        # berechne Werte fuer den main regessor
        #reg_name = get_beh_tbl_data_by_group(input$group1, input$mainregressor)
        b1 = get( input$mainregressor, x_beh)
        b2 = get( input$mainregressor, y_beh)
        g_b1tmp <<- b1
        g_b2tmp <<- b2
        g_x_con <<- x_con
        g_y_con <<- y_con


        #        b1 = get_beh_tbl_data_by_group(input$group1, input$mainregressor, tbl_beh = x_beh)
#        b2 = get_beh_tbl_data_by_group(input$group2, input$mainregressor, tbl_beh = y_beh)
#        cat(file = stderr(), paste0("b1 = ", b1,"\n"))
#        cat(file = stderr(), paste0("b2 = ", b2,"\n"))
        cat(file = stderr(), paste0("1-\n\n"))
        df <- append_correlation_row(x1 = x_con-y_con, b1 = b1, x2 = y_con-x_con, b2 = b2,
                                     method = "pearson",
                                     t = g_trials()[as.numeric(input$trial1)],
                                     g1 = input$group1,
                                     g2 = input$group2,
                                     reg_name = input$mainregressor)
        cat(file = stderr(), paste0("2-\n\n"))
        df <- append_correlation_row(x1 = x_con, b1 = b1, x2 = y_con, b2 = b2,
                                     method = "pearson",
                                     t = g_trials()[as.numeric(input$trial1)],
                                     g1 = input$group1,
                                     g2 = input$group2,
                                     reg_name = input$mainregressor,
                                     df = df)
        cat(file = stderr(), paste0("3-\n\n"))

        df <- append_correlation_row(x1 = x_con, b1 = b1, x2 = y_con, b2 = b2,
                                     method = "pearson",
                                     t = g_trials()[as.numeric(input$trial1)],
                                     g1 = input$group1,
                                     g2 = input$group2,
                                     reg_name = input$mainregressor,
                                     df = df)

        #cat(file = stderr(), "now for loop")
        for ( i in 1:length(input$regressors)){
          #b1 = get_beh_tbl_data_by_group(input$group1, input$regressors[i], tbl_beh = x_beh)
          #b2 = get_beh_tbl_data_by_group(input$group2, input$regressors[i], tbl_beh = y_beh)
          b1 = get( input$regressors[i], x_beh)
          b2 = get( input$regressors[i], y_beh)
          df <- append_correlation_row(x1 = x_con, b1 = b1, x2 = y_con, b2 = b2,
                                       method = "pearson",
                                       t = g_trials()[as.numeric(input$trial1)],
                                       g1 = input$group1,
                                       g2 = input$group2,
                                       reg_name = input$regressors[i], df=df)



          df <- append_correlation_row(x1 = x_con, b1 = b1, x2 = y_con, b2 = b2,
                                       method = "pearson",
                                       t = g_trials()[as.numeric(input$trial1)],
                                       g1 = input$group1,
                                       g2 = input$group2,
                                       reg_name = input$regressors[i], df=df)
        }
        return(df)

      })

      ###########################################
      # the newly created statistics section
      output$tab_simple_group_correlation <- renderTable({
        req(input$plot_click)
        cat(file = stderr(), paste0("output$tab_simpple_group_correlation"))
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
                                     reg_name = input$mainregressor,
                                     df = df)

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
        req(input$plot_click)

        cat(create_partial_correlation_string(group=2,trial=2))

      })

      ####
      #################################################################

      ## general function specific for this tab
      #################################################################

      create_partial_correlation_string <- function( group = 1, trial = 1){
        cat(file = stderr(), paste0("levelx = ", level_x(), "levely = ", level_y(), "\n"))
        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]
        cat(file = stderr(), paste0("region_x = ", region_x, "  region_y = ", region_y, "\n"))


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
            x_in_g2t1<<-data_g2_t1()
            x_in = data_g2_t1()[,level_y(),level_x()]
          }
          if (trial == 2){
            tin = g_trials()[input$trial2]
            x_in_g2t1<<-data_g2_t2()
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



    ttest_estimation2 <- function(compare = "groups"){
      x_con = curdata()$data1[,level_y(),level_x()]
      #x_beh = curdata()$df_data1[,level_y(),level_x()]
      y_con = curdata()$data2[,level_y(),level_x()]
      #y_beh = curdata()$df_data2[,level_y(),level_x()]

        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]

        mystring = ""
        ispaired = FALSE
        m1 = 0
        m2 = 0

        # vergleiche 2 Gruppen mit einem Trial
          mystring = paste0(mystring, "time ", input$ld_1, " vs. ", input$ld_2)
          ispaired = TRUE
          if (input$ld_1 == input$ld_2){
            cat("no output in case of same time")
            return()
          }

            z = t.test(x_con,y_con, paired = ispaired)
            m1 = mean(x_con)
            m2 = mean(y_con)

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

      create_df_for_histplot2 <- function(compare = "groups",
                                         group = 1, trial = 1){
        x = curdata()$data1[,level_y(),level_x()]
        y = curdata()$data2[,level_y(),level_x()]

        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]
        #string1 = paste0(input$trial1," vs ", input$trial2, "in group ", input$group1, "\n")
            cat(file = stderr(), paste0("\n create_df_for_histplot2 with time \n"))
            cat(file = stderr(), paste0("curdata%data1 with dim(x)=", dim(x),"\n"))
            cat(file = stderr(), paste0("curdata%data2 with dim(y)=", dim(y),"\n"))



          df <- data.frame(Gruppe=c(rep(input$group1, times=length(x)),
                                    rep(input$group2, times=length(y))),
                           val=c(x, y))
          df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
          # means for geomline
          df_hline = data.frame(Gruppe = c(input$group1,input$group2), Means=c(mean(x), mean(y)))



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
                                   df = NULL, description = "no desc.") {
  m1 = cor.test(x1,b1, method = method)
  m2 = cor.test(x2,b2, method = method)
  #cat(file = stderr(), m1$estimate)
  #cat(file = stderr(), m2$estimate)

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
                    descri = description,
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









