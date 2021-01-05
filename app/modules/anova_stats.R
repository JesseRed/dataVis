library(shiny)


regressionStatsUI <- function(id){
  ns <- NS(id)
  # Thanks to the namespacing, we only need to make sure that the IDs
  # are unique within this function, rather than unique across the entire app.
  tagList(
    uiOutput(ns("uiANOVA")),
    #verbatimTextOutput(ns("textANOVAStats")),


    absolutePanel(
      bottom = 20, right = 20, width = 200,
      draggable = TRUE,
      wellPanel(
        htmlOutput(ns("html_text")),
        sliderInput("n", "", min=3, max=20, value=5),
        plotOutput("plot2", height="200px")

      ),
      style = "opacity: 0.92"
    ),

  )

}

regressionStatsServer <- function(id, input_glob_sig, freq) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
      ns<-session$ns
      #f_utrial_list_all <- reactive({c("all", g_trials())})
      output$uiANOVA <- renderUI({

        fluidPage(
          fluidRow(

            column(5,

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
                                        choices = g_groups(), selected = g_groups()[3])
                     )

                   )

            ),
            column(5,
                   style = "background-color: #fcfcfc;",
                   style = 'border-right: 2px solid gray',
                   h4("trial comparison", align = "center"),
                   fluidRow(
                     column(6,
                            selectInput(ns("trial1"), h5("Select Trial 1", align = "center"),
                                        choices = g_trials_named(), selected = g_trials_named()[1])
                     ),
                     column(6,
                            selectInput(ns("trial2"), h5("Select Trial 2", align = "center"),
                                        choices = g_trials_named(), selected = g_trials_named()[2])
                     )

                   )
            ),
            column(2,
                   style = "background-color: #fcfcfc;",
                   h4("Visualize", align = "center"),
                   selectInput(ns("statsMethod"), h5("method"),
                               choices = c("Regression","ANOVA"), selected = 1)

            )
          ),
        fluidRow(
          column(9,
                 plotOutput(ns("plot"), width = "auto", height = "800px", click = ns("plot_click")),
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
        # fluidRow(
        #   column(9,
        #          plotOutput(ns("hist"), width = "auto", height = "300px", click = ns("plot_click_hist")),
        #   ),
        #   column(3,
        #          verbatimTextOutput(ns("text_bottom")),
        #   )),
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

      # filter data by group
      data_freqmean <- reactive({
        get_data_freqmean(g_data(), freq())
      })


      data_1 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial1), freq())
      })
      data_2 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial2), freq())
      })
      data_g1_t1 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial1), freq())
      })
      data_g1_t2 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial2), freq())
      })
      data_g2_t1 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial1), freq())
      })
      data_g2_t2 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial2), freq())
      })

      level_x <- reactive({round(input$plot_click$x)})
      level_y <- reactive({abs(round(input$plot_click$y)-length(g_regions())-1)})

      ###########################################################
      ### RENDERPLOT
      output$plot<-renderPlot({
        req(input$trial1)
        req(input$trial2)
        req(input$group1)
        req(input$group2)
        my_paired = FALSE
        if (input$trial1 == input$trial2) {
          string1 = paste0(input$group1," vs ", input$group2, " in trial ", g_trials()[input$trial1], "\n",
                           "independent t-test\n")
          my_paired = FALSE

        }
        if (input$group1 == input$group2){
          string1 = paste0(g_trials()[input$trial1]," vs ", g_trials()[input$trial2], "in group ", input$group1, "\n",
                           "paired t-test\n")
          my_paired = TRUE
        }
        #data1 = get_data_group_trial_freqmean(data,input$group1, as.numeric(input$trial1), freq())
        #data2 = get_data_group_trial_freqmean(data,input$group2, as.numeric(input$trial2), freq())
        data1 = data_1()
        data2 = data_2()
        mat_p = matrix(data=NA, nrow=dim(data1)[2], ncol=dim(data1)[3])
        mat_t = matrix(data=NA, nrow=dim(data1)[2], ncol=dim(data1)[3])
        color1 = colorRampPalette(c("blue","red","green"))

        for (i in 1:(dim(data1)[2])){
          for (j in 1:(dim(data1)[3])){
            x = data1[,i,j]
            y = data2[,i,j]
            z = t.test(x,y, paired = my_paired)
            mat_p[i,j] = z$p.value
            mat_t[i,j] = z$statistic
          }
        }

        ###################
        # CORRPLOT

          colnames(mat_p) = g_regions()
          rownames(mat_p) = vector(mode="character", length=length(g_regions()))
          colnames(mat_t) = vector(mode="character", length=length(g_regions()))
          #colnames(mat_t) = g_regions()
          rownames(mat_t) = g_regions()
          # corrplot(mat_p, method="number", type = "upper", is.corr = FALSE,
          #          p.mat = mat_p, sig.level = input_glob_sig(), col = color1(100) )
          if (g_act_method()=="Coherence") {
            corrplot(mat_p, method="number", tl.cex = 0.9, type = "upper", is.corr = FALSE,
                     p.mat = mat_p, sig.level = input_glob_sig(),
                     col=colorRampPalette(c("blue","red","green"))(200))
            corrplot(mat_t, add = TRUE, method="number", tl.cex = 0.9, type = "lower", is.corr = FALSE,
                     p.mat = mat_p, sig.level = input_glob_sig())
          }else if (g_act_method()=="Transferentropy" || g_act_method()=="Granger"){
            corrplot(mat_p, method="number", tl.cex = 0.9, is.corr = FALSE,
                     p.mat = mat_p, sig.level = input_glob_sig(),
                     col=colorRampPalette(c("blue","red","green"))(200))

          }else if (g_act_method()=="Frequency"){
            print("not implemented")
          }

      })

      output$text_bottom <- renderPrint({
        req(input$plot_click)
        req(input$trial1)
        req(input$trial2)
        req(input$group1)
        req(input$group2)
        req(input$statsMethod)
        my_paired = FALSE
        if (input$trial1 == input$trial2) {
          string1 = paste0(input$group1," vs ", input$group2, " in trial ", names(g_trials_named())[input$trial1], "\n", #utrial_list[input$trial1], "\n",
                           "independent t-test\n")
          my_paired = FALSE

        }
        if (input$group1 == input$group2){
          string1 = paste0(g_trials()[input$trial1]," vs ", input$trial2, "in group ", input$group1, "\n",
                           "paired t-test\n")
          my_paired = TRUE
        }

        #lvls <- levels(g_regions())
        data1 = data_1()
        data2 = data_2()
        #level_x = round(input$plot_click$x)
        #level_y = abs(round(input$plot_click$y)-length(g_regions())-1)
        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]


        x = data1[,level_y(),level_x()]
        y = data2[,level_y(),level_x()]
        z = t.test(x,y, paired = my_paired)
        #cat( paste0("Analysis of regions ",region_x," vs. ", region_y ," \n ", string1))

        #print(z)
        if (is.null(input$plot_click$x)) return("null")
        else {
          lvls <- levels(g_regions())
          name <- lvls[round(input$plot_click$x)]
          # HTML("You've selected <code>", name, "</code>",
          #      "<br><br>Here are the first 10 rows that ",
          #      "match that category:")
        }
        t = z$statistic
        df = z$parameter
        r = sqrt((t^2)/((t^2)+df))
        out <- paste0(z$method,"\n\n",
                      region_x, "\n        vs. \n", region_y, " \n\n",
                      "mean= ", round(z$estimate[2],3), " vs. ", round(z$estimate[1],3)," \n",
                      "t=", round(z$statistic,2), " \n",
                      "p=", z$p.value, "  \n",
                      "df=", round(z$parameter,1),"  \n",
                      "CI(",attributes(z$conf.int),")= ",round(z$conf.int[1],3)," ; ",round(z$conf.int[2],3)," \n",
                      "effect size r = ", round(r,4), "\n",
                      "r = [sqrt((t^2)/((t^2)+df))]"
        )
        cat(out)

        #        keeprows <- round(input$plot_click$x) == as.numeric(g_regions())

      })




      output$html_text <- renderUI({
        # x <- paste0("sdf")
        # HTML("You've selected <code>", x, "</code>",
        #      "<br><br>Here are the first 10 rows that ",
        #      "match that category:")
        x = 5
        HTML(markdownToHTML(fragment.only=TRUE, text=c(
          "This is an absolutePanel that uses `bottom` and `right` attributes.
            It also has `draggable = TRUE`",x
        )))

      })
      output$hist <- renderPlot({
        req(input$plot_click$x)
        req(input$plot_click$y)
        region_x = g_regions()[level_x()]
        #cat(file = stderr(), region_x)
        #level_x = round(input$plot_click$x)
        #level_y = abs(round(input$plot_click$y)-length(g_regions())-1)
        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]
        #        level_x = 1
        #        level_y = 2
        #df = g_beh()
        d = data_freqmean()

        if (input$trial1 == input$trial2) {
          cat(file = stderr(), "trial1 == trial2\n")
          string1 = paste0(input$group1," vs ", input$group2, " in trial ", names(g_trials_named())[input$trial1], "\n") #utrial_list[input$trial1], "\n")
          d1 = get_data_group_freqmean(g_data(), input$group1, freq())
          d2 = get_data_group_freqmean(g_data(), input$group2, freq())
          x = d1[,level_x(), level_y(), as.numeric(input$trial1)]
          y = d2[,level_x(), level_y(), as.numeric(input$trial1)]
          df <- data.frame(Gruppe=c(rep(input$group1, times=length(x)),
                                    rep(input$group2, times=length(y))),
                           val=c(x, y))
          df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
          # means for geomline
          df_hline = data.frame(Gruppe = c(input$group1,input$group2), Means=c(mean(x), mean(y)))
          # df$val = d[,level_x, level_y, as.numeric(input$trial1)]
          # df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
          # dummy2 = data.frame(Gruppe = c(0,1), Means=c(0.4, 0.5))

        }
        if (input$group1 == input$group2){
          string1 = paste0(g_trials()[input$trial1]," vs ", g_trials()[input$trial2], "in group ", input$group1, "\n")
          data1 = data_1()
          data2 = data_2()
          x = data1[,level_x(), level_y()]
          y = data2[,level_x(), level_y()]
          df <- data.frame(Gruppe=c(rep(g_trials()[as.numeric(input$trial1)], times=length(x)),
                                    rep(g_trials()[as.numeric(input$trial2)], times=length(y))),
                           val=c(x, y))
          df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
          # means for geomline
          df_hline = data.frame(Gruppe = c(g_trials()[as.numeric(input$trial1)],
                                           g_trials()[as.numeric(input$trial2)]),
                                Means=c(mean(x), mean(y)))

          #p<-ggplot(df, aes(num, val, fill=Gruppe))
          #p + geom_bar(stat="identity") + facet_wrap(~Gruppe)
        }

        ##########later delete
        # temporary
        string1 = paste0(g_trials()[input$trial1]," vs ", g_trials()[input$trial2], "in group ", input$group1, "\n")
        data1 = data_1()
        data2 = data_2()
        x = data1[,level_x(), level_y()]
        y = data2[,level_x(), level_y()]
        df <- data.frame(Gruppe=c(rep(g_trials()[as.numeric(input$trial1)], times=length(x)),
                                  rep(g_trials()[as.numeric(input$trial2)], times=length(y))),
                         val=c(x, y))
        df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
        # means for geomline
        df_hline = data.frame(Gruppe = c(g_trials()[as.numeric(input$trial1)],
                                         g_trials()[as.numeric(input$trial2)]),
                              Means=c(mean(x), mean(y)))


        ###################################

        ggplot(df, aes(num, val, fill=Gruppe)) +
          geom_bar(stat="identity") +
          facet_wrap(~Gruppe) +
          geom_hline(data = df_hline, aes(yintercept = Means))
        #        data1 = data_1()
        #        data2 = data_2()

        #tbl_beh$idu <- as.numeric(row.names(tbl_beh))

        # df <- data.frame(cond=c(rep("x", times=length(x)),
        #                         rep("y", times=length(y))),
        #                  rating=c(x, y))
      })





        # df <- data.frame(Gruppe=c(rep(g_trials()[as.numeric(input$trial1)], times=length(xg1t1)),
        #                           rep(g_trials()[as.numeric(input$trial2)], times=length(xg1t2))),
        #                  val=c(xg1t1, xg1t2))
        # mymeans = c(mean(xg1t1), mean(xg1t2))
        #
        # df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
        # # means for geomline
        # df_hline = data.frame(Gruppe = c(g_trials()[as.numeric(input$trial1)],
        #                                  g_trials()[as.numeric(input$trial2)]),
        #                       Means=c(mean(xg1t1), mean(xg1t2)))



      ################################################################
      # THE histogram plots of individual subjects
      ###########################################

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





      output$facet <- renderPlot({
        #level_x = round(input$plot_click$x)
        #level_y = abs(round(input$plot_click$y)-length(g_regions())-1)
        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]

        df = tbl_beh
        d = data_freqmean()
        df$data1 = d[,level_x(), level_y(), input$trial1]
        df$num <- ave(df$data1, df$Gruppe, FUN = seq_along)

      })



      ###########################################
      # the newly created statistics section
      output$simple_correlation <- renderPrint({
        req(input$plot_click)
        req(input$trial1)
        req(input$trial2)
        req(input$group1)
        req(input$group2)
        req(input$statsMethod)
        my_paired = FALSE
        if (input$trial1 == input$trial2) {
          string1 = paste0(input$group1," vs ", input$group2, " in trial ", names(g_trials_named())[input$trial1], "\n", #utrial_list[input$trial1], "\n",
                           "independent t-test\n")
          my_paired = FALSE

        }
        if (input$group1 == input$group2){
          string1 = paste0(input$trial1," vs ", input$trial2, "in group ", input$group1, "\n",
                           "paired t-test\n")
          my_paired = TRUE
        }

        #lvls <- levels(g_regions())
        data1 = data_1()
        data2 = data_2()
        #level_x = round(input$plot_click$x)
        #level_y = abs(round(input$plot_click$y)-length(g_regions())-1)
        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]


        x = data1[,level_y(),level_x()]
        y = data2[,level_y(),level_x()]
        z = t.test(x,y, paired = my_paired)
        cat("\ninput_regressors = ")
        cat(input$regressors)
        cat("\n")
        cat(file = stderr(), input$regressors[1])
        cat(file = stderr(), "\n")
        cat(file = stderr(), get_beh_tbl_data_by_group(input$group2, input$regressors[1]))
        cat(file = stderr(), "\n")
        cat(file = stderr(), get_beh_tbl_data_by_group(input$group1, input$regressors[1]))
        cat(file = stderr(), "\n")
        cat(file = stderr(), "length(input$regressors=")
        cat(file = stderr(), length(input$regressors))
        b1xxx <- get_beh_tbl_data_by_group(input$group1, input$regessors[1])
        mycor = cor(b1xxx, x)
        cat(file = stderr(), "\nb1 = ")
        cat(file = stderr(), b1xxx)
        cat(file = stderr(), "\n")

        c1 = 0
        c2 = 0
        name = "Template"
        for ( i in 1:length(input$regressors)){
          b1 = get_beh_tbl_data_by_group(input$group1, input$regressors[i])
          b2 = get_beh_tbl_data_by_group(input$group2, input$regressors[i])

          c1 = c(c1, cor(x,b1))
          c2 = c(c2,cor(y,b2))
          name = c(name, input$regressors[i])
        }
        c1 = c1[-c(1)]
        c2 = c2[-c(1)]
        name = name[-c(1)]

        cat("\n\nc1 =")
        cat(c1)
        cat("\n\nc2=")
        cat(c2)
        cat("\n\n")
        df <- data.frame("regressor"= name, "cor2G1"= c1, "cor2G2" = c2)
        print(file = stderr(), df)

        cat("input_regressors = ")
        cat(input$regessors)
        out <- paste0(z$method,"\n\n",
                      region_x, "\n        vs. \n", region_y, " \n\n",
                      "mean= ", round(z$estimate[2],3), " vs. ", round(z$estimate[1],3)," \n",
                      "t=", round(z$statistic,2), " \n",
                      "p=", z$p.value, "  \n",
                      "df=", round(z$parameter,1),"  \n",
                      "CI(",attributes(z$conf.int),")= ",round(z$conf.int[1],3)," ; ",round(z$conf.int[2],3)," \n"
        )
        cat(out)
        out <- paste0(z$method,"\n\n",
                      region_x, "\n        vs. \n", region_y, " \n\n",
                      "mean= ", round(z$estimate[2],3), " vs. ", round(z$estimate[1],3)," \n",
                      "t=", round(z$statistic,2), " \n",
                      "p=", z$p.value, "  \n",
                      "df=", round(z$parameter,1),"  \n",
                      "CI(",attributes(z$conf.int),")= ",round(z$conf.int[1],3)," ; ",round(z$conf.int[2],3)," \n"
        )


        #        keeprows <- round(input$plot_click$x) == as.numeric(g_regions())

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
      ###########################################
      # the newly created statistics section
      output$tab_simple_group_correlation <- renderTable({
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
        req(input$plot_click)

        cat(create_partial_correlation_string(group=2,trial=2))

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
        # vergleiche 2 Gruppen mit einem Trial
        if (compare == "groups"){
          mystring = paste0(mystring, input$group1, " vs. ", input$group2)

          if (input$group1 == input$group2){
            cat("no output in case of same groups")
            return()
          }

          if (trial==1){
            mystring = paste0(mystring, " of trial ", g_trials()[input$trial1], "\n")
            z = t.test(xg1t1,xg2t1, paired = FALSE)
          }
          if (trial == 2){
            mystring = paste0(mystring, " of trial ", g_trials()[input$trial2], "\n")
            z = t.test(xg1t2,xg2t2, paired = FALSE)
          }
        }
        # if comparing 2 trails of the same group
        if (compare == "trials"){
          mystring = paste0(mystring, g_trials()[input$trial1], " vs. ", g_trials()[input$trial2])
          if (group==1){
            z = t.test(xg1t1,xg1t2, paired = TRUE)
            mystring = paste0(mystring, " of group ", input$group1, "\n")
          }
          if (group == 2){
            z = t.test(xg2t1,xg2t2, paired = TRUE)
            mystring = paste0(mystring, " of group ", input$group2, "\n")
          }
        }
        z$mydescription <- paste0(mystring, create_ttest_string(z))

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

# create_empty_df_for_correlation<- function(num_groups = 2){
#
#   infs = c("group", "r", "p","t","df","CI_low","CI_high")
#   x<- c("reg_name", "method", infs)
#   if (num_groups>1){
#     x <- c(x, infs)
#   }
#   df <- data.frame(colnames = x)
#   return(df)
# }

append_correlation_row <- function(x1 = NULL, b1 = NULL, x2 = NULL, b2 = NULL,
                                   t = "not known",
                                   method = "pearson", reg_name = "no_reg_name",
                                   g1 = "not known", g2 = "not known",
                                   df = NULL) {
  m1 = cor.test(x1,b1, method = method)
  m2 = cor.test(x2,b2, method = method)
  cat(file = stderr(), m1$estimate)
  cat(file = stderr(), m2$estimate)
  cat(file = stderr(), length(x1))
  cat(file = stderr(), length(x2))
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



  # m1 = cor.test(x1,b1, method = method)
  # m2 = cor.test(x2,b2, method = method)
  # df[nrow(df) + 1,] = c(reg_name, method,
  #                       1, m1$estimate, m1$p.value,
  #                       m1$statistic, m1$parameter,
  #                       m1$conf.int[1], m1$conf.int[2],
  #                       2, m2$estimate, m2$p.value,
  #                       m2$statistic, m2$parameter,
  #                       m2$conf.int[1], m2$conf.int[2]
  #                       )

}


create_ttest_string <- function(z){
  t = z$statistic
  df = z$parameter
  r = sqrt((t^2)/((t^2)+df))
  out <- paste0(z$method,"\n\n",
                "mean= ", round(z$estimate[2],3), " vs. ", round(z$estimate[1],3)," \n",
                "t=", round(z$statistic,2), " \n",
                "p=", z$p.value, "  \n",
                "df=", round(z$parameter,1),"  \n",
                "CI(",attributes(z$conf.int),")= ",round(z$conf.int[1],3)," ; ",round(z$conf.int[2],3)," \n",
                "effect size r = ", round(r,4), "\n",
                "r = [sqrt((t^2)/((t^2)+df))]"
  )

  return(out)
}








