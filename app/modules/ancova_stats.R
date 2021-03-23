library(shiny)


ancovaStatsUI <- function(id){
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

ancovaStatsServer <- function(id, input_glob_sig, freq) {
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
                            # selectInput(ns("trial2"), h5("Select Trial 2", align = "center"),
                            #             choices = g_trials_named(), selected = g_trials_named()[2])
                     )

                   )
            ),
            column(2,
                   style = "background-color: #fcfcfc;",
                   h4("Visualize", align = "center"),
                   selectInput(ns("statsMethod"), h5("method"),
                               choices = c("ANCOVA","ANOVA"), selected = 1)

            )
          ),
        fluidRow(
          column(9,
                 plotOutput(ns("plot"), width = "auto", height = "800px", click = ns("plot_click")),
          ),
          column(3,
                 selectInput(ns("regressors"), h4("potential regressors"),
                             multiple = TRUE, selectize = FALSE,
                             size = 30,
                             choices = colnames(g_beh()),
                             selected = 3)
                 )
        ),
        fluidRow(
          column(12,
                 box(title = "ANCOVA ..........expand for help", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp_ancova"))),
          )
        ),
        #   )),
        fluidRow(align = "center", h4(" original group means for selected trial"),
                 column(12,
                        plotOutput(ns("boxplot_ancova"), width = "auto", height = "300px", click = ns("plot_click_hist")),
#                        verbatimTextOutput(ns("text_stats_ancova")),
                 )
                 # column(3, align = "left",
                 #        #verbatimTextOutput(ns("text_stats_ancova")),
                 # )
        ),

        fluidRow(align = "center", h4("adjusted group means by using ANCOVA for selected trial"),
                 column(9,
                        plotOutput(ns("boxplot_ancova_adjusted"), width = "auto", height = "300px", click = ns("plot_click_hist")),
                        verbatimTextOutput(ns("text_stats_ancova")),
                 ),
                 column(3, align = "left",
                        verbatimTextOutput(ns("text_stats_ancova_adjusted_means")),
                 )
                ),
        fluidRow(align = "center", h4("check that the covariate and any independent variables are independent"),
                column(12,
                       verbatimTextOutput(ns("text_stats_ancova_check_independence")),
                                 #                        verbatimTextOutput(ns("text_stats_ancova")),
                )
                          # column(3, align = "left",
                          #        #verbatimTextOutput(ns("text_stats_ancova")),
                          # )
        )




    )
  })


      curdata <- reactive({
<<<<<<< HEAD:app/modules/ancova_stats.R
        get_currently_selected_data_long3(g_D(), input$group1, input$group2, as.numeric(input$trial1), as.numeric(input$trial1), g_sel_freqs())
        # get_currently_selected_data(g_data(), input$group1, input$group2, as.numeric(input$trial1), as.numeric(input$trial1), g_sel_freqs())
=======
        get_currently_selected_data(g_data(), input$group1, input$group2, as.numeric(input$trial1), as.numeric(input$trial1), g_sel_freqs())
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc:app/modules/ancova_stats.R
      })

      ############################################
      ##########################################
      ##### Reactive Functions
      # filter data by group
      data_freqmean <- reactive({
        get_data_freqmean(g_data(), freq())
      })

      data_g1_t1 <- reactive({get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial1), freq())      })
      #data_g1_t2 <- reactive({get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial2), freq())      })
      data_g2_t1 <- reactive({get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial1), freq())      })
      #data_g2_t2 <- reactive({get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial2), freq())      })
      level_x <- reactive({round(input$plot_click$x)})
      level_y <- reactive({abs(round(input$plot_click$y)-length(g_regions())-1)})


      reactive_adjustedMeans <- reactive({
        adjustedMeans<-effect("Gruppe",reactive_ancova_model(), se=TRUE)

        return(adjustedMeans)
      })

      reactive_ancova_model <- reactive({ancova_estimation()})
      #
      #   xg1t1 <-- data_g1_t1()[,level_y(),level_x()]
      #   #xg1t2 = data_g1_t2()[,level_y(),level_x()]
      #   xg2t1 <-- data_g2_t1()[,level_y(),level_x()]
      #   #xg2t2 = data_g2_t2()[,level_y(),level_x()]
      #   mystring = ""
      #   # vergleiche 2 Gruppen mit einem Trial
      #
      #
      #
      #   if (input$group1 == input$group2){
      #     cat("no output in case of same groups")
      #     return()
      #   }
      #
      #   xg1 = xg1t1
      #   xg2 = xg2t1
      #   # cat(file = stderr(), "length(xg1)=")
      #   # cat(file = stderr(), length(xg1))
      #   # cat(file = stderr(), "\n")
      #   # cat(file = stderr(), "length(xg2)=")
      #   # cat(file = stderr(), length(xg2))
      #   # cat(file = stderr(), "\n")
      #
      #
      #   # mydf <- data.frame(Gruppe=c(rep(input$group1, times=length(xg1)),
      #   #                             rep(input$group2, times=length(xg2))),
      #   #                    val=c(xg1, xg2))
      #   # mydf <- data.frame(Gruppe=c(rep(input$group1, times=length(xg1)),
      #   #                             rep(input$group2, times=length(xg2))))
      #   groupfac=c(rep(input$group1, times=length(xg1)),
      #              rep(input$group2, times=length(xg2)))
      #   groupfac<-factor(groupfac,levels = c(input$group1, input$group2), labels= c(input$group1, input$group2))
      #
      #   #        Y =c(xg1, xg2)
      #   #        df <- dataframe(val=c(xg1,xg2))
      #   #        df <- dataframe(Gruppe = groupfac)
      #   df <- data.frame(val=c(xg1,xg2))
      #
      #
      #   n = c("val")
      #   # cat(file = stderr(), "before the for loop\n")
      #   for ( i in 1:length(input$regressors)){
      #     bg1 = get_beh_tbl_data_by_group(input$group1, input$regressors[i])
      #     bg2 = get_beh_tbl_data_by_group(input$group2, input$regressors[i])
      #     df<-cbind(df, c(bg1, bg2))
      #     n <- c(n,input$regressors[i])
      #   }
      #
      #   # zum Schluss noch die Gruppe dazu
      #   df<-cbind(df, groupfac)
      #
      #   n<-c(n, "Gruppe")
      #
      #   names(df)<-n
      #
      #   #        myModel <-aov(val~ Alter+BDI.II, data = mydf)
      #   # alle meine Regessoren in mydf gesteckt
      #   # print(file = stderr(), ncol(df))
      #   #print(file = stderr(), df)
      #   #message(df)
      #
      #   contrasts(df$Gruppe)<-contr.helmert(2)
      #
      #   myModel <-aov(val ~ . , data = df)
      #   #resmydf <-Anova(myModel, type = "III")
      #   return(myModel)
      #
      # })




      ###########################################################
      ### RENDERPLOT
      output$plot<-renderPlot({
        req(input$trial1)
        #req(input$trial2)
        req(input$group1)
        req(input$group2)

        d <- curdata()
        mat_t <- d$mat_t
        mat_p <- d$mat_p
<<<<<<< HEAD:app/modules/ancova_stats.R
        cat(file = stderr(), "renderplot\n")
=======
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc:app/modules/ancova_stats.R
        #         #data1 = get_data_group_trial_freqmean(data,input$group1, as.numeric(input$trial1), freq())
        # #data2 = get_data_group_trial_freqmean(data,input$group2, as.numeric(input$trial2), freq())
        # data1 = data_g1_t1()
        # data2 = data_g2_t1()
        # mat_p = matrix(data=NA, nrow=dim(data1)[2], ncol=dim(data1)[3])
        # mat_t = matrix(data=NA, nrow=dim(data1)[2], ncol=dim(data1)[3])
        # color1 = colorRampPalette(c("blue","red","green"))
        #
        # for (i in 1:(dim(data1)[2])){
        #   for (j in 1:(dim(data1)[3])){
        #     x = data1[,i,j]
        #     y = data2[,i,j]
        #     z = t.test(x,y, paired = FALSE)
        #     mat_p[i,j] = z$p.value
        #     mat_t[i,j] = z$statistic
        #   }
        # }

        ###################
        # CORRPLOT

          colnames(mat_p) = g_regions()
          rownames(mat_p) = vector(mode="character", length=length(g_regions()))
          colnames(mat_t) = vector(mode="character", length=length(g_regions()))
          #colnames(mat_t) = g_regions()
          rownames(mat_t) = g_regions()
          # corrplot(mat_p, method="number", type = "upper", is.corr = FALSE,
          #          p.mat = mat_p, sig.level = input_glob_sig(), col = color1(100) )
          if ((g_act_method()=="Coherence") || (g_act_method()=="Connectivity")) {
            corrplot(mat_p, method="number", tl.cex = 0.9, type = "upper", is.corr = FALSE,
                     p.mat = mat_p, sig.level = input_glob_sig(),
                     title = paste0(input$group1, " vs. ", input$group2 , " of trial ", g_trials()[as.numeric(input$trial1)]),
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
      ################################################################
      # THE histogram plots of individual subjects
      ###########################################
      output$boxplot_ancova <- renderPlot({
        req(input$plot_click$x)
        create_df_for_boxplot()
      })
      output$boxplot_ancova_adjusted <- renderPlot({
        req(input$plot_click$x)
        create_df_for_boxplot_adjusted()
      })
      ################################################################
      # THE text about t-statistics
      ###########################################
      output$text_stats_ancova <- renderPrint({
        req(input$plot_click)
        ancova_Model = reactive_ancova_model()
        print(Anova(ancova_Model, type = "III"))
        #reactive_adjustedMeans()


#        adjustedMeans<-effect("Gruppe",df, se=TRUE)
#        print(summary(adjustedMeans))
#        print(adjustedMeans$se)
#        myadjustedmean <- reactive({return(adjustedMeans)})
#        a = ancova_estimation()
        #z = ttest_estimation(compare = "trials", group = 1, trial = 1)
 #       print(a)
        # z = ttest_estimation(compare = "groups", group = 1, trial = 1)
        # cat(z$mydescription)
      })
      output$text_stats_ancova_adjusted_means <- renderPrint({
        req(input$plot_click)
        cat("original means:\n")
        cat(paste0("mean(",input$group1 , ")=", round(mean(data_g1_t1()[,level_y(),level_x()]),4), " +- ", round(sd(data_g1_t1()[,level_y(),level_x()]),4),"\n"))
        cat(paste0("mean(",input$group2 , ")=", round(mean(data_g2_t1()[,level_y(),level_x()]),4), " +- ", round(sd(data_g2_t1()[,level_y(),level_x()]),4),"\n"))

        xg2t1 <-- data_g2_t1()[,level_y(),level_x()]
        cat("adjusted means\n")
        print(summary(reactive_adjustedMeans()))
        cat("sd=")
        cat(reactive_adjustedMeans()$se)
      })

      output$text_stats_ancova_check_independence <- renderPrint({
        req(input$plot_click)

        cat(paste0("testing for differences of the covariate between groups\n"))
        for ( i in 1:length(input$regressors)){
          cat(paste0("testing ... ", input$regressors[i], "\n") )
          x = get_beh_tbl_data_by_group(input$group1, input$regressors[i])
          y = get_beh_tbl_data_by_group(input$group2, input$regressors[i])
          z = t.test(x,y, paired = FALSE)
          if (z$p.value<0.05){
            cat(paste0("ATTENTION ....\n"))
            cat(paste0("ATTENTION ....\n"))
            cat(paste0("Assumption of the ANCOVA is compromised\n"))
            cat(paste0("the covariate ",input$regressors[i], " should not be included\n"))
          }
          cat(create_ttest_string(z))
        }
        # cat("adjusted means\n")
        # print(summary(reactive_adjustedMeans()))
        # cat("sd=")
        # cat(reactive_adjustedMeans()$se)
      })



      #################################################################

      output$htmlhelp_ancova <- renderUI({
        # if (showhtml()){
        includeMarkdown(rmarkdown::render("./documentation/ancova_markdown.md"))
        # }
      })

      output$htmlhelp_parial_correlation <- renderUI({
        # if (showhtml()){
        includeMarkdown("./documentation/partial_correlation_markdown.md")
        # }
      })


      ancova_estimation <- function(){
        #cat(file = stderr(), "ancova_estimation\n")
        xg1t1 <-- data_g1_t1()[,level_y(),level_x()]
        #xg1t2 = data_g1_t2()[,level_y(),level_x()]
        xg2t1 <-- data_g2_t1()[,level_y(),level_x()]
        #xg2t2 = data_g2_t2()[,level_y(),level_x()]
        mystring = ""
        # vergleiche 2 Gruppen mit einem Trial



            if (input$group1 == input$group2){
              cat("no output in case of same groups")
              return()
            }
        #cat(file = stderr(), "1\n")
        xg1 = xg1t1
        xg2 = xg2t1
        # cat(file = stderr(), "length(xg1)=")
        # cat(file = stderr(), length(xg1))
        # cat(file = stderr(), "\n")
        # cat(file = stderr(), "length(xg2)=")
        # cat(file = stderr(), length(xg2))
        # cat(file = stderr(), "\n")


        # mydf <- data.frame(Gruppe=c(rep(input$group1, times=length(xg1)),
        #                             rep(input$group2, times=length(xg2))),
        #                    val=c(xg1, xg2))
        # mydf <- data.frame(Gruppe=c(rep(input$group1, times=length(xg1)),
        #                             rep(input$group2, times=length(xg2))))
        #cat(file = stderr(), "before groupfac\n")
        groupfac=c(rep(input$group1, times=length(xg1)),
                 rep(input$group2, times=length(xg2)))
        #cat(file = stderr(), "before groupfac2\n")
        groupfac<-factor(groupfac,levels = c(input$group1, input$group2), labels= c(input$group1, input$group2))

        #cat(file = stderr(), "3\n")

#        Y =c(xg1, xg2)
#        df <- dataframe(val=c(xg1,xg2))
#        df <- dataframe(Gruppe = groupfac)
        df <- data.frame(val=c(xg1,xg2))

        #cat(file = stderr(), "4\n")

        n = c("val")
        #cat(file = stderr(), "before the for loop\n")
        for ( i in 1:length(input$regressors)){
          bg1 = get_beh_tbl_data_by_group(input$group1, input$regressors[i])
          bg2 = get_beh_tbl_data_by_group(input$group2, input$regressors[i])
          df<-cbind(df, c(bg1, bg2))
          n <- c(n,input$regressors[i])
        }

        # zum Schluss noch die Gruppe dazu
        df<-cbind(df, groupfac)

        n<-c(n, "Gruppe")

        names(df)<-n

#        myModel <-aov(val~ Alter+BDI.II, data = mydf)
        # alle meine Regessoren in mydf gesteckt
       # print(file = stderr(), ncol(df))
        #print(file = stderr(), df)
        #message(df)
        #cat(file = stderr(), "6\n")

        contrasts(df$Gruppe)<-contr.helmert(2)
        #cat(file = stderr(), "7\n")

        myModel <-aov(val ~ . , data = df)
        #cat(file = stderr(), "8\n")

        resmydf <-Anova(myModel, type = "III")
        #print(resmydf)
        #cat(file = stderr(), "9\n")

        g_myModel <<- myModel

        return(myModel)
        # adjustedMeans<-effect("Gruppe",df, se=TRUE)
        # print(summary(adjustedMeans))
        # print(adjustedMeans$se)
        # myadjustedmean <- reactive({return(adjustedMeans)})
      }

      #ttest_estimation <- function(compare = "groups",
      create_df_for_boxplot <- function(){
        xg1t1 = data_g1_t1()[,level_y(),level_x()]
        # = data_g1_t2()[,level_y(),level_x()]
        xg2t1 = data_g2_t1()[,level_y(),level_x()]
        #xg2t2 = data_g2_t2()[,level_y(),level_x()]
        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]

        if (input$group1 == input$group2){
          cat("no output in case of same groups")
          return()
        }
        x = xg1t1
        y = xg2t1
        df <- data.frame(Gruppe=c(rep(input$group1, times=length(x)),
                                  rep(input$group2, times=length(y))),
                         val=c(x, y),
                         note = c(rep("Data", times = length(x)+length(y))))

        #        df$num <- ave(df$val, df$Gruppe, FUN = seq_along)




        n = c("Gruppe", "val")
        # cat(file = stderr(), "before the for loop\n")
        for ( i in 1:length(input$regressors)){

          x = get_beh_tbl_data_by_group(input$group1, input$regressors[i])
          y = get_beh_tbl_data_by_group(input$group2, input$regressors[i])
          #cat(file = stderr(), paste0("loop ",i , "reg = ",input$regressor[i],"\n"))
          #cat(file = stderr(), paste0("length(c(bg1,bg2)=",length(mycomb),"\n"))
          df_new<-data.frame(Gruppe=c(rep(input$group1, times=length(x)),
                                      rep(input$group2, times=length(y))),
                             val=c(x, y),
                             note = c(rep(input$regressors[i], times=length(x)+length(y))))
          #            cbind(df, c(bg1, bg2))
          df <- rbind(df, df_new)

          #names(mydf)[names(mydf)=="V1"]<-input$regressors[i]
          #          n <- c(n,input$regressors[i])
          #cat(file = stderr(), paste0("n =",n,"\n"))
        }




        # means for geomline

        #       df_hline = data.frame(Gruppe = c(input$group1,input$group2), Means=c(mean(x), mean(y)))

        # boxplot(df$val~df$Gruppe, col= terrain.colors(4))
        #
        ggplot(df, aes(x = note, y = val, fill=Gruppe)) +
          geom_boxplot()+
          geom_jitter(color="black", size=0.4, alpha=0.9)+
          facet_wrap(~note, scale="free")+
          ggtitle(" a boxplot with jitter")+
          xlab("Source ")
        #          geom_bar(stat="identity") +
        #facet_wrap(~Gruppe) +
        #geom_hline(data = df_hline, aes(yintercept = Means))


      }

      #ttest_estimation <- function(compare = "groups",
      create_df_for_boxplot_adjusted <- function(){
        #cat(file = stderr(), "starte create_df_for_boxplot_adjusted\n")
        req(reactive_adjustedMeans())
        #cat(file = stderr(), "b2\n")
        xg1t1 = data_g1_t1()[,level_y(),level_x()]
        # = data_g1_t2()[,level_y(),level_x()]
        xg2t1 = data_g2_t1()[,level_y(),level_x()]
        #xg2t2 = data_g2_t2()[,level_y(),level_x()]
        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]
        #cat(file = stderr(), "b3\n")

        df <- data.frame(Gruppe = c(input$group1, paste0("adj",input$group1),
                                    input$group2, paste0("adj_",input$group2)),
          mymean = c(mean(xg1t1), reactive_adjustedMeans()$fit[1],
                     mean(xg2t1), reactive_adjustedMeans()$fit[2]),
                     sd = c(sd(xg1t1), reactive_adjustedMeans()$se[1],
                            sd(xg2t1), reactive_adjustedMeans()$se[2]))
        #cat(file = stderr(), "new DF\n")


        df$Gruppe <- as.character(df$Gruppe)

                df$Gruppe <- factor(df$Gruppe, levels=c(input$group1, paste0("adj",input$group1),
                                                    input$group2, paste0("adj_",input$group2)))
                cat(file = stderr(), "new DF2\n")
                ggplot(df, aes(x=Gruppe, y=mymean)) +
          geom_errorbar(aes(ymin=mymean-sd, ymax=mymean+sd), width=.2) +
          geom_line() +
          geom_point()

        # org_meang1 = mean(xg1t1)
        #
        #
        # if (input$group1 == input$group2){
        #   cat("no output in case of same groups")
        #   return()
        # }
        # x = xg1t1
        # y = xg2t1
        # df <- data.frame(Gruppe=c(rep(input$group1, times=length(x)),
        #                           rep(input$group2, times=length(y))),
        #                  val=c(x, y),
        #                  note = c(rep("Data", times = length(x)+length(y))))
        #
        # #        df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
        #
        # n = c("Gruppe", "val")
        # # cat(file = stderr(), "before the for loop\n")
        # for ( i in 1:length(input$regressors)){
        #
        #   x = get_beh_tbl_data_by_group(input$group1, input$regressors[i])
        #   y = get_beh_tbl_data_by_group(input$group2, input$regressors[i])
        #   #cat(file = stderr(), paste0("loop ",i , "reg = ",input$regressor[i],"\n"))
        #   #cat(file = stderr(), paste0("length(c(bg1,bg2)=",length(mycomb),"\n"))
        #   df_new<-data.frame(Gruppe=c(rep(input$group1, times=length(x)),
        #                               rep(input$group2, times=length(y))),
        #                      val=c(x, y),
        #                      note = c(rep(input$regressors[i], times=length(x)+length(y))))
        #   #            cbind(df, c(bg1, bg2))
        #   df <- rbind(df, df_new)
        # }
        #
        # # means for geomline

        #       df_hline = data.frame(Gruppe = c(input$group1,input$group2), Means=c(mean(x), mean(y)))

        # boxplot(df$val~df$Gruppe, col= terrain.colors(4))
        #
        # ggplot(df, aes(x = note, y = val, fill=Gruppe)) +
        #   geom_boxplot()+
        #   geom_jitter(color="black", size=0.4, alpha=0.9)+
        #   #facet_wrap(~note, scale="free")+
        #   ggtitle(" a boxplot with jitter")+
        #   xlab("Source ")
        #          geom_bar(stat="identity") +
        #facet_wrap(~Gruppe) +
        #geom_hline(data = df_hline, aes(yintercept = Means))

      }



      create_df_for_boxplot_old <- function(){
        xg1t1 = data_g1_t1()[,level_y(),level_x()]
        # = data_g1_t2()[,level_y(),level_x()]
        xg2t1 = data_g2_t1()[,level_y(),level_x()]
        #xg2t2 = data_g2_t2()[,level_y(),level_x()]
        region_x = g_regions()[level_x()]
        region_y = g_regions()[level_y()]

          if (input$group1 == input$group2){
            cat("no output in case of same groups")
            return()
          }
            x = xg1t1
            y = xg2t1
          df <- data.frame(Gruppe=c(rep(input$group1, times=length(x)),
                                    rep(input$group2, times=length(y))),
                           val=c(x, y))
          df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
          # means for geomline
          df_hline = data.frame(Gruppe = c(input$group1,input$group2), Means=c(mean(x), mean(y)))

       # boxplot(df$val~df$Gruppe, col= terrain.colors(4))
#
         ggplot(df, aes(x =num, y = val, fill=Gruppe)) +
           geom_boxplot()+
           geom_jitter(color="black", size=0.4, alpha=0.9)+
           ggtitle(" a boxplot with jitter")+
           xlab("Gruppe")
#          geom_bar(stat="identity") +
          #facet_wrap(~Gruppe) +
          #geom_hline(data = df_hline, aes(yintercept = Means))


      }
})
}

















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








