library(shiny)
library(markdown)


compareDiffOfDiffPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("fluidRow_oben")),
    plotOutput(ns("plot"), width = "auto", height = "800px", click = ns("plot_click")),
    fluidRow(
      column(9,
      plotOutput(ns("hist"), width = "auto", height = "300px", click = ns("plot_click_hist")),
      ),
    column(3,
      verbatimTextOutput(ns("text_bottom")),
    )),
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

compareDiffOfDiffPlotServer <- function(id, data, input_glob_sig, freq, trial_list, group_names) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
      ns<-session$ns


      output$fluidRow_oben <- renderUI({

        fluidRow(
          column(5,

                 style = "background-color: #fcfcfc;",
                 #style = 'border-bottom: 2px solid gray',
                 style = "border-right: 2px solid black",
                 h4("group comparison", align = "center"),
                 fluidRow(
                   column(6,
                          selectInput(ns("group1"), h5("Select Group1", align = "center"),
                                      choices = group_names, selected = group_names[2])
                          ),
                   column(6,
                          selectInput(ns("group2"), h5("Select Group 2", align = "center"),
                                      choices = group_names, selected = group_names[3])
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
                                      choices = trial_list, selected = group_names[2])
                   ),
                   column(6,
                          selectInput(ns("trial2"), h5("Select Trial 2", align = "center"),
                                      choices = trial_list, selected = group_names[3])
                   )

                 )
          ),
          # column(3,
          #        selectInput(ns("mod_group3"), h4("Select Group 3"),
          #                    choices = group_names, selected = group_names[3])
          # ),
          column(2,
                 style = "background-color: #fcfcfc;",
                 h4("Visualize", align = "center"),
                 selectInput(ns("method"), h5("method"),
                             choices = c("Corrplot", "Heatmap", "Circle", "Pheatmap"), selected = 1)
          )
        )

      })

      # filter data by group
      data_freqmean <- reactive({
        get_data_freqmean(data, freq())
      })


      data_1 <- reactive({
        get_data_group_trial_freqmean(data,input$group1, as.numeric(input$trial1), freq())
      })
      data_2 <- reactive({
        get_data_group_trial_freqmean(data,input$group2, as.numeric(input$trial2), freq())
      })

      ###########################################################
      ### RENDERPLOT
      output$plot<-renderPlot({
        req(input$trial1)
        req(input$trial2)
        req(input$group1)
        req(input$group2)
        req(input$method)
        if (input$trial1 == input$trial2) {
          string1 = paste0(input$group1," vs ", input$group2, " in trial ", utrial_list[input$trial1], "\n",
                           "independent t-test\n")
          paired = FALSE

        }
        if (input$group1 == input$group2){
          string1 = paste0(input$trial1," vs ", input$trial2, "in group ", input$group1, "\n",
                           "paired t-test\n")
          paired = TRUE
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
            z = t.test(x,y, paired = paired)
            mat_p[i,j] = z$p.value
            mat_t[i,j] = z$statistic
          }
        }

          ###################
          # CORRPLOT
        if (input$method=="Corrplot"){
          colnames(mat_p) = uregion_list
          rownames(mat_p) = vector(mode="character", length=length(uregion_list))
          colnames(mat_t) = vector(mode="character", length=length(uregion_list))
          #colnames(mat_t) = uregion_list
          rownames(mat_t) = uregion_list
          # corrplot(mat_p, method="number", type = "upper", is.corr = FALSE,
          #          p.mat = mat_p, sig.level = input_glob_sig(), col = color1(100) )
          corrplot(mat_p, method="number", tl.cex = 0.9, type = "upper", is.corr = FALSE,
                   p.mat = mat_p, sig.level = input_glob_sig(),
                   col=colorRampPalette(c("blue","red","green"))(200))
          corrplot(mat_t, add = TRUE, method="number", tl.cex = 0.9, type = "lower", is.corr = FALSE,
                   p.mat = mat_p, sig.level = input_glob_sig())
        }
        if (input$method=="Heatmap"){
          plot(mat_t)

        }
        if (input$method=="Circle"){
          rownames(mat_t) = uregion_list
          colnames(mat_t) = uregion_list
          x = data1[,1,2]
          y = data2[,2,3]
          z = t.test(x,y)
          df = z$parameter
          t_threshold = qt(input_glob_sig(), df, lower=FALSE)
          # delete lower triangle
          M = mat_t
          M[is.nan(M)]=0
          M[upper.tri(M)]=0.001
          chordDiagram(
            M ,
            col = map2color4threshold(
              M,brewer.pal(n=11, name = "RdYlBu"),
              threshold = t_threshold,
              invert_col_map = TRUE
              )
            )


        }
        if (input$method=="Pheatmap"){
          colnames(mat_p) = uregion_list
          rownames(mat_p) = uregion_list
          #myplot <- reactive({
          pheatmap(
            mat                   = mat_p,
            display_numbers       = TRUE,
            color                 = inferno(20),
            fontsize              = 18,
            main                  = "P-Values Pheatmap",
            show_rownames         = TRUE,
            show_colnames         = TRUE,
            cluster_cols          = FALSE,
            cluster_rows          = FALSE,
          )
          #})
          #myplot()
        }
      }, res = 96
      )

      output$text_bottom <- renderPrint({
        req(input$plot_click)
        req(input$trial1)
        req(input$trial2)
        req(input$group1)
        req(input$group2)
        req(input$method)

        if (input$trial1 == input$trial2) {
          string1 = paste0(input$group1," vs ", input$group2, " in trial ", utrial_list[input$trial1], "\n",
                           "independent t-test\n")
          paired = FALSE

        }
        if (input$group1 == input$group2){
          string1 = paste0(input$trial1," vs ", input$trial2, "in group ", input$group1, "\n",
                           "paired t-test\n")
          paired = TRUE
        }

        #lvls <- levels(uregion_list)
        data1 = data_1()
        data2 = data_2()
        level_x = round(input$plot_click$x)
        level_y = abs(round(input$plot_click$y)-length(uregion_list)-1)
        region_x = uregion_list[level_x]
        region_y = uregion_list[level_y]


        x = data1[,level_x,level_y]
        y = data2[,level_x,level_y]
        z = t.test(x,y, paired = paired)
        #cat( paste0("Analysis of regions ",region_x," vs. ", region_y ," \n ", string1))

        #print(z)
        if (is.null(input$plot_click$x)) return("null")
        else {
          lvls <- levels(uregion_list)
          name <- lvls[round(input$plot_click$x)]
          # HTML("You've selected <code>", name, "</code>",
          #      "<br><br>Here are the first 10 rows that ",
          #      "match that category:")
        }

        out <- paste0(z$method,"\n\n",
                      region_x, "\n        vs. \n", region_y, " \n\n",
                      "mean= ", round(z$estimate[2],3), " vs. ", round(z$estimate[1],3)," \n",
                      "t=", round(z$statistic,2), " \n",
                      "p=", z$p.value, "  \n",
                      "df=", round(z$parameter,1),"  \n",
                      "CI(",attributes(z$conf.int),")= ",round(z$conf.int[1],3)," ; ",round(z$conf.int[2],3)," \n"
                      )
        cat(out)

#        keeprows <- round(input$plot_click$x) == as.numeric(uregion_list)

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
        # req(input$plot_click)
        # req(input$trial1)
        # req(input$trial2)
        # req(input$group1)
        # req(input$group2)
        # req(input$method)
        # #lvls <- levels(uregion_list)
        # data1 = data_1()
        # data2 = data_2()
        # level_x = round(input$plot_click$x)
        # level_y = abs(round(input$plot_click$y)-length(uregion_list)-1)
        # region_x = uregion_list[level_x]
        # region_y = uregion_list[level_y]
        #
        # if (input$trial1 == input$trial2) {
        #   string1 = paste0(input$group1," vs ", input$group2, " in trial ", utrial_list[input$trial1], "\n")
        #
        # }
        # if (input$group1 == input$group2){
        #   string1 = paste0(input$trial1," vs ", input$trial2, "in group ", input$group1, "\n")
        # }
        # cat( paste0("Analysis of regions ",region_x," vs. ", region_y ," \n ", string1))
        # x = data1[,level_x,level_y]
        # y = data2[,level_x,level_y]
        # z = t.test(x,y)
        # print(z)
        #
        # out <- paste0(region_x, " vs. ", region_y, "\n",
        #               "t=", z$statistic)
        # HTML(out)
        # if (is.null(input$plot_click$x)) return("null")
        # else {
        #   lvls <- levels(uregion_list)
        #   name <- lvls[round(input$plot_click$x)]
        # }


      })
      output$hist <- renderPlot({
        req(input$plot_click$x)
        req(input$plot_click$y)
        region_x = uregion_list[round(input$plot_click$x)]
        cat(file = stderr(), region_x)
        level_x = round(input$plot_click$x)
        level_y = abs(round(input$plot_click$y)-length(uregion_list)-1)
        region_x = uregion_list[level_x]
        region_y = uregion_list[level_y]
#        level_x = 1
#        level_y = 2
        df = tbl_beh
        d = data_freqmean()

        if (input$trial1 == input$trial2) {
          cat(file = stderr(), "trial1 == trial2\n")
          string1 = paste0(input$group1," vs ", input$group2, " in trial ", utrial_list[input$trial1], "\n")
          d1 = get_data_group_freqmean(data, input$group1, freq())
          d2 = get_data_group_freqmean(data, input$group2, freq())
          x = d1[,level_x, level_y, as.numeric(input$trial1)]
          y = d2[,level_x, level_y, as.numeric(input$trial1)]
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
          string1 = paste0(input$trial1," vs ", input$trial2, "in group ", input$group1, "\n")
          data1 = data_1()
          data2 = data_2()
          x = data1[,level_x, level_y]
          y = data2[,level_x, level_y]
          df <- data.frame(Gruppe=c(rep(utrial_list[as.numeric(input$trial1)], times=length(x)),
                        rep(utrial_list[as.numeric(input$trial2)], times=length(y))),
                        val=c(x, y))
          df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
          # means for geomline
          df_hline = data.frame(Gruppe = c(utrial_list[as.numeric(input$trial1)],
                                         utrial_list[as.numeric(input$trial2)]),
                              Means=c(mean(x), mean(y)))

          #p<-ggplot(df, aes(num, val, fill=Gruppe))
          #p + geom_bar(stat="identity") + facet_wrap(~Gruppe)
        }
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
      output$facet <- renderPlot({
        level_x = round(input$plot_click$x)
        level_y = abs(round(input$plot_click$y)-length(uregion_list)-1)
        region_x = uregion_list[level_x]
        region_y = uregion_list[level_y]

        df = tbl_beh
        d = data_freqmean()
        df$data1 = d[,level_x, level_y, input$trial1]
        df$num <- ave(df$data1, df$Gruppe, FUN = seq_along)

      })
    }
  )
}



























