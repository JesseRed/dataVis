library(shiny)
library(markdown)
library(corrr)
library(GGally)
library(ggcorrplot)
library(ggplot2)
library(plotly)


compareTrialsPlotUI <- function(id){
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

compareTrialsPlotServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns<-session$ns

      output$fluidRow_oben <- renderUI({
        fluidPage(
        fluidRow(
          column(4,

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
          column(4,
                 style = "background-color: #fcfcfc;",
                 style = 'border-right: 2px solid gray',
                 h4("trial comparison", align = "center"),
                 fluidRow(
                   column(6,
                          selectInput(ns("trial1"), h5("Select Trial 1", align = "center"),
                                      choices = g_trials_named(), selected = g_groups()[1])
                   ),
                   column(6,
                          selectInput(ns("trial2"), h5("Select Trial 2", align = "center"),
                                      choices = g_trials_named(), selected = g_groups()[2])
                   )
                 )
          ),
          column(2,
                 style = "background-color: #fcfcfc;",
                 h4("Visualize", align = "center"),
                 selectInput(ns("method"), h5("method"),
                             choices = c("Corrplot", "Corrplot_mixed", "Corrplot_clustered", "ggcorr", "Circle", "Pheatmap"), selected = 1)
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
        fluidRow(
        #  plotOutput(ns("plot"), width = "auto", height = "800px", click = ns("plot_click"))
          plotOutput(ns("plot"), width = "auto", height = "auto", click = ns("plot_click"))
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
        # fluidRow(
        #   column(2, actionButton(ns("saveImage"), "save Image"),),
        #   column(2, textInput(ns("saveimage_filename"),"filename ","mycompplotimage")),
        #   column(2, numericInput(ns("saveimage_height"),"height (cm)",8)),
        #   column(2, numericInput(ns("saveimage_width"),"width (cm)",8)),
        #   column(2, numericInput(ns("saveimage_dpi"),"dpi",600)),
        #   column(2, selectInput(ns("saveimage_fileformat"), "file format", choices = c("tiff", "pdf", "png"))),
        # ),
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


      data_1 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial1), g_sel_freqs())
      })
      data_2 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial2), g_sel_freqs())
      })
      data_g1t1 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial1), g_sel_freqs())
      })
      data_g1t2 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial2), g_sel_freqs())
      })
      data_g2t1 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial1), g_sel_freqs())
      })
      data_g2t2 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial2), g_sel_freqs())
      })

      curdata <- reactive({
        get_currently_selected_data_long(g_data(), input$group1, input$group2, as.numeric(input$trial1), as.numeric(input$trial2), g_sel_freqs())
#        get_currently_selected_data(g_data(), input$group1, input$group2, as.numeric(input$trial1), as.numeric(input$trial2), g_sel_freqs())
      })

      plotwidth <- reactive({
        if (input$plot_width == 0){ return("auto")            }
        else{                       return(input$plot_width)  }
      })

      plotheight <- reactive({
        if (input$plot_height == 0){ return("auto")}
        else{ return(input$plot_height) }
      })





      ###########################################################
      ### RENDERPLOT
      output$myplotly<-renderPlotly({
        start_time = Sys.time()
        cat(file=stderr(), "before curdata() in myplotly\n")

        d <- curdata()
        mat_t <<- d$mat_t
        mat_p <<- d$mat_p
        p <-generate_plot_ggplot_corrplot_handmade(mat_p, mat_t)
        p
        cat(file = stderr(),paste0("renderPlotly duration =",Sys.time()-start_time,"\n"))

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
        #cat(file = stderr(), cur_dev)
        #cat(file=stderr(), "before curdata() in plot\n")
        d <- curdata()
        renderplotcurdata<<-curdata()
        # mat_t <<- d$mat_t
        # mat_p <<- d$mat_p
        # cat(file = stderr(), "in renderPlot\n")
        # cat(file = stderr(), "\nmat_p\n")
        # cat(file = stderr(), mat_p)
        # cat(file = stderr(), "\nmat_t\n")
        # cat(file = stderr(), mat_t)

        ###################
        # CORRPLOT
        if (input$method=="Corrplot"){
          generate_plot_Corrplot(d$mat_p, d$mat_t)
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

        x = curdata()$data1[, level_y_rval(), level_x_rval()]
        y = curdata()$data2[, level_y_rval(), level_x_rval()]
        z = t.test(x,y, paired = curdata()$my_paired)
        out <- create_my_ttest_string(z, paired = curdata()$my_paired, mean1 = mean(x), mean2 = mean(y))
        cat(out)
      })

      output$hist <- renderPlot({

        generate_histogram_plot_facet(input$group1, input$group2, input$trial1, input$trial2, g_sel_freqs(), level_x_rval(), level_y_rval())

      })


      output$facet <- renderPlot({
        df = tbl_beh
        d = data_freqmean()
        df$data1 = d[,level_x_rval, level_y_rval, input$trial1]
        df$num <- ave(df$data1, df$Gruppe, FUN = seq_along)

      })

      output$htmlhelp_Comp_Plot <- renderUI({
        # if (showhtml()){
        includeMarkdown(rmarkdown::render("./documentation/comp_plot_markdown.md"))
        # }
      })





      # Observe Funktion fuer den zentralen Specherbutton
      observeEvent(g_saveImage_button(),{
        req(input$group1)
        req(input$group1)
        req(input$trial1)
        req(input$trial2)

        #cat(file = stderr(), "observeEvent(g_save_Image_button(), with input$method =", input$method,"\n")
        #cat(file = stderr(), "dpi=",g_saveImage_dpi(),"\n")
        #cat(file=stderr(), "before curdata() in g_saveImage_button\n")

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



























