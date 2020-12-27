library(shiny)
library(markdown)


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

compareTrialsPlotServer <- function(id,  freq) {
  moduleServer(
    id,
    function(input, output, session) {
      #selectInput(NS(id, "var"), "Variable", choices = NULL)
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
          # column(3,
          #        selectInput(ns("mod_group3"), h4("Select Group 3"),
          #                    choices = group_names, selected = group_names[3])
          # ),
          column(2,
                 style = "background-color: #fcfcfc;",
                 h4("Visualize", align = "center"),
                 selectInput(ns("method"), h5("method"),
                             choices = c("Corrplot", "Heatmap", "Circle", "Pheatmap"), selected = 1)
          ),
          column(2,
#                 fluidRow(actionButton(ns("saveImage"), "save Image")),
#                 fluidRow(actionButton(ns("ExportData"), "export Data")),
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
          plotOutput(ns("plot"), width = "auto", height = "800px", click = ns("plot_click"))
          #uiOutput("plot.ui") #ns("plot"), width = "auto", height = "800px", click = ns("plot_click"))
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
          column(2, actionButton(ns("saveImage"), "save Image"),),
          column(2, textInput(ns("saveimage_filename"),"filename ","mycompplotimage")),
          column(2, numericInput(ns("saveimage_height"),"height (cm)",8)),
          column(2, numericInput(ns("saveimage_width"),"width (cm)",8)),
          column(2, numericInput(ns("saveimage_dpi"),"dpi",600)),
          column(2, selectInput(ns("saveimage_fileformat"), "file format", choices = c("tiff", "pdf", "png"))),
        ),
        fluidRow(
          column(12,
                 box(title = "Plot ..........expand for help (comp_plot_markdown.md)", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp_Comp_Plot"))),
          )
        ),

)
      })

      x1<<- NULL
      x2<<- NULL
      myTabPlots <<- list()

      # filter data by group
      data_freqmean <- reactive({
        get_data_freqmean(g_data(), freq())
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
      save_image1_rval <- reactiveVal(FALSE) # zur Speicherung des Plot Images
      save_image2_rval <- reactiveVal(FALSE) # zur Speicherung des Histogram Images

      data_1 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial1), freq())
      })
      data_2 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial2), freq())
      })
      data_g1t1 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial1), freq())
      })
      data_g1t2 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group1, as.numeric(input$trial2), freq())
      })
      data_g2t1 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial1), freq())
      })
      data_g2t2 <- reactive({
        get_data_group_trial_freqmean(g_data(),input$group2, as.numeric(input$trial2), freq())
      })

      curdata <- reactive({
        get_currently_selected_data(g_data(), input$group1, input$group2, as.numeric(input$trial1), as.numeric(input$trial2), freq())
      })

      plotwidth <- reactive({
        if (input$plot_width == 0){
          return("auto")
        } else{
          return(input$plot_width)
        }
      })

      plotheight <- reactive({
        if (input$plot_height == 0){
          return("auto")
        } else{
          return(input$plot_height)
        }
      })


      ###########################################################
      ### RENDERPLOT
      output$plot<-renderPlot(
        #if (input$plot_width == 0){
        #  width = "auto" #function() input$plot_width,
        #  height = function() input$plot_height
        #}else{
          width = function() plotwidth(),
          height = function() plotheight(),
        #},
        res = input$plot_res,
        {
        req(input$trial1)
        req(input$trial2)
        req(input$group1)
        req(input$group2)
        d <- curdata()
        # colnames(d$mat_p) = g_regions()
        # rownames(d$mat_p) = g_regions()

        ###################
        # CORRPLOT
        if (input$method=="Corrplot"){
        # colnames(d$mat_p) = g_regions()
        # rownames(d$mat_p) = vector(mode="character", length=length(g_regions()))
        # colnames(d$mat_t) = vector(mode="character", length=length(g_regions()))
        # rownames(d$mat_t) = g_regions()
        if (g_act_method()=="Coherence") {
          rownames(d$mat_p) = vector(mode="character", length=length(g_regions()))
          x1 <<- corrplot(d$mat_p, method="number", tl.cex = 0.9, type = "upper", is.corr = FALSE,
                          p.mat = d$mat_p, sig.level = g_sig(),
                          col=colorRampPalette(c("blue","red","green"))(200))
          colnames(d$mat_t) = vector(mode="character", length=length(g_regions))
          x2 <<- corrplot(d$mat_t, add = TRUE, method="number", tl.cex = 0.9, type = "lower", is.corr = FALSE,
                          p.mat = d$mat_p, sig.level = g_sig())
          #dev.copy(png,'mylocalcorrplot.png')
          #dev.off()
        }else if (g_act_method()=="Transferentropy" || g_act_method()=="Granger"){
          corrplot(d$mat_p, method="number", tl.cex = 0.9, is.corr = FALSE,
                   p.mat = d$mat_p, sig.level = g_sig(),
                   col=colorRampPalette(c("blue","red","green"))(200))

        }else if (g_act_method()=="Frequency"){
          print("not implemented")
        }
        }


        if (input$method=="Heatmap"){
          #png("mypng.png")
          x1 <<- plot(d$mat_t)
          #dev.off()


        }
        if (input$method=="Circle"){
          myplotcircle = generate_plot_Circle(d$mat_p, d$mat_t, d$data1, d$data2)

          #          chordDiagram(M)

          #
          # rownames(d$mat_t) = g_regions()
          # colnames(d$mat_t) = g_regions()
          # x = d$data1[,1,2]
          # y = d$data2[,2,3]
          # z = t.test(x,y)
          # df = z$parameter
          #
          # #M = 1/d$mat_p
          # M = abs(log(d$mat_p))
          #
          # t_threshold = abs(log(g_sig()))
          # rownames(M) = g_regions()
          # colnames(M) = g_regions()
          # M[is.nan(M)]=0
          # M[upper.tri(M)]=0.001
          #
          # # cat(file=stderr(), M)
          # # RdYlBu hat 11 Farbstufen daher nicht fuer diese Palette veraendern
          # mycol = map2color4threshold(
          #   M,brewer.pal(n=11, name = "RdYlBu"),
          #   threshold = t_threshold,
          #   invert_col_map = TRUE
          # )
          # dim(mycol) = dim(M)
          #
          # chordDiagram(M ,col = mycol)
          #
          # #dev.copy(png,'mylocalCicleplot.png')
          # #dev.off()
          # tiff(paste0("./outputimages/Circleplot.tif"),
          #      width = 2000, height = 2000,
          #      units  = "px", res = 600)
          # chordDiagram(M ,col = mycol)
          # dev.off()
          # #          chordDiagram(M)
          #
        }
        if (input$method=="Pheatmap"){

            generate_plot_Pheatmap(d$mat_p, d$mat_t, myfontsize = 18)
#           colnames(d$mat_p) = g_regions()
#           rownames(d$mat_p) = g_regions()
#           #myplot <- reactive({
# #          mat_p[mat_p>0.05]=0.1
#           #m <- matrix(c(rnorm(1000)), ncol=100)
#           #distmat <- dist(t(m))
#
#           # Returns a vector of 'num.colors.in.palette'+1 colors. The first 'cutoff.fraction'
#           # fraction of the palette interpolates between colors[1] and colors[2], the remainder
#           # between colors[3] and colors[4]. 'num.colors.in.palette' must be sufficiently large
#           # to get smooth color gradients.
#           makeColorRampPalette <- function(colors, cutoff.fraction, num.colors.in.palette)
#           {
#             stopifnot(length(colors) == 4)
#             ramp1 <- colorRampPalette(colors[1:2])(num.colors.in.palette * cutoff.fraction)
#             ramp2 <- colorRampPalette(colors[3:4])(num.colors.in.palette * (1 - cutoff.fraction))
#             return(c(ramp1, ramp2))
#           }
#
#           cutoff.distance <- 0.05
#           cols <- makeColorRampPalette(c("red", "orange",    # distances 0 to 3 colored from white to red
#                                          "gray", "black"), # distances 3 to max(distmat) colored from green to black
#                                        cutoff.distance / 1, #max(mat_p), #max(distmat),
#                                        100)
#
#           pheatmap(
#             mat                   = d$mat_p,
#             display_numbers       = TRUE,
#             color                 = cols,
# #            color                 = inferno(20),
#             fontsize              = 18,
#             main                  = "P-Values Pheatmap",
#             show_rownames         = TRUE,
#             show_colnames         = TRUE,
#             cluster_cols          = FALSE,
#             cluster_rows          = FALSE,
#           )

        }

      }
      )

      output$text_bottom <- renderPrint({
        x = curdata()$data1[, level_y_rval(), level_x_rval()]
        y = curdata()$data2[, level_y_rval(), level_x_rval()]
        z = t.test(x,y, paired = curdata()$my_paired)
        out <- create_my_ttest_string(z, paired = curdata()$my_paired, mean1 = mean(x), mean2 = mean(y))
        cat(out)
      })

      output$hist <- renderPlot({

        generate_histogram_plot_facet(input$group1, input$group2, input$trial1, input$trial2, freq(), level_x_rval(), level_y_rval())

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

        cat(file = stderr(), "observeEvent(g_save_Image_button(), with input$method =", input$method,"\n")
        cat(file = stderr(), "dpi=",g_saveImage_dpi(),"\n")
        d <- curdata()
        #if (g_saveImage_button()>0){
          filename = paste0(g_saveImage_filename(),"_hist", format(Sys.time(), "%Y-%m-%d-%H-%M-%S."), g_saveImage_fileext())
          myplot<-generate_histogram_plot_facet(input$group1, input$group2, input$trial1, input$trial2, freq(), level_x_rval(), level_y_rval(), d)
          ggsave(file = filename, width = g_saveImage_width(), height =g_saveImage_height(), units = "cm", plot = myplot, type = "cairo", dpi = g_saveImage_dpi())
        #}


          filename2 = paste0(g_saveImage_filename(),"_",input$method, format(Sys.time(), "%Y-%m-%d-%H-%M-%S."), g_saveImage_fileext())
          open_device_for_save(filename2)
          myplot = switch(
            input$method,
            "Corrplot"     = generate_plot_Coherence(d$mat_p, d$mat_t),
            "Circle"       = generate_plot_Circle(d$mat_p, d$mat_t, d$data1, d$data2),
            "Pheatmap"     = generate_plot_Pheatmap(d$mat_p, d$mat_t)
          )
          dev.off()
      }
      )

      observeEvent(input$ExportData, {
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'saving data now')
        #ggsave(file = "tmpbutton.png")
        #ggsave(plot = x1, filename = "x1plot.png", type = "cairo", dpi = 600)
        req(input$trial1)
        req(input$trial2)
        req(input$group1)
        req(input$group2)
        d <- curdata()
        data1 <- d$data1
        data2 <- d$data2
        string1 <-d$string1
        mat_p <- d$mat_p
        mat_t <- d$mat_t
        saveRDS(mat_p, file = "./exported_variables_from_visualizer/ExportData2D_mat_p.Rds")
        saveRDS(mat_t, file = "./exported_variables_from_visualizer/ExportData2D_mat_t.Rds")
        saveRDS(data1, file = "./exported_variables_from_visualizer/ExportData3D_data1_subj_reg1_reg2.Rds")
        saveRDS(data2, file = "./exported_variables_from_visualizer/ExportData3D_data2_subj_reg1_reg2.Rds")

      })


    }
  )
}



























