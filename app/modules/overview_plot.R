# ehemals plot


overviewPlotUI <- function(id){
  ns <- NS(id)
  # Thanks to the namespacing, we only need to make sure that the IDs
  # are unique within this function, rather than unique across the entire app.
  tagList(
    #uiOutput(ns("uiCompareGroupsStats")),
    #verbatimTextOutput(ns("textCompareGroupsStats")),
    plotOutput(ns("contents"), height = 700, width = "auto"),
    uiOutput(ns("underplotOutput")),
    uiOutput(ns("visualize")),
    #verbatimTextOutput(ns("stats")),
    #selectInput(ns("var"), "Variable", choices = c("one", "two"), selected = "two"),
    #actionButton(ns("do1"),"Click me")
  )
}

overviewPlotServer <- function(id, datatype, input_glob_sig, freq) {
  moduleServer(
    id,
    function(input, output, session) {
      # global Variables
      # my_data      <<- reactive({ get_data(act_data_dir())                })
      # tbl_beh      <<- reactive({ get_global_tbl_beh(act_data_dir())      })
      # uregion_list <<- reactive({ get_global_uregion_list(act_data_dir()) })
      # utrial_list  <<- reactive({ get_global_utrial_list(act_data_dir())  })
      # group_names  <<- reactive({ get_global_group_names(act_data_dir())  })
      # ufreq_list   <<- reactive({ get_global_ufreq_list(act_data_dir())   })
      # region_names <<- reactive({ get_global_region_names(act_data_dir()) })
      # trial_names  <<- reactive({ get_global_trial_names(act_data_dir())  })
      # sel_freq_list<<-

      ns<-session$ns
      #cat(file=stderr(),"starte OverviewPlotServer\n")
      f_utrial_list_all <- reactive({ c("all", g_trials()) })
      output$underplotOutput <- renderUI({
        fluidRow(
          column(3,
                 selectInput(ns("group"), h4("Select Group"),
                             choices = g_groups(), selected = g_groups()[1])
          ),
          column(3,
                 sliderInput(ns("sig_slider"), h4("r-threshold"), min =0 , max = 1, value = 0.05, step = 0.01)
          ),
          column(3,
                 selectInput(ns("trial"), h4("Select Trial"),
                             choices = g_trials(), selected = g_trials()[1]),
                 selectInput(ns("visualize"), h3("visualize method"),
                             choices = c("Heatmap", "Circle"), selected = 1)
          ),
          column(2,
                 selectInput(ns("tabplot_selectInput_dendrogram"), "dendrogram", c("none","row","column","both")),
          ),
          column(1,
                 actionButton(ns("saveImage"), "save Image"),
          )
        )
      })

     # cat(file = stderr(), "\noverview plot\n")
     # cat(file = stderr(), g_trials())
      #cat(file = stderr(), g_trials())

      # filter data by group
      # data_group <- reactive({
      #   get_data_group(my_data(),input$mod_group1)
      # })
      myimage = NULL

      trial_idx <- reactive({match(input$trial,g_trials())})
      #cat(file = stderr(), trial_idx())
      data_groupmean_trial_freq <- reactive({
        get_data_groupmean_trial_freq(g_data(), input$group, trial_idx(), freq())
      })

      observeEvent(input$saveImage, {
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'saving now')
        cat("button pressed")
      })

      ######################
      # PLOT
      output$contents<-renderPlot({
        req(input$trial)
        req(input$group)
        req(input$sig_slider)

                #cat(file=stderr(),paste0("renderplot with input$dataset =",input$dataset,"   input$sel_dim2_3=",input$sel_dim2_3, "\n"))
        cat(file = stderr(), "\n\n")
        cat(file = stderr(), paste0("input$trial=", input$trial , "\n"))
        cat(file = stderr(), paste0("input$trial=", as.numeric(g_trials_named()[input$trial]), "\n"))
        cat(file = stderr(), paste0("trial_idx=", trial_idx(), "\n"))
        cat(file = stderr(), paste0("dim(data)="))
        cat(file = stderr(), dim(g_data()))
        cat(file = stderr(), paste0("\ndim(data_groupmean_trial_freq)="))
        cat(file = stderr(), dim(data_groupmean_trial_freq()))
        #cat(file = stderr(), data_groupmean_trial_freq())

        cat(file = stderr(), paste0("\ninput$group=", input$group, "\n"))
        cat(file = stderr(), paste0("freq=", freq(), "\n"))

            title = "Region vs. REGION"
            #data = data_groupmean_trial_freq()
            mat <- reactive({
              tmp = apply(data_groupmean_trial_freq(), c(1,2), mean)
              tmp[is.nan(tmp)]=0
              return(tmp)
              })


            if (input$visualize=="Heatmap"){
              #req(input$tabplot_checkbox_dendrogram)
              #choice_dend =
              #if (input$tabplot_checkbox_dendrogram){choice_dend = "row"}
              mycolormap = map2color4threshold(mat(),brewer.pal(n=11, name = "RdYlBu"), threshold = input$sig_slider, invert_col_map = TRUE)
              #cat(file = stderr(), "\n\n myclolormap\n")
              #cat(file = stderr(), mycolormap)
              #cat(input$tabplot_selectInput_dendrogram)
              heatmap.2(mat(), scale = "row", #col = bluered(100),
                        col = mycolormap,
                        trace = "none", #density.info = "none",
                        main = "r of Region vs. Region",
                        margins = c(14,13),
                        dendrogram = input$tabplot_selectInput_dendrogram,
                        #xlab = "Frequency", #ylab = "Regions",
                        #Rowv = NA,  Colv = NA, #turns ordering out
                        Colv = NA,
                        labCol = g_regions(), labRow = g_regions()
                        #lwid = c(9,10) #, lhei = c(5,5)
              )
            }
            if (input$visualize=="Circle"){
              #        cat(file = stderr(), paste0("Circle dim = ",dim(mat()),"\n"))
              # cat(file = stderr(), paste0("mat() = ",mat(),"\n"))
              #        print("visualize Circle")
              #rownames(mat()) = f_uregion_list
              #colnames(mat()) = f_uregion_list
              #        col_mat = rand_color(length(mat), transparency = 0.5)
              #        col_mat[mat < 0.8] = "#00000000"
              #        chordDiagram(mat, col = col_mat, symmetric = TRUE)

              # delete lower triangle
              M = mat()
              M[upper.tri(M)]=0.001
              chordDiagram(mat() , col = map2color4threshold(mat(),brewer.pal(n=11, name = "RdYlBu"), threshold = input$sig_slider, invert_col_map = TRUE))
              #        chordDiagram(mat , col = map2color(mat_sig*-1,brewer.pal(n=11, name = "RdYlBu")))
              #        chordDiagramFromMatrix(mat, col = col_mat, symmetric = TRUE)
            }

      } # renderPlot
      )# renderPlot

      ###################
      # RENDERPRINT
      # output$stats <- renderPrint({
      #   req(input$dataset)
      #   req(input$sel_dim2_3)
      #   #tmp2 = my_arr_freq()
      #   #tmp = tmp2[,as.numeric(input$region), as.numeric(input$trial), ]
      #   if (input$dataset == "Frequency"){
      #     print("frequency")
      #     d = data_group()
      #     print(dim(d))
      #     x = apply(data_group_region_trial_freq(), c(1),mean)
      #     y = beh_tbl_data_by_group()
      #     cor_sumary = cor.test(x, y, type = "pearson")
      #     cat(paste0("cor.test ", input$colname1, " vs. ", input$dataset))
      #     print(cor_sumary)
      #     cat(paste0("R2 = ",cor_sumary$estimate*cor_sumary$estimate))
      #   }
      #   if (input$dataset == "Coherence"){
      #
      #
      #
      #     if (input$sel_dim2_3=="Region2Region"){
      #
      #       cat(paste0("coherence region vs. region","\n"))
      #       data = data_group_trial_freq()
      #       data = apply(data, c(1,2,3),mean)
      #       x = data[,as.numeric(input$region), as.numeric(input$region2)]
      #       y = beh_tbl_data_by_group()
      #       cor_sumary = cor.test(x, y, type = "pearson")
      #       cat(paste0("cor.test ", input$colname1, " vs. ", input$dataset, " \n", f_uregion_list[as.numeric(input$region)], "*", f_uregion_list[as.numeric(input$region2)],"\n"))
      #       f = input$freq
      #       cat(paste0("frequency range = ", f[1], "-",f[2]))
      #       print(cor_sumary)
      #       cat(paste0("R2 = ",cor_sumary$estimate*cor_sumary$estimate))
      #     }
      #
      #   }
      #   if (input$dataset == "Granger"){
      #     print("Granger is not implemented")
      #   }
      #   if (input$dataset == "Transferentropy"){
      #     print("Transferentropy is not implemented")
      #   }
      #
      # })
      #









    }
  )
}

