library(shiny)
compareTrialsStatsLongUI <- function(id){
  ns <- NS(id)
  # Thanks to the namespacing, we only need to make sure that the IDs
  # are unique within this function, rather than unique across the entire app.
  tagList(
    uiOutput(ns("fluidRow_oben")),
    #uiOutput(ns("uiCompareTrialsStats")),


  )
}

compareTrialsStatsLongServer<- function(id, input_glob_sig, freq) {
  moduleServer(
    id,
    function(input, output, session) {
      ns<-session$ns
      #f_utrial_list_all <- reactive({ c("all", f_utrial_list()) })

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
            column(3,

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
            column(4,
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
          ),
          fluidRow(
            column(3,
                   style = "background-color: #fcfcfc;",
                   #style = 'border-bottom: 2px solid gray',
                   style = "border-right: 2px solid black",
                   h4("Trial comparison", align = "center"),
                   selectInput(ns("group"), h5("Select Group", align = "center"),
                               choices = g_groups(), selected = g_groups()[2])
            ),
            column(9,

            )
          ),
          fluidRow(
            column(12,
                   box(title = "Stats..........expand for help (comp_trials_stats_markdown.md)", width = 12, collapsible = TRUE, collapsed = TRUE, htmlOutput(ns("htmlhelp_Comp_Trials")))
            )
          ),
          fluidRow(
            verbatimTextOutput(ns("textCompareTrialsStats"))
          ),
          fluidRow(
            column(12,
                   box(title = "Was wurde berechnet?...", width = 12, collapsible = TRUE, collapsed = TRUE, verbatimTextOutput(ns("text_explanation"))),
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


        )
      })


  output$htmlhelp_Comp_Trials <- renderUI({
    # if (showhtml()){
    includeMarkdown(rmarkdown::render("./documentation/comp_trials_stats_markdown.md"))
    # }
  })


  curdata <- reactive({
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
                                           subjects_to_exclude = subjects_to_exclude(),
                                           network = network_new()
    )
    gM <<- M

    return(M)
  })




   output$textCompareTrialsStats <- renderPrint({
      #output$textCompareTrialsStats <- renderText({
         req(input$group)

        cat(paste0("sig     = ", input_glob_sig(), "\n"))
        cat(paste0("group   = ", input$group, "\n"))
        cat(paste0("freq    = ", freq()[1], "-", freq()[2],"\n"))
        # cat(file = stderr(), paste0("\n\nXXXXsig     = ", input_glob_sig(), "\n"))
        # cat(file = stderr(), paste0("group   = ", input$group, "\n"))
        # cat(file = stderr(), paste0("freq    = ", freq()[1], "-", freq()[2],"\n"))

        #mydata <- curdata()

        #f_data_group_freqmean <- reactive({get_data_group_freqmean(g_data(), input$group, g_sel_freqs())})
#        f_data_group_freqmean <- reactive({get_data_group_freqmean(g_data(), input$group, freq())})
   #     data1 = f_data_group_freqmean()
  #      data2 = f_data_group_freqmean()

 #       gdata1 <<- data1
#        gdata2 <<- data2
        for (trial1_id in 1:length(g_trials())){
          trial2_id = trial1_id + 1
          while (trial2_id<=length(g_trials())){
            cat(paste0(g_trials()[trial1_id]," vs. ", g_trials()[trial2_id],"\n"))


            M <- get_currently_selected_data_long3(g_D(),
                                                   input$group1,
                                                   input$group2,
                                                   trial1_id,
                                                   trial2_id,
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
            )


          #       if (p<input_glob_sig()){
          #
          #         cat(paste0(g_regions()[region1_id], " vs ", g_regions()[region2_id],"\n"))
          #         print(z)
          #
          #       }
          #
          #       region2_id = region2_id + 1
          #     }
          #   }
          #   trial2_id = trial2_id + 1
          # }
          }
        }


      })

      #
      # output$textCompareTrialsStats <- renderPrint({
      #   #output$textCompareTrialsStats <- renderText({
      #   req(input$group)
      #
      #   cat(paste0("sig     = ", input_glob_sig(), "\n"))
      #   cat(paste0("group   = ", input$group, "\n"))
      #   cat(paste0("freq    = ", freq()[1], "-", freq()[2],"\n"))
      #   # cat(file = stderr(), paste0("\n\nXXXXsig     = ", input_glob_sig(), "\n"))
      #   # cat(file = stderr(), paste0("group   = ", input$group, "\n"))
      #   # cat(file = stderr(), paste0("freq    = ", freq()[1], "-", freq()[2],"\n"))
      #
      #   mydata <- curdata()
      #
      #   f_data_group_freqmean <- reactive({get_data_group_freqmean(g_data(), input$group, g_sel_freqs())})
      #   #        f_data_group_freqmean <- reactive({get_data_group_freqmean(g_data(), input$group, freq())})
      #   data1 = f_data_group_freqmean()
      #   data2 = f_data_group_freqmean()
      #
      #   gdata1 <<- data1
      #   gdata2 <<- data2
      #   for (trial1_id in 1:length(g_trials())){
      #     trial2_id = trial1_id + 1
      #     while (trial2_id<=length(g_trials())){
      #       cat(paste0(g_trials()[trial1_id]," vs. ", g_trials()[trial2_id],"\n"))
      #       for (region1_id in 1:length(g_regions())){
      #         region2_id = region1_id + 1
      #         while (region2_id <= length(g_regions())){
      #
      #           #cat(file = stderr(), paste0("dim(data1) = ", dim(data1), " \n"))
      #           #cat(file = stderr(), paste0("dim(data2) = ", dim(data2), " \n"))
      #           #cat(file = stderr(), paste0("region1 = ", region1_id, "  region2_id = ", region2_id, "\n"))
      #           #cat(file = stderr(), paste0("trial1 = ", trial1_id, "  trial2_id = ", trial2_id, "\n"))
      #
      #           x = data1[,region1_id,region2_id,trial1_id]
      #           y = data2[,region1_id,region2_id,trial2_id]
      #           # cat(file = stderr(), "x = ")
      #           # cat(file = stderr(), x)
      #           # cat(file = stderr(), "\n")
      #           # cat(file = stderr(), "y = ")
      #           # cat(file = stderr(), y)
      #           # cat(file = stderr(), "\n")
      #           z = t.test(x,y, paired = TRUE)
      #           t = z$statistic
      #           p = z$p.value
      #           if (p<input_glob_sig()){
      #
      #             cat(paste0(g_regions()[region1_id], " vs ", g_regions()[region2_id],"\n"))
      #             print(z)
      #
      #           }
      #
      #           region2_id = region2_id + 1
      #         }
      #       }
      #       trial2_id = trial2_id + 1
      #     }
      #   }
      #
      #
      # })
      #


    }
  )
}
