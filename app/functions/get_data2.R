library(abind)


# has tests
filter_datastruct <- function(D, group = NULL, myfilter = NULL){
  # reduziert die Datenstruktur D indem entsprechend der gruppe nach dem uebergebenen Filter gefiltert wird
  # group : integer
  # character :
  # z.B. group = 1, "Zeichen__1>0, Zeichen__2>1, ToInclude==1"
  #    -> aus dem Datensatz D werden alle Eintraege entfert die Gruppe = 1 haben und in D$df_BD eine Spalte mit Zeichen__1 Werte stehen
  #         haben die nicht groesser 0 sind, sowie Zeichen__2 nicht groesser 0 und die nicht ToInclude==1 sind
  #
  if (is.null(group) | is.null(myfilter)){
    cat(file = stderr(), paste0("Aufruf of filter_datastruct without proper parameters group =", group, "  filter = ", myfilter, "\n"))
    return(D)
  }
  #filter
  vfilter = unlist(strsplit(myfilter, ","))
  if (length(vfilter)==0){
    cat(file = stderr(), paste0("lenth of filter == 0 return original struct without filter ... myfilter= ", myfilter, "\n"))

    return(D)
  }
  for (i in 1:length(vfilter)){
    filter_entry = trimws(vfilter[i])
    cat(file= stderr(), paste0("i = ", i , " with filter_entry = ", filter_entry, "\n"))
    # filtere nur wenn nicht empty
    if (nchar(filter_entry)>0){


      D<- tryCatch(
        {
          #cat(file = stderr(), paste0("in filter \n"))
          # hole alle der Gruppe
          df_g1 <- D$df_BD %>% filter(D$df_BD$Gruppe == group)
          # filtere nach dem filterkriterium

          df_g1f <- df_g1 %>% filter(eval(rlang::parse_expr(filter_entry)))

          ids_to_eleminate <- setdiff(df_g1$ID,df_g1f$ID)

          delete_subject_from_data_struct(D = D, ids_to_delete = ids_to_eleminate)
        },
        error = function(cond){
          cat(file = stderr(), paste0("error filter estimation myfilter = ",filter_entry,"\n"))
          #cat(file = stderr(), paste0("error message =",cond,"\n"))
          #cat(file = stderr(), paste0("\nend of error message\n"))
          return(D)
        },
        warning= function(cond){
          cat(file = stderr(), paste0("warning filter estimation myfilter = ",filter_entry,"\n"))
          #cat(file = stderr(), paste0("warning message =",cond,"\n"))
          return(D)
        }
      )


    }
  }

  return(D)
}


get_currently_selected_data_long3<-function(D, g1, g2, t1, t2, freq,
                                            trials=g_trials(),
                                            regions=g_regions(),
                                            tbl_beh = g_beh(),
                                            method = g_act_method(),
                                            long_def1 = NULL,
                                            long_def2 = NULL,
                                            is_exclude_not_reoccuring_subj = TRUE,
                                            averagelong = TRUE,
                                            estimate_time_first = TRUE,
                                            filter_g1 = "",
                                            filter_g2 = "",
                                            iscausal = FALSE,
                                            subjects_to_exclude = NULL,
                                            network = NULL){

  # die funktion gibt eine LIste von mehreren Variablen zurueck
  # gedacht fuer tabs in denen gruppen und trials ausgewaehlt werden
  # d$mypaired ... bool handelt es sich um einen gepaarten oder ungeparrten test
  # d$data1 ... 3D matrix ... der gruppe 1 bzw trial 1 (subjects x region1 x region2 )
  # d$data2 ... 3D matrix ... der gruppe 2 bzw trial 2
  # d$mat_p ... 2D matrix ... p Werte des t-tests ueber alle Regionen
  # d$mat_t ... 2D matrix ... t Werte des t-tests ueber alle Regionen
  # d$string1 ... string .... eine beschreibung des durchgefuehrten Vergleiches
  # d$color1 ... col ........ die Color palette die zu den Werten passen
  cat(file = stderr(),paste0("gcsdl3 estimate time first = ", estimate_time_first,"\n"))

  # Es gibt nun nur noch gerichtet und ungerichtet
  if (iscausal){
    method = "Transferentropy"
  }else{
    method = "Connectivity"
  }

  #cat(file = stderr() , paste0(tbl_beh))
  # if (is.null(network)){
  #   network <-
  #   network = list()
  #   for (i in 1:length(g_regions())){
  #     network[g_regions()[i]]=i
  #   }
  # }

  gDS0 <<-D
  ###################
  # 1. Entferne die Subjects aus der exclude liste
  if (!is.null(subjects_to_exclude)){
    if (length(subjects_to_exclude)>0){
      cat(file = stderr(), "subjects to exclude in get_currently_selected_data_long3 = ", subjects_to_exclude, "\n")
      D <- delete_subject_from_data_struct(D = D, ids_to_delete = subjects_to_exclude)
    }
  }
  gDS1 <<-D
  ###################
  # 2. Veraendere (zusammenfassen) das Netzwerk (regions x regions)
  #    entsprechend des uebergebenen NEtzwerkes (network)
  #
  start_time = Sys.time()
  cat(file = stderr(),paste0("gcsdl3 bfore change_network_in_data_struct\n"))
  if (! is.null(network)){
    D <- change_network_in_data_struct(D = D, new_uregion_list_named = network)
    regions <- D$uregion_list
  }
  cat(file = stderr(),paste0("change_network_in_data_struct duration =",Sys.time()-start_time,"\n"))
  gDS2<<-D



  ###################
  # 1. Filtere die Daten anhand von filter_g1 und filter_g2
  # das Filtern muss als erstes erfolgen, da +++wenn wir hier Probanden
  # rauswerfen hinterher die gleichen Probanden noch beim longitudinalen
  # Design rausgeworfen werden muessen
  D <- filter_datastruct(D, group = g1, myfilter = filter_g1)
  D <- filter_datastruct(D, group = g2, myfilter = filter_g2)
  gDS3<<-D
  g_freq <<- freq
  # wenn moeglich dann teile in longitudinale Daten auf
  S<- split_data_by_longitudinal_info(D, long_def1, long_def2,
                                      is_exclude_not_reoccuring_subj = is_exclude_not_reoccuring_subj,
                                      averagelong = averagelong)
  gdx11 <<- S$D1
  gdx12 <<- S$D2

  data <- S$D1$mdat
  tbl_beh = S$D1$df_BD
  datalong <- S$D2$mdat
  start_time <- Sys.time()
  # fuer die ersten Daten (alle oder longitudinal 1)


  cat(file = stderr(),"\nlongitudinal data analyse start \n")
  cat(file = stderr(),paste0("g1 = ",g1, "   g2 = ", g2,"\n"))
  cat(file = stderr(),paste0("t1 = ",t1, "   t2 = ", t2,"\n"))
  #cat(file = stderr(),paste0("freq = ",freq,"\n"))
  cat(file = stderr(),paste0("trials = ",trials,"\n"))
  cat(file = stderr(),paste0("method = ",method,"\n"))
  d <- get_selected_data_considering_group_trial(data, g1, g2, t1, t2, freq,  trials = trials, tbl_beh = tbl_beh, method = method)

  # Ich muss noch die regionen mit einbinden in die Datenstruktur
  d$regions = regions

  gdx2<<-d
  if (!is.null(datalong)){
    cat(file = stderr(), "update \n")
    d<-update_data_structure_by_longitudinal_data(d, datalong, g1,g2,t1,t2, freq, trials = trials,
                                               tbl_beh_long = S$D2$df_BD, method = method, estimate_time_first = estimate_time_first)
  }

  gdx3 <<-d
  #####################
  # d$data1 und d$data2 sind erhoben


  cat(file = stderr(), paste0("dim(d$data1) = ", dim(d$data1),"\n"))
  cat(file = stderr(), paste0("dim(d$data2) = ", dim(d$data2),"\n"))


  d <- estimate_mat_t_p(d, method = method, regions = regions)
  gdx4 <<- d
  cat(file = stderr(),paste0("get_currently_selected_datalong3 duration =",Sys.time()-start_time,"\n"))
  gd_get_long_data <<- d
  return(d)

}


#
#
# update_data_structure_by_longitudinal_data<-function(d1, datalong,  g1,g2,t1,t2, freq,
#                                            trials=g_trials(),
#                                            tbl_beh_long = NULL,
#                                            method = g_act_method(),
#                                            estimate_time_first = TRUE){
#
#
#
#         #############################
#     # longitudinale Daten
#     #    if (is_debug){
#     cat(file = stderr(),"\nlongitudinal data analyse start \n")
#     cat(file = stderr(),paste0("g1 = ",g1, "   g2 = ", g2,"\n"))
#     cat(file = stderr(),paste0("t1 = ",t1, "   t2 = ", t2,"\n"))
#     cat(file = stderr(),paste0("freq = ",freq,"\n"))
#     cat(file = stderr(),paste0("trials = ",trials,"\n"))
#     cat(file = stderr(),paste0("method = ",method,"\n"))
#     cat(file = stderr(),paste0("update_data_structure_by_longitudinal_data length(dim(data))= ",length(dim(data)),"\n"))
#     cat(file = stderr(),paste0("update_data_structure_by_longitudinal_data dim(data)= ",dim(data),"\n"))
#     cat(file = stderr(),paste0("update_data_structure_by_longitudinal_data length(dim(datalong))= ",length(dim(datalong)),"\n"))
#     cat(file = stderr(),paste0("update_data_structure_by_longitudinal_data dim(datalong)= ",dim(datalong),"\n"))
#     cat(file = stderr(),paste0("estimate time first = ", estimate_time_first,"\n"))
#
#     #   }
#
#     #Z127 <<- data
#     #cat(file = stderr(),"1\n")
#     d2 <- get_selected_data_considering_group_trial(datalong, g1,g2,t1,t2, freq, trials = trials, tbl_beh = tbl_beh_long, method = method)
#     #cat(file = stderr(),"2\n")
#     d <- d1
#     glob_d1<<-d1
#     glob_d2<<-d2
#     glob_data<<-data
#     glob_datalong<<-datalong
#     # d1 und d2 sind die Daten der beiden Zeitpunkte mit jeweils
#     # data1 and data2 sind 3D arrays
#     # dim1 subjects ... koennen fuer data1 und data 2 die gleichen sein (d$paired = TRUE)
#     #                           oder unterschiedliche subjects (d$paired = FALSE)
#     # dim2 regions
#     # dim3 regions
#     #
#     # wenn unterschiedliche Gruppen dann
#     if (d2$my_paired==FALSE){
#       #subtrahiere jeden Subject voneinander ... d2$data1 - d1$data1
#       # Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten
#       # Frage: gibt es einen signifikanten Unterschied zwischen diesen zeitbezogenen unterschieden?
#       #d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"
#
#       d$data1 <- d2$data1-d1$data1
#       d$data2 <- d2$data2-d1$data2
#       d$df_data1 <- estimate_df_difference(d2$df_data1, d1$df_data1)
#       d$df_data2 <- estimate_df_difference(d2$df_data2, d1$df_data2)
#       d$explanation = paste0("Was wird hier berechnet? ...\n",
#                              "Gruppe ist verschieden ...beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
#                              "Algorithmus:\n",
#                              "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind\n",
#                              "2. falls unterschiedliche trial gewaehlt wurden wird der subjectspezifische Unterschied zwischen den Trials berechnet\n",
#                              "3. Berechne X1 = Data_Zeitpunkt2_group1_trial(evtl. dif) - Data_Zeitpunkt1_group1_trial(evtl. dif) (Subjects x Regions x Regions)\n",
#                              "            X2 = Data_Zeitpunkt2_group2_trial(evtl. dif) - Data_Zeitpunkt1_group2_trial(evtl. dif) (Subjects x Regions x Regions)\n",
#                              "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen\n",
#                              "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)\n",
#                              "4. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (unpaired fuer die 2 Gruppen)\n",
#                              "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
#                              "Unterscheidet sich der Einfluss der Zeit/Intervention zwischen den beiden Gruppen fuer den ausgewaehlten trial?")
#     }else{
#       # 2. Vergleiche den longitudinalen Unterschied jedem trial1 und trial2
#       # ttest(trial1_zeitpunkt1-trial2_zeitpunkt1 vs. trial1_zeitpunkt2-trial2_zeitpunkt2)
#       # was ist der Unterschied in der Trialdifferenz zwischen dem ersten und dem 2. Zeitpunkt
#       if (!(estimate_time_first)){
#         d$explanation = "Beide Gruppen haben die selben Subjects mit unteschiedlichen Trials .... Ermittle zuerst die Unterschiede zwischen den Trials zu einem Zeitpunkt und teste dann auf signifikante Unterschiede zwischen den 2 Zeitpunkten "
#         d$data1 <- d1$data1-d1$data2
#         d$data2 <- d2$data1-d2$data2
#         d$df_data1 <- estimate_df_difference(d1$df_data1, d1$df_data2)
#         d$df_data2 <- estimate_df_difference(d2$df_data1, d2$df_data2)
#         d$explanation = paste0("Was wird hier berechnet? ...\n",
#                                "Gruppe ist identisch .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
#                                "Algorithmus Time first:\n",
#                                "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind\n",
#                                "2. Berechne X1 = Data_Zeitpunkt1_group1_task1 - Data_Zeitpunkt1_group2_task2 (Subjects x Regions x Regions)\n",
#                                "            X2 = Data_Zeitpunkt2_group2_task1 - Data_Zeitpunkt2_group2_task2 (Subjects x Regions x Regions)\n",
#                                "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen\n",
#                                "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)\n",
#                                "!!!wenn time first NICHT ausgewaehlt wird dann wird zuerst der Trialunterschied statt des Zeitunterschieds eines trials berechnet und dieser Unterschied anschliessen zwiechen den Zeitpunkten vergleichen\n",
#                                "3. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (paired fuer die 2 Gruppen)\n",
#                                "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
#                                "Der Unterschied zwischen trial1 und trial 2 wird von der Zeit/Intervention beeinflusst\n",
#                                "Nullhypothese: Zeit/Intervention haben keinen Einfluss auf den Unterschied zwischen trial_1 und trial_2\n")
#       }else{
#         #Wenn beide Gruppen gleich sind unterscheiden wir den FAll, dass
#         #  1 der Unterschied zwischen 2 Trials berechnet wird
#
#
#
#         # wenn die gruppen gleich sind, d.h. wenn wir nur 2 trials vergleichen
#         # in der gleichen Gruppe dann haben wir 2 Moeglichekeiten
#         # den longitudinalen unterschied zu berechnen
#         # 1. der longitudinale unterschied vom unterschied eines Zeitpunkts
#         # was ist der Unterschied zwischen dem Unterschied von Trial1 zwischen den beiden Messzeitpunkten
#         #     und Trial 2 zwischen den beiden MEsszeitpunkten
#         d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"
#         d$data1 <- d2$data1-d1$data1
#         d$data2 <- d2$data2-d1$data2
#         d$df_data1 <- estimate_df_difference(d2$df_data1, d1$df_data1)
#         d$df_data2 <- estimate_df_difference(d2$df_data2, d1$df_data2)
#         d$explanation = paste0("Was wird hier berechnet? ...\n",
#                                "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
#                                "Algorithmus:\n",
#                                "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind",
#                                "2. Berechne X1 = Data_Zeitpunkt2_group1_task1 - Data_Zeitpunkt1_group1_task1 (Subjects x Regions x Regions)",
#                                "            X2 = Data_Zeitpunkt2_group2_task1 - Data_Zeitpunkt1_group2_task1 (Subjects x Regions x Regions)",
#                                "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen",
#                                "   es wird somit zuerst der Gruppenunterschied berechnet und denn der Unterschied zwischen den wenn time first ausgewaehlt wird dann wird zuerst der Zeitunterschied berechnet und dieser dann zwischen den Messungen verglichen\n",
#                                "!!!wenn time first ausgewaehlt wird dann wird zuerst der Zeitunterschied berechnet und dieser dann zwischen den Messungen verglichen\n",
#                                "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)",
#                                "3. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (paired fuer die Matrizen welche die gleichen Subjects enthalten)",
#                                "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
#                                "Hypothese: Zeit/Intervention unterscheidet sich in ihrem Einfluss auf trial_1 vom Einfluss auf trial2?\n",
#                                "Nullhypothese: Der Einfluss von Zeit/Intervention ist fuer trial_1 und trial_2 gleich\n")
#       }
#     }
#
#  return(d)
# }




update_data_structure_by_longitudinal_data<-function(d1, datalong,  g1,g2,t1,t2, freq,
                                                     trials=g_trials(),
                                                     tbl_beh_long = NULL,
                                                     method = g_act_method(),
                                                     estimate_time_first = TRUE){



  #############################
  # longitudinale Daten
  #    if (is_debug){
  cat(file = stderr(),"\nlongitudinal data analyse start \n")
  cat(file = stderr(),paste0("g1 = ",g1, "   g2 = ", g2,"\n"))
  cat(file = stderr(),paste0("t1 = ",t1, "   t2 = ", t2,"\n"))
  cat(file = stderr(),paste0("freq = ",freq,"\n"))
  cat(file = stderr(),paste0("trials = ",trials,"\n"))
  cat(file = stderr(),paste0("method = ",method,"\n"))
  cat(file = stderr(),paste0("update_data_structure_by_longitudinal_data length(dim(data))= ",length(dim(data)),"\n"))
  cat(file = stderr(),paste0("update_data_structure_by_longitudinal_data dim(data)= ",dim(data),"\n"))
  cat(file = stderr(),paste0("update_data_structure_by_longitudinal_data length(dim(datalong))= ",length(dim(datalong)),"\n"))
  cat(file = stderr(),paste0("update_data_structure_by_longitudinal_data dim(datalong)= ",dim(datalong),"\n"))
  cat(file = stderr(),paste0("estimate time first = ", estimate_time_first,"\n"))

  #   }

  #Z127 <<- data
  #cat(file = stderr(),"1\n")
  d2 <- get_selected_data_considering_group_trial(datalong, g1,g2,t1,t2, freq, trials = trials, tbl_beh = tbl_beh_long, method = method)
  #cat(file = stderr(),"2\n")
  d <- d1
  glob_d1<<-d1
  glob_d2<<-d2
  glob_data<<-data
  glob_datalong<<-datalong
  # d1 und d2 sind die Daten der beiden Zeitpunkte mit jeweils
  # data1 and data2 sind 3D arrays
  # dim1 subjects ... koennen fuer data1 und data 2 die gleichen sein (d$paired = TRUE)
  #                           oder unterschiedliche subjects (d$paired = FALSE)
  # dim2 regions
  # dim3 regions
  #
  # wenn unterschiedliche Gruppen dann
  if (d1$g1!=d1$g2){
    #subtrahiere jeden Subject voneinander ... d2$data1 - d1$data1
    # Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten
    # Frage: gibt es einen signifikanten Unterschied zwischen diesen zeitbezogenen unterschieden?
    #d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"

    d$data1 <- d2$data1-d1$data1
    d$data2 <- d2$data2-d1$data2
    d$df_data1 <- estimate_df_difference(d2$df_data1, d1$df_data1)
    d$df_data2 <- estimate_df_difference(d2$df_data2, d1$df_data2)
    d$explanation = paste0("Was wird hier berechnet? ...\n",
                           "Gruppe ist verschieden ...beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
                           "Algorithmus:\n",
                           "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind\n",
                           "2. falls unterschiedliche trial gewaehlt wurden wird der subjectspezifische Unterschied zwischen den Trials berechnet\n",
                           "3. Berechne X1 = Data_Zeitpunkt2_group1_trial(evtl. dif) - Data_Zeitpunkt1_group1_trial(evtl. dif) (Subjects x Regions x Regions)\n",
                           "            X2 = Data_Zeitpunkt2_group2_trial(evtl. dif) - Data_Zeitpunkt1_group2_trial(evtl. dif) (Subjects x Regions x Regions)\n",
                           "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen\n",
                           "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)\n",
                           "4. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (unpaired fuer die 2 Gruppen)\n",
                           "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
                           "Unterscheidet sich der Einfluss der Zeit/Intervention zwischen den beiden Gruppen fuer den ausgewaehlten trial?")
  }else{
    #######################
    # Wenn gleiche Gruppe
    #
    # Wenn Gruppe und Trial gleich ist
    if ((d1$t1==d1$t2) ){
      cat(file=stderr(), "(d1$t1==d1$t2) && (d1$g1==d1$g2)\n")
      #estimation_performed <- estimation_performed +1
      d$explanation = "Gleiches Trial, Gleiche Gruppen, kein longitudinales Design, -> kein statistischer Test moeglich"
      d$data1 <- d1$data1
      d$data2 <- d2$data1
      d$df_data1 <- d1$df_data1
      d$df_data2 <- d2$df_data2
      d$explanation = paste0("Was wird hier berechnet? ...\n",
                             "Longitudinales Design mit gleichen Trials und gleichen Gruppen\n",
                             "Gleiches Trial, Gleiche Gruppen, kein longitudinales Design, -> kein statistischer Test moeglich\n",
                             "uebergebe fuer Gruppe 1 die Orginalwerte und Gruppe 2 die gleichen Orginalwerte\n")

    }else{
    # 2. Vergleiche den longitudinalen Unterschied jedem trial1 und trial2
    # ttest(trial1_zeitpunkt1-trial2_zeitpunkt1 vs. trial1_zeitpunkt2-trial2_zeitpunkt2)
    # was ist der Unterschied in der Trialdifferenz zwischen dem ersten und dem 2. Zeitpunkt
    if (!(estimate_time_first)){
      d$explanation = "Beide Gruppen haben die selben Subjects mit unteschiedlichen Trials .... Ermittle zuerst die Unterschiede zwischen den Trials zu einem Zeitpunkt und teste dann auf signifikante Unterschiede zwischen den 2 Zeitpunkten "
      d$data1 <- d1$data1-d1$data2
      d$data2 <- d2$data1-d2$data2
      d$df_data1 <- estimate_df_difference(d1$df_data1, d1$df_data2)
      d$df_data2 <- estimate_df_difference(d2$df_data1, d2$df_data2)
      d$explanation = paste0("Was wird hier berechnet? ...\n",
                             "Gruppe ist identisch .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
                             "Algorithmus Time first:\n",
                             "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind\n",
                             "2. Berechne X1 = Data_Zeitpunkt1_group1_task1 - Data_Zeitpunkt1_group2_task2 (Subjects x Regions x Regions)\n",
                             "            X2 = Data_Zeitpunkt2_group2_task1 - Data_Zeitpunkt2_group2_task2 (Subjects x Regions x Regions)\n",
                             "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen\n",
                             "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)\n",
                             "!!!wenn time first NICHT ausgewaehlt wird dann wird zuerst der Trialunterschied statt des Zeitunterschieds eines trials berechnet und dieser Unterschied anschliessen zwiechen den Zeitpunkten vergleichen\n",
                             "3. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (paired fuer die 2 Gruppen)\n",
                             "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
                             "Der Unterschied zwischen trial1 und trial 2 wird von der Zeit/Intervention beeinflusst\n",
                             "Nullhypothese: Zeit/Intervention haben keinen Einfluss auf den Unterschied zwischen trial_1 und trial_2\n")
    }else{
      #Wenn beide Gruppen gleich sind unterscheiden wir den FAll, dass
      #  1 der Unterschied zwischen 2 Trials berechnet wird



      # wenn die gruppen gleich sind, d.h. wenn wir nur 2 trials vergleichen
      # in der gleichen Gruppe dann haben wir 2 Moeglichekeiten
      # den longitudinalen unterschied zu berechnen
      # 1. der longitudinale unterschied vom unterschied eines Zeitpunkts
      # was ist der Unterschied zwischen dem Unterschied von Trial1 zwischen den beiden Messzeitpunkten
      #     und Trial 2 zwischen den beiden MEsszeitpunkten
      d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"
      d$data1 <- d2$data1-d1$data1
      d$data2 <- d2$data2-d1$data2
      d$df_data1 <- estimate_df_difference(d2$df_data1, d1$df_data1)
      d$df_data2 <- estimate_df_difference(d2$df_data2, d1$df_data2)
      d$explanation = paste0("Was wird hier berechnet? ...\n",
                             "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
                             "Algorithmus:\n",
                             "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind",
                             "2. Berechne X1 = Data_Zeitpunkt2_group1_task1 - Data_Zeitpunkt1_group1_task1 (Subjects x Regions x Regions)",
                             "            X2 = Data_Zeitpunkt2_group2_task1 - Data_Zeitpunkt1_group2_task1 (Subjects x Regions x Regions)",
                             "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen",
                             "   es wird somit zuerst der Gruppenunterschied berechnet und denn der Unterschied zwischen den wenn time first ausgewaehlt wird dann wird zuerst der Zeitunterschied berechnet und dieser dann zwischen den Messungen verglichen\n",
                             "!!!wenn time first ausgewaehlt wird dann wird zuerst der Zeitunterschied berechnet und dieser dann zwischen den Messungen verglichen\n",
                             "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)",
                             "3. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (paired fuer die Matrizen welche die gleichen Subjects enthalten)",
                             "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
                             "Hypothese: Zeit/Intervention unterscheidet sich in ihrem Einfluss auf trial_1 vom Einfluss auf trial2?\n",
                             "Nullhypothese: Der Einfluss von Zeit/Intervention ist fuer trial_1 und trial_2 gleich\n")
    }
  }
  }

  return(d)
}
#
#
# update_data_structure_by_longitudinal_data<-function(d1, datalong,  g1,g2,t1,t2, freq,
#                                                      trials=g_trials(),
#                                                      tbl_beh_long = NULL,
#                                                      method = g_act_method(),
#                                                      estimate_time_first = TRUE){
#
#
#
#   #############################
#   # longitudinale Daten
#   #    if (is_debug){
#   cat(file = stderr(),"\nlongitudinal data analyse start \n")
#   cat(file = stderr(),paste0("g1 = ",g1, "   g2 = ", g2,"\n"))
#   cat(file = stderr(),paste0("t1 = ",t1, "   t2 = ", t2,"\n"))
#   cat(file = stderr(),paste0("freq = ",freq,"\n"))
#   cat(file = stderr(),paste0("trials = ",trials,"\n"))
#   cat(file = stderr(),paste0("method = ",method,"\n"))
#   cat(file = stderr(),paste0("update_data_structure_by_longitudinal_data length(dim(data))= ",length(dim(data)),"\n"))
#   cat(file = stderr(),paste0("update_data_structure_by_longitudinal_data dim(data)= ",dim(data),"\n"))
#   cat(file = stderr(),paste0("update_data_structure_by_longitudinal_data length(dim(datalong))= ",length(dim(datalong)),"\n"))
#   cat(file = stderr(),paste0("update_data_structure_by_longitudinal_data dim(datalong)= ",dim(datalong),"\n"))
#   cat(file = stderr(),paste0("estimate time first = ", estimate_time_first,"\n"))
#
#   #   }
#
#   #Z127 <<- data
#   #cat(file = stderr(),"1\n")
#   d2 <- get_selected_data_considering_group_trial(datalong, g1,g2,t1,t2, freq, trials = trials, tbl_beh = tbl_beh_long, method = method)
#   #cat(file = stderr(),"2\n")
#   d <- d1
#   glob_d1<<-d1
#   glob_d2<<-d2
#   glob_data<<-data
#   glob_datalong<<-datalong
#   # d1 und d2 sind die Daten der beiden Zeitpunkte mit jeweils
#   # data1 and data2 sind 3D arrays
#   # dim1 subjects ... koennen fuer data1 und data 2 die gleichen sein (d$paired = TRUE)
#   #                           oder unterschiedliche subjects (d$paired = FALSE)
#   # dim2 regions
#   # dim3 regions
#   #
#   estimation_performed = 0
#   cat(file=stderr(), paste0("(d1$t1=",d1$t1, " d1$t2=", d1$t2, " d1$g1=", d1$g1, " d1$g2=", d1$g2, " d1$mypaired=", d1$my_paired,"\n"))
#
#   if ((d1$t1==d1$t2) && (d1$g1==d1$g2) && (d1$my_paired==TRUE)){
#     cat(file=stderr(), "(d1$t1==d1$t2) && (d1$g1==d1$g2) && (d1$my_paired==TRUE)\n")
#     estimation_performed <- estimation_performed +1
#     # wenn die gruppen gleich sind und die trials auch gleich sind vergleichen wir den Unterschied
#     # nur zwischen den 2 Zeitpunkten
#     d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden fuer den gleichen Trial"
#     d$data1 <- d1$data1
#     d$data2 <- d2$data1
#     d$df_data1 <- d1$df_data1
#     d$df_data2 <- d2$df_data2
#     d$explanation = paste0("Was wird hier berechnet? ...\n",
#                            "Longitudinales Design mit gleichen Trials und gleichen Gruppen\n",
#                            "Gruppe 1 besteht aus Orginalwerten des ersten Zeitpunkts\n",
#                            "Gruppe 2 besteht aus Orginalwerten des zweiten Zeitpunkts\n",
#                            "Diese Werte werden im Histogramm gezeigt\n",
#                            "Der statistische Test tested auf unterschiede zwischen den beiden Zeitpunkten mittels paired t-test\n",
#                            "Algorithmus:\n",
#                            "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind",
#                            "2. Berechne X1 = Data_Zeitpunkt1_group1_task1 (Subjects x Regions x Regions)",
#                            "            X2 = Data_Zeitpunkt2_group1_task1 (Subjects x Regions x Regions)",
#                            "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Werte eines Trials der gewaehlten Gruppe des gewaehlten Zeitpunkts, .. keine Differenzen",
#                            "  Die Option time - first hat hier keine Bedeutung\n",
#                            "3. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (paired fuer die Matrizen welche die gleichen Subjects enthalten)",
#                            "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
#                            "Hypothese: Gibt es einen Unterschied zwischen den beiden Zeitpunkten fuer die gewaehlte Region?\n",
#                            "Nullhypothese: Die Zeit hat keinen Der Einfluss\n")
#   }
#
#   if (d1$my_paired == TRUE){
#
#
#     if ((d1$t1==d1$t2) && (d1$g1==d1$g2) ){
#       cat(file=stderr(), "(d1$t1==d1$t2) && (d1$g1==d1$g2) && (d1$my_paired==FALSE)\n")
#       estimation_performed <- estimation_performed +1
#       d$explanation = "Gleiches Trial, Gleiche Gruppen, kein longitudinales Design, -> kein statistischer Test moeglich"
#       d$data1 <- d1$data1
#       d$data2 <- d2$data1
#       d$df_data1 <- d1$df_data1
#       d$df_data2 <- d2$df_data2
#       d$explanation = paste0("Was wird hier berechnet? ...\n",
#                              "Kein Longitudinales Design mit gleichen Trials und gleichen Gruppen\n",
#                              "Gleiches Trial, Gleiche Gruppen, kein longitudinales Design, -> kein statistischer Test moeglich\n",
#                              "uebergebe fuer Gruppe 1 die Orginalwerte und Gruppe 2 die gleichen Orginalwerte\n")
#
#       # 2. Vergleiche den longitudinalen Unterschied jedem trial1 und trial2
#       # ttest(trial1_zeitpunkt1-trial2_zeitpunkt1 vs. trial1_zeitpunkt2-trial2_zeitpunkt2)
#       # was ist der Unterschied in der Trialdifferenz zwischen dem ersten und dem 2. Zeitpunkt
#     }else{
#       if (!(estimate_time_first)){
#         d$explanation = "Beide Gruppen haben die selben Subjects mit unteschiedlichen Trials .... Ermittle zuerst die Unterschiede zwischen den Trials zu einem Zeitpunkt und teste dann auf signifikante Unterschiede zwischen den 2 Zeitpunkten "
#         d$data1 <- d1$data1-d1$data2
#         d$data2 <- d2$data1-d2$data2
#         d$df_data1 <- estimate_df_difference(d1$df_data1, d1$df_data2)
#         d$df_data2 <- estimate_df_difference(d2$df_data1, d2$df_data2)
#         d$explanation = paste0("Was wird hier berechnet? ...\n",
#                                "Gruppe ist identisch .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
#                                "Algorithmus Time first:\n",
#                                "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind\n",
#                                "2. Berechne X1 = Data_Zeitpunkt1_group1_task1 - Data_Zeitpunkt1_group2_task2 (Subjects x Regions x Regions)\n",
#                                "            X2 = Data_Zeitpunkt2_group2_task1 - Data_Zeitpunkt2_group2_task2 (Subjects x Regions x Regions)\n",
#                                "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen\n",
#                                "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)\n",
#                                "!!!wenn time first NICHT ausgewaehlt wird dann wird zuerst der Trialunterschied statt des Zeitunterschieds eines trials berechnet und dieser Unterschied anschliessen zwiechen den Zeitpunkten vergleichen\n",
#                                "3. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (paired fuer die 2 Gruppen)\n",
#                                "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
#                                "Der Unterschied zwischen trial1 und trial 2 wird von der Zeit/Intervention beeinflusst\n",
#                                "Nullhypothese: Zeit/Intervention haben keinen Einfluss auf den Unterschied zwischen trial_1 und trial_2\n")
#       }else{
#         #Wenn beide Gruppen gleich sind unterscheiden wir den FAll, dass
#         #  1 der Unterschied zwischen 2 Trials berechnet wird
#
#
#
#         # wenn die gruppen gleich sind, d.h. wenn wir nur 2 trials vergleichen
#         # in der gleichen Gruppe dann haben wir 2 Moeglichekeiten
#         # den longitudinalen unterschied zu berechnen
#         # 1. der longitudinale unterschied vom unterschied eines Zeitpunkts
#         # was ist der Unterschied zwischen dem Unterschied von Trial1 zwischen den beiden Messzeitpunkten
#         #     und Trial 2 zwischen den beiden MEsszeitpunkten
#         d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"
#         d$data1 <- d2$data1-d1$data1
#         d$data2 <- d2$data2-d1$data2
#         d$df_data1 <- estimate_df_difference(d2$df_data1, d1$df_data1)
#         d$df_data2 <- estimate_df_difference(d2$df_data2, d1$df_data2)
#         d$explanation = paste0("Was wird hier berechnet? ...\n",
#                                "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
#                                "Algorithmus:\n",
#                                "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind",
#                                "2. Berechne X1 = Data_Zeitpunkt2_group1_task1 - Data_Zeitpunkt1_group1_task1 (Subjects x Regions x Regions)",
#                                "            X2 = Data_Zeitpunkt2_group2_task1 - Data_Zeitpunkt1_group2_task1 (Subjects x Regions x Regions)",
#                                "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen",
#                                "   es wird somit zuerst der Gruppenunterschied berechnet und denn der Unterschied zwischen den wenn time first ausgewaehlt wird dann wird zuerst der Zeitunterschied berechnet und dieser dann zwischen den Messungen verglichen\n",
#                                "!!!wenn time first ausgewaehlt wird dann wird zuerst der Zeitunterschied berechnet und dieser dann zwischen den Messungen verglichen\n",
#                                "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)",
#                                "3. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (paired fuer die Matrizen welche die gleichen Subjects enthalten)",
#                                "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
#                                "Hypothese: Zeit/Intervention unterscheidet sich in ihrem Einfluss auf trial_1 vom Einfluss auf trial2?\n",
#                                "Nullhypothese: Der Einfluss von Zeit/Intervention ist fuer trial_1 und trial_2 gleich\n")
#       }
#
#     }
#     }
#   #
#   # if ((d1$t1!=d1$t2) && (d1$g1==d1$g2) && (d1$my_paired==TRUE)){
#   #   cat(file=stderr(), "(d1$t1!=d1$t2) && (d1$g1==d1$g2) && (d1$my_paired==TRUE)\n")
#   #   estimation_performed <- estimation_performed +1
#   #   # wenn die gruppen gleich sind aber die Trials unterschiedlich sind vergleichenwir diese Trials
#   #   # in der gleichen Gruppe dann haben wir 2 Moeglichekeiten
#   #   # den longitudinalen unterschied zu berechnen
#   #   # 1. der longitudinale unterschied vom unterschied eines Zeitpunkts
#   #   # was ist der Unterschied zwischen dem Unterschied von Trial1 zwischen den beiden Messzeitpunkten
#   #   #     und Trial 2 zwischen den beiden MEsszeitpunkten
#   #   d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"
#   #   d$data1 <- d2$data1-d1$data1
#   #   d$data2 <- d2$data2-d1$data2
#   #   d$df_data1 <- estimate_df_difference(d2$df_data1, d1$df_data1)
#   #   d$df_data2 <- estimate_df_difference(d2$df_data2, d1$df_data2)
#   #   d$explanation = paste0("Was wird hier berechnet? ...\n",
#   #                          "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
#   #                          "Algorithmus:\n",
#   #                          "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind",
#   #                          "2. Berechne X1 = Data_Zeitpunkt2_group1_task1 - Data_Zeitpunkt1_group1_task1 (Subjects x Regions x Regions)",
#   #                          "            X2 = Data_Zeitpunkt2_group2_task1 - Data_Zeitpunkt1_group2_task1 (Subjects x Regions x Regions)",
#   #                          "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen",
#   #                          "   es wird somit zuerst der Gruppenunterschied berechnet und denn der Unterschied zwischen den wenn time first ausgewaehlt wird dann wird zuerst der Zeitunterschied berechnet und dieser dann zwischen den Messungen verglichen\n",
#   #                          "!!!wenn time first ausgewaehlt wird dann wird zuerst der Zeitunterschied berechnet und dieser dann zwischen den Messungen verglichen\n",
#   #                          "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)",
#   #                          "3. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (paired fuer die Matrizen welche die gleichen Subjects enthalten)",
#   #                          "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
#   #                          "Hypothese: Zeit/Intervention unterscheidet sich in ihrem Einfluss auf trial_1 vom Einfluss auf trial2?\n",
#   #                          "Nullhypothese: Der Einfluss von Zeit/Intervention ist fuer trial_1 und trial_2 gleich\n")
#   #
#   # }
#
#   # if (!(estimate_time_first)){
#   #   d$explanation = "Beide Gruppen haben die selben Subjects mit unteschiedlichen Trials .... Ermittle zuerst die Unterschiede zwischen den Trials zu einem Zeitpunkt und teste dann auf signifikante Unterschiede zwischen den 2 Zeitpunkten "
#   #   d$data1 <- d1$data1-d1$data2
#   #   d$data2 <- d2$data1-d2$data2
#   #   d$df_data1 <- estimate_df_difference(d1$df_data1, d1$df_data2)
#   #   d$df_data2 <- estimate_df_difference(d2$df_data1, d2$df_data2)
#   #   d$explanation = paste0("Was wird hier berechnet? ...\n",
#   #                          "Gruppe ist identisch .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
#   #                          "Algorithmus Time first:\n",
#   #                          "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind\n",
#   #                          "2. Berechne X1 = Data_Zeitpunkt1_group1_task1 - Data_Zeitpunkt1_group2_task2 (Subjects x Regions x Regions)\n",
#   #                          "            X2 = Data_Zeitpunkt2_group2_task1 - Data_Zeitpunkt2_group2_task2 (Subjects x Regions x Regions)\n",
#   #                          "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen\n",
#   #                          "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)\n",
#   #                          "!!!wenn time first NICHT ausgewaehlt wird dann wird zuerst der Trialunterschied statt des Zeitunterschieds eines trials berechnet und dieser Unterschied anschliessen zwiechen den Zeitpunkten vergleichen\n",
#   #                          "3. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (paired fuer die 2 Gruppen)\n",
#   #                          "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
#   #                          "Der Unterschied zwischen trial1 und trial 2 wird von der Zeit/Intervention beeinflusst\n",
#   #                          "Nullhypothese: Zeit/Intervention haben keinen Einfluss auf den Unterschied zwischen trial_1 und trial_2\n")
#   # }else{
#
#   # if ((d1$t1==d1$t2) && (d1$g1!=d1$g2) && (d1$my_paired==TRUE)){
#   #   cat(file=stderr(), "(d1$t1==d1$t2) && (d1$g1!=d1$g2) && (d1$my_paired==TRUE)\n")
#   #   estimation_performed <- estimation_performed +1
#   #
#   # }
#   #
#   # if ((d1$t1!=d1$t2) && (d1$g1!=d1$g2) && (d1$my_paired==TRUE)){
#   #   cat(file=stderr(), "(d1$t1!=d1$t2) && (d1$g1!=d1$g2) && (d1$my_paired==TRUE)\n")
#   #   estimation_performed <- estimation_performed +1
#   #
#   # }
#
#
#
#
#   if (d1$my_paired==FALSE){
#
#     if ((d1$t1==d1$t2) && (d1$g1==d1$g2) ){
#       cat(file=stderr(), "(d1$t1==d1$t2) && (d1$g1==d1$g2) && (d1$my_paired==FALSE)\n")
#       estimation_performed <- estimation_performed +1
#       d$explanation = "Gleiches Trial, Gleiche Gruppen, kein longitudinales Design, -> kein statistischer Test moeglich"
#       d$data1 <- d1$data1
#       d$data2 <- d2$data1
#       d$df_data1 <- d1$df_data1
#       d$df_data2 <- d2$df_data2
#       d$explanation = paste0("Problem ... bei gleicher Gruppe sollte paired nicht FALSE sein PROBLEM\n",
#                              "Was wird hier berechnet? ...\n",
#                              "Longitudinales Design mit gleichen Trials und gleichen Gruppen\n",
#                              "Gleiches Trial, Gleiche Gruppen, kein longitudinales Design, -> kein statistischer Test moeglich\n",
#                              "uebergebe fuer Gruppe 1 die Orginalwerte und Gruppe 2 die gleichen Orginalwerte\n")
#     }else{
#
#       if ((d1$t1!=d1$t2) && (d1$g1==d1$g2) ){
#         cat(file=stderr(), "(d1$t1!=d1$t2) && (d1$g1==d1$g2) && (d1$my_paired==FALSE)\n")
#         cat(file=stderr(), paste0("(d1$t1=",d1$t1, " d1$t2=", d1$t2, " d1$g1=", d1$g1, " d1$g2=", d1$g2, " d1$mypaired=", d1$my_paired,"\n"))
#
#         #subtrahiere jeden Subject voneinander ... d2$data1 - d1$data1
#         # Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten
#         # Frage: gibt es einen signifikanten Unterschied zwischen diesen zeitbezogenen unterschieden?
#         #d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"
#
#         d$data1 <- d2$data1-d1$data1
#         d$data2 <- d2$data2-d1$data2
#         d$df_data1 <- estimate_df_difference(d2$df_data1, d1$df_data1)
#         d$df_data2 <- estimate_df_difference(d2$df_data2, d1$df_data2)
#         d$explanation = paste0("Problem ... bei gleicher Gruppe sollte paired nicht FALSE sein PROBLEM\n",
#                                "Was wird hier berechnet? ...\n",
#                                "Gruppe ist verschieden ...beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
#                                "Algorithmus:\n",
#                                "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind\n",
#                                "2. falls unterschiedliche trial gewaehlt wurden wird der subjectspezifische Unterschied zwischen den Trials berechnet\n",
#                                "3. Berechne X1 = Data_Zeitpunkt2_group1_trial(evtl. dif) - Data_Zeitpunkt1_group1_trial(evtl. dif) (Subjects x Regions x Regions)\n",
#                                "            X2 = Data_Zeitpunkt2_group2_trial(evtl. dif) - Data_Zeitpunkt1_group2_trial(evtl. dif) (Subjects x Regions x Regions)\n",
#                                "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen\n",
#                                "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)\n",
#                                "4. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (unpaired fuer die 2 Gruppen)\n",
#                                "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
#                                "Unterscheidet sich der Einfluss der Zeit/Intervention zwischen den beiden Gruppen fuer den ausgewaehlten trial?")
#
#
#       }
#       if ((d1$t1==d1$t2) && (d1$g1!=d1$g2) ){
#         cat(file=stderr(), "(d1$t1==d1$t2) && (d1$g1!=d1$g2) && (d1$my_paired==FALSE)\n")
#         cat(file=stderr(), paste0("(d1$t1=",d1$t1, " d1$t2=", d1$t2, " d1$g1=", d1$g1, " d1$g2=", d1$g2, " d1$mypaired=", d1$my_paired,"\n"))
#
#         #subtrahiere jeden Subject voneinander ... d2$data1 - d1$data1
#         # Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten
#         # Frage: gibt es einen signifikanten Unterschied zwischen diesen zeitbezogenen unterschieden?
#         #d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"
#
#         d$data1 <- d2$data1-d1$data1
#         d$data2 <- d2$data2-d1$data2
#         d$df_data1 <- estimate_df_difference(d2$df_data1, d1$df_data1)
#         d$df_data2 <- estimate_df_difference(d2$df_data2, d1$df_data2)
#         d$explanation = paste0("Was wird hier berechnet? ...\n",
#                                "Gruppe ist verschieden ...beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
#                                "Algorithmus:\n",
#                                "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind\n",
#                                "2. falls unterschiedliche trial gewaehlt wurden wird der subjectspezifische Unterschied zwischen den Trials berechnet\n",
#                                "3. Berechne X1 = Data_Zeitpunkt2_group1_trial(evtl. dif) - Data_Zeitpunkt1_group1_trial(evtl. dif) (Subjects x Regions x Regions)\n",
#                                "            X2 = Data_Zeitpunkt2_group2_trial(evtl. dif) - Data_Zeitpunkt1_group2_trial(evtl. dif) (Subjects x Regions x Regions)\n",
#                                "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen\n",
#                                "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)\n",
#                                "4. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (unpaired fuer die 2 Gruppen)\n",
#                                "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
#                                "Unterscheidet sich der Einfluss der Zeit/Intervention zwischen den beiden Gruppen fuer den ausgewaehlten trial?")
#
#
#       }
#       if ((d1$t1!=d1$t2) && (d1$g1==d1$g2) ){
#         cat(file=stderr(), "(d1$t1!=d1$t2) && (d1$g1==d1$g2) && (d1$my_paired==FALSE)\n")
#         cat(file=stderr(), paste0("(d1$t1=",d1$t1, " d1$t2=", d1$t2, " d1$g1=", d1$g1, " d1$g2=", d1$g2, " d1$mypaired=", d1$my_paired,"\n"))
#
#         #subtrahiere jeden Subject voneinander ... d2$data1 - d1$data1
#         # Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten
#         # Frage: gibt es einen signifikanten Unterschied zwischen diesen zeitbezogenen unterschieden?
#         #d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"
#
#         d$data1 <- d2$data1-d1$data1
#         d$data2 <- d2$data2-d1$data2
#         d$df_data1 <- estimate_df_difference(d2$df_data1, d1$df_data1)
#         d$df_data2 <- estimate_df_difference(d2$df_data2, d1$df_data2)
#         d$explanation = paste0("Was wird hier berechnet? ...\n",
#                                "Gruppe ist verschieden ...beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
#                                "Algorithmus:\n",
#                                "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind\n",
#                                "2. falls unterschiedliche trial gewaehlt wurden wird der subjectspezifische Unterschied zwischen den Trials berechnet\n",
#                                "3. Berechne X1 = Data_Zeitpunkt2_group1_trial(evtl. dif) - Data_Zeitpunkt1_group1_trial(evtl. dif) (Subjects x Regions x Regions)\n",
#                                "            X2 = Data_Zeitpunkt2_group2_trial(evtl. dif) - Data_Zeitpunkt1_group2_trial(evtl. dif) (Subjects x Regions x Regions)\n",
#                                "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen\n",
#                                "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)\n",
#                                "4. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (unpaired fuer die 2 Gruppen)\n",
#                                "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
#                                "Unterscheidet sich der Einfluss der Zeit/Intervention zwischen den beiden Gruppen fuer den ausgewaehlten trial?")
#
#
#       }
#     }
#     # if ((d1$t1==d1$t2) && (d1$g1!=d1$g2) && (d1$my_paired==FALSE)){
#     #   cat(file=stderr(), "(d1$t1==d1$t2) && (d1$g1!=d1$g2) && (d1$my_paired==FALSE)\n")
#     #
#     #   estimation_performed <- estimation_performed +1
#     #
#     # }
#     #
#     # if ((d1$t1!=d1$t2) && (d1$g1!=d1$g2) && (d1$my_paired==FALSE)){
#     #   cat(file=stderr(), "(d1$t1!=d1$t2) && (d1$g1!=d1$g2) && (d1$my_paired==FALSE)\n")
#     #   estimation_performed <- estimation_performed +1
#     #
#     # }
#
#
#
#
#
#     # wenn unterschiedliche Gruppen dann
#     #
#     #     if (d2$my_paired==FALSE){
#     #       #subtrahiere jeden Subject voneinander ... d2$data1 - d1$data1
#     #       # Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten
#     #       # Frage: gibt es einen signifikanten Unterschied zwischen diesen zeitbezogenen unterschieden?
#     #       #d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"
#     #
#     #       d$data1 <- d2$data1-d1$data1
#     #       d$data2 <- d2$data2-d1$data2
#     #       d$df_data1 <- estimate_df_difference(d2$df_data1, d1$df_data1)
#     #       d$df_data2 <- estimate_df_difference(d2$df_data2, d1$df_data2)
#     #       d$explanation = paste0("Was wird hier berechnet? ...\n",
#     #                              "Gruppe ist verschieden ...beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
#     #                              "Algorithmus:\n",
#     #                              "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind\n",
#     #                              "2. falls unterschiedliche trial gewaehlt wurden wird der subjectspezifische Unterschied zwischen den Trials berechnet\n",
#     #                              "3. Berechne X1 = Data_Zeitpunkt2_group1_trial(evtl. dif) - Data_Zeitpunkt1_group1_trial(evtl. dif) (Subjects x Regions x Regions)\n",
#     #                              "            X2 = Data_Zeitpunkt2_group2_trial(evtl. dif) - Data_Zeitpunkt1_group2_trial(evtl. dif) (Subjects x Regions x Regions)\n",
#     #                              "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen\n",
#     #                              "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)\n",
#     #                              "4. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (unpaired fuer die 2 Gruppen)\n",
#     #                              "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
#     #                              "Unterscheidet sich der Einfluss der Zeit/Intervention zwischen den beiden Gruppen fuer den ausgewaehlten trial?")
#     #     }else{
#     #       # 2. Vergleiche den longitudinalen Unterschied jedem trial1 und trial2
#     #       # ttest(trial1_zeitpunkt1-trial2_zeitpunkt1 vs. trial1_zeitpunkt2-trial2_zeitpunkt2)
#     #       # was ist der Unterschied in der Trialdifferenz zwischen dem ersten und dem 2. Zeitpunkt
#     #       if (!(estimate_time_first)){
#     #         d$explanation = "Beide Gruppen haben die selben Subjects mit unteschiedlichen Trials .... Ermittle zuerst die Unterschiede zwischen den Trials zu einem Zeitpunkt und teste dann auf signifikante Unterschiede zwischen den 2 Zeitpunkten "
#     #         d$data1 <- d1$data1-d1$data2
#     #         d$data2 <- d2$data1-d2$data2
#     #         d$df_data1 <- estimate_df_difference(d1$df_data1, d1$df_data2)
#     #         d$df_data2 <- estimate_df_difference(d2$df_data1, d2$df_data2)
#     #         d$explanation = paste0("Was wird hier berechnet? ...\n",
#     #                                "Gruppe ist identisch .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden\n",
#     #                                "Algorithmus Time first:\n",
#     #                                "1. entferne alle Subjects die nicht in den Daten beider Zeitpunkte zu finden sind\n",
#     #                                "2. Berechne X1 = Data_Zeitpunkt1_group1_task1 - Data_Zeitpunkt1_group2_task2 (Subjects x Regions x Regions)\n",
#     #                                "            X2 = Data_Zeitpunkt2_group2_task1 - Data_Zeitpunkt2_group2_task2 (Subjects x Regions x Regions)\n",
#     #                                "   In diesen beiden 3d Matrizen steht somit der gruppenspezifische Unterschied eines Trials zwischen den Messungen\n",
#     #                                "   Ein positiver Wert in dieser Matrix zeigt einen positiven Effekt der Zeit/Intervention an (in der 2. Messung groesser)\n",
#     #                                "!!!wenn time first NICHT ausgewaehlt wird dann wird zuerst der Trialunterschied statt des Zeitunterschieds eines trials berechnet und dieser Unterschied anschliessen zwiechen den Zeitpunkten vergleichen\n",
#     #                                "3. Es wird dann zwischen X1 und X2 ein t-test fuer jede Region gerechnet (paired fuer die 2 Gruppen)\n",
#     #                                "Jedes Feld der Corrplot Matrix im Bild testet somit auf folgende Hypothese\n",
#     #                                "Der Unterschied zwischen trial1 und trial 2 wird von der Zeit/Intervention beeinflusst\n",
#     #                                "Nullhypothese: Zeit/Intervention haben keinen Einfluss auf den Unterschied zwischen trial_1 und trial_2\n")
#     #       }else{
#     #         #Wenn beide Gruppen gleich sind unterscheiden wir den FAll, dass
#     #         #  1 der Unterschied zwischen 2 Trials berechnet wird
#     #
#     #         #  2 die trails gleich sind
#     #
#     #
#     #         #1
#     #
#     #         if (d$t1 != d$t2){
#     #
#     #         }
#     #       }
#   }
#
#   return(d)
# }

estimate_df_difference<-function(df1, df2){
  # Berchnung der Difference in den behavioralen Daten zwischen 2Dataframes
  # es wird angenommen, dass sie identische Dimensionen haben
  # anwendung fuer die longitudinalen Daten
  # d ist die Datenstruktur des Ursprungsdatensatzes an den die Anpassung erfolgen muss
  if (nrow(df1)!= nrow(df2)){
    cat(file = stderr(), paste0("!!!ERROR in estimate_df_dfference ... "))
    cat(file = stderr(), paste0("difference in nrwos detected  ... nrow(df1)=", nrow(df1), "   nrow(df2)=", nrow(df2),"\n"))
    return(NULL)
  }
  if (ncol(df1)!= ncol(df2)){
    cat(file = stderr(), paste0("!!!ERROR in estimate_df_dfference ... "))
    cat(file = stderr(), paste0("difference in ncols detected  ... ncol(df1)=", ncol(df1), "   ncol(df2)=", ncol(df2),"\n"))
    return(NULL)
  }
  is_col_numeric <- unlist(lapply(df1, is.numeric))
  df = df1
  for (i in 1:ncol(df1)){
    cur_colname <- colnames(df)[i]
    # Strings bzw. not numeric

    # nicht numerische koennen wir nicht mittelen ... daher fuegen wir zusammen
    if (!is_col_numeric[i]){
      df[,i] = paste0("diff(", df1[[cur_colname]], ",", df2[[cur_colname]],")")

      # for (j in 1:nrow(df1)){
      #   my_string <- paste0(my_string, df[[j,i]] ,"+")
      # }
      # my_string<-paste0(my_string,")")
      # dfm[1,i] <- my_string
    }else{
      # NUMERIC COLUMN
      df[,i] = df1[[cur_colname]] - df2[[cur_colname]]
#      df[,i] <- df1[,i] - df2[,i]
    }
  }
  return(df)
}

estimate_mat_t_p<- function(d, method = g_act_method(), regions = g_regions(),
                            p_cor_method = g_p_cor_method(),
                            sig_alpha = g_sig()){

  d$mat_p = matrix(data = NA, nrow = dim(d$data1)[2], ncol=dim(d$data1)[3])

  #d$mat_p = ones(dim(d$data1)[2], dim(d$data1)[3])
d$mat_t = zeros(dim(d$data1)[2], dim(d$data1)[3])

# berechne die mean difference um auszusagen welche Gruppe groesser ist
# bei positiven WErten ist data1 groesser, bei negativen data2
d$mat_mean_diff = apply(d$data1, c(2,3), function(x) mean(na.omit(x)))-apply(d$data2, c(2,3), function(x) mean(na.omit(x)))

colnames(d$mat_t) = regions
rownames(d$mat_t) = regions
d$color1 = colorRampPalette(c("blue","red","green"))

if (identical(d$data1, d$data2)){
  # wenn identisch dann handelt es sich um gleiche trial und group
  # hier zeigen wir dann nur die Coherence direct ohne tests
  d$mat_p = apply(d$data1, c(2,3), function(x) mean(na.omit(x)))
  d$mat_t = apply(d$data1, c(2,3), function(x) mean(na.omit(x)))
  return(d)
}
#cat(file = stderr(),paste0("get_currently_selected_data_long only filter the data duration =",Sys.time()-start_time,"\n"))


for (i in 1:(dim(d$data1)[2])-1){
#  start_idx = i+1
  start_idx = i # changed 26.03.2021 for diagonal elements
  if (method =="Granger" | method == "Transferentropy"){
    start_idx = 1
  }
  for (j in start_idx:(dim(d$data1)[3])){

    #if (!(i==j)){
      x <- na.omit(d$data1[,i,j])
      y <- na.omit(d$data2[,i,j])
      out<- tryCatch(
        {
          z = t.test(x,y, paired = d$my_paired)
          d$mat_p[i,j] = z$p.value
          d$mat_t[i,j] = z$statistic
        },
        error = function(cond){
          #cat(file = stderr(), paste0("error ttest estimation of in i=",i," j=",j,"\n"))
          #cat(file = stderr(), paste0("error message =",cond,"\n"))
          d$mat_p[i,j] = 1
          d$mat_t[i,j] = 0
        },
        warning= function(cond){
          #cat(file = stderr(), paste0("warning ttest estimation of in i=",i," j=",j,"\n"))
          #cat(file = stderr(), paste0("warning message =",cond,"\n"))
          d$mat_p[i,j] = 1
          d$mat_t[i,j] = 0
        })
    #}
  }
}

if(!(method=="Granger") && !(method=="Transferentropy")){
  lowerTriangle(d$mat_p) = upperTriangle(d$mat_p, byrow=TRUE)
  lowerTriangle(d$mat_t) = upperTriangle(d$mat_t, byrow=TRUE)

}

# das kann ich machen weil bei nicht directen Methoden die Elemente auf NA stehen
d$mat_p <- matrix(p.adjust(d$mat_p, method = p_cor_method),nrow=dim(d$data1)[2])
# setze nun die Diagonalelemente auf 1
diag(d$mat_p)<-1
colnames(d$mat_p) = regions
rownames(d$mat_p) = regions
# ergaenze noch eine Matrix fuer die Schrittweise Berechnung der Significanzkorrektur

# d$mat_sig = estimate_p_value_cor_matrix(mat_p = mat_p, regions = regions, p_cor_method = p_cor_method,
#                                         p_cor_num = p_cor_num, sig_alpha = sig_alpha)

return(d)
}



# aus der P-Matrix wird eine p-Wert Korrektur berechnet als bool
# gleiche Dimension wie die P-matrix
# estimate_p_value_cor_matrix <- function(mat_p = NULL, regions = g_regions(),
#                                         p_cor_method = g_p_cor_method(),
#                                         p_cor_num = g_p_cor_num(),
#                                         sig_alpha = g_sig()){
#   # anlegen der Significanzmatrix
#   mat_sig <- matrix(rep(F,dim(mat_p)[1]*dim(mat_p)[2]),ncol = dim(mat_p)[2])
#
#
#   return(mat_sig)
# }





get_included_subjects<-function(all_ids, ids_to_exclude){
  # creates a bool vector with false for those subjects
  # which have a pattern common with ids_to_exclude
  #cat(file = stderr(), paste0("class(ids_to_exclude = ", class(ids_to_exclude), "\n"))
  #cat(file = stderr(), paste0("length(ids_to_exclude = ", length(ids_to_exclude), "\n"))
  #cat(file = stderr(), paste0("all_ids = ", all_ids, "\n"))
  #cat(file = stderr(), paste0("get_included_subjects ids_to_exclude = ", ids_to_exclude, "\n"))

  all_ids_bool =  !logical(length= length(all_ids))
  #cat(file = stderr(), paste("all_ids_bool = ", all_ids_bool, "\n"))
  if (is.null(ids_to_exclude)){
    #cat(file = stderr(), "return because ids_to_exclude is null\n")
    return(all_ids_bool)
  }

  if (length(ids_to_exclude)==0){
    #cat(file = stderr(), "length(ids_to_exlude = 0)\n")
  }else{
    for (i in 1:length(ids_to_exclude)){
      cat(file = stderr(), paste0("in for loop i=", i,"\n"))
      all_ids_bool = all_ids_bool & !str_detect(all_ids, ids_to_exclude[i])
    }
  }

  gall_ids <<- all_ids
  gids_to_exclude <<- ids_to_exclude



  #cat(file = stderr(), paste("all_ids_bool before return= ", all_ids_bool, "\n"))

  return(all_ids_bool)
}


get_excluded_subjects<-function(all_ids, ids_to_exclude){
  # creates a bool vector with false for those subjects
  # which have a pattern common with ids_to_exclude
  #cat(file = stderr(), paste0("class(ids_to_exclude = ", class(ids_to_exclude), "\n"))
  #cat(file = stderr(), paste0("length(ids_to_exclude = ", length(ids_to_exclude), "\n"))
  #cat(file = stderr(), paste0("all_ids = ", all_ids, "\n"))
  #cat(file = stderr(), paste0("ids_to_exclude = ", ids_to_exclude, "\n"))

  all_ids_bool =  logical(length= length(all_ids))
  if (is.null(ids_to_exclude)){ return(all_ids_bool)}

  for (i in 1:length(ids_to_exclude)){
    all_ids_bool = all_ids_bool | str_detect(all_ids, ids_to_exclude[i])
  }
  return(all_ids_bool)
}
