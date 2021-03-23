#
# uregion_list = readRDS("../data/uregion_list.Rda")
# utrial_list = readRDS("../data/utrial_list.Rda")
# ufreq_list = readRDS("../data/ufreq_list.Rda")
# tbl_beh = readRDS("../data/tbl_beh.Rda")
library(abind)

create_my_ttest_string <- function(z, paired = FALSE, mean1 = 0, mean2 = 0,
                                   sig_threshold = g_sig(),
                                   p_cor_method = g_p_cor_method(),
                                   p_cor_num = g_p_cor_num()){
  t = z$statistic
  df = z$parameter
  r = sqrt((t^2)/((t^2)+df))
  if (paired){
    meanstring <- paste0("mean of difference = ", round(z$estimate[1],3)," \nmean= ", round(mean1,3), " vs. ", round(mean2,3)," \n")
  }else{
    meanstring <- paste0("mean= ", round(z$estimate[1],3), " vs. ", round(z$estimate[2],3)," \n")
  }

  out <- paste0(z$method,"\n\n",
                meanstring,
                "t=", round(z$statistic,2), " \n",
                "p=", z$p.value, "  \n",
                "df=", round(z$parameter,1),"  \n",
                "CI(",attributes(z$conf.int),")= ",round(z$conf.int[1],3)," ; ",round(z$conf.int[2],3)," \n",
                "effect size r = ", round(r,4), "\n",
                "r = [sqrt((t^2)/((t^2)+df))]", "\n",
                "FWE error rate (",p_cor_num ," comparisons) = ", 1-(1-sig_threshold)**p_cor_num,"\n",
                "FWE p critical = ",sig_threshold/p_cor_num, "\n",
                "Holm p critical = ",sig_threshold/p_cor_num, "\n"



  )

  return(out)
}


estimate_p_by_cor_method<-function(p_cor_method = "uncor.", p_cor_num=1, alpha= 0.05){
  switch(p_cor_method,
         "uncor." = {
           cat(file = stderr(), "uncor.\n")
           return(alpha)
         },
         "FWE" = {
           cat(file = stderr(), "FWE\n")
           return(alpha/p_cor_num)
         },
         "FDR" = {
           cat(file = stderr(), "FDR\n")
           p_fdr = alpha
           return(p_fdr)
         },
         "Holm" = {
           cat(file = stderr(), "Holm\n")
           p_holm = alpha
           return(p_holm)
         },
         "Benjamin-Hochberg" = {
           cat(file = stderr(), "Benjamin-Hochberg\n")
           p_benj = alpha
           return(p_benj)
         }
  )
  return(alpha)
}

my_lexical_sort <- function(x) {
  as.numeric(sort(as.character(x)))

}

get_longitudinal_currently_selected_data<-function(D1, D2, g1, g2, t1, t2, freq,
                                                   trials=g_trials(),
                                                   regions=g_regions(),
                                                   tbl_beh = g_beh(),
                                                   method = g_act_method(),
                                                   estimate_time_first = TRUE){

  cat(file = stderr(),paste0("Deprecated function of get_longitudinal_currently_selected_data ...\n",
                             "better to use split first and than get_currently_selected_data_long\n"))
  # generierung von t- und p-Maps fuer longitudinale Daten
  # Es muss insbesondere die Gruppenintegritaet ueberprueft werden, da
  # sich die Gruppen zwischen den beiden Messungen unterscheiden werden durch drop outs
  homogen_group_data = exclude_data_from_not_reoccuring_subjects(D1,D2)
  HD1 <- homogen_group_data$D1
  HD2 <- homogen_group_data$D2
  glob_HD1 <<-HD1
  glob_HD2 <<-HD2

  cat(file = stderr(), "now get the datalong\n")
  d = get_currently_selected_data_long(HD1$mdat, g1, g2, t1, t2, freq,
                                       trials=HD1$utrial_list,
                                       regions=HD1$uregion_list,
                                       tbl_beh = HD1$df_BD,
                                       method = method,
                                       datalong = HD2$mdat,
                                       estimate_time_first= estimate_time_first)

  return(d)
}



get_currently_selected_data_long2<-function(D, g1, g2, t1, t2, freq,
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
                                           filter_g2 = ""){

  # die funktion gibt eine LIste von mehreren Variablen zurueck
  # gedacht fuer tabs in denen gruppen und trials ausgewaehlt werden
  # d$mypaired ... bool handelt es sich um einen gepaarten oder ungeparrten test
  # d$data1 ... 3D matrix ... der gruppe 1 bzw trial 1 (subjects x region1 x region2 )
  # d$data2 ... 3D matrix ... der gruppe 2 bzw trial 2
  # d$mat_p ... 2D matrix ... p Werte des t-tests ueber alle Regionen
  # d$mat_t ... 2D matrix ... t Werte des t-tests ueber alle Regionen
  # d$string1 ... string .... eine beschreibung des durchgefuehrten Vergleiches
  # d$color1 ... col ........ die Color palette die zu den Werten passen

  # wenn moeglich dann teile in longitudinale Daten auf

  cat(file = stderr(), paste0("get_currently_selected_data_long2 ist deprecated !!!!\n",
                              "!!!!!!!    DEPRECATED    !!!!!!!n"))

  S<- split_data_by_longitudinal_info(D, long_def1, long_def2,
                                  is_exclude_not_reoccuring_subj = is_exclude_not_reoccuring_subj,
                                  averagelong = averagelong)

  data <- S$D1$mdat
  tbl_beh = S$D1$df_BD

  datalong <- S$D2$mdat


  start_time <- Sys.time()
  d <- list()
  d$explanation = "not filled"
  d$my_paired = FALSE


  if (is.null(datalong)){
    ###############################
    # nicht longitudinale Daten
    if ((! t1 == t2) && (!g1==g2)){
      d$string1 = paste0("Compare the in group diff of ", t1," vs ", t2, "between groups", "\n",
                         "unpaired t-test\n")
      d$data1 = get_data_group_trial_freqmean(data,g1, t1, freq, tbl_beh = tbl_beh, method=method)-get_data_group_trial_freqmean(data,g1, t2, freq, tbl_beh = tbl_beh, method=method)
      d$data2 = get_data_group_trial_freqmean(data,g2, t1, freq, tbl_beh = tbl_beh, method=method)-get_data_group_trial_freqmean(data,g2, t2, freq, tbl_beh = tbl_beh, method=method)
      d$my_paired = FALSE

    }else{
      d$data1 = get_data_group_trial_freqmean(data,g1, t1, freq, tbl_beh = tbl_beh, method=method)
      d$data2 = get_data_group_trial_freqmean(data,g2, t2, freq, tbl_beh = tbl_beh, method=method)
      if (t1 == t2) {
        d$string1 = paste0(g1," vs ", g2, " in trial ", trials[t1], "\n",
                           "independent t-test\n")
        # d$string1 = paste0(g1," vs ", g2, " in trial ", g_trials()[t1], "\n",
        #                    "independent t-test\n")
      }
      if (g1 == g2){
        d$string1 = paste0(t1," vs ", t2, "in group ", g1, "\n paired t-test\n")
        d$my_paired = TRUE
      }
    }

  }else{
    #############################
    # longitudinale Daten
    #    if (is_debug){
     cat(file = stderr(),"\nlongitudinal data analyse start \n")
     cat(file = stderr(),paste0("g1 = ",g1, "   g2 = ", g2,"\n"))
     cat(file = stderr(),paste0("t1 = ",t1, "   t2 = ", t2,"\n"))
     cat(file = stderr(),paste0("freq = ",freq,"\n"))
     cat(file = stderr(),paste0("trials = ",trials,"\n"))
     cat(file = stderr(),paste0("method = ",method,"\n"))
     cat(file = stderr(),paste0("get_currently_selected_data_long2 length(dim(data))= ",length(dim(data)),"\n"))
     cat(file = stderr(),paste0("get_currently_selected_data_long2 dim(data)= ",dim(data),"\n"))
     cat(file = stderr(),paste0("get_currently_selected_data_long2 length(dim(datalong))= ",length(dim(datalong)),"\n"))
     cat(file = stderr(),paste0("get_currently_selected_data_long2 dim(datalong)= ",dim(datalong),"\n"))
#     cat(file = stderr(),dim(data))

    #   }
    tbl_beh_long = S$D2$df_BD

    #Z127 <<- data
    method = "Coherence"
    d1 <- get_selected_data_considering_group_trial(data, g1,g2,t1,t2, freq,  trials = trials, tbl_beh = tbl_beh, method = method)
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
    if (d2$my_paired==FALSE){
      #subtrahiere jeden Subject voneinander ... d2$data1 - d1$data1
      # Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten
      # Frage: gibt es einen signifikanten Unterschied zwischen diesen zeitbezogenen unterschieden?
      #d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"
      d$data1 <- d2$data1-d1$data1
      d$data2 <- d2$data2-d1$data2
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
      # 2. Vergleiche den longitudinalen Unterschied jedem trial1 und trial2
      # ttest(trial1_zeitpunkt1-trial2_zeitpunkt1 vs. trial1_zeitpunkt2-trial2_zeitpunkt2)
      # was ist der Unterschied in der Trialdifferenz zwischen dem ersten und dem 2. Zeitpunkt
      if (!(estimate_time_first)){
        d$explanation = "Beide Gruppen haben die selben Subjects mit unteschiedlichen Trials .... Ermittle zuerst die Unterschiede zwischen den Trials zu einem Zeitpunkt und teste dann auf signifikante Unterschiede zwischen den 2 Zeitpunkten "
        d$data1 <- d1$data1-d1$data2
        d$data2 <- d2$data1-d2$data2
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
        # wenn die gruppen gleich sind, d.h. wenn wir nur 2 trials vergleichen
        # in der gleichen Gruppe dann haben wir 2 Moeglichekeiten
        # den longitudinalen unterschied zu berechnen
        # 1. der longitudinale unterschied vom unterschied eines Zeitpunkts
        # was ist der Unterschied zwischen dem Unterschied von Trial1 zwischen den beiden Messzeitpunkten
        #     und Trial 2 zwischen den beiden MEsszeitpunkten
        d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"
        d$data1 <- d2$data1-d1$data1
        d$data2 <- d2$data2-d1$data2
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
  #####################
  # d$data1 und d$data2 sind erhoben


  cat(file = stderr(), paste0("dim(d$data1) = ", dim(d$data1),"\n"))
  cat(file = stderr(), paste0("dim(d$data2) = ", dim(d$data2),"\n"))
  #d$mat_p = matrix(data=NA, nrow=dim(d$data1)[2], ncol=dim(d$data1)[3])
  #d$mat_t = matrix(data=NA, nrow=dim(d$data1)[2], ncol=dim(d$data1)[3])
  d$mat_p = ones(dim(d$data1)[2], dim(d$data1)[3])
  d$mat_t = zeros(dim(d$data1)[2], dim(d$data1)[3])

  d$color1 = colorRampPalette(c("blue","red","green"))
  #cat(file = stderr(),paste0("get_currently_selected_data_long only filter the data duration =",Sys.time()-start_time,"\n"))

  gd_get_long_data <<- d
  #cat(file = stderr(), "entering for loop ... now \n")
  for (i in 1:(dim(d$data1)[2])-1){
    start_idx = i+1
    if (method =="Granger"){
      start_idx = 1
    }
    for (j in start_idx:(dim(d$data1)[3])){
      if (!(i==j)){
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
      }
    }
  }

  if(!(method=="Granger")){
    lowerTriangle(d$mat_p) = upperTriangle(d$mat_p, byrow=TRUE)
    lowerTriangle(d$mat_t) = upperTriangle(d$mat_t, byrow=TRUE)

  }

  colnames(d$mat_p) = regions
  rownames(d$mat_p) = regions
  colnames(d$mat_t) = regions
  rownames(d$mat_t) = regions
  cat(file = stderr(),paste0("get_currently_selected_datalong duration =",Sys.time()-start_time,"\n"))
  gd_get_long_data <<- d
  return(d)
}




get_currently_selected_data_long<-function(data, g1, g2, t1, t2, freq,
                                           trials=g_trials(),
                                           regions=g_regions(),
                                           tbl_beh = g_beh(),
                                           method = g_act_method(),
                                           datalong = NULL,
                                           tbl_beh_long = NULL,
                                           estimate_time_first = TRUE){
  # die funktion gibt eine LIste von mehreren Variablen zurueck
  # gedacht fuer tabs in denen gruppen und trials ausgewaehlt werden
  # d$mypaired ... bool handelt es sich um einen gepaarten oder ungeparrten test
  # d$data1 ... 3D matrix ... der gruppe 1 bzw trial 1 (subjects x region1 x region2 )
  # d$data2 ... 3D matrix ... der gruppe 2 bzw trial 2
  # d$mat_p ... 2D matrix ... p Werte des t-tests ueber alle Regionen
  # d$mat_t ... 2D matrix ... t Werte des t-tests ueber alle Regionen
  # d$string1 ... string .... eine beschreibung des durchgefuehrten Vergleiches
  # d$color1 ... col ........ die Color palette die zu den Werten passen

  start_time <- Sys.time()
  d <- list()
  d$explanation = "not filled"
  d$my_paired = FALSE


  if (is.null(datalong)){
    ###############################
    # nicht longitudinale Daten
    if ((! t1 == t2) && (!g1==g2)){
      d$string1 = paste0("Compare the in group diff of ", t1," vs ", t2, "between groups", "\n",
                         "unpaired t-test\n")
      d$data1 = get_data_group_trial_freqmean(data,g1, t1, freq, tbl_beh = tbl_beh, method=method)-get_data_group_trial_freqmean(data,g1, t2, freq, tbl_beh = tbl_beh, method=method)
      d$data2 = get_data_group_trial_freqmean(data,g2, t1, freq, tbl_beh = tbl_beh, method=method)-get_data_group_trial_freqmean(data,g2, t2, freq, tbl_beh = tbl_beh, method=method)
      d$my_paired = FALSE

    }else{
      d$data1 = get_data_group_trial_freqmean(data,g1, t1, freq, tbl_beh = tbl_beh, method=method)
      d$data2 = get_data_group_trial_freqmean(data,g2, t2, freq, tbl_beh = tbl_beh, method=method)
      if (t1 == t2) {
        d$string1 = paste0(g1," vs ", g2, " in trial ", trials[t1], "\n",
                           "independent t-test\n")
        # d$string1 = paste0(g1," vs ", g2, " in trial ", g_trials()[t1], "\n",
        #                    "independent t-test\n")
      }
      if (g1 == g2){
        d$string1 = paste0(t1," vs ", t2, "in group ", g1, "\n paired t-test\n")
        d$my_paired = TRUE
      }
    }

  }else{
    #############################
    # longitudinale Daten
#    if (is_debug){
    # cat(file = stderr(),"\nlongitudinal data analyse start \n")
    # cat(file = stderr(),paste0("g1 = ",g1, "   g2 = ", g2,"\n"))
    # cat(file = stderr(),paste0("t1 = ",t1, "   t2 = ", t2,"\n"))
    # cat(file = stderr(),paste0("freq = ",freq,"\n"))
    # cat(file = stderr(),paste0("trials = ",trials,"\n"))
    # cat(file = stderr(),paste0("method = ",method,"\n"))
    # cat(file = stderr(),paste0("get_currently_selected_data_long length(dim(data))= ",length(dim(data)),"\n"))
    # cat(file = stderr(),paste0("get_currently_selected_data_long dim(data)= ",dim(data),"\n"))
    # cat(file = stderr(),dim(data))

    #   }
    #Z127 <<- data
    d1 <- get_selected_data_considering_group_trial(data, g1,g2,t1,t2, freq,  trials = trials, tbl_beh = tbl_beh, method = method)
    #cat(file = stderr(),"1\n")
    d2 <- get_selected_data_considering_group_trial(datalong, g1,g2,t1,t2, freq, trials = trials, tbl_beh = tbl_beh_long, method = method)
    #cat(file = stderr(),"2\n")
    d <- d1
    glob_d1<<-d1
    glob_d2<<-d2
    # d1 und d2 sind die Daten der beiden Zeitpunkte mit jeweils
    # data1 and data2 sind 3D arrays
    # dim1 subjects ... koennen fuer data1 und data 2 die gleichen sein (d$paired = TRUE)
    #                           oder unterschiedliche subjects (d$paired = FALSE)
    # dim2 regions
    # dim3 regions
    #
    # wenn unterschiedliche Gruppen dann
    if (d2$my_paired==FALSE){
      #subtrahiere jeden Subject voneinander ... d2$data1 - d1$data1
      # Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten
      # Frage: gibt es einen signifikanten Unterschied zwischen diesen zeitbezogenen unterschieden?
      #d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"
      d$data1 <- d2$data1-d1$data1
      d$data2 <- d2$data2-d1$data2
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
      # 2. Vergleiche den longitudinalen Unterschied jedem trial1 und trial2
      # ttest(trial1_zeitpunkt1-trial2_zeitpunkt1 vs. trial1_zeitpunkt2-trial2_zeitpunkt2)
      # was ist der Unterschied in der Trialdifferenz zwischen dem ersten und dem 2. Zeitpunkt
      if (!(estimate_time_first)){
      d$explanation = "Beide Gruppen haben die selben Subjects mit unteschiedlichen Trials .... Ermittle zuerst die Unterschiede zwischen den Trials zu einem Zeitpunkt und teste dann auf signifikante Unterschiede zwischen den 2 Zeitpunkten "
      d$data1 <- d1$data1-d1$data2
      d$data2 <- d2$data1-d2$data2
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
      # wenn die gruppen gleich sind, d.h. wenn wir nur 2 trials vergleichen
      # in der gleichen Gruppe dann haben wir 2 Moeglichekeiten
      # den longitudinalen unterschied zu berechnen
      # 1. der longitudinale unterschied vom unterschied eines Zeitpunkts
      # was ist der Unterschied zwischen dem Unterschied von Trial1 zwischen den beiden Messzeitpunkten
      #     und Trial 2 zwischen den beiden MEsszeitpunkten
      d$explanation = "Beide Gruppen haben unterschiede zwischen den 2 Zeitpunkten .... Die Analyse testet auf signifikante Unterschiede zwischen diesen zeitbezogenen Unterschieden"
      d$data1 <- d2$data1-d1$data1
      d$data2 <- d2$data2-d1$data2
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
    #####################
    # d$data1 und d$data2 sind erhoben


    #cat(file = stderr(), paste0("dim(d$data1) = ", dim(d$data1),"\n"))
    #cat(file = stderr(), paste0("dim(d$data2) = ", dim(d$data2),"\n"))
    #d$mat_p = matrix(data=NA, nrow=dim(d$data1)[2], ncol=dim(d$data1)[3])
    #d$mat_t = matrix(data=NA, nrow=dim(d$data1)[2], ncol=dim(d$data1)[3])
    d$mat_p = ones(dim(d$data1)[2], dim(d$data1)[3])
    d$mat_t = zeros(dim(d$data1)[2], dim(d$data1)[3])

    d$color1 = colorRampPalette(c("blue","red","green"))
    #cat(file = stderr(),paste0("get_currently_selected_data_long only filter the data duration =",Sys.time()-start_time,"\n"))


    #cat(file = stderr(), "entering for loop ... now \n")
    for (i in 1:(dim(d$data1)[2])-1){
      start_idx = i+1
      if (method =="Granger"){
        start_idx = 1
      }
      for (j in start_idx:(dim(d$data1)[3])){
        if (!(i==j)){
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
        }
      }
    }

    if(!(method=="Granger")){
      lowerTriangle(d$mat_p) = upperTriangle(d$mat_p, byrow=TRUE)
      lowerTriangle(d$mat_t) = upperTriangle(d$mat_t, byrow=TRUE)

    }

  colnames(d$mat_p) = regions
  rownames(d$mat_p) = regions
  colnames(d$mat_t) = regions
  rownames(d$mat_t) = regions
  cat(file = stderr(),paste0("get_currently_selected_datalong duration =",Sys.time()-start_time,"\n"))
  return(d)
}


get_selected_data_considering_group_trial<-function(data, g1,g2,t1,t2, freq, trials=g_trials(), tbl_beh = g_beh(), method = g_act_method() ){
  d <- list()
  d$my_paired = FALSE

  #cat(file = stderr(), paste0("get_selected_data_considering_group_trial \n"))
  #cat(file = stderr(), paste0("g1 = ", g1, "  g2 = ",g2, "  t1 = ", t1, "  t2 = ", t2, "\n"))
  d$explanation = "not filled"
  g_tbl_beh_get_selected <<- tbl_beh
  d$df_data1 =  tbl_beh %>% filter(tbl_beh$Gruppe == g1)
  d$df_data2 =  tbl_beh %>% filter(tbl_beh$Gruppe == g2)
  if ((! t1 == t2) && (!g1==g2)){
    d$string1 = paste0("Compare the in group diff of ", t1," vs ", t2, "between groups", "\n",
                       "unpaired t-test\n")
    d$explanation = paste0("Compare the in group diff of ", t1," vs ", t2, "between group ", g1, " and ", g2, "\n",
                           "unpaired t-test\n")
    d$data1 = get_data_group_trial_freqmean(data,g1, t1, freq, tbl_beh = tbl_beh, method=method)-get_data_group_trial_freqmean(data,g1, t2, freq, tbl_beh = tbl_beh, method=method)
    d$data2 = get_data_group_trial_freqmean(data,g2, t1, freq, tbl_beh = tbl_beh, method=method)-get_data_group_trial_freqmean(data,g2, t2, freq, tbl_beh = tbl_beh, method=method)

  }else{
    Z281<<-data
    d$data1 = get_data_group_trial_freqmean(data,g1, t1, freq, tbl_beh = tbl_beh, method=method)
    d$data2 = get_data_group_trial_freqmean(data,g2, t2, freq, tbl_beh = tbl_beh, method=method)

    if ((t1 == t2) && (g1 == g2)){
      d$string1 =  paste0("same trial, same group, not longitudinal .. group ", g1, " in trial ", trials[t1], "\n",
                          " no statistical tests\n")
      d$explanation = paste0("same trial, same group, not longitudinal .. group ", g1, " in trial ", trials[t1], "\n",
                             " no statistical tests\n")

    }else{

    if (t1 == t2) {
      d$string1 = paste0(g1," vs ", g2, " in trial ", trials[t1], "\n",
                         "independent t-test\n")
      d$explanation = paste0("same trial not longitudinal... Compare the same trial between 2 groups: ", g1," vs ", g2, " in trial ", trials[t1], "\n",
                         "independent t-test\n")

      # d$string1 = paste0(g1," vs ", g2, " in trial ", g_trials()[t1], "\n",
      #                    "independent t-test\n")
    }
    if (g1 == g2){
      #cat(file = stderr(), "g1 == g2\n")
      d$string1 = paste0(t1," vs ", t2, "in group ", g1, "\n paired t-test\n")
      d$explanation = paste0("same group different trial not longitudinal... Compare the same group between 2 trials ", t1," vs ", t2, " in group ", g1, "\n",
                             "paired t-testindependent t-test\n")
      d$my_paired = TRUE
    }
    }
  }
  return(d)
}


exclude_data_from_not_reoccuring_subjects<-function(D1,D2){
  # es werden nur subjects uebernommen die in D1 und in D2 vorkommen
  # teste zuerst ob die beiden Datensaetze auch wirklich identische Eigenschaften haben
  if (is.null(D1) | is.null(D2)){
    return(list(D1=D1, D2=D2))
  }
  if (!(identical(D1$method, D2$method))){
    cat(file = stderr(), paste0("selected longitudinal data have not the same method description (", D1$method,"!=",D2$method,")\n"))
  }
  if (!(identical(D1$ugroup_list, D2$ugroup_list))){
    cat(file = stderr(), paste0("selected longitudinal data have not the same ugroup_list description (", D1$ugroup_list,"!=",D2$ugroup_list,")\n"))
  }
  if (!(identical(D1$uregion_list, D2$uregion_list))){
    cat(file = stderr(), paste0("selected longitudinal data have not the same region list description (", D1$uregion_list,"!=",D2$uregion_list,")\n"))
  }
  if (!(identical(D1$ufreq_list, D2$ufreq_list))){
    cat(file = stderr(), paste0("selected longitudinal data have not the same ufreq_list description (", D1$ufreq_list,"!=",D2$ufreq_list,")\n"))
  }
  if (!(identical(D1$dimcontent, D2$dimcontent))){
    cat(file = stderr(), paste0("selected longitudinal data have not the same dimcontent description (", D1$dimcontent,"!=",D2$dimcontent,")\n"))
  }

  ids1 = sapply(strsplit(D1$id_list,"__"),"[",1)
  ids2 = sapply(strsplit(D2$id_list,"__"),"[",1)

  common_id_base = intersect(ids1, ids2)
  # laufe ueber die jeden Eintrag aus D1 und D2 und entferne ihn
  # falls die ID nicht in der common_id_base steht

  D1 <- delete_subject_from_data_struct(D = D1, ids_to_keep =  match(common_id_base,ids1))
  D2 <- delete_subject_from_data_struct(D = D2, ids_to_keep =  match(common_id_base,ids2))

#   HD1$id_list = HD2$id_list = intersect(ids1, ids2)
# #  HD1$id_list = HD2$id_list = intersect(D1$id_list, D2$id_list)
#   HD1$df_BD = D1$df_BD[D1$df_BD$ID %in% HD1$id_list,]
#   HD2$df_BD = D2$df_BD[D2$df_BD$ID %in% HD2$id_list,]
#   HD1$mdat = D1$mdat[D1$df_BD$ID %in% HD1$id_list,,,,,drop = FALSE]
#   HD2$mdat = D2$mdat[D2$df_BD$ID %in% HD2$id_list,,,,,drop = FALSE]
  return(list(D1=D1, D2=D2))
}

# has tests
# entfernt subjects aus der Datenstruktur
delete_subject_from_data_struct<-function(D = NULL, ids_to_keep = NULL, ids_to_delete = NULL){
  # entfernt ein oder mehrere Subjects aus der DAtenstruktur anhand entweder der id als String
  # oder der Position (die Position in der id_list = Position in Matrix = Position in der df_BD)
  # es kann auch ein Vector mit mehreren Werten uebergebn werden
  # wenn ein vector von Strings uebergeben wird so wandeln wir diesen dann in numbers um

  # pruefe ob
  if (is.null(D)){
    cat(file = stderr(), paste0("Function delete_subject_from_data_struct\n"))
    cat(file = stderr(), paste0("The function delete_subject_from_data_struct needs\n",
                                "a Data Object that is not NULL\n",
                                "returning D unchanged\n"))
    return(D)
  }
  if (!("mdat" %in% names(D))){
    cat(file = stderr(), paste0("Function delete_subject_from_data_struct\n"))
    cat(file = stderr(), paste0("The given Data Structure is not compatible\n",
                                "the field 'mdat' was not found\n",
                                "returning D unchanged\n"))
    return(D)
  }
  if (!("id_list" %in% names(D))){
    cat(file = stderr(), paste0("Function delete_subject_from_data_struct\n"))
    cat(file = stderr(), paste0("The given Data Structure is not compatible\n",
                                "the field 'id_list' was not found\n",
                                "returning D unchanged\n"))
    return(D)
  }
  if (!("df_BD" %in% names(D))){
    cat(file = stderr(), paste0("Function delete_subject_from_data_struct\n"))
    cat(file = stderr(), paste0("The given Data Structure is not compatible\n",
                                "the field 'df_BD' was not found\n",
                                "returning D unchanged\n"))
    return(D)
  }
  if ((length(D$id_list)!= dim(D$mdat)[1]) | (length(D$id_list)!=nrow(D$df_BD))){
    cat(file = stderr(), paste0("Function delete_subject_from_data_struct\n"))
    cat(file = stderr(), paste0("The given Data Structure is not compatible\n",
                                "(length(D$id_list)!= dim(D$mdat)[1]) | (length(D$id_list)!=nrow(D$df_BD)) \n",
                                "returning D unchanged\n"))
    return(D)
  }


  if (is.null(ids_to_keep) && is.null(ids_to_delete)){
    cat(file = stderr(), paste0("Function delete_subject_from_data_struct\n"))
    cat(file = stderr(), paste0("The function delete_subject_from_data_struct needs\n",
                                "either ids_to_keep argument or ids_to_delete argument\n",
                                "none of these arguments are given\n returning D unchanged\n"))
    return(D)
  }

  if (!is.null(ids_to_keep) && !is.null(ids_to_delete)){
    cat(file = stderr(), paste0("Function delete_subject_from_data_struct\n"))
    cat(file = stderr(), paste0("The function delete_subject_from_data_struct accepts\n",
                                "either ids_to_keep argument or ids_to_delete argument\n",
                                "but not both\n returning D unchanged\n"))
    return(D)
  }



  # wenn ids_to_delete uebergeben wurden
  if (is.null(ids_to_keep) && !is.null(ids_to_delete)){
    if (!is.numeric(ids_to_delete)){
      ids_to_delete <- match(ids_to_delete, D$id_list)
      ids_to_delete <- ids_to_delete[!is.na(ids_to_delete)]
    }
    # entferne auf Basis des id Strings
    ids_to_keep   <- c(1:length(D$id_list))[-ids_to_delete]
  }

  # wenn ids_to_keep uebergeben wurden
  if (!is.null(ids_to_keep) && is.null(ids_to_delete)){
    if (!is.numeric(ids_to_keep)){
      ids_to_keep <- match(ids_to_keep, D$id_list)
      ids_to_keep <- ids_to_keep[!is.na(ids_to_keep)]
    }
    # entferne auf Basis des id Strings
    ids_to_delete   <- c(1:length(D$id_list))[-ids_to_keep]
  }

  # Veraenderung der Datenstruktur
  # wenn die id_num uebergeben wurde oder von uns zugeordnet
  if (!is.null(ids_to_keep) && !is.null(ids_to_delete)){
    # entferne nun das Subject anhand der id_num
    # entfernt werden muss in D$id_list; D$df_BD; D$mdat
    # geloescht wird aber nur wenn es auch etwas zu loeschen gibt
    # sonst ist ids_to_delete integer(0) am besten zu pruefen mit length=0
    if(length(ids_to_delete)>0){
      D$id_list = D$id_list[-ids_to_delete]
      D$df_BD = D$df_BD[-ids_to_delete,]
      D$mdat = R.utils::extract(D$mdat,"1"=ids_to_keep, drop = F)

    #D$mdat = D$mdat[-id_num,,,,,drop = FALSE]
    }
  }
  return(D)

}


get_currently_selected_data<-function(data, g1, g2, t1, t2, freq, trials=g_trials(), regions=g_regions(), tbl_beh = g_beh(), method = g_act_method()){
# die funktion gibt eine LIste von mehreren Variablen zurueck
# gedacht fuer tabs in denen gruppen und trials ausgewaehlt werden
# d$mypaired ... bool handelt es sich um einen gepaarten oder ungeparrten test
# d$data1 ... 3D matrix ... der gruppe 1 bzw trial 1 (subjects x region1 x region2 )
# d$data2 ... 3D matrix ... der gruppe 2 bzw trial 2
# d$mat_p ... 2D matrix ... p Werte des t-tests ueber alle Regionen
# d$mat_t ... 2D matrix ... t Werte des t-tests ueber alle Regionen
# d$string1 ... string .... eine beschreibung des durchgefuehrten Vergleiches
# d$color1 ... col ........ die Color palette die zu den Werten passen
  cat(file = stderr(), "\n********************************\n")
  cat(file = stderr(), "DEPRECATED...\n")
  cat(file = stderr(), "The function get_currently_selected_data is deprecated ... please use get_currently_seleceted_datalong\n\n")
  start_time <- Sys.time()
  d <- list()
  d$my_paired = FALSE
  if ((! t1 == t2) && (!g1==g2)){
    d$string1 = paste0("Compare the in group diff of ", t1," vs ", t2, "between groups", "\n",
                       "unpaired t-test\n")
    d$data1 = get_data_group_trial_freqmean(data,g1, t1, freq, tbl_beh = tbl_beh, method=method)-get_data_group_trial_freqmean(data,g1, t2, freq, tbl_beh = tbl_beh, method=method)
    d$data2 = get_data_group_trial_freqmean(data,g2, t1, freq, tbl_beh = tbl_beh, method=method)-get_data_group_trial_freqmean(data,g2, t2, freq, tbl_beh = tbl_beh, method=method)
    d$my_paired = FALSE

  }else{
    xdata <<- data
    #cat(file = stderr(), paste0("get_data_group_trial_freqmean(data,g1=",g1,",t1=",t1,"freq=",freq,",tbl_beh, method=",method,")\n"))
    #cat(file = stderr(), paste0("get_data_group_trial_freqmean(data,g2=",g2,",t2=",t2,"freq=",freq,",tbl_beh, method=",method,")\n"))
    d$data1 = get_data_group_trial_freqmean(data,g1, t1, freq, tbl_beh = tbl_beh, method=method)
    d$data2 = get_data_group_trial_freqmean(data,g2, t2, freq, tbl_beh = tbl_beh, method=method)
    if (t1 == t2) {
      d$string1 = paste0(g1," vs ", g2, " in trial ", trials[t1], "\n",
                         "independent t-test\n")
    }
    if (g1 == g2){
      d$string1 = paste0(t1," vs ", t2, "in group ", g1, "\n paired t-test\n")
      d$my_paired = TRUE
    }
  }
  #cat(file = stderr(), paste0("dim(d$data1) = ", dim(d$data1),"\n"))
  #cat(file = stderr(), paste0("dim(d$data2) = ", dim(d$data2),"\n"))
  #d$mat_p = matrix(data=NA, nrow=dim(d$data1)[2], ncol=dim(d$data1)[3])
  #d$mat_t = matrix(data=NA, nrow=dim(d$data1)[2], ncol=dim(d$data1)[3])
  d$mat_p = ones(dim(d$data1)[2], dim(d$data1)[3])
  d$mat_t = zeros(dim(d$data1)[2], dim(d$data1)[3])

  d$color1 = colorRampPalette(c("blue","red","green"))
  cat(file = stderr(),paste0("get_currently_selected_data only filter the data duration =",Sys.time()-start_time,"\n"))

  #x <<- d

  cat(file = stderr(), "entering for loop ... now \n")
  for (i in 1:(dim(d$data1)[2])-1){
    #cat(file = stderr(),paste0("i=",i,"\n"))
    start_idx = i+1
    if (method =="Granger"){
      start_idx = 1
    }
    for (j in start_idx:(dim(d$data1)[3])){
     # cat(file = stderr(),paste0("j=",j,"\n"))
      if (!(i==j)){
          x <- na.omit(d$data1[,i,j])
          y <- na.omit(d$data2[,i,j])
#          cat(file = stderr(),paste0("x=",x,"\n"))
#          cat(file = stderr(),paste0("y=",y,"\n"))

          out<- tryCatch(
          {
            z = t.test(x,y, paired = d$my_paired)
            d$mat_p[i,j] = z$p.value
            d$mat_t[i,j] = z$statistic
           # cat(file = stderr(),paste0("tryCatch d$mat_p[",i, ",",j,"] =", d$mat_p[i,j],"\n"))

          },
        error = function(cond){
          #cat(file = stderr(), paste0("error ttest estimation of in i=",i," j=",j,"\n"))
          cat(file = stderr(), paste0("error message =",cond,"\n"))
          d$mat_p[i,j] = 1
          d$mat_t[i,j] = 0
        },
        warning= function(cond){
          #cat(file = stderr(), paste0("warning ttest estimation of in i=",i," j=",j,"\n"))
          cat(file = stderr(), paste0("warning message =",cond,"\n"))
          d$mat_p[i,j] = 1
          d$mat_t[i,j] = 0
        })
       }
    }
  }

  if(!(method=="Granger")){
  #  d$mat_p[lower.tri(d$mat_p)]<-d$mat_p[upper.tri(d$mat_p)]
  #  d$mat_t[lower.tri(d$mat_t)]<-d$mat_t[upper.tri(d$mat_t)]
    lowerTriangle(d$mat_p) = upperTriangle(d$mat_p, byrow=TRUE)
    lowerTriangle(d$mat_t) = upperTriangle(d$mat_t, byrow=TRUE)
  }


  colnames(d$mat_p) = regions
  rownames(d$mat_p) = regions
  colnames(d$mat_t) = regions
  rownames(d$mat_t) = regions
  # colnames(d$mat_p) = g_regions()
  # rownames(d$mat_p) = g_regions()
  # colnames(d$mat_t) = g_regions()
  # rownames(d$mat_t) = g_regions()
  cat(file = stderr(),paste0("get_currently_selected_data duration =",Sys.time()-start_time,"\n"))
  return(d)
}


# die gesamte Datenstruktur des D.RDS Files wird hier eingelesen und geprueft ob der vector in long_marker hier
# enthalten ist, nur diese werden dann zurueck gegeben
# die Funktion ist Teil von Split_data_by_longitudinal_info
get_data_by_longitudinal_info <-function(D_org, long_marker){
  # long_marker ... vector of numbers ... gibt die nummern an die beibehalten werden sollen
  # geaendert werden muss
  # D$id_list
  # D$df_BD
  # D$mdat

  # teste nocheinmal zur Sicherheit das die Dimensionen auch gleich sind
  # hier sollten keine Fehler auftreten
  if (length(D_org$id_list)!=nrow(D_org$df_BD)){
    stop("error in get_data_by_longitudinal_info length(D_org$id_list)!=nrow(D_org$df_BD)")
  }

  if (length(D_org$id_list)!=dim(D_org$mdat)[1]){
    stop("error in get_data_by_longitudinal_info length(D_org$id_list)!=dim(D_org$mdat)[1])")
  }

  # obgleich nicht am schnellsten ist es am uebersichtlichsten jeden Subject einzeln durchzugehen
  # anstatt den long_marker elemente als pattern zu nutzen um die richtige Reichenfolge der
  # Subjects wirklich sicher zu stellen ansonsten kaeme es auf die Reihenfolge im uebergebenen Vector an


  # fuege zu den uebergebenen Vector long_marker noch die Unterstriche
  long_markerfull = sprintf("__%.0f", long_marker)

  idx_to_keep = c()
  id_list_base = c()
  for (i in 1:length(D_org$id_list)){
    # ueber jedes Subject
    # Soll das Subject in die neue liste? TRUE/FALSE
    if (any(str_detect(D_org$id_list[i], long_markerfull))){
      x = which((str_detect(D_org$id_list[i], long_markerfull)),T)

      idx_to_keep = c(idx_to_keep, i)
      id_list_base = c(id_list_base, str_remove(D_org$id_list[i],long_markerfull[x]))
    }
  }

  D = D_org
  D$id_list = D_org$id_list[idx_to_keep]
  D$id_list_base = id_list_base
  D$df_BD = D_org$df_BD[idx_to_keep,]
  D$mdat = D_org$mdat[idx_to_keep,,,,,drop = F]

  if (length(D$id_list)== 0){
    cat(file = stderr(),paste0("keine longitudinalen Daten gefunden fuer long_id = ",long_marker,"\n"))
    D<-NULL
  }

  return(D)
}


# die gesamte Datenstruktur des D.RDS Files wird hier eingelesen und geprueft ob der vector in long_marker hier
# enthalten ist, nur diese werden dann zurueck gegeben aufgeteilt fuer die 2 Zeitpunkte
# zusaetzlich werden ggf. Daten rausgeworfen die nicht in beiden Zeitpunkten auftauchen
split_data_by_longitudinal_info <-function(D_org, long_marker1, long_marker2,
                                           is_exclude_not_reoccuring_subj = TRUE,
                                           averagelong = TRUE){
  # long_marker ... vector of numbers ... gibt die nummern an die beibehalten werden sollen
  # Trennen des Gesamtdatensatzes in Gruppen von Zeitlichen Subjects
  S = list()

  cat(file = stderr(), paste0("long_marker1 = ", long_marker1, "\n"))
  cat(file = stderr(), paste0("long_marker2 = ", long_marker2, "\n"))
  cat(file = stderr(), paste0("is_exclude_not_reoccuring_subj = ", is_exclude_not_reoccuring_subj, "\n"))
  cat(file = stderr(), paste0("averagelong = ", averagelong, "\n"))
  # Problem ist hier, dass es noch die alten Daten gibt ohne die Formatierung __1 __2 in der id_list
  # deshalb hier eine entsprechende Abfrage ... wenn es noch das alte Design ist
  # dann gibt es keine longitudinale Info und es wird der D_org als S$D1 zurueck gegeben
  # waehrend S$D2 = NULL gesetzt wird
  if (!is_longitudinal_info_in_data(D_org)) {
    cat(file = stderr(), paste0("split_data_by_longitudinal_info Data in old format without longitudinal info ... \n"))
    cat(file = stderr(), paste0("no longitudinal analyses possible \n"))

    S$D1 = D_org
    S$D2 = NULL
    return(S)
  }

  if (is.null(long_marker1)){
    cat(file = stderr(), paste0("no longitudinal marker supplied ... \n"))
    cat(file = stderr(), paste0("no longitudinal analyses possible \n"))

    S$D1 = D_org
    S$D2 = NULL
    return(S)
  }




  cat(file = stderr(), paste0("split_data_by_longitudinal_info with longmarker1 = ", long_marker1, "  longmarker2 =", long_marker2, "\n"))
  cat(file = stderr(), paste0("split_data_by_longitudinal_info with length(D_org)=", length(D_org), "\n"))
  S$D1 <- get_data_by_longitudinal_info(D_org, long_marker1)
  S$D2 <- get_data_by_longitudinal_info(D_org, long_marker2)


  # eleminieren von subjects die nicht in beiden Zeitpunkten vorkommen
  if(is_exclude_not_reoccuring_subj){
    S <-exclude_data_from_not_reoccuring_subjects(S$D1,S$D2)
  }

  gD1 <<- S$D1
  gD2 <<- S$D2
  # averaging falls eine Zeitspanne aus mehreren Zeitpunkten besteht
  if (averagelong){
    S$D1<- average_data_from_reoccuring_subjects(S$D1)
    S$D2<- average_data_from_reoccuring_subjects(S$D2)
  }

  return(S)
}

is_longitudinal_info_in_data<- function(D){
  # testet ob es ueberhaupt longitudinale Infos gibt ... aktuell gibt es diese
  # Infos bei den Daten von Jenny und Laura nicht
  g_is_longitudinal_info_in_data <<- D
  if (any(str_detect(D$id_list,"__"))){
    # falls es aber nur einen Zeitpunkt gibt dann ist letztlich auch keine longitudinale Info vorhanden
    return(TRUE)
  }
  return(FALSE)
}

average_data_from_reoccuring_subjects<-function(D){
  # es werden Subjects die die gleiche ID aber unterschiedliche Zeitpunkte haben gemittelt
  if (is.null(D)){return(D)}
  id_list = c()
  # leere data.frame with same columns
  df_BD = D$df_BD[0,]
  # get the ids without the time tag
  ids = sapply(strsplit(D$id_list,"__"),"[",1)
  # der time marker als vector von strings
  ids_tp = sapply(strsplit(D$id_list,"__"),"[",2)
  uids = unique(ids)
  # die datenmatrix mit falschen Zeilennamen und falsch gefuellt als Platzhalter
  # Columnames bereits correct
  #mdat = R.utils::extract(D$mdat,"1"=1:length(uids), drop = F)
  is_matrix_inizialized = FALSE

  for (i in 1:length(uids)){
#    cat(file = stderr(),paste0("i=",i,"\n"))
    current_id_name = uids[i]
    # get the index of all occurences of the first ID
    idxs = which(str_detect(ids,current_id_name))
    new_id_name = paste0(uids[i],"__",paste(ids_tp[idxs],collapse ="_"))
    id_list = c(id_list, new_id_name)
    df_BD = rbind(df_BD,estimate_average_of_rows(D$df_BD[idxs,]))

    # setze den richtigen Zeilennamen

    # select the rows which should than be
    tmp = R.utils::extract(D$mdat,"1"=idxs, drop = F)

    mx = means.along(tmp,1)
    # erweitere die dimension um die erste dimension
    dim(mx)<-c(1,dim(mx))

    if (is_matrix_inizialized){
      # hinzufuegen zur finalen Matrix
      mdat <-abind(mdat,mx,along=1)
    }else{
      mdat <- mx
      is_matrix_inizialized = TRUE
    }
    # entferne die ids aus der id lists
    #ids = ids[-idxs]
    rownames(mdat)[i] <- new_id_name
  }
  # die colnames werden uebertragen
  colnames(mdat) <- colnames(D$mdat)
  D$id_list <- id_list
  D$df_BD   <- df_BD
  D$mdat    <- mdat

  return(D)
}


estimate_average_of_rows<- function(df){
  # estimate the mean of all rows
  # return a dataframe with a single row
  dfm <- df[1,]
  is_col_numeric <- unlist(lapply(df, is.numeric))
  for (i in 1:ncol(df)){
    # Strings bzw. not numeric
    # nicht numerische koennen wir nicht mittelen ... daher fuegen wir zusammen
    if (!is_col_numeric[i]){
      my_string = "mean("
      for (j in 1:nrow(df)){
        my_string <- paste0(my_string, df[[j,i]] ,"+")
      }
      my_string<-paste0(my_string,")")
      dfm[1,i] <- my_string
    }else{
      # NUMERIC COLUMN
      dfm[[i]]<-as.double(dfm[[i]])
      cur_colname <- colnames(df)[i]
      dfm[1,i] <- mean(df[[cur_colname]], na.rm = T)
    }
  }
  return(dfm)
}



# has tests
get_data_group <-function(data, group, tbl_beh = g_beh(), method = g_act_method()){
  #cat(file = stderr(), paste0("get_data_group with group ==", group,"\n"))
  #cat(file = stderr(), paste0("get_data_group with dim(data) ==", dim(data),"\n"))
  if (group == "all_groups") {
    data_group = data
  } else {
    data_group = asub(data, list(tbl_beh$Gruppe==group), 1, drop=F)
  }
  return(data_group)
}

# has tests
get_data_freqmean <- function(data, freq, method = g_act_method()){
  data_freq = filter_by_selfreq(data, freq, method)
  data_freqmean = get_freqmean(data_freq, method = method)
  return(data_freqmean)
}


# has tests
get_beh_tbl_data_by_group <- function(group, target_colname, tbl_beh = g_beh()){

  #cat(file = stderr(),paste0("Alter=", target_colname,"\n"))
  if (group == "all_groups") {
    data = get(target_colname, tbl_beh)
   # data = tbl_beh[ ,target_colname]
  } else {
    data = get(target_colname, tbl_beh[tbl_beh$Gruppe ==group])
  #  data = tbl_beh[tbl_beh$Gruppe==group,target_colname]
  }

  #
  # if (group == "all_groups") {
  #   data = tbl_beh[ ,target_coloumnname]
  # } else if (group == "Group0"){
  #   data = tbl_beh[tbl_beh$Gruppe==0,target_coloumnname]
  # } else if (group == "Group1"){
  #   data = tbl_beh[tbl_beh$Gruppe==1,target_coloumnname]
  # }
  return(data)

}


# has tests
get_data_groupmean <-function(data, group, tbl_beh = g_beh(), method = g_act_method()){
  data_group = get_data_group(data,group, tbl_beh=tbl_beh, method = method)
  data_group_mean = apply(data_group, 2:length(dim(data_group)),mean, na.rm=TRUE)
  return(data_group_mean)
}


# has tests
get_data_groupmean_trial <- function(data, group,trial, tbl_beh = g_beh(), method = g_act_method()){
  data_groupmean = get_data_groupmean(data, group, tbl_beh = tbl_beh, method = method)
  # auch die FRequenz sollte eine einzelne Dimension als 2. region haben
  # transferentrpy hat eine single dimension als letztes

  # Problem ... es koennen auch mehrere trails ausgewaehlt werden ... was dann
  # drop oder nicht? ... mean von mehreren Trails macht aber praktisch keinen sinn
  # wenn es eine Zahl ist dann dropen wir die dimension

    data_groupmean_trial = data_groupmean[,,trial,]
  # wenn ein vector uebergeben wird dann wird die Dimension nicht gedropped


  return(data_groupmean_trial)
}

# has tests
get_data_groupmean_trial_freq <- function(data, group,trial,freq, tbl_beh = g_beh(), method = g_act_method()){
  #cat(file = stderr(), paste0("get_data_groupmean_trial_freq dim(data)=",dim(data),"\n"))
  data_groupmean_trial = get_data_groupmean_trial(data, group,trial, tbl_beh = tbl_beh, method = method)
  #cat(file = stderr(), paste0("get_data_groupmean_trial_freq dim(data_groupmean_trial)=",dim(data_groupmean_trial),"\n"))
  data_groupmean_trial_freq = filter_by_selfreq(data_groupmean_trial, freq, method = method)
  #cat(file = stderr(), paste0("get_data_groupmean_trial_freq dim(data_groupmean_trial_freq)=",dim(data_groupmean_trial_freq),"\n"))

  return(data_groupmean_trial_freq)
}

# has tests
get_data_group_region <- function(data, group, region, tbl_beh = g_beh(), method = g_act_method()){
  #cat(file=stderr(),paste0("get_data_group_region - length(dim(data))=",length(dim(data)),"\n"))

  data_group = get_data_group(data, group, tbl_beh = tbl_beh, method = method)
  #cat(file = stderr(),"data_group dim = ", dim(data_group), "\n")
  data_group_region = data_group[,region,,,]
  # if (length(dim(data))==4){
  #   data_group_region = data_group[,region,,]
  # }
  # if (length(dim(data))==5){
  # #  data_group_region = data_group[,region,region,,]
  #   data_group_region = data_group[,region,,,]
  # }
  return(data_group_region)
}



# has tests
get_data_group_trial <- function(data, group, trial, tbl_beh = g_beh(),method = g_act_method()){
  #cat(file = stderr(), paste0("get_data_group_trial dim(data)=",dim(data),"\n"))
  data_group = get_data_group(data, group, tbl_beh = tbl_beh, method = method)
  #cat(file = stderr(), paste0("in get_data_group_trial dim(data_group)=",dim(data_group),"\n"))

  # verhindere das die gruppe gedroppt wird wenn sie nur ein Subject hat
  #cat(file = stderr(), paste0("before drop_except dim(data_group)=",dim(data_group),"\n"))
  data_group_trial = drop_except(data_group[,,,trial,,drop=FALSE], c(1,2,3,5))
  #cat(file = stderr(), paste0("after drop_except dim(data_group_trial)=",dim(data_group_trial),"\n"))

    #cat(file = stderr(), paste0("get_data_group_trial dim(data_group_trial)=",dim(data_group_trial),"\n"))
#  data_group_trial = data_group[,,,trial,]
  #cat(file=stderr(),paste0("data_group in get_data_group_trial - dim(data_group)=",dim(data_group),"\n"))
  # if (method == "Transferentropy"){
  #   if (length(dim(data_group))==3){
  #     data_group_trial = data_group[,,trial]
  #   }
  #   if (length(dim(data_group))==4){
  #     data_group_trial = data_group[,,,trial]
  #   }
  #
  # }else{
  # if (length(dim(data))==4){
  #   data_group_trial = data_group[,,trial,]
  # }
  # if (length(dim(data))==5){
  #   data_group_trial = data_group[,,,trial,]
  # }
  # }
  return(data_group_trial)
}

# has tests
get_data_group_freq <- function(data, group, freq, tbl_beh = g_beh(), method = g_act_method()){
  #cat(file = stderr(), paste0("get_data_group_freq dim(data)=",dim(data),"\n"))
  data_group = get_data_group(data, group, tbl_beh = tbl_beh, method = method)
  #cat(file = stderr(), paste0("dim(data_group)=",dim(data_group),"\n"))

  data_group_freq = filter_by_selfreq(data_group, freq, method = method)
  # #x = (as.numeric(ufreq_list)>freq[1]) == (as.numeric(ufreq_list)<freq[2])
  # x = sel_freqs
  # if (length(dim(data_group))==4){
  #   data_group_freq = data_group[ , , , x]
  # }
  # if (length(dim(data_group))==5){
  #   if (method=="Transferentropy"){
  #     #Transferentropy has no frequencies
  #     data_group_freq = data_group[ , , , , ]
  #   }else{
  #     data_group_freq = data_group[ , , , , x]
  #   }
  # }
  #cat(file = stderr(), paste0("before return dim(data_group_freq)=",dim(data_group_freq),"\n"))
  return(data_group_freq)
}


# has tests
filter_by_selfreq <- function(data, freq, method = g_act_method()){
  # freq ... ist in den meisten faellen g_sel_freq()
  # ein vector von TRUE und FALSe gleicher Dimensionen wie dim(data)[5]
  # das drop=F ist hier sehr wichtig weil andere funtionen ueber die letzte Dimension mitteln
#  cat(file = stderr(), paste0("in filter_by_selfreq freq=",freq,"\n"))
#  cat(file = stderr(), paste0("dim(data) = ",dim(data),"\n"))
#  gffreq <<- freq
#  gfdata <<- data
  data_selfreq = asub(data, list(freq), length(dim(data)), drop=F)
  #
  # num_dims = length(dim(data))
  # if (method=="Transferentropy"){
  #   #Transferentropy has no frequencies
  #   #
  #   data_selfreq = data
  # }else{
  #   if (num_dims==2){
  #     data_selfreq = data[ , freq]
  #   }else if (num_dims==3){
  #     data_selfreq = data[ , , freq]
  #   } else if (num_dims==4){
  #     data_selfreq = data[ , , , freq]
  #   } else if (num_dims==5){
  #     data_selfreq = data[ , , , ,freq]
  #   }else{
  #     cat(file = stderr(), paste0("problem with matrix dim in filter_by_selfreq with method =",method,"\n"))
  #     cat(file = stderr(), "dim(data)=")
  #     cat(file = stderr(), dim(data))
  #     cat(file = stderr(), "\n")
  #   }
  # }
  return(data_selfreq)
}


# has tests
get_freqmean <- function(data, method = g_act_method()){
  # mittelt die Daten ueber die letzte Dimension (Frequenzen)
#  if (dim(data)[length(dim(data))]==1){
#    data_freqmean =drop_except(data, 1:length(dim(data))-1)
 #   return(data_freqmean)
 # }

  #data_freqmean2 = apply(data, 1:length(dim(data))-1,mean, na.rm=TRUE)
  # way faster
  data_freqmean <- means.along(data, length(dim(data)))
  #cat(file = stderr(),paste0("is identical =", identical(data_freqmean, data_freqmean2),"\n"))
  return(data_freqmean)
}

# has tests
get_data_group_freqmean <- function(data, group, freq, tbl_beh = g_beh(), method = g_act_method()){
  data_group_freq = get_data_group_freq(data, group, freq, tbl_beh = tbl_beh, method = method)
  data_group_freqmean = get_freqmean(data_group_freq, method = method)
  return(data_group_freqmean)
}

# has tests
get_data_group_trial_freq <- function(data, group, trial, freq, tbl_beh = g_beh(), method = g_act_method()){
  data_group_trial = get_data_group_trial(data, group, trial, tbl_beh = tbl_beh, method = method)
  data_group_trial_freq = filter_by_selfreq(data_group_trial, freq, method = method)
  return(data_group_trial_freq)
}

# has tests
get_data_group_trial_freqmean <- function(data, group, trial, freq, tbl_beh = g_beh(), method = g_act_method()){
  #cat(file = stderr(), paste0("start get_data_group_trial_freqmean with dim(data)=",dim(data),"\n"))
  start_time2 = Sys.time()
  data_group_trial = get_data_group_trial(data, group, trial, tbl_beh = tbl_beh, method = method)
  #cat(file = stderr(), paste0("in get_data_group_trial_freqmean with dim(data_group_trial)=",dim(data_group_trial),"\n"))
  #cat(file = stderr(),paste0("get_data_group_trial duration =",Sys.time()-start_time2,"\n"))
  start_time2 = Sys.time()
  data_group_trial_freq = filter_by_selfreq(data_group_trial, freq, method = method)
  #cat(file = stderr(), paste0("in get_data_group_trial_freqmean with dim(data_group_trial_freq)=",dim(data_group_trial_freq),"\n"))
  #cat(file = stderr(),paste0("filter_by_selfreq duration =",Sys.time()-start_time2,"\n"))
  start_time2 = Sys.time()
  data_group_trial_freqmean = get_freqmean(data_group_trial_freq, method = method)
  #cat(file = stderr(), paste0("in get_data_group_trial_freqmean with dim(data_group_trial_freqmean)=",dim(data_group_trial_freqmean),"\n"))
  #cat(file = stderr(),paste0("get_freqmean duration =",Sys.time()-start_time2,"\n"))

  return(data_group_trial_freqmean)
}


get_selected_freq_list <- function(freq_list, freq){
  selected_freq_list = (as.numeric(freq_list)>=freq[1]) == (as.numeric(freq_list)<=freq[2])
  return(selected_freq_list)
}

# has tests
get_data_group_region_trial <- function(data, group, region, trial, tbl_beh = g_beh(), method = g_act_method()){
 # cat(file=stderr(),paste0("get_data_group_region_trial - length(dim(data))=",length(dim(data)),"\n"))
  #cat(file = stderr(), paste0("dim(data) =", dim(data),"\n"))
  data_group_region = get_data_group_region(data, group, region, tbl_beh = tbl_beh, method = method)
  #cat(file = stderr(), paste0("dim(data_group_region) =", dim(data_group_region),"\n"))
  if (length(dim(data))==4){
    data_group_region_trial = data_group_region[,trial,]
  }
  if (length(dim(data))==5){
    data_group_region_trial = data_group_region[,,trial,]
  }
  return(data_group_region_trial)
}

# has tests
get_data_group_region_trial_freq <- function(data, group, region, trial, freq, tbl_beh = g_beh(), method = g_act_method()){
  data_group_region_trial = get_data_group_region_trial(data, group, region, trial, tbl_beh = tbl_beh, method = method)
  data_group_region_trial_freq = filter_by_selfreq(data_group_region_trial, freq, method = method)
  return(data_group_region_trial_freq)
}


read_data_from_dir<- function(directory){
  D<-readRDS(file.path(directory, "D.Rda"))
  return(D)
}

# get_data <- function(directory){
#  # cat(file=stderr(), "read mdat from folder ", directory,"\n")
#   mdatc = readRDS(file.path("../data", directory, "tbl_data.Rda"))
#   num_dims = length(dim(mdatc))
#
#   # wenn die Frequenz nur aus einer Dimension besteht dann nehmen wir sie raus
#   if (num_dims==3 && dim(mdatc)[num_dims]==1){
#     mdatc = mdatc[,,1]
#   }
#
#   if (num_dims==4 && dim(mdatc)[num_dims]==1){
#     mdatc = mdatc[,,,1]
#   }
#   if (num_dims==5 && dim(mdatc)[num_dims]==1){
#     mdatc = mdatc[,,,,1]
#   }
#   return(mdatc)
# }

# get_data <- function(method){
#   cat(file=stderr(),"read mdat from folder ", method,"\n")
#   mdatc = readRDS(file.path("../data",method,"tbl_data.Rda"))
#   return(mdatc)
# }


get_global_D <- function(dirname){
  cat(file = stderr(),paste0("get_global_D ... loading DAta from file...",dirname," \n"))

  D <- readRDS(file.path(dirname,"D.Rda"))
  return(D)
}

get_global_data <- function(dirname, tbl_beh = g_beh()){
  cat(file = stderr(),"get_global_data is deprecated\n" )
  cat(file = stderr(),"get_global_data is deprecated\n" )
  cat(file = stderr(),"get_global_data is deprecated\n" )

  D <- readRDS(file.path(dirname,"D.Rda"))
  # uregion_list <<- readRDS(file.path(dirname,"uregion_list.Rda"))
  # utrial_list <<- readRDS(file.path(dirname,"utrial_list.Rda"))
  # ufreq_list <<- readRDS(file.path(dirname,"ufreq_list.Rda"))
  # tbl_beh <<- readRDS(file.path(dirname, "tbl_beh.Rda"))

  group_names<<-D$ugroup_list
  region_names <<- D$uregion_list_named
  trial_names <<- D$utrial_list_named
  tbl_beh <<- D$df_BD
  ufreq_list <<- D$ufreq_list
  utrial_list <<- D$utrial_list
  uregion_list <<-D$uregion_list


  # region_names <<- list()
  # trial_names <<- list()
  # j = 0
  # for (i in uregion_list){j=j+1;  region_names[i]=j }
  # j = 0
  # for (i in utrial_list){j=j+1;  trial_names[i]=j }
  # j = 1
  # group_names <<- c("all_groups")
  # for (i in unique(tbl_beh()$Gruppe)) {
  #   j=j+1;  group_names[j]=paste("Group", toString(i), sep = "")
  # }

}
#
#
# get_global_data_for_debugging <- function(method){
#   cat(file = stderr(), "load data into global environment for debugging\n")
# #  cat(file = stderr(),file.path("../data",method,"uregion_list.Rda") )
#   uregion_list <<- readRDS(file.path("../data",method,"uregion_list.Rda"))
#   utrial_list <<- readRDS(file.path("../data",method,"utrial_list.Rda"))
#   ufreq_list <<- readRDS(file.path("../data",method,"ufreq_list.Rda"))
#   tbl_beh <<- readRDS(file.path("../data",method,"tbl_beh.Rda"))
#   data <<- get_data(method)
#
#
#   # create a named list for selection Box
#   region_names <<- list()
#   trial_names <<- list()
#   #group_names <<- list()
#   j = 0
#   for (i in uregion_list){j=j+1;  region_names[i]=j }
#   j = 0
#   for (i in utrial_list){j=j+1;  trial_names[i]=j }
#   j = 1
#   # group_names <<- c("all_groups")
#   # for (i in unique(g_beh()$Gruppe)) {
#   #   j=j+1;  group_names[j]=paste("Group", toString(i), sep = "")
#   # }
#
#
#   #  mdatc = readRDS(file.path("../data",method,"tbl_data.Rda"))
# }



drop_except <- function(m, omit){
  ds <- dim(m)
  dv <- ds == 1 & !(seq_along(ds) %in% omit)
  adrop(m, dv)
}

#
# get_global_uregion_list <- function(method){
#   req(g_reload_rVal())
#   #cat(file = stderr(), paste0("get_global_uregion with reload = ", g_reload_rVal(),"\n"))
#   uregion_list <- readRDS(file.path("../data",method,"uregion_list.Rda"))
#   return(uregion_list)
# }
#
#
#
#get_global_utrial_list <- function(method){
#  utrial_list <- readRDS(file.path("../data",method,"utrial_list.Rda"))
#return(utrial_list)
# }
#
# get_global_ufreq_list <- function(method){
#   ufreq_list <- readRDS(file.path("../data",method,"ufreq_list.Rda"))
#   return(ufreq_list)
# }
#
# get_global_tbl_beh <- function(method){
#   tbl_beh <- readRDS(file.path("../data",method,"tbl_beh.Rda"))
#   return(tbl_beh)
# }
#
# get_global_region_names <- function(method){
#   uregion_list <- readRDS(file.path("../data",method,"uregion_list.Rda"))
#   # create a named list for selection Box
#   region_names <- list()
#   j = 0
#   for (i in uregion_list){j=j+1;  region_names[i]=j }
#   return(region_names)
# }
#
# get_global_trial_names <- function(method){
#   utrial_list <- readRDS(file.path("../data",method,"utrial_list.Rda"))
#   trial_names <- list()
#   j = 0
#   for (i in utrial_list){j=j+1;  trial_names[i]=j }
#   return(trial_names)
# }

get_global_group_names <- function(datadir){

  # ich nehmen die DAten aus dem D file und nicht aus dem
  # behavioral file damit nicht weitere gruppen die nicht
  # in den DAten sind noch dazu kommen koennen
  filename <- file.path(datadir,"D.Rda")
  D <- readRDS(filename)
  tbl_beh = D$df_BD
  group_names = c("all_groups",D$ugroup_list)

  # create a named list for selection Box
  #
  # j = 1
  # group_names <- c("all_groups")
  # for (i in unique(tbl_beh$Gruppe)) {
  #   j=j+1;  group_names[j]=toString(i), sep = "")
  # }
  return(group_names)

}


comparing_independent_rs<-function(r1, r2, n1, n2){
  zd<-(atanh(r1)-atanh(r2))/sqrt(1/(n1-3)+1/(n2-3))
  p <-1 - pnorm(abs(zd))
  return(c(zd,p))
}

comparing_dependent_rs <-function(rxy, rxz, rzy, n)
{
  df<-n-3
  td<-(rxy-rzy)*sqrt((df*(1 + rxz))/(2*(1-rxy^2-rxz^2-rzy^2+(2*rxy*rxz*rzy))))
  p <-pt(td, df)
return(c(td,p))
}

means.along <- function(a, i) {
  n <- length(dim(a))
  b <- aperm(a, c(seq_len(n)[-i], i))
  rowMeans(b, dims = n - 1)
}




