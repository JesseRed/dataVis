#
# uregion_list = readRDS("../data/uregion_list.Rda")
# utrial_list = readRDS("../data/utrial_list.Rda")
# ufreq_list = readRDS("../data/ufreq_list.Rda")
# tbl_beh = readRDS("../data/tbl_beh.Rda")
library(abind)

create_my_ttest_string <- function(z, paired = FALSE, mean1 = 0, mean2 = 0){
  t = z$statistic
  df = z$parameter
  r = sqrt((t^2)/((t^2)+df))
  if (paired){
    meanstring <- paste0("mean of difference = ", round(z$estimate[1],3)," \nmean= ", round(mean1,3), " vs. ", round(mean2,3)," \n")
  }else{
    meanstring <- paste0("mean= ", round(z$estimate[2],3), " vs. ", round(z$estimate[1],3)," \n")
  }

  out <- paste0(z$method,"\n\n",
                meanstring,
                "t=", round(z$statistic,2), " \n",
                "p=", z$p.value, "  \n",
                "df=", round(z$parameter,1),"  \n",
                "CI(",attributes(z$conf.int),")= ",round(z$conf.int[1],3)," ; ",round(z$conf.int[2],3)," \n",
                "effect size r = ", round(r,4), "\n",
                "r = [sqrt((t^2)/((t^2)+df))]"
  )

  return(out)
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
  # generierung von t- und p-Maps fuer longitudinale Daten
  # Es muss insbesondere die Gruppenintegritaet ueberprueft werden, da
  # sich die Gruppen zwischen den beiden Messungen unterscheiden werden durch drop outs
  homogen_group_data = exclude_data_from_not_reoccuring_subjects(D1,D2)
  HD1 <- homogen_group_data$HD1
  HD2 <- homogen_group_data$HD2
  glob_HD1 <<-HD1
  glob_HD2 <<-HD2
  # d1 = get_currently_selected_data(HD1, g1, g2, t1, t2, freq,
  #                                  trials = HD1$utrial_list,
  #                                  regions = HD1$uregion_list,
  #                                  tbl_beh = HD1$df_BD)
  #
  # d2 = get_currently_selected_data(HD2, g1, g2, t1, t2, freq,
  #                                  trials = HD2$utrial_list,
  #                                  regions = HD2$uregion_list,
  #                                  tbl_beh = HD2$df_BD)
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


get_currently_selected_data_long<-function(data, g1, g2, t1, t2, freq,
                                           trials=g_trials(),
                                           regions=g_regions(),
                                           tbl_beh = g_beh(),
                                           method = g_act_method(),
                                           datalong = NULL,
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
    cat(file = stderr(),"\nlongitudinal data analyse start \n")
    d1 <- get_selected_data_considering_group_trial(data, g1,g2,t1,t2, freq,  trials = trials, tbl_beh = tbl_beh, method = method)
    cat(file = stderr(),"1\n")
    d2 <- get_selected_data_considering_group_trial(datalong, g1,g2,t1,t2, freq, trials = trials, tbl_beh = tbl_beh, method = method)
    cat(file = stderr(),"2\n")
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
                             "Unterscheidet sich der Einfluss der Zeit/Intervention zwischen den beiden Gruppen fuer den ausgewaehlten trial?"
      )
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
                             "Nullhypothese: Zeit/Intervention haben keinen Einfluss auf den Unterschied zwischen trial_1 und trial_2\n"
      )
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
                             "Nullhypothese: Der Einfluss von Zeit/Intervention ist fuer trial_1 und trial_2 gleich\n",
      )
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
    cat(file = stderr(),paste0("get_currently_selected_data_long only filter the data duration =",Sys.time()-start_time,"\n"))


    cat(file = stderr(), "entering for loop ... now \n")
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
  if ((! t1 == t2) && (!g1==g2)){
    d$string1 = paste0("Compare the in group diff of ", t1," vs ", t2, "between groups", "\n",
                       "unpaired t-test\n")
    d$data1 = get_data_group_trial_freqmean(data,g1, t1, freq, tbl_beh = tbl_beh, method=method)-get_data_group_trial_freqmean(data,g1, t2, freq, tbl_beh = tbl_beh, method=method)
    d$data2 = get_data_group_trial_freqmean(data,g2, t1, freq, tbl_beh = tbl_beh, method=method)-get_data_group_trial_freqmean(data,g2, t2, freq, tbl_beh = tbl_beh, method=method)

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
  return(d)
}


exclude_data_from_not_reoccuring_subjects<-function(D1,D2){
  # es werden nur subjects uebernommen die in D1 und in D2 vorkommen
  # teste zuerst ob die beiden Datensaetze auch wirklich identische Eigenschaften haben
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
  HD1 = D1
  HD2 = D2
  HD1$id_list = HD2$id_list = intersect(D1$id_list, D2$id_list)
  HD1$df_BD = D1$df_BD[D1$df_BD$ID %in% HD1$id_list,]
  HD2$df_BD = D2$df_BD[D2$df_BD$ID %in% HD2$id_list,]
  HD1$mdat = D1$mdat[D1$df_BD$ID %in% HD1$id_list,,,,,drop = FALSE]
  HD2$mdat = D2$mdat[D2$df_BD$ID %in% HD2$id_list,,,,,drop = FALSE]
  return(list(HD1=HD1, HD2=HD2))
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

# has tests
get_data_group <-function(data, group, tbl_beh = g_beh(), method = g_act_method()){

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

  # das drop=F ist hier sehr wichtig weil andere funtionen ueber die letzte Dimension mitteln
  #cat(file = stderr(), paste0("in filter_by_selfreq freq=",freq,"\n"))
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
  #start_time2 = Sys.time()
  data_group_trial = get_data_group_trial(data, group, trial, tbl_beh = tbl_beh, method = method)
  #cat(file = stderr(), paste0("in get_data_group_trial_freqmean with dim(data_group_trial)=",dim(data_group_trial),"\n"))
  #cat(file = stderr(),paste0("get_data_group_trial duration =",Sys.time()-start_time2,"\n"))
  #start_time2 = Sys.time()
  data_group_trial_freq = filter_by_selfreq(data_group_trial, freq, method = method)
  #cat(file = stderr(), paste0("in get_data_group_trial_freqmean with dim(data_group_trial_freq)=",dim(data_group_trial_freq),"\n"))
  #cat(file = stderr(),paste0("filter_by_selfreq duration =",Sys.time()-start_time2,"\n"))
  #start_time2 = Sys.time()
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
  cat(file = stderr(),"get_global_D ... loading DAta from file\n" )

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


get_global_data_for_debugging <- function(method){
  cat(file = stderr(), "load data into global environment for debugging\n")
#  cat(file = stderr(),file.path("../data",method,"uregion_list.Rda") )
  uregion_list <<- readRDS(file.path("../data",method,"uregion_list.Rda"))
  utrial_list <<- readRDS(file.path("../data",method,"utrial_list.Rda"))
  ufreq_list <<- readRDS(file.path("../data",method,"ufreq_list.Rda"))
  tbl_beh <<- readRDS(file.path("../data",method,"tbl_beh.Rda"))
  data <<- get_data(method)


  # create a named list for selection Box
  region_names <<- list()
  trial_names <<- list()
  #group_names <<- list()
  j = 0
  for (i in uregion_list){j=j+1;  region_names[i]=j }
  j = 0
  for (i in utrial_list){j=j+1;  trial_names[i]=j }
  j = 1
  # group_names <<- c("all_groups")
  # for (i in unique(g_beh()$Gruppe)) {
  #   j=j+1;  group_names[j]=paste("Group", toString(i), sep = "")
  # }


  #  mdatc = readRDS(file.path("../data",method,"tbl_data.Rda"))
}



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


