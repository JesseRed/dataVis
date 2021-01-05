#
# uregion_list = readRDS("../data/uregion_list.Rda")
# utrial_list = readRDS("../data/utrial_list.Rda")
# ufreq_list = readRDS("../data/ufreq_list.Rda")
# tbl_beh = readRDS("../data/tbl_beh.Rda")

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
  d <- list()
  d$my_paired = FALSE
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
  cat(file = stderr(), paste0("dim(d$data1) = ", dim(d$data1),"\n"))
  d$mat_p = matrix(data=NA, nrow=dim(d$data1)[2], ncol=dim(d$data1)[3])
  d$mat_t = matrix(data=NA, nrow=dim(d$data1)[2], ncol=dim(d$data1)[3])
  d$color1 = colorRampPalette(c("blue","red","green"))

  for (i in 1:(dim(d$data1)[2])){
    for (j in 1:(dim(d$data1)[3])){
      x = d$data1[,i,j]
      y = d$data2[,i,j]
      z = t.test(x,y, paired = d$my_paired)
      d$mat_p[i,j] = z$p.value
      d$mat_t[i,j] = z$statistic
    }
  }
  colnames(d$mat_p) = regions
  rownames(d$mat_p) = regions
  colnames(d$mat_t) = regions
  rownames(d$mat_t) = regions
  # colnames(d$mat_p) = g_regions()
  # rownames(d$mat_p) = g_regions()
  # colnames(d$mat_t) = g_regions()
  # rownames(d$mat_t) = g_regions()
  return(d)
}

get_data_group <-function(data, group, tbl_beh = g_beh(), method = g_act_method()){

  if (length(dim(data))==4){
    if (group == "all_groups") {
      data_group = data
    } else if (group == "Group0"){
      data_group = data[tbl_beh$Gruppe==0,, ,]
      #data_group = apply(tmp, c(2,3,4),mean)
    } else if (group == "Group1"){
      data_group = data[tbl_beh$Gruppe==1,, ,]
      #data_group = apply(tmp, c(2,3,4),mean)
    }
  }

  if (length(dim(data))==5){
    if (group == "all_groups") {
      data_group = data
    } else if (group == "Group0"){
      data_group = data[tbl_beh$Gruppe==0,, ,,]
      #data_group = apply(tmp, c(2,3,4),mean)
    } else if (group == "Group1"){
      data_group = data[tbl_beh$Gruppe==1,, ,,]
      #data_group = apply(tmp, c(2,3,4),mean)
    }
  }

  return(data_group)
}


get_data_freqmean <- function(data, freq, method = g_act_method()){
  # cat(file = stderr(), paste0("get_data_freqmean with...\n"))
  # cat(file = stderr(), paste0("dim(data)=", dim(data),"\n"))
  # cat(file = stderr(), paste0("dim(freq)=", dim(freq),"\n"))
  # cat(file = stderr(), paste0("freq=", freq,"\n"))
  # cat(file = stderr(), paste0("method=", method,"\n"))
  data_freq = filter_by_selfreq(data, freq, method)
  data_freqmean = get_freqmean(data_freq, freq, method = method)
  return(data_freqmean)
}


get_beh_tbl_data_by_group <- function(group, target_coloumnname, tbl_beh = g_beh()){

  if (group == "all_groups") {
    data = tbl_beh[ ,target_coloumnname]
  } else if (group == "Group0"){
    data = tbl_beh[tbl_beh$Gruppe==0,target_coloumnname]
  } else if (group == "Group1"){
    data = tbl_beh[tbl_beh$Gruppe==1,target_coloumnname]
  }
  return(data)

}


get_data_groupmean <-function(data, group, tbl_beh = g_beh()){
  if (length(dim(data))==4){
    if (group == "all_groups") {
      tmp = data
      data_group = apply(tmp, c(2,3,4),mean)
    } else if (group == "Group0"){
      tmp = data[tbl_beh$Gruppe==0,, ,]
      data_group = apply(tmp, c(2,3,4),mean)
    } else if (group == "Group1"){
      tmp = data[tbl_beh$Gruppe==1,, ,]
      data_group = apply(tmp, c(2,3,4),mean)
    }}
  if (length(dim(data))==5){
    if (group == "all_groups") {
      tmp = data
      data_group = apply(tmp, c(2,3,4,5),mean)
    } else if (group == "Group0"){
      tmp = data[tbl_beh$Gruppe==0,, ,,]
      data_group = apply(tmp, c(2,3,4,5),mean)
    } else if (group == "Group1"){
      tmp = data[tbl_beh$Gruppe==1,, ,,]
      data_group = apply(tmp, c(2,3,4,5),mean)
    }
  }
  return(data_group)
}



get_data_groupmean_trial <- function(data, group,trial, tbl_beh = g_beh(), method = g_act_method()){
  data_groupmean = get_data_groupmean(data, group, tbl_beh = tbl_beh)
  if (method=="Transferentropy"){
    data_groupmean_trial = data_groupmean[,,trial]
  }
  else{
    if (length(dim(data))==4){
      data_groupmean_trial = data_groupmean[,trial,]
    }
    if (length(dim(data))==5){
      data_groupmean_trial = data_groupmean[,,trial,]
    }
  }
  return(data_groupmean_trial)
}

get_data_groupmean_trial_freq <- function(data, group,trial,freq, tbl_beh = g_beh(), method = g_act_method()){
  #cat(file = stderr(), paste0("get_data_groupmean_trial_freq dim(data)=",dim(data),"\n"))
  data_groupmean_trial = get_data_groupmean_trial(data, group,trial, tbl_beh = tbl_beh, method = method)
  #cat(file = stderr(), paste0("get_data_groupmean_trial_freq dim(data_groupmean_trial)=",dim(data_groupmean_trial),"\n"))
  data_groupmean_trial_freq = filter_by_selfreq(data_groupmean_trial, freq, method = method)
  #cat(file = stderr(), paste0("get_data_groupmean_trial_freq dim(data_groupmean_trial_freq)=",dim(data_groupmean_trial_freq),"\n"))

  return(data_groupmean_trial_freq)
}

get_data_group_region <- function(data, group, region, tbl_beh = g_beh(), method = g_act_method()){
  #cat(file=stderr(),paste0("get_data_group_region - length(dim(data))=",length(dim(data)),"\n"))

  data_group = get_data_group(data, group, tbl_beh = tbl_beh, method = method)
  if (length(dim(data))==4){
    data_group_region = data_group[,region,,]
  }
  if (length(dim(data))==5){
  #  data_group_region = data_group[,region,region,,]
    data_group_region = data_group[,region,,,]
  }
  return(data_group_region)
}


# NR
get_data_group_trial <- function(data, group, trial, tbl_beh = g_beh(),method = g_act_method()){
  #cat(file = stderr(), paste0("get_data_group_trial dim(data)=",dim(data),"\n"))
  data_group = get_data_group(data, group, tbl_beh = tbl_beh, method = method)
  #cat(file=stderr(),paste0("data_group in get_data_group_trial - dim(data_group)=",dim(data_group),"\n"))
  if (method == "Transferentropy"){
    if (length(dim(data_group))==3){
      data_group_trial = data_group[,,trial]
    }
    if (length(dim(data_group))==4){
      data_group_trial = data_group[,,,trial]
    }

  }else{
  if (length(dim(data))==4){
    data_group_trial = data_group[,,trial,]
  }
  if (length(dim(data))==5){
    data_group_trial = data_group[,,,trial,]
  }
  }
  return(data_group_trial)
}

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

filter_by_selfreq <- function(data, freq, method = g_act_method()){
  num_dims = length(dim(data))
  if (method=="Transferentropy"){
    #Transferentropy has no frequencies
    #
    data_selfreq = data
  }else{
    if (num_dims==2){
      data_selfreq = data[ , freq]
    }else if (num_dims==3){
      data_selfreq = data[ , , freq]
    } else if (num_dims==4){
      data_selfreq = data[ , , , freq]
    } else if (num_dims==5){
      data_selfreq = data[ , , , ,freq]
    }else{
      cat(file = stderr(), paste0("problem with matrix dim in filter_by_selfreq with method =",method,"\n"))
      cat(file = stderr(), "dim(data)=")
      cat(file = stderr(), dim(data))
      cat(file = stderr(), "\n")
    }
  }
  return(data_selfreq)
}



get_freqmean <- function(data, freq, method = g_act_method()){
  num_dims = length(dim(data))
  if (method=="Transferentropy"){
    #Transferentropy has no frequencies
    #
    # die 5. dimension ist eine einzelne Frequenz
    if (num_dims==2 && dim(data)[num_dims]==1){
      data_freqmean = data[,1]
    }else if (num_dims==3 && dim(data)[num_dims]==1){
      data_freqmean = data[,,1]
    }else if (num_dims==4 && dim(data)[num_dims]==1){
      data_freqmean = data[,,,1]
    }else if (num_dims==5 && dim(data)[num_dims]==1){
      data_freqmean = data[,,,,1]
    }else{
      data_freqmean = data
      #cat(file = stderr(), "\n!!!!!!!!!!PROBLEM!!!!!!!!!! \n")
      cat(file = stderr(), paste0("please check dim in get_freqmean with method = Transferentropy\n"))
      cat(file = stderr(), "dim(data)=")
      cat(file = stderr(), dim(data))
      cat(file = stderr(), "\n")
    }
  }else{
    if (num_dims == 2){
      data_freqmean = apply(data, c(1),mean)
    }
    else if (num_dims == 3){
      data_freqmean = apply(data, c(1,2),mean)
    }
    else if (num_dims == 4){
      data_freqmean = apply(data, c(1,2,3),mean)
    }
    else if (num_dims == 5){
      data_freqmean = apply(data, c(1,2,3,4),mean)
    }else{
      cat(file = stderr(), paste0("problem with matrix dim in get_freqmean with method =",method,"\n"))
      cat(file = stderr(), "dim(data)=")
      cat(file = stderr(), dim(data))
      cat(file = stderr(), "\n")
    }
  }
  return(data_freqmean)
}

get_data_group_freqmean <- function(data, group, freq, tbl_beh = g_beh(), method = g_act_method()){
  #cat(file = stderr(), paste0("get_data_group_freqmean dim(data)=",dim(data),"\n"))
  #  dim(Transferentropy) = 46,28,28,2,1
  data_group_freq = get_data_group_freq(data, group, freq, tbl_beh = tbl_beh, method = method)
  data_group_freqmean = get_freqmean(data_group_freq, freq, method = method)
  # cat(file = stderr(), paste0("dim(data_group_freq)=",dim(data_group_freq),"\n"))
  #
  # if (length(dim(data))==4){
  #   data_group_freqmean = apply(data_group_freq, c(1,2,3),mean)
  # }
  # if (length(dim(data))==5){
  #   data_group_freqmean = apply(data_group_freq, c(1,2,3,4),mean)
  # }
  return(data_group_freqmean)
}


get_data_group_trial_freq <- function(data, group, trial, freq, tbl_beh = g_beh(), method = g_act_method()){
  data_group_trial = get_data_group_trial(data, group, trial, tbl_beh = tbl_beh, method = method)
  data_group_trial_freq = filter_by_selfreq(data_group_trial, freq, method = method)
  return(data_group_trial_freq)
}


get_data_group_trial_freqmean <- function(data, group, trial, freq, tbl_beh = g_beh(), method = g_act_method()){
  #cat(file = stderr(), paste0("trial = ", trial, "\n"))
  data_group_trial = get_data_group_trial(data, group, trial, tbl_beh = tbl_beh, method = method)
  data_group_trial_freq = filter_by_selfreq(data_group_trial, freq, method = method)
  data_group_trial_freqmean = get_freqmean(data_group_trial_freq, freq, method = method)
#  x = (as.numeric(ufreq_list)>freq[1]) == (as.numeric(ufreq_list)<freq[2])
  # x = sel_freqs
  # if (length(dim(data))==4){
  #   data_group_trial_freq = data_group_trial[ , , x]
  #   data_group_trial_freqmean = apply(data_group_trial_freq, c(1,2),mean)
  # }
  # if (length(dim(data))==5){
  #   data_group_trial_freq = data_group_trial[ , , , x]
  #   data_group_trial_freqmean = apply(data_group_trial_freq, c(1,2,3),mean)
  # }
  return(data_group_trial_freqmean)
}
#
# get_data_group_trial_freqmean <- function(data, group, trial, freq){
#   data_group_trial = get_data_group_trial(data, group, trial)
#   x = (as.numeric(ufreq_list)>freq[1]) == (as.numeric(ufreq_list)<freq[2])
#   if (length(dim(data))==4){
#     data_group_trial_freq = data_group_trial[ , , x]
#   }
#   if (length(dim(data))==5){
#     data_group_trial_freq = data_group_trial[ , , , x]
#   }
#   data_group_trial_freqmean = apply(data_group_trial_freq, c(1,2,3),mean)
#   return(data_group_trial_freqmean)
# }

get_selected_freq_list <- function(freq_list, freq){
  selected_freq_list = (as.numeric(freq_list)>freq[1]) == (as.numeric(freq_list)<freq[2])
  return(selected_freq_list)
}

get_data_group_region_trial <- function(data, group, region, trial, tbl_beh = g_beh(), method = g_act_method()){
 # cat(file=stderr(),paste0("get_data_group_region_trial - length(dim(data))=",length(dim(data)),"\n"))
  cat(file = stderr(), paste0("dim(data) =", dim(data),"\n"))
  data_group_region = get_data_group_region(data, group, region, tbl_beh = tbl_beh, method = method)
  cat(file = stderr(), paste0("dim(data_group_region) =", dim(data_group_region),"\n"))
  if (length(dim(data))==4){
    data_group_region_trial = data_group_region[,trial,]
  }
  if (length(dim(data))==5){
    data_group_region_trial = data_group_region[,,trial,]
  }
  return(data_group_region_trial)
}

get_data_group_region_trial_freq <- function(data, group, region, trial, freq, tbl_beh = g_beh(), method = g_act_method()){
  cat(file=stderr(),paste0("get_data_group_region_trial_freq - length(dim(data))=",length(dim(data)),"\n"))

  data_group_region_trial = get_data_group_region_trial(data, group, region, trial, tbl_beh = tbl_beh, method = method)
#  x = (as.numeric(ufreq_list)>freq[1]) == (as.numeric(ufreq_list)<freq[2])
  data_group_region_trial_freq = filter_by_selfreq(data_group_region_trial, freq, method = method)
  #   x = sel_freqs
  # if (length(dim(data))==4){
  #   data_group_region_trial_freq = data_group_region_trial[ , x]
  # }
  # if (length(dim(data))==5){
  #   data_group_region_trial_freq = data_group_region_trial[ , , x]
  # }
  return(data_group_region_trial_freq)
}


get_data <- function(directory){
 # cat(file=stderr(), "read mdat from folder ", directory,"\n")
  mdatc = readRDS(file.path("../data", directory, "tbl_data.Rda"))
  num_dims = length(dim(mdatc))

  # wenn die Frequenz nur aus einer Dimension besteht dann nehmen wir sie raus
  if (num_dims==3 && dim(mdatc)[num_dims]==1){
    mdatc = mdatc[,,1]
  }

  if (num_dims==4 && dim(mdatc)[num_dims]==1){
    mdatc = mdatc[,,,1]
  }
  if (num_dims==5 && dim(mdatc)[num_dims]==1){
    mdatc = mdatc[,,,,1]
  }
  return(mdatc)
}

# get_data <- function(method){
#   cat(file=stderr(),"read mdat from folder ", method,"\n")
#   mdatc = readRDS(file.path("../data",method,"tbl_data.Rda"))
#   return(mdatc)
# }

get_global_data <- function(method, tbl_beh = g_beh()){
  cat(file = stderr(),file.path("../data",method,"uregion_list.Rda") )
  uregion_list <<- readRDS(file.path("../data",method,"uregion_list.Rda"))
  utrial_list <<- readRDS(file.path("../data",method,"utrial_list.Rda"))
  ufreq_list <<- readRDS(file.path("../data",method,"ufreq_list.Rda"))
  tbl_beh <<- readRDS(file.path("../data",method,"tbl_beh.Rda"))



  #coln <- colnames(tbl_freq)

  # create a named list for selection Box
  region_names <<- list()
  trial_names <<- list()
  #group_names <<- list()
  j = 0
  for (i in uregion_list){j=j+1;  region_names[i]=j }
  j = 0
  for (i in utrial_list){j=j+1;  trial_names[i]=j }
  j = 1
  group_names <<- c("all_groups")
  for (i in unique(tbl_beh()$Gruppe)) {
    j=j+1;  group_names[j]=paste("Group", toString(i), sep = "")
  }


  #  mdatc = readRDS(file.path("../data",method,"tbl_data.Rda"))
}


get_global_data_for_debugging <- function(method){
  cat(file = stderr(), "load data into global environment for debugging\n")
#  cat(file = stderr(),file.path("../data",method,"uregion_list.Rda") )
  uregion_list <<- readRDS(file.path("../data",method,"uregion_list.Rda"))
  utrial_list <<- readRDS(file.path("../data",method,"utrial_list.Rda"))
  ufreq_list <<- readRDS(file.path("../data",method,"ufreq_list.Rda"))
  tbl_beh <<- readRDS(file.path("../data",method,"tbl_beh.Rda"))
  data <<- get_data(method)


  #coln <- colnames(tbl_freq)

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



get_global_uregion_list <- function(method){
  req(g_reload_rVal())
  #cat(file = stderr(), paste0("get_global_uregion with reload = ", g_reload_rVal(),"\n"))
  uregion_list <- readRDS(file.path("../data",method,"uregion_list.Rda"))
  return(uregion_list)
}



get_global_utrial_list <- function(method){
  utrial_list <- readRDS(file.path("../data",method,"utrial_list.Rda"))
  return(utrial_list)
}

get_global_ufreq_list <- function(method){
  ufreq_list <- readRDS(file.path("../data",method,"ufreq_list.Rda"))
  return(ufreq_list)
}

get_global_tbl_beh <- function(method){
  tbl_beh <- readRDS(file.path("../data",method,"tbl_beh.Rda"))
  return(tbl_beh)
}

get_global_region_names <- function(method){
  uregion_list <- readRDS(file.path("../data",method,"uregion_list.Rda"))
  # create a named list for selection Box
  region_names <- list()
  j = 0
  for (i in uregion_list){j=j+1;  region_names[i]=j }
  return(region_names)
}

get_global_trial_names <- function(method){
  utrial_list <- readRDS(file.path("../data",method,"utrial_list.Rda"))
  trial_names <- list()
  j = 0
  for (i in utrial_list){j=j+1;  trial_names[i]=j }
  return(trial_names)
}

get_global_group_names <- function(method){

  cat(file = stderr(), "working dir = " )
  cat(file = stderr(), paste0(getwd(),"\n"))
  filename <- file.path("../data",method,"tbl_beh.Rda")
  cat(file = stderr(), paste0("filename = ", filename, "\n"))
  tbl_beh <- readRDS(filename)
  # create a named list for selection Box
  j = 1
  group_names <- c("all_groups")
  for (i in unique(tbl_beh$Gruppe)) {
    j=j+1;  group_names[j]=paste("Group", toString(i), sep = "")
  }
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




