#
# uregion_list = readRDS("./data/uregion_list.Rda")
# utrial_list = readRDS("./data/utrial_list.Rda")
# ufreq_list = readRDS("./data/ufreq_list.Rda")
# tbl_beh = readRDS("./data/tbl_beh.Rda")


get_data_group <-function(data, group){
  #cat(file=stderr(),paste0("get_data_group - length(dim(data))=",length(dim(data)),"\n"))
  #browser()
  #cat(dim(data))
  #print(dim(data))
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


get_data_freqmean <- function(data, freq){
  x = (as.numeric(ufreq_list)>freq[1]) == (as.numeric(ufreq_list)<freq[2])

  if (length(dim(data))==4){
    data_freq = data[ , , , x]
    data_freqmean = apply(data_freq, c(1,2,3),mean)
  }
  if (length(dim(data))==5){
    data_freq = data[ , , , , x]
    data_freqmean = apply(data_freq, c(1,2,3,4),mean)
  }
  return(data_freqmean)
}


get_beh_tbl_data_by_group <- function(group, target_coloumnname){

  if (group == "all_groups") {
    data = tbl_beh[ ,target_coloumnname]
  } else if (group == "Group0"){
    data = tbl_beh[tbl_beh$Gruppe==0,target_coloumnname]
  } else if (group == "Group1"){
    data = tbl_beh[tbl_beh$Gruppe==1,target_coloumnname]
  }
  return(data)

}


get_data_groupmean <-function(data, group){
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



get_data_groupmean_trial <- function(data, group,trial){
  data_groupmean = get_data_groupmean(data, group)
  if (length(dim(data))==4){
    data_groupmean_trial = data_groupmean[,trial,]
  }
  if (length(dim(data))==5){
    data_groupmean_trial = data_groupmean[,,trial,]
  }
  return(data_groupmean_trial)
}

get_data_groupmean_trial_freq <- function(data, group,trial,freq){

  data_groupmean_trial = get_data_groupmean_trial(data, group,trial)
  x = (as.numeric(ufreq_list())>freq[1]) == (as.numeric(ufreq_list())<freq[2])

  if (length(dim(data))==4){
    data_groupmean_trial_freq = data_groupmean_trial[,x]
  }
  if (length(dim(data))==5){
    # in Transferentrpy no frequency is given ... 1 dim
    if (dim(data)[5]==1){
      data_groupmean_trial_freq = data_groupmean_trial
    }else{
      data_groupmean_trial_freq = data_groupmean_trial[,,x]
    }
  }
  return(data_groupmean_trial_freq)
}

get_data_group_region <- function(data, group, region){
  cat(file=stderr(),paste0("get_data_group_region - length(dim(data))=",length(dim(data)),"\n"))

  data_group = get_data_group(data, group)
  if (length(dim(data))==4){
    data_group_region = data_group[,region,,]
  }
  if (length(dim(data))==5){
    data_group_region = data_group[,region,region,,]
  }
  return(data_group_region)
}

get_data_group_trial <- function(data, group, trial){
  cat(file = stderr(), paste0("get_data_group_trial dim(data)=",dim(data),"\n"))
  data_group = get_data_group(data, group)
  if (length(dim(data))==4){
    data_group_trial = data_group[,,trial,]
  }
  if (length(dim(data))==5){
    data_group_trial = data_group[,,,trial,]
  }
  return(data_group_trial)
}

get_data_group_freq <- function(data, group, freq){
  cat(file = stderr(), paste0("get_data_group_freq dim(data)=",dim(data),"\n"))
  data_group = get_data_group(data, group)
  x = (as.numeric(ufreq_list)>freq[1]) == (as.numeric(ufreq_list)<freq[2])
  if (length(dim(data))==4){
    data_group_freq = data_group[ , , , x]
  }
  if (length(dim(data))==5){
    data_group_freq = data_group[ , , , , x]
  }
  return(data_group_freq)
}


get_data_group_freqmean <- function(data, group, freq){
  cat(file = stderr(), paste0("get_data_group_freqmean dim(data)=",dim(data),"\n"))
  data_group_freq = get_data_group_freq(data, group, freq)
  if (length(dim(data_group_freq))==4){
    data_group_freqmean = apply(data_group_freq, c(1,2,3),mean)
  }
  if (length(dim(data_group_freq))==5){
    data_group_freqmean = apply(data_group_freq, c(1,2,3,4),mean)
  }
  return(data_group_freqmean)
}


get_data_group_trial_freq <- function(data, group, trial, freq){
  data_group_trial = get_data_group_trial(data, group, trial)
  x = (as.numeric(ufreq_list)>freq[1]) == (as.numeric(ufreq_list)<freq[2])
  if (length(dim(data))==4){
    data_group_trial_freq = data_group_trial[ , , x]
  }
  if (length(dim(data))==5){
    data_group_trial_freq = data_group_trial[ , , , x]
  }
  return(data_group_trial_freq)
}


get_data_group_trial_freqmean <- function(data, group, trial, freq){
  cat(file = stderr(), paste0("trial = ", trial, "\n"))
  data_group_trial = get_data_group_trial(data, group, trial)
  x = (as.numeric(ufreq_list)>freq[1]) == (as.numeric(ufreq_list)<freq[2])
  if (length(dim(data))==4){
    data_group_trial_freq = data_group_trial[ , , x]
    data_group_trial_freqmean = apply(data_group_trial_freq, c(1,2),mean)
  }
  if (length(dim(data))==5){
    data_group_trial_freq = data_group_trial[ , , , x]
    data_group_trial_freqmean = apply(data_group_trial_freq, c(1,2,3),mean)
  }
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

get_data_group_region_trial <- function(data, group, region, trial){
  cat(file=stderr(),paste0("get_data_group_region_trial - length(dim(data))=",length(dim(data)),"\n"))

  data_group_region = get_data_group_region(data, group, region)
  if (length(dim(data))==4){
    data_group_region_trial = data_group_region[,trial,]
  }
  if (length(dim(data))==5){
    data_group_region_trial = data_group_region[,,trial,]
  }
  return(data_group_region_trial)
}

get_data_group_region_trial_freq <- function(data, group, region, trial, freq){
  cat(file=stderr(),paste0("get_data_group_region_trial_freq - length(dim(data))=",length(dim(data)),"\n"))

  data_group_region_trial = get_data_group_region_trial(data, group, region, trial)
  x = (as.numeric(ufreq_list)>freq[1]) == (as.numeric(ufreq_list)<freq[2])
  if (length(dim(data))==4){
    data_group_region_trial_freq = data_group_region_trial[ , x]
  }
  if (length(dim(data))==5){
    data_group_region_trial_freq = data_group_region_trial[ , , x]
  }
  return(data_group_region_trial_freq)
}


get_data <- function(directory){
  cat(file=stderr(), "read mdat from folder ", directory,"\n")
  mdatc = readRDS(file.path("./data", directory, "tbl_data.Rda"))
  return(mdatc)
}

# get_data <- function(method){
#   cat(file=stderr(),"read mdat from folder ", method,"\n")
#   mdatc = readRDS(file.path("./data",method,"tbl_data.Rda"))
#   return(mdatc)
# }

get_global_data <- function(method){
  cat(file = stderr(),file.path("./data",method,"uregion_list.Rda") )
  uregion_list <<- readRDS(file.path("./data",method,"uregion_list.Rda"))
  utrial_list <<- readRDS(file.path("./data",method,"utrial_list.Rda"))
  ufreq_list <<- readRDS(file.path("./data",method,"ufreq_list.Rda"))
  tbl_beh <<- readRDS(file.path("./data",method,"tbl_beh.Rda"))



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
  for (i in unique(tbl_beh$Gruppe)) {
    j=j+1;  group_names[j]=paste("Group", toString(i), sep = "")
  }


  #  mdatc = readRDS(file.path("./data",method,"tbl_data.Rda"))
}



get_global_uregion_list <- function(method){
  uregion_list <- readRDS(file.path("./data",method,"uregion_list.Rda"))
  return(uregion_list)
}



get_global_utrial_list <- function(method){
  utrial_list <- readRDS(file.path("./data",method,"utrial_list.Rda"))
  return(utrial_list)
}

get_global_ufreq_list <- function(method){
  ufreq_list <- readRDS(file.path("./data",method,"ufreq_list.Rda"))
  return(ufreq_list)
}

get_global_tbl_beh <- function(method){
  tbl_beh <- readRDS(file.path("./data",method,"tbl_beh.Rda"))
  return(tbl_beh)
}

get_global_region_names <- function(method){
  uregion_list <- readRDS(file.path("./data",method,"uregion_list.Rda"))
  # create a named list for selection Box
  region_names <- list()
  j = 0
  for (i in uregion_list){j=j+1;  region_names[i]=j }
  return(region_names)
}

get_global_trial_names <- function(method){
  utrial_list <- readRDS(file.path("./data",method,"utrial_list.Rda"))
  trial_names <- list()
  j = 0
  for (i in utrial_list){j=j+1;  trial_names[i]=j }
  return(trial_names)
}

get_global_group_names <- function(method){

  tbl_beh <- readRDS(file.path("./data",method,"tbl_beh.Rda"))
  # create a named list for selection Box
  j = 1
  group_names <- c("all_groups")
  for (i in unique(tbl_beh$Gruppe)) {
    j=j+1;  group_names[j]=paste("Group", toString(i), sep = "")
  }
  return(group_names)

}


