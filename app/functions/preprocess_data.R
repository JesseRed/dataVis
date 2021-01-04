library(readr)
library(stringr)

perform_preprocessing <-function(beha, data, savedirname, method = "Coherence", inshiny = TRUE){
  #cat(file = stderr(), "entering perform_preprocessing with method = ")
  #cat(file = stderr(), method)
  #cat(file = stderr(), "\n")
  #cat(file = stderr(), paste0("getwd()=",getwd(), "\n"))

if (method == "Coherence"){
  #cat(file = stderr(), "entering perform_preprocessing... with method = Coherence\n")
  cat(file = stderr(), "now reading the data out of the csv file ... no feedback can be given please be patient for large files\n")

  # if (inshiny){
  # showModal(modalDialog(
  #   title = "Please wait",
  #   "... reading the csv file"
  # ))
  # }
  tbl_inp = data
  # reduce behavioral data to those that are in the data table
  tbl_beh_tmp <- beha[beha$ID %in% data$ID,]
  # now reorder the tbl_beh that this order match the order of the matix
  # later on ... the matrix is indexed only by numbers ... there is no further id
  # therefore it is needed that both are in the same order
  tbl_beh <- tbl_beh_tmp[match(tbl_inp$ID, tbl_beh_tmp$ID),]
  #if (inshiny){ removeModal()}
  test_that_IDs_are_the_same(tbl_beh, tbl_inp)

  id_list = tbl_inp[['ID']]

  coln = colnames(tbl_inp)
  region_list <- character(length=1)
  trial_list <- integer(length=1)

  freq_list <- integer(length=1)



  cat(file = stderr(), "first schleife\n")
  j <- 1
  #withProgress(message = 'parsing columnnames', value = 0, {
  #cat(file = stderr(), paste0("Data read sucessfull with number of columns =",length(coln),"\n"))
  for (i in coln) {
    if (grepl("__",i)) {
      tmp <- strsplit(i,"__")
      region_list[j]<-tmp[[1]][1]
      trial_list[j]<-tmp[[1]][2]
      freq_list[j]<-tmp[[1]][3]
      j <- j+1
    }
  }

  #cat(file = stderr(), "\n\nregion_list = \n")
  #cat(file = stderr(), region_list)
  #cat(file = stderr(), "\n\ntrial_list = \n")
  #cat(file = stderr(), trial_list)
  #cat(file = stderr(), "\n\nfreq_list = \n")
  #cat(file = stderr(), freq_list)
  #  incProgress(1/length(coln), detail = paste("part", j))
  #}) # Progress bar

  uregion_comp_list = unique(region_list)
  # zerlege in erstes und 2. Region
  j = 1
  region_list <- character(length=1)
  #cat(file = stderr(), "second schleife\n")
  for (i in uregion_comp_list){
    if (grepl(">",i)) {
      tmp <- strsplit(i,">")
      region_list[j]<-tmp[[1]][1]
      j = j + 1
      region_list[j]<-tmp[[1]][2]
      j = j + 1
    }
  }

  uregion_list = unique(region_list)
  utrial_list = unique(trial_list)
  ufreq_list = unique(freq_list)
  subj_list <- seq(1,nrow(tbl_inp))
  beh_list <- colnames(tbl_beh)

  # cat(file = stderr(), "\n\nuregion_list = \n")
  # cat(file = stderr(), uregion_list)
  # cat(file = stderr(), "\n\nutrial_list = \n")
  # cat(file = stderr(), utrial_list)
  # cat(file = stderr(), "\n\nufreq_list = \n")
  # cat(file = stderr(), ufreq_list)

  #cat(file = stderr(), "reserving memory\n")
  mdat <- array(data = NA,
                dim = c(nrow(tbl_inp),
                        length(uregion_list),
                        length(uregion_list),
                        length(utrial_list),
                        length(ufreq_list)
                )
  )
  # nun fuellen des datenarrays mdat
  idx = 1
  #cat(file = stderr(), "entering proress bar\n")
  #withProgress(message = 'Creating Datastructure', value = 0, {
  for (num_subj in subj_list){
    if (idx!=num_subj){
      idx = num_subj
      cat(file= stderr(), paste0("subject number ", num_subj ,"/", length(subj_list),"\n"))
    }
    #subject_idx_in_beh_table <- get_subject_idx_in_beh_table(tbl_inp_)
    num_region1 = 0
    for (n_region1 in uregion_list){
      num_region2 = 0
      num_region1 = num_region1 + 1
      for (n_region2 in uregion_list){
        num_trial = 0
        num_region2 = num_region2 + 1
        for (n_trial in utrial_list){
          num_freq = 0
          num_trial = num_trial + 1
          for (n_freq in ufreq_list){
            num_freq = num_freq + 1

            if (n_region1==n_region2){ tmp = 0
            } else{

              region_name = paste0(n_region1,">",n_region2)
              col_name <- paste(region_name,toString(n_trial),toString(n_freq),sep = "__")
              tmp =  tbl_inp[[col_name]][num_subj]
              if (is.null(tmp)){
                region_name = paste0(n_region2,">",n_region1)
                col_name <- paste(region_name,toString(n_trial),toString(n_freq),sep = "__")
                tmp =  tbl_inp[[col_name]][num_subj]
              }

              if (is.null(tmp)){
                tmp = NaN
              }
            }
            mdat[num_subj, num_region1, num_region2, num_trial, num_freq] = tmp
          }
        }
      }
    }
  }
  # Increment the progress bar, and update the detail text.
  #  incProgress(1/length(subj_list), detail = paste("Doing part", num_subj))
  #}) # Progress bar
}

  #####################################
  # Transferentropy
  if (method == "Transferentropy"){
    cat(file = stderr(), "entering perform_preprocessing for Transferentropy\n")
    # if (inshiny){
    # showModal(modalDialog(
    #   title = "Please wait",
    #   "... reading the csv file"
    # ))
    # }
    tbl_inp = data
    # reduce behavioral data to those that are in the data table
    tbl_beh <- beha[beha$ID %in% data$ID,]
    #if (inshiny){ removeModal()}


    coln = colnames(tbl_inp)
    region_list <- character(length=1)
    trial_list <- integer(length=1)
    freq_list <- integer(length=1)

    cat(file = stderr(), "first schleife\n")
    j <- 1
    #withProgress(message = 'parsing columnnames', value = 0, {
    cat(file = stderr(), paste0("length(coln)=",length(coln),"\n"))
    for (i in coln) {
      if (grepl("__",i)) {
        tmp <- strsplit(i,"__")
        region_list[j]<-tmp[[1]][1]
        trial_list[j]<-tmp[[1]][2]
        freq_list[j]<-tmp[[1]][3]
        j <- j+1
      }
    }
    # cat(file = stderr(), "\n\nregion_list = \n")
    # cat(file = stderr(), region_list)
    # cat(file = stderr(), "\n\ntrial_list = \n")
    # cat(file = stderr(), trial_list)
    # cat(file = stderr(), "\n\nfreq_list = \n")
    # cat(file = stderr(), freq_list)
    #  incProgress(1/length(coln), detail = paste("part", j))
    #}) # Progress bar

    uregion_comp_list = unique(region_list)
    # zerlege in erstes und 2. Region
    j = 1
    region_list <- character(length=1)
    cat(file = stderr(), "second schleife\n")

    for (i in uregion_comp_list){
      if (grepl(">",i)) {
        tmp <- strsplit(i,">")
        region_list[j]<-delete_A_B_if_possible(tmp[[1]][1])
        j = j + 1
        region_list[j]<-delete_A_B_if_possible(tmp[[1]][2])
        j = j + 1
      }
    }
    uregion_list = unique(region_list)
    utrial_list = unique(trial_list)
    ufreq_list = unique(freq_list)
    subj_list <- seq(1,nrow(tbl_inp))
    beh_list <- colnames(tbl_beh)

    cat(file = stderr(), "\n\nuregion_list = \n")
    cat(file = stderr(), uregion_list)
    cat(file = stderr(), "\n\nutrial_list = \n")
    cat(file = stderr(), utrial_list)
    cat(file = stderr(), "\n\nufreq_list = \n")
    cat(file = stderr(), ufreq_list)

    cat(file = stderr(), "reserving memory")
    mdat <- array(data = NA,
                  dim = c(nrow(tbl_inp),
                          length(uregion_list),
                          length(uregion_list),
                          length(utrial_list),
                          length(ufreq_list)
                  )
    )
    # nun fuellen des datenarrays mdat
    idx = 1
    cat(file = stderr(), "entering proress bar\n")
    cat(file = stderr(), paste0("length(uregion_list)=",length(uregion_list),"\n"))
    #withProgress(message = 'Creating Datastructure', value = 0, {
    for (num_subj in subj_list){
      if (idx!=num_subj){
        idx = num_subj
       # print(cat(sprintf("subject number %d / %d", num_subj, length(subj_list))))
      }
      num_region1 = 0
      for (n_region1 in uregion_list){
        num_region2 = 0
        num_region1 = num_region1 + 1
        for (n_region2 in uregion_list){
          num_trial = 0
          num_region2 = num_region2 + 1
          for (n_trial in utrial_list){
            num_freq = 0
            num_trial = num_trial + 1
            for (n_freq in ufreq_list){
              num_freq = num_freq + 1

              if (n_region1==n_region2){ tmp = 0
              } else{

                region_name = paste0(n_region1,"_A>",n_region2,"_B")
                col_name <- paste(region_name,toString(n_trial),toString(n_freq),sep = "__")
                tmp =  tbl_inp[[col_name]][num_subj]
                if (is.null(tmp)){
                  region_name = paste0(n_region2,"_A",">",n_region1,"_B")
                  col_name <- paste(region_name,toString(n_trial),toString(n_freq),sep = "__")
                  tmp =  tbl_inp[[col_name]][num_subj]
                }

                if (is.null(tmp)){
                  tmp = NaN
                }
              }
              mdat[num_subj, num_region1, num_region2, num_trial, num_freq] = tmp
            }
          }
        }
      }
    }
    # Increment the progress bar, and update the detail text.
    #  incProgress(1/length(subj_list), detail = paste("Doing part", num_subj))
    #}) # Progress bar
  }


  mybasepath = file.path("../data", savedirname)
  cat(file = stderr(),paste0("mybasepath=",mybasepath,"\n"))
  saveRDS(uregion_list, file = file.path(mybasepath, "uregion_list.Rda"))
  saveRDS(utrial_list,  file = file.path(mybasepath, "utrial_list.Rda" ))
  saveRDS(ufreq_list,   file = file.path(mybasepath, "ufreq_list.Rda"  ))
  saveRDS(id_list,      file = file.path(mybasepath, "id_list.Rda"     ))
  saveRDS(tbl_beh,      file = file.path(mybasepath, "tbl_beh.Rda"     ))
  saveRDS(mdat,         file = file.path(mybasepath, "tbl_data.Rda"    ))

}


delete_A_B_if_possible <- function(mystring){
  # wenn der String mit "_A" oder "_B" endet dann loesche diese beiden Buchstaben
if (str_sub(mystring,-2,-1)=="_A" || str_sub(mystring,-2,-1)=="_B") {
  region_name<-str_sub(mystring,1,-3)
}else{
  region_name<-mystring
}
  return(region_name)
}


test_that_IDs_are_the_same<-function(t1, t2){
  id1 = t1$ID
  id2 = t2$ID
  if (!(length(id1)==length(id2))){
    stop('error length(id1) not == length(id2)')
  }
  for (idx in 1:length(id1)){
    if (!(id1[idx]==id2[idx])){
      stop('error in matching tables ids are not matching ')
    }
  }

}
