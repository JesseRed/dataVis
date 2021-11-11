library(readr)
library(stringr)
library(rjson)
library(shinyalert)

perform_preprocessing2 <-function(outdir, df_BD=NULL, datafilename = NULL,
                                  postfix="tmp",inshiny = TRUE,
                                  istest = FALSE,
                                  datarootpath = g_datarootpath()){
  # Funktion um die DAten von Stefan und die Behavioralen Daten in
  # ein zur Weiterverarbeitung geeignetes Format zusammen zu bringen
  # uebergeben wird ein
  # outdir               ...  Verzeichnis in das geschrieben werden soll
  # df_BD (data.frame) mit dem inhalt des behavioralen csv
  # data_file (character) ... ein filename von dem Datenfile von Stefan das
  #                           weiter vorverarbeitet werden soll
  # postfix               ... ein individueller Name der Analyse
  #
  # Die Funktion legt am Ende in das Verzeichnis
  start_time <- Sys.time()
  cat(file = stderr(), paste0("outdir = ", outdir,"\n"))

  if (is.null(datafilename)){datafilename =  "./app/tests/testthat/data/MEG/export_conn_coh.json"}
  data <- fromJSON(file = datafilename)


  if (is.null(data)){ data = fromJSON(file = "./app/tests/testthat/data/MEG/export_conn_coh.json")}
  #if (is.null(data)){ data = fromJSON(file = "../dataVisdata/prepro/MEG/export_conn_coh.json")}
  #data2$subjects= data$subjects[1:3]
  if (is.null(df_BD)){ df_BD = read.csv(file = "./app/tests/testthat/data/MEG/bd.csv", header = TRUE, sep = ';', check.names = FALSE)}
  #if (is.null(df_BD)){ df_BD = read.csv(file = "../dataVisdata/prepro/MEG/bd.csv", header = TRUE, sep = ';', check.names = FALSE)}
#  cat(file = stderr(), "get_methodname\n")
#  cat(file = stderr(), paste0("class(data)=",class(data), "\n"))

  method <- get_methodname(data)

  outdir<- check_and_create_data_dir(method = method, postfix = postfix, datarootpath = datarootpath, istest = istest)

  # check for consistency and eleminate empty trials and frequencies
  data<-check_data_structure(data, df_BD, method)
  prepro_data <<- data
  prepro_df_BD <<- df_BD
  cat(file = stderr(), "extract_data_array\n")
  mdat <- extract_data_array(data, df_BD, method)

  cat(file = stderr(), "create_new_data_structure\n")
  D <- create_new_data_structure(data, df_BD, mdat, method)

  cat(file = stderr(), "save_data_structure\n")
  save_data_structure(outdir, D)
  cat(file = stderr(), "preprocessing finished in \n")
  preprocessing_time = Sys.time()-start_time
  if (!(istest)){
    shinyalert("Yeaahhh!", paste0("preprocessing finished afer ",round(preprocessing_time,3)," sec."), type = "success")
  }
return(D)
}

check_data_structure<-function(data, df_BD, method){
  # create data$channelcmb$from_num und data$channelcmb$to_num
  # die Datachannels sind als Strings abgelegt ... wir brauchen sie aber als Nummern

  # wenn das Feld "channelcmb" fehlt dann werden alle mit allen kombiniert
  if (!("channelcmb" %in% names(data))) {
    channelcmb = list("from"=c(), "to"=c())
    idx = 0
    for (i in 1:length(data$channels)){
      for (j in i+1:length(data$channels)){
        data$channelcmb$from[idx] = data$channels[i]
        data$channelcmb$to[idx]   = data$channels[j]
        idx = idx + 1
      }
    }
    data$channelcmb = channelcmb
  }

  data$channelcmb$from_num = match(data$channelcmb$from, data$channels)
  data$channelcmb$to_num = match(data$channelcmb$to, data$channels)

  # an dieser Stelle werden die uebergebenen INformationen
  # genutzt
  # es kann jedoch sein, dass sich hieran noch etwas aendert
  # z.B. wenn ein Subject komplett leer ist und er noch
  # raus genommen werden muss
  uregion_list = data$channels
  utrial_list = as.character(data$trials)
  if (length(utrial_list)==0){
    utrial_list = c("no_desc_given")
    data$channels = utrial_list

  }

  ufreq_list = as.character(data$freq)
  if (length(ufreq_list)==0){
    cat(file = stderr(),"ufreq_list length = 0")
    ufreq_list = c("0")
    data$freq = utrial_list
  }

 return(data)
}

create_new_data_structure <- function(data, df_BD, mdat, method){

  # die Frage ist hier ob ich wirklich die Dateninformationen
  # nehmen kann oder ob ich noch Funktionen schreiben muss
  # die ggf. Daten rauswerfen und dann auch diese Liste anpassen
  #

  #ugroup_list = paste("Group",as.character(unique(df_BD$Gruppe)),sep="")
  ugroup_list = as.character(unique(df_BD$Gruppe))
  #cat(file = stderr(), paste0("ugroup_list = ",ugroup_list,"\n"))
  uregion_list = data$channels
  uregion_list_named = list()
  uregion_list_named[uregion_list] = 1:length(uregion_list)

  utrial_list = as.character(data$trials)
  if (length(utrial_list)==0){ utrial_list = c("no_desc_given") }

  utrial_list_named = list()
  utrial_list_named[utrial_list] = 1:length(utrial_list)

  ufreq_list = as.character(data$freq)
  if (length(ufreq_list)==0){ ufreq_list = c("0") }
  ufreq_list_named = list()
  ufreq_list_named[ufreq_list] = 1:length(ufreq_list)

  ufreq_list_num = data$freq
  if (length(ufreq_list_num)==0){ ufreq_list_num = c(0) }

  dimcontent = c("sub","reg","reg","tri","fre")

  # anpassen der behavioralen Daten so dass sie zu der neuen DAtenstruktur passen
  df_BD <- df_BD[df_BD$ID %in% data$subjects_id,]
  df_BD <- df_BD[match(data$subjects_id, df_BD$ID),]

  id_list = df_BD[['ID']]

  if (!identical(id_list,data$subjects_id)){

    cat(file = stderr(), "----------------------------------------------------------------------------------------\n")
    cat(file = stderr(), "|---------------------------------ERROR-Description-------------------------------------|\n")

    cat(file = stderr(), "| filtered id_list from the behavioral data and data$subjects_id are not identical     |\n")

    if (length(id_list)<length(data$subjects_id)){
      cat(file = stderr(), "| Behavioral file has less Subjects than the json file ... this is not allowed         |\n")
    }
    cat(file = stderr(), "setdiff(data$subjects_id, df_BD$ID) ... \n")
    cat(file = stderr(), paste0(setdiff(data$subjects_id, df_BD$ID)," is not in both datasets \n"))

    cat(file = stderr(), "for every subject in the json file there MUST be an row in the csv file with the same ID \n")
    cat(file = stderr(), "however, not every ID in the csv file needs an entry in the json data file\n")
    #cat(file = stderr(), "most probably: IDs in the data file are not in the behavioral table\n")
    #cat(file = stderr(), "... here commes the id_list from the behavioral file\n")
    #cat(file = stderr(), paste0(id_list,"\n"))
    #cat(file = stderr(), "\n\n... here commes the id_list from stefans file\n")
    #cat(file = stderr(), paste0(data$subjects_id,"\n"))
    cat(file = stderr(), "please review the BD file according to the setdiff information\n")

    stop("\n end now\n")
  }



D <- list(method              = method,
          ugroup_list         = ugroup_list,
          dimcontent          = dimcontent,
          uregion_list        = uregion_list,
          uregion_list_named  = uregion_list_named,
          utrial_list         = utrial_list,
          utrial_list_named   = utrial_list_named,
          ufreq_list          = ufreq_list,
          ufreq_list_named    = ufreq_list_named,
          ufreq_list_num      = ufreq_list_num,
          id_list             = id_list,
          df_BD               = df_BD,
          mdat                = mdat
          )

}

save_data_structure<- function(outdir, D){

cat(file = stderr(),paste0("saving to outdir=",outdir,"\n"))
saveRDS(D$uregion_list,   file = file.path(outdir, "uregion_list.Rda"))
saveRDS(D$utrial_list,    file = file.path(outdir, "utrial_list.Rda" ))
saveRDS(D$ufreq_list,     file = file.path(outdir, "ufreq_list.Rda"  ))
saveRDS(D$ufreq_list_num, file = file.path(outdir, "ufreq_list.Rda"  ))
saveRDS(D$id_list,        file = file.path(outdir, "id_list.Rda"     ))
saveRDS(D$df_BD,          file = file.path(outdir, "df_BD.Rda"     ))
saveRDS(D$mdat,           file = file.path(outdir, "matrix_data.Rda"    ))
saveRDS(D,                file = file.path(outdir, "D.Rda"    ))



}



extract_data_array<-function(data, df_BD, method){
  #This function takes the data and behavioral data
  # to create the data matrix
  # the dimensions are named



  # create data$channelcmb$from_num und data$channelcmb$to_num
  # die Datachannels sind als Strings abgelegt ... wir brauchen sie aber als Nummern
  data$channelcmb$from_num = match(data$channelcmb$from, data$channels)
  data$channelcmb$to_num = match(data$channelcmb$to, data$channels)

  # an dieser Stelle werden die uebergebenen INformationen
  # genutzt
  # es kann jedoch sein, dass sich hieran noch etwas aendert
  # z.B. wenn ein Subject komplett leer ist und er noch
  # raus genommen werden muss
  uregion_list = data$channels
  utrial_list = as.character(data$trials)
  if (length(utrial_list)==0){
       utrial_list = c("no_desc_given")

  }

  ufreq_list = as.character(data$freq)
  if (length(ufreq_list)==0){
    cat(file = stderr(),"ufreq_list length = 0")
    ufreq_list = c("0")
  }

  # reserviere den Speicher fuer das Datenarray
  mdat <- array(data = NA,
                dim = c(length(data$subjects_id),
                        length(uregion_list),
                        length(uregion_list),
                        length(utrial_list),
                        length(ufreq_list)
                ),
                dimnames = list(data$subjects_id,
                                uregion_list,
                                uregion_list,
                                utrial_list,
                                ufreq_list
                )
  )

  # fuellen des datenarrays mdat
  for (num_subj in seq(1,length(data$subjects_id))){
    cat(file= stderr(), paste0("subject number ", num_subj ,"/", length(data$subjects_id),"\n"))
    dat_subj = data$subjects[[num_subj]]
    if (dat_subj$subject_id != data$subjects_id[num_subj]) {stop("error in compare subject_id")}


    for (num_trial in 1:length(dat_subj$trials)){
      dat_trial = dat_subj$trials[[num_trial]]
      #if (dat_trial$trial_id != n_trial) {stop("error in compare trial_id")}

      for (i in 1:length(dat_trial$dat)){
        # die Daten koennen numerisch sein, wenn aber NULL drin steht sind es listen
        if (class(dat_trial$dat[[i]])=="numeric"){
          #cat(file = stderr(), paste0("num_subj = ", num_subj, "\nfrom_num=",data$channelcmb$from_num[i],"\nto_num=",data$channelcmb$to_num[i], "\nnumtrial=",i,"\n dim(dat)=",dim(dat_trial$dat[[i]]),"\n" ))
          mdat[num_subj, data$channelcmb$from_num[i], data$channelcmb$to_num[i], num_trial, ] = dat_trial$dat[[i]]
        }
        if (class(dat_trial$dat[[i]])=="list"){
          mdat[num_subj, data$channelcmb$from_num[i], data$channelcmb$to_num[i], num_trial, ] = NA
        }
        # erzeuge Symmetrie der Matrix
        if (method == "Coherence"){
          mdat[num_subj, data$channelcmb$to_num[i], data$channelcmb$from_num[i], num_trial, ] = mdat[num_subj, data$channelcmb$from_num[i], data$channelcmb$to_num[i], num_trial, ]
        }
      }

    }
  }
  mdat_save <<- mdat
  return(mdat)
}



get_methodname<-function(data){

  if (data$type=="conn_coh"){method = "Coherence"
  }else if (data$type=="conn_trans"){method = "Transferentropy"
  }else if (data$type=="conn_freq"){method = "Frequency"
  }else if (data$type=="conn_granger"){method = "Granger"
  }else if (data$type=="conn_erp"){method = "ERP"
  }else if (data$type=="fmri_corr"){method = "RS"
  }else { stop(paste0("unknown datatype detected with type=",data$type,"\n")) }

  return(method)
}



check_and_create_data_dir<-function(method = method, postfix = postfix, datarootpath = g_datarootpath(), istest = FALSE){

  myDirName = paste0(method,"_",postfix)
  dir_to_save = file.path(datarootpath, myDirName)


  if (dir.exists(dir_to_save)){

  if (!(istest)){
    showNotification("A directory with this name already exist\n please choose a differnt or delete by hand", type= "error")
    shinyalert(title = "Warning",
               text = "Directory already exist\n if you procede, all content in this directory will be deleted",
               type = "warning",
               showCancelButton = TRUE,
               cancelButtonText = "Cancel",
               showConfirmButton = TRUE,
               confirmButtonText = "delete",
               callbackR = function(x) {
                 cat(file = stderr(), paste0("\nx=",x,"\n"))

                 if(x) {
                   cat(file = stderr(), paste0("delet ... ", dir_to_save,"\n"))
                   cat(file = stderr(), paste0("delete all\n"))
                   unlink(file.path(dir_to_save,"*"))
                 }
               }
    )
  }else{
    unlink(file.path(dir_to_save,"*"))
}
  }else{
    dir.create(dir_to_save)
  }
  return(dir_to_save)

}



