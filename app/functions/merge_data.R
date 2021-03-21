library(readr)
library(stringr)
library(rjson)
library(shinyalert)

perform_mergedata <-function(outdir, datafilename1 = NULL, datafilename2 = NULL,
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
  cat(file = stderr(), paste0("perform_mergedata with outdir = ", outdir,", datafilename1 = ",datafilename1,", datafilename2 = ",datafilename2,", postfix = ", postfix,"\n"))
  cat(file = stderr(), paste0("outdir = ", outdir,"\n"))

  if (is.null(datafilename1)){datafilename1 =  "./app/tests/testthat/data/MEG/export_conn_coh.json"}
  data1 <- readRDS(file = datafilename1)
  if (is.null(datafilename2)){datafilename2 =  "./app/tests/testthat/data/MEG/export_conn_coh.json"}
  data2 <- readRDS(file = datafilename2)



  method <- data1$method




  outdir<- check_and_create_data_dir(method = method, postfix = postfix, datarootpath = datarootpath, istest = istest)

  # check for consistency and eleminate empty trials and frequencies
  check_data_structure(data1, data2, method)

  cat(file = stderr(), "create_new_data_structure\n")
  D <- merge_data_structure(data1, data2)

  cat(file = stderr(), "save_data_structure\n")
  save_data_structure(outdir, D)
  cat(file = stderr(), "merging of data finished in \n")
  preprocessing_time = Sys.time()-start_time
  if (!(istest)){
    shinyalert("Yeaahhh!", paste0("merging of data finished afer ",round(preprocessing_time,3)," sec."), type = "success")
  }
return(D)
}

check_data_structure<-function(data1, data2, method){
  if (!identical(data1$method, data2$method)){
    cat(file = stderr(), paste0("methods not identical:", data1$method, " vs. ", data2$method,"\n"))
  }
  if (!identical(data1$utrial_list, data2$utrial_list)){
    cat(file = stderr(), paste0("utrial_list not identical:", data1$utrial_list, " vs. ", data2$utrial_list,"\n"))
  }

  if (!identical(data1$ugroup_list, data2$ugroup_list)){
    cat(file = stderr(), paste0("ugroup_list not identical:", data1$ugroup_list, " vs. ", data2$ugroup_list,"\n"))
  }

  if (!identical(data1$uregion_list, data2$uregion_list)){
    cat(file = stderr(), paste0("uregion_list not identical:", data1$uregion_list, " vs. ", data2$uregion_list,"\n"))
  }

  if (!identical(data1$ufreq_list, data2$ufreq_list)){
    cat(file = stderr(), paste0("ufreq_list not identical:", data1$ufreq_list, " vs. ", data2$ufreq_list,"\n"))
  }


}

merge_data_structure <- function(data1, data2){

  # geaendert muessen die Felder id_list, df_BD und mdata
  # zuerst haenge ich alle einfach aneinander
  # danach gehe ich die id_liste durch und schmeisse dann in allen drei Feldern
  # doppelte raus
  data <- data1
  id_list <- c(data1$id_list, data2$id_list)
  df_BD <- rbind(data1$df_BD, data2$df_BD)
  mdat <- abind(data1$mdat, data2$mdat, along=1)

  # liste True fuer erstes erscheinen ... wenn gleiche subj_id 2. mal kommt dann FALSE
  keep <- (!duplicated(id_list))

  id_list <- id_list[keep]
  df_BD <- df_BD[keep,]
  mdat <- asub(mdat, keep,1, drop = FALSE)
#  mdat <- mdat[keep,,,,,,drop = FALSE]



D <- list(method              = data1$method,
          ugroup_list         = data1$ugroup_list,
          dimcontent          = data1$dimcontent,
          uregion_list        = data1$uregion_list,
          uregion_list_named  = data1$uregion_list_named,
          utrial_list         = data1$utrial_list,
          utrial_list_named   = data1$utrial_list_named,
          ufreq_list          = data1$ufreq_list,
          ufreq_list_named    = data1$ufreq_list_named,
          ufreq_list_num      = data1$ufreq_list_num,
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



