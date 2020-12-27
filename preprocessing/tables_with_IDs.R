
library(readr)
#export_table_coh <- read_csv("G:/OneDrive/R/Laura/export_table_coh.csv")
export_table_fre <- read_csv("G:/OneDrive/R/Laura/export_table_freq.csv")
tbl_subj_id <- read.csv("G:/OneDrive/R/Laura/export_table_subjectsID.csv", header = TRUE, sep = ";")
tbl_fre_ID <- cbind(tbl_subj_id, export_table_fre)
tbl_fre <- export_table_fre

#tbl_subj_beh <- read.csv("G:/OneDrive/R/Laura/table_behavioral.csv", header = TRUE, sep = ";")
#tbl_subj_id <- read.csv("G:/OneDrive/R/Laura/export_table_subjectsID.csv", header = TRUE, sep = ";")
#tbl_subj_sub <- merge(tbl_subj_id, tbl_subj_beh, by="ID")
#tbl_fre <- cbind(tbl_subj_id, export_table_fre)


region_list <- ""
beh_list <- ""
trial_list <- ""
freq_list <- ""
for (i in coln) {
  if (grepl("__",i)) {
  tmp <- strsplit(i,"__")
  region_list<-c(region_list,tmp[[1]][1])
  trial_list<-c(trial_list,tmp[[1]][2])
  freq_list<-c(freq_list,tmp[[1]][3])
  
  } else {
    beh_list <- c(beh_list, tmp[[1]][1])
  }
}
