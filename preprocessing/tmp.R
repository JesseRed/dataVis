#region_list <- ""
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
