library(readr)
export_table_coh <- read_csv("G:/OneDrive/R/Laura/export_table_coh.csv")
tbl_subj_id <- read.csv("G:/OneDrive/R/Laura/export_table_subjects.csv", header = TRUE, sep = ";")
tbl_subj_beh <- read.csv("G:/OneDrive/R/Laura/table_behavioral.csv", header = TRUE, sep = "|")


# lege die Inputtable fest
tbl_inp <- export_table_coh
#tbl_inp <- export_table_fre
tbl_beh <- merge(tbl_subj_id, tbl_subj_beh, by="ID")




coln = colnames(tbl_inp)
region_list <- character(length=1)
trial_list <- integer(length=1)
freq_list <- integer(length=1)

j <- 1
for (i in coln) {
  if (grepl("__",i)) {
    tmp <- strsplit(i,"__")
    region_list[j]<-tmp[[1]][1]
    trial_list[j]<-tmp[[1]][2]
    freq_list[j]<-tmp[[1]][3]
    j <- j+1
  }
}

uregion_comp_list = unique(region_list)
# zerlege in erstes und 2. Region
j = 1
region_list <- character(length=1)
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
  
# coh_mdat <- array(seq(0,1, length.out = ncol(tbl_inp)*nrow(tbl_inp)),
#               dim = c(nrow(tbl_inp),
#                     length(uregion_list), 
#                     length(utrial_list),
#                     length(ufreq_list)
#               )
#             )
coh_mdat <- array(data = NA,
              dim = c(nrow(tbl_inp),
                      length(uregion_list),
                      length(uregion_list), 
                      length(utrial_list),
                      length(ufreq_list)
              )
)
# nun fuellen des datenarrays coh_mdat
idx = 1
for (num_subj in subj_list){
  if (idx!=num_subj){
    idx = num_subj
    print(cat(sprintf("subject number %d / %d", num_subj, length(subj_list))))
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
            #print(tmp)
            
            #print(col_name)
            #print(cat(sprintf("coh_mdat[%d,%d,%d,%d,%d]=%g ", num_subj, num_region1, num_region2, num_trial, num_freq,tmp)))
            #print("x")
            coh_mdat[num_subj, num_region1, num_region2, num_trial, num_freq] = tmp
          }
      }      
    }
  }
}

coh_uregion_list = uregion_list
coh_ufreq_list = ufreq_list
coh_utrial_list = utrial_list
saveRDS(coh_uregion_list,file = 'uregion_list.Rda')
saveRDS(coh_utrial_list,file = 'utrial_list.Rda')
saveRDS(coh_ufreq_list,file = 'ufreq_list.Rda')
saveRDS(tbl_beh, file = 'tbl_beh.Rda')
saveRDS(coh_mdat,file = 'mcoh.Rda')

