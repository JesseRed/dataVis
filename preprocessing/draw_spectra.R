library(readr)
readRDS("mfreq.Rda")
readRDS('uregion_list.Rda')
readRDS('utrial_list.Rda')
readRDS('ufreq_list.Rda')


print(utrial_list)
sel_r="frontal_rechts_A"
sel_t="13952"
sel_fl = 0
sel_fh = 10
mf1 = mdat[,sel_r,sel_t,]


# export_table_coh <- read_csv("G:/OneDrive/R/Laura/export_table_coh.csv")
# export_table_fre <- read_csv("G:/OneDrive/R/Laura/export_table_freq.csv")
# tbl_subj_id <- read.csv("G:/OneDrive/R/Laura/export_table_subjectsID.csv", header = TRUE, sep = ";")
# tbl_subj_beh <- read.csv("G:/OneDrive/R/Laura/table_behavioral.csv", header = TRUE, sep = ";")
# 
# tbl_fre_ID <- cbind(tbl_subj_id, export_table_fre)
# 
# # lege die Inputtable fest
# tbl_inp <- export_table_coh
# tbl_inp <- export_table_fre
# tbl_beh <- merge(tbl_subj_id, tbl_subj_beh, by="ID")
# 
# 
# 
# 
# coln = colnames(tbl_inp)
# region_list <- character(length=1)
# trial_list <- integer(length=1)
# freq_list <- integer(length=1)
# 
# j <- 1
# for (i in coln) {
#   if (grepl("__",i)) {
#     tmp <- strsplit(i,"__")
#     region_list[j]<-tmp[[1]][1]
#     trial_list[j]<-tmp[[1]][2]
#     freq_list[j]<-tmp[[1]][3]
#     j <- j+1
#   }
# }
# 
# uregion_list = unique(region_list)
# utrial_list = unique(trial_list)
# ufreq_list = unique(freq_list)
# subj_list <- seq(1,nrow(tbl_inp))
# beh_list <- colnames(tbl_beh)
# 
# mdat <- array(seq(0,1, length.out = ncol(tbl_inp)*nrow(tbl_inp)),
#               dim = c(nrow(tbl_inp),
#                       length(uregion_list), 
#                       length(utrial_list),
#                       length(ufreq_list)
#               )
# )
# # nun fuellen des datenarrays mdat
# 
# for (num_subj in subj_list){
#   num_region = 0
#   for (n_region in uregion_list){
#     num_trial = 0
#     num_region = num_region + 1
#     for (n_trial in utrial_list){
#       num_freq = 0
#       num_trial = num_trail + 1
#       for (n_freq in ufreq_list){
#         num_freq = num_freq + 1
#         col_name <- paste(n_region,toString(n_trial),toString(n_freq),sep = "__")
#         tmp =  tbl_inp[[col_name]][num_subj]
#         #print(col_name)
#         #print(tmp)
#         mdat[num_subj, num_region, num_trial, num_freq] = tmp
#       }      
#     }
#   }
# }
# saveRDS(mdat,file = 'mfreq.Rda')
# saveRDS(uregion_list,file = 'uregion_list.Rda')
# 
# saveRDS(mdat,file = 'mcoh.Rda')

