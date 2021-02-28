
export_selected_tab_data <- function(data = NULL){
  if (!is.null(data)){
    string1 <-data$string1
    saveRDS(data$mat_p, file = file.path(g_act_data_dir(), "ExportData2D_mat_p.Rds"))
    saveRDS(data$mat_t, file = file.path(g_act_data_dir(), "ExportData2D_mat_t.Rds"))
    saveRDS(data$data1, file = file.path(g_act_data_dir(), "ExportData3D_data1_subj_reg1_reg2.Rds"))
    saveRDS(data$data2, file = file.path(g_act_data_dir(), "ExportData3D_data2_subj_reg1_reg2.Rds"))
    cat(file = stderr(), paste("data saved into folder",g_act_data_dir(),"\n"))
  }

}
