---
title: "Comp_Plot"
output:
  html_document:
    toc: yes
pagetitle: Comp_Plot
---


# Comp_Plot
--------------------------------------

Plotting ability for trial and Group comparison 

The groups and Trials are selectable
1. If group1==group2 and Trial1!=Trail2: comparison of two trials within the same group
1. If group1!=group2 and Trial1==Trail2: Comparison of the same Trial between groups
1. If group1!=group2 and Trial1!=Trail2: Estimate the difference between two trials within a group. Then use these differences to test for group differences

-----------------------------------
save Data
saving the data of the plot (2D) in the folder exported_variables_from_visualizer


1. with p Values of each region comparison (mat_p) in file ExportData2D_mat_p.Rds
1. with t Values of each region comparison (mat_t) in file ExportData2D_mat_t.Rds
1. 3D data that build the base for the t-test (data1) in file ExportData3D_data1_subj_reg1_reg2.Rds
1. 3D data that build the base for the t-test (data2) in file ExportData3D_data2_subj_reg1_reg2.Rds

The data differ in respect to what was selected for groups, trials and frequency

******************************
The lines in the circle plot are the log of the corresponding p-value of this connection
The width of the line corresponds to its relevance in respect to all connections and represents the importance of this connection 
(log(p)/(sum of all(log(p))
******************************

