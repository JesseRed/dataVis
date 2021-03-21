---
title: "Longitudinal_Plot"
output:
  html_document:
    toc: yes
pagetitle: Longitudinal_Plot
---


# Longitudinal_Plot
--------------------------------------

Plotting ability for Trial and Group comparison of longitudinal data

The groups and Trials are selectable
1. If group1==group2 and Trial1!=Trail2: comparison of the difference between two trials within the same group across two measurements (Zeitpunkt 2 - Zeitpunkt 1 ... positive values indicate a positive Effect of time (greater values in the second group))
1. If group1!=group2 and Trial1==Trail2: Comparison of the difference between one Trial of groups across two measurements
1. If group1!=group2 and Trial1!=Trail2: Estimate the difference between two trials within a group. Then subtract between time points. use these differences between time point to test for group differences

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


