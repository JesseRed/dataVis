---
title: "ANCOVA"
output:
  html_document:
    toc: yes
pagetitle: ANCOVA
---


# ANCOVA
--------------------------------------

General procedure for ANCOVA

1. Check that the covariate and any independent variables are independent> yout need to run an ANOVA with the covariate as the outcoem and any independet variabeles as predictoras to check that the covariate does not differ significantly across levels of these variables. If you get a significant result then stop the analysis here. You have basically entered a bottomless pit of despair from which there is no escape. 
1. Do the ANCOVA> assumin all was fine in step 1, run the main analysis of covariance. Depending on what you found with step 2, you might need to run a robust version of the test. 
1. Compute contrasts or post hoc tests> you can try to follow up the analxysis to see which groups differ.
1. Check for homogeneity of regression slopes> rerun the ANCOVA, including the interaction between the independent variable an dthe covariate. If this interaction is significant then you cannot assume homogneitiy of regression slopes.


-------------------------------
See Chapter 11 of the Book "Discovering Statistics with R" - Andy Fields
******************************


