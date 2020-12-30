---
title: "Simple Correlation"
output:
  html_document:
    toc: yes
pagetitle: SimpleCorrelation
---


# Simple Correlation
--------------------------------------

The table below shows the correlation between the choosen 
Regressors the selected Regions in the heatmap
Every correlation coefficient stand for himself.
No ANOVA, no Partial correlation only one regressor and the 
electrophysiological Signal.

-------------------------------

$$r = \frac{cov_{xy}}{s_xs_y}= \frac{\sum{(x_i-\bar{x})(y_i-\bar{y})}}{(N-1)s_xs_y}$$

x is our estimation of stefans Tool e.g. Coherence between 2 regions
we have one value for each subject
y represents the values of the behavioral table in the column names as the choosen regressors

The columns z_dif and p_dif are estimates of comparing independent rs (z_dif and p_dif)
for details please see (Andy Fields, Discovering statistics using R page 239)

******************************


