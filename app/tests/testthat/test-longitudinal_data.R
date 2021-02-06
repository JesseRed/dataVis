# Test the lexical_sort function from R/example.R
library(data.table)
context("sort")

check_between_HD_and_D<-function(HD1, D1){
  expect_equal(HD1$method, D1$method)
  expect_equal(HD1$ugroup_list, D1$ugroup_list)
  expect_equal(HD1$dimcontent, D1$dimcontent)
  expect_equal(HD1$ufreq_list, D1$ufreq_list)
  expect_lt(length(HD1$id_list),length(D1$id_list))
  expect_equal(length(HD1$id_list), dim(HD1$mdat)[1])
  expect_equal(length(HD1$uregion_list), dim(HD1$mdat)[2])
  expect_equal(length(HD1$uregion_list), dim(HD1$mdat)[3])
  expect_equal(length(HD1$utrial_list), dim(HD1$mdat)[4])
  expect_equal(length(HD1$ufreq_list), dim(HD1$mdat)[5])


}

test_that("longitudinal data analysis works",{
  datapath = file.path(".","data","fMRI")

  cat(file = stderr(), paste0("\n current working dir = ", getwd(),"\n"))

  D1 <- readRDS(file.path(datapath,'D1.Rda'))
  D2 <- readRDS(file.path(datapath,'D2.Rda'))

  freq = c(TRUE)
  g1 = 0
  g2 = 1
  t1 = 1
  t2 = 1

  d <- get_longitudinal_currently_selected_data(D1, D2, g1, g2, t1,t2,
                                                freq,
                                                trials  = D1$utrial_list,
                                                regions = D1$uregion_list,
                                                tbl_beh = D1$df_BD,
                                                method = "RS")




  homogen_group_data = exclude_data_from_not_reoccuring_subjects(D1,D2)
  HD1 <- homogen_group_data$HD1
  HD2 <- homogen_group_data$HD2
  check_between_HD_and_D(HD1,D1)
  check_between_HD_and_D(HD2,D2)


  d1 <<- get_selected_data_considering_group_trial(HD1$mdat, g1, g2 ,t1,t2, freq,  trials = HD1$utrial_list, tbl_beh = HD1$df_BD, method = HD1$method)
  expect_equal(d1$my_paired, FALSE)
  expect_equal(d1$data1, get_data_group_trial_freqmean(HD1$mdat,g1, t1, freq, tbl_beh = HD1$df_BD, method=HD1$method))
  expect_equal(d1$data2, get_data_group_trial_freqmean(HD1$mdat,g2, t2, freq, tbl_beh = HD1$df_BD, method=HD1$method))
  expect_equal(dim(d1$data1), c(2,3,3))
  expect_equal(dim(d1$data2), c(2,3,3))


  d2 <<- get_selected_data_considering_group_trial(HD2$mdat, g1,g2,t1,t2, freq,  trials = HD2$utrial_list, tbl_beh = HD2$df_BD, method = HD2$method)
  expect_equal(d2$my_paired, FALSE)
  expect_equal(d2$data1, get_data_group_trial_freqmean(HD2$mdat,g1, t1, freq, tbl_beh = HD2$df_BD, method=HD2$method))
  expect_equal(d2$data2, get_data_group_trial_freqmean(HD2$mdat,g2, t2, freq, tbl_beh = HD2$df_BD, method=HD2$method))
  expect_equal(dim(d2$data1), c(2,3,3))
  expect_equal(dim(d2$data2), c(2,3,3))



  d <- get_currently_selected_data_long(HD1$mdat, g1, g2, t1, t2, freq,
                                       trials=HD1$utrial_list,
                                       regions=HD1$uregion_list,
                                       tbl_beh = HD1$df_BD,
                                       method = HD1$method,
                                       datalong = HD2$mdat)
  expect_equal(length(dim(d$mat_p)), 2)
  expect_equal(dim(d$mat_p)[1], 3)
  expect_equal(dim(d$mat_p)[2], 3)
  #
#   uregion_list <- readRDS(file.path(resultspath,"uregion_list.Rda"))
#   utrial_list <- readRDS(file.path(resultspath,"utrial_list.Rda"))
#   ufreq_list <- readRDS(file.path(resultspath,"ufreq_list.Rda"))
#   tbl_behx <- readRDS(file.path(resultspath,"df_BD.Rda"))
#   Ms <- readRDS(file.path(resultspath, "matrix_data.Rda"))
#   D<-readRDS(file.path(resultspath, "D.Rda"))
#   Dsave <<-D
#   tbl_behx <- D$df_BD
#
#   M <<- D$mdat

#  expect_equal(M, Ms)



  })





# mybasepath = file.path("../data", savedirname)
# saveRDS(uregion_list, file = file.path(mybasepath, "uregion_list.Rda"))
# saveRDS(utrial_list,  file = file.path(mybasepath, "utrial_list.Rda" ))
# saveRDS(ufreq_list,   file = file.path(mybasepath, "ufreq_list.Rda"  ))
# saveRDS(id_list,      file = file.path(mybasepath, "id_list.Rda"     ))
# saveRDS(tbl_beh,      file = file.path(mybasepath, "tbl_beh.Rda"     ))
# saveRDS(D,         file = file.path(mybasepath, "tbl_data.Rda"    ))
