# Test the lexical_sort function from R/example.R
library(data.table)
context("sort")

test_that("Lexical sorting works", {
  expect_equal(my_lexical_sort(c(1, 2, 3)), c(1, 2, 3))
  expect_equal(my_lexical_sort(c(1, 2, 3, 13, 11, 21)), c(1, 11, 13, 2, 21, 3))
})

test_that("preprocessing works",{
  datapath = file.path("data")
  resultspath = file.path(datapath,"test_results")

  method = "Coherence"

  coh_csv_filename = file.path(datapath, "test_coh.csv")
  beh_csv_filename = file.path(datapath, "test_beh.csv")
  df_beh <- fread(beh_csv_filename, header = TRUE, sep = ";", check.names = FALSE)
  df_coh <- fread(coh_csv_filename, header = TRUE, sep = ",", check.names = FALSE)
  cat(file = stderr(), paste0("resultspath =",resultspath,"\n"))
  perform_preprocessing(df_beh, df_coh, file.path("..","testthat",resultspath,method), method = method)

  uregion_list <- readRDS(file.path(resultspath,method,"uregion_list.Rda"))
  utrial_list <- readRDS(file.path(resultspath,method,"utrial_list.Rda"))
  ufreq_list <- readRDS(file.path(resultspath,method,"ufreq_list.Rda"))
  tbl_behx <- readRDS(file.path(resultspath,method,"tbl_beh.Rda"))
  D2 <- readRDS(file.path(resultspath,method, "tbl_data.Rda"))
  D <- get_data("../testthat/data/test_results/Coherence")
  expect_equal(D,D2)
  # test the dimentions
  expect_equal(length(dim(D)),5)
  expect_equal(dim(D)[1],46)
  expect_equal(dim(D)[2],3)
  expect_equal(dim(D)[3],3)
  expect_equal(dim(D)[4],3)
  expect_equal(dim(D)[5],3)

  cat(file = stderr(), paste0("round(sum(D[,1,2,1,1]),2)=",round(sum(D[,1,2,1,1]),2),"\n"))
  expect_equal(round(sum(D[,1,2,1,1]),2),16.44)
  #teste auf Symmetrie
  expect_equal(sum(D[,1,2,1,1]),sum(D[,1,2,1,1]))
  expect_equal(sum(D[,2,3,2,2]),sum(D[,3,2,2,2]))
  # test that groups are correct
  Dg0 <- get_data_group(D,"Group0", tbl_behx, "Coherence")
  Dg1 <- get_data_group(D,"Group1", tbl_behx, "Coherence")
  expect_equal(dim(Dg0)[1],22)
  expect_equal(dim(Dg1)[1],24)

  # trial names
  trial_list <- list("5248"=1, "5760"=2, "6272"=3)
  expect_equal(get_global_trial_names("../testthat/data/test_results/Coherence"),trial_list)
  #expect_equal(get_global_trial_names("../testthat/data/test_results/Coherence"),c("5245", "5760", "6272"))

  # region names
  region_list <- list("frontopolar_A"=1, "central_A"=2, "occipital_A"=3)
  expect_equal(get_global_region_names("../testthat/data/test_results/Coherence"),region_list)
  #expect_equal(get_global_trial_names("../testthat/data/test_results/Coherence"),c("5245", "5760", "6272"))

  tmp<-get_currently_selected_data(D, "Group0", "Group1",1,2,c(TRUE, TRUE, FALSE),
                                   trials = trial_list, regions = region_list,
                                   tbl_beh = tbl_behx, method = "Coherence")
  # tmp$data1 (subjects of group0 x regions x regions)
  Dg0f = apply(Dg0[,,,,1:2], c(1,2,3,4), mean)
  data1_tar = Dg0f[,,,1]-Dg0f[,,,2]
  expect_equal(tmp$data1, data1_tar)
  # tmp$data2 (subjects of group1 x regions x regions)
  Dg1f = apply(Dg1[,,,,1:2], c(1,2,3,4), mean)
  data2_tar = Dg1f[,,,1]-Dg1f[,,,2]
  expect_equal(tmp$data2, data2_tar)


  #expect_equal(tmp$matp,matrix[])
  # get_data_group_region_trial_freq
  Dg0f = apply(Dg0[,,,,1:2],c(1,2,3,4),mean)
#  Dt <<- Dg0f[,]
  Dt <- Dg0[,2,,2,]
  De <- get_data_group_region_trial(D,"Group0",2,2,tbl_beh = tbl_behx, method = "Coherence")
  expect_equal(De,Dt)

#  De <<- get_data_group_region_trial_freq(D,"Group0",1,c(TRUE,TRUE,FALSE),
 #                                         tbl_beh = tbl_behx, method = "Coherence")
  #expect_equal(De,Dt)
  #expect_equal(get_global_trial_names("../testthat/data/test_results/Coherence"),c("5245", "5760", "6272"))


  })


# mybasepath = file.path("../data", savedirname)
# saveRDS(uregion_list, file = file.path(mybasepath, "uregion_list.Rda"))
# saveRDS(utrial_list,  file = file.path(mybasepath, "utrial_list.Rda" ))
# saveRDS(ufreq_list,   file = file.path(mybasepath, "ufreq_list.Rda"  ))
# saveRDS(id_list,      file = file.path(mybasepath, "id_list.Rda"     ))
# saveRDS(tbl_beh,      file = file.path(mybasepath, "tbl_beh.Rda"     ))
# saveRDS(D,         file = file.path(mybasepath, "tbl_data.Rda"    ))
