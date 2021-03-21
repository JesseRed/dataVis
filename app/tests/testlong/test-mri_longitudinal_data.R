# Test the lexical_sort function from R/example.R
library(data.table)
context("sort")

# check_between_HD_and_D<-function(HD1, D1){
#   expect_equal(HD1$method, D1$method)
#   expect_equal(HD1$ugroup_list, D1$ugroup_list)
#   expect_equal(HD1$dimcontent, D1$dimcontent)
#   expect_equal(HD1$ufreq_list, D1$ufreq_list)
#   expect_lt(length(HD1$id_list),length(D1$id_list))
#   expect_equal(length(HD1$id_list), dim(HD1$mdat)[1])
#   expect_equal(length(HD1$uregion_list), dim(HD1$mdat)[2])
#   expect_equal(length(HD1$uregion_list), dim(HD1$mdat)[3])
#   expect_equal(length(HD1$utrial_list), dim(HD1$mdat)[4])
#   expect_equal(length(HD1$ufreq_list), dim(HD1$mdat)[5])
#
#
# }

test_that("new longitudinal data analysis works",{
  datapath = file.path(".","data","fMRI2")

  cat(file = stderr(), paste0("\n current working dir = ", getwd(),"\n"))

  Dorg <- readRDS(file.path(datapath,'Dorg.Rda'))
  # nun erstellen wir aus dem Datensatz unseren Testdatensatz
  D <- Dorg
  # reduziere auf 3 Regionen
  D$uregion_list = Dorg$uregion_list[1:3]
  D$uregion_list_named = Dorg$uregion_list_named[1:3]
  # reduziere auf 4 Subjects + einen der nur einmal vorkommt (ohne longitudinalen Wert)
  D$id_list = Dorg$id_list[9:17]
  D$df_BD = Dorg$df_BD[9:17]
  D$mdat = Dorg$mdat[9:17,1:3,1:3,,,drop=F]


  freq = c(TRUE)
  g1 = 1
  g2 = 1
  t1 = 1
  t2 = 2

  ####################################################################################
  ####################################################################################
  # Testing the Funktion delete_subject_from_data_struct<-function(D = NULL, ids_to_keep = NULL, ids_to_delete = NULL)
  to_delete_str = D$id_list[2:5]
  to_delete_id  = c(2:5)
  to_keep_str   = D$id_list[-c(2:5)]
  to_keep_id   = c(1:length(D$id_list))[-c(2:5)]
  Dtmp1 <- delete_subject_from_data_struct(D=D, ids_to_keep = to_keep_str)
  Dtmp2 <- delete_subject_from_data_struct(D=D, ids_to_keep = to_keep_id)
  Dtmp3 <- delete_subject_from_data_struct(D=D, ids_to_delete = to_delete_str)
  Dtmp4 <- delete_subject_from_data_struct(D=D, ids_to_delete = to_delete_id)
  expect_equal(Dtmp1, Dtmp2)
  expect_equal(Dtmp2, Dtmp3)
  expect_equal(Dtmp3, Dtmp4)
  expect_equal(length(Dtmp1$id_list),5)
  expect_equal(dim(Dtmp1$mdat)[1],5)
  expect_equal(nrow(Dtmp1$df_BD),5)


  ####################################################################################
  ####################################################################################


  # get the data for the second time point
  # die longitudinalen Daten sind kodiert als nummern hinter den IDs der Subjects XY001_1
  # daher teilen wir hier die Subjects einfach entsprechend auf
  filter_g1 = "Zeichen__1>0"
  filter_g2 = "Zeichen__1>0"
  ld_1 = "1"
  ld_2 = "2, 3"

  D1    <- get_data_by_longitudinal_info(D,as.numeric(unlist(strsplit(ld_1, split=","))))
  expect_equal(length(D1$uregion_list), 3)
  expect_equal(length(dim(D1$mdat)), 5)
  expect_equal(dim(D1$mdat)[1], 5)
  expect_equal(dim(D1$mdat)[2], 3)
  expect_equal(dim(D1$mdat)[3], 3)
  expect_equal(dim(D1$mdat)[4], 2)
  expect_equal(dim(D1$mdat)[5], 1)

  # D$df_BD$Zeichen__1 ... 297 822 488 801 268 564 202 796 257
  Dtest <- D
  gDtest <<-D
  #cat(file = stderr(), paste0("\nnrow(D)= ", nrow(D$df_BD),"\n"))
  #cat(file = stderr(), paste0("nrow(Dtest)= ", nrow(Dtest$df_BD),"\n"))
  myfilter = "Zeichen__1>300"
  expect_equal(nrow(Dtest$df_BD),9)
  Dtest <- filter_datastruct(Dtest, group = 1, myfilter = myfilter)
  #cat(file = stderr(), paste0(Dtest2$df_BD))
  expect_equal(nrow(Dtest$df_BD),5)

  #orgIDs       ID       Gruppe ToInclude Lektion Zeichen__1 Zeichen__2 Amin__1 Amin__2 Tippfehler__1 Tippfehler__2
  #1:  VP_05 ID005__1      1         1       3        297        442     101     153             5            16
  #2:  VP_05 ID005__2      1         1       3        822        916     283     315            27            29
  #3:  VP_06 ID006__1      1         1       3        488        507     168     176            15            21
  #4:  VP_06 ID006__2      1         1       3        801        757     277     262            29            29
  #5:  VP_07 ID007__1      1         1       3        268        334      94     118            14            19
  #6:  VP_07 ID007__2      1         1       3        564        551     192     188            11            14
  #7:  VP_08 ID008__1      1         1       3        202        338      68     116             2             9
  #8:  VP_08 ID008__2      1         1       3        796        725     273     245            22            10
  #:   VP_09 ID009__1      1         1       3        257        321      91     112            17            15
  Dtest <- D
  myfilter = "Zeichen__1>300, Tippfehler__1<20"
  expect_equal(nrow(Dtest$df_BD),9)
  Dtest <- filter_datastruct(Dtest, group = 1, myfilter = myfilter)
  expect_equal(nrow(Dtest$df_BD),2)





  # nun erneut mit Ausschluss von Subjects die nicht in beiden Messungen vorhanden sind
  D_new <- split_data_by_longitudinal_info(D, as.numeric(unlist(strsplit(ld_1, split=","))),
                                           as.numeric(unlist(strsplit(ld_2, split=","))),
                                           is_exclude_not_reoccuring_subj = TRUE,
                                           averagelong = TRUE)
  D1    <- D_new$D1
  D2    <- D_new$D2
  data1 <- D1$mdat
  data2 <- D2$mdat
  expect_equal(length(D1$uregion_list), 3)
  expect_equal(length(dim(D1$mdat)), 5)
  expect_equal(dim(D1$mdat)[1], 4)
  expect_equal(dim(D1$mdat)[1], nrow(D1$df_BD))
  expect_equal(dim(D1$mdat)[1], length(D1$id_list))
  expect_equal(dim(D1$mdat)[2], 3)
  expect_equal(dim(D1$mdat)[3], 3)
  expect_equal(dim(D1$mdat)[4], 2)
  expect_equal(dim(D1$mdat)[5], 1)
  expect_equal(length(D2$uregion_list), 3)
  expect_equal(length(dim(D2$mdat)), 5)
  expect_equal(dim(D2$mdat)[1], 4)
  expect_equal(dim(D2$mdat)[1], nrow(D2$df_BD))
  expect_equal(dim(D2$mdat)[1], length(D2$id_list))
  expect_equal(dim(D2$mdat)[2], 3)
  expect_equal(dim(D2$mdat)[3], 3)
  expect_equal(dim(D2$mdat)[4], 2)
  expect_equal(dim(D2$mdat)[5], 1)


  freq = c(TRUE)
  g1 = 1
  g2 = 1
  t1 = 1
  t2 = 2
  #cat(file = stderr(), "........\n\n")
  d<-get_selected_data_considering_group_trial(D$mdat,g1,g2,t1,t2,freq, D$utrials, D$df_BD, method = "Connectivity")
  cat(file = stderr(),paste0(".......explanation=", d$explanation,"\n"))
  gdx <<- d
  expect_equal(dim(d$data1)[1], 9)
  expect_equal(dim(d$data2)[1], 9)
  cat(file = stderr(), "\nthe end \n")

    # subject 1 Trial 1
  #            LH_Vis_1   LH_Vis_2  LH_Vis_3
  # LH_Vis_1  1.0000000 -0.2456532 0.2341862
  # LH_Vis_2 -0.2456532  1.0000000 0.3027033
  # LH_Vis_3  0.2341862  0.3027033 1.0000000


  # M <- get_longitudinal_currently_selected_data3(D1, D2, g1, g2, t1, t2 ,freq,
  #
  #                                                 input$group1,
  #                                                 input$group2,
  #                                                 as.numeric(input$trial1),
  #                                                 as.numeric(input$trial2),
  #                                                 g_sel_freqs(),
  #                                                 estimate_time_first = estimate_time_first())
  #   return(M)
  # })
  #
  #
  # d <- get_longitudinal_currently_selected_data(D1, D2, g1, g2, t1,t2,
  #                                               freq,
  #                                               trials  = D1$utrial_list,
  #                                               regions = D1$uregion_list,
  #                                               tbl_beh = D1$df_BD,
  #                                               method = "RS")
  #
  #
  #
  #
  # homogen_group_data = exclude_data_from_not_reoccuring_subjects(D1,D2)
  # HD1 <- homogen_group_data$HD1
  # HD2 <- homogen_group_data$HD2
  # check_between_HD_and_D(HD1,D1)
  # check_between_HD_and_D(HD2,D2)
  #
  #
  # d1 <<- get_selected_data_considering_group_trial(HD1$mdat, g1, g2 ,t1,t2, freq,  trials = HD1$utrial_list, tbl_beh = HD1$df_BD, method = HD1$method)
  # expect_equal(d1$my_paired, FALSE)
  # expect_equal(d1$data1, get_data_group_trial_freqmean(HD1$mdat,g1, t1, freq, tbl_beh = HD1$df_BD, method=HD1$method))
  # expect_equal(d1$data2, get_data_group_trial_freqmean(HD1$mdat,g2, t2, freq, tbl_beh = HD1$df_BD, method=HD1$method))
  # expect_equal(dim(d1$data1), c(2,3,3))
  # expect_equal(dim(d1$data2), c(2,3,3))
  #
  #
  # d2 <<- get_selected_data_considering_group_trial(HD2$mdat, g1,g2,t1,t2, freq,  trials = HD2$utrial_list, tbl_beh = HD2$df_BD, method = HD2$method)
  # expect_equal(d2$my_paired, FALSE)
  # expect_equal(d2$data1, get_data_group_trial_freqmean(HD2$mdat,g1, t1, freq, tbl_beh = HD2$df_BD, method=HD2$method))
  # expect_equal(d2$data2, get_data_group_trial_freqmean(HD2$mdat,g2, t2, freq, tbl_beh = HD2$df_BD, method=HD2$method))
  # expect_equal(dim(d2$data1), c(2,3,3))
  # expect_equal(dim(d2$data2), c(2,3,3))
  #
  #
  #
  # d <- get_currently_selected_data_long(HD1$mdat, g1, g2, t1, t2, freq,
  #                                      trials=HD1$utrial_list,
  #                                      regions=HD1$uregion_list,
  #                                      tbl_beh = HD1$df_BD,
  #                                      method = HD1$method,
  #                                      datalong = HD2$mdat)
  # expect_equal(length(dim(d$mat_p)), 2)
  # expect_equal(dim(d$mat_p)[1], 3)
  # expect_equal(dim(d$mat_p)[2], 3)
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
