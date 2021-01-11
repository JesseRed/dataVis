# Test the lexical_sort function from R/example.R
library(data.table)
context("sort")

test_that("Lexical sorting works", {
  expect_equal(my_lexical_sort(c(1, 2, 3)), c(1, 2, 3))
  expect_equal(my_lexical_sort(c(1, 2, 3, 13, 11, 21)), c(1, 11, 13, 2, 21, 3))
})


get_sum_freq<- function(data, target){
  #cat(file = stderr(), "get_sumfreq\n")
  s1=0
  for (i in 1:length(data$subjects)){
    #cat(file = stderr(), "subject = ", i,"\n")
    for (j in 1:length(data$subjects[[i]]$trials)){
      for (k in 1:length(data$subjects[[i]]$trials[[j]]$dat)){
        tmp = data$subjects[[i]]$trials[[j]]$dat[[k]][target]
        #cat(file = stderr(),paste0(tmp,"\n"))
        #cat(file = stderr(),paste0("class(tmp)=",tmp,"\n"))

        if (class(tmp)=="list"){s1 = s1 + 0}
        else{
          #cat(file = stderr(),paste0(tmp,"\n"))
          if (is.nan(tmp)){ s1 = s1 + 0
          } else if (is.na(tmp)){s1 = s1 + 0
          } else if (is.double(tmp)){s1 = s1 + tmp
          } else { cat(file = stderr(), paste0("unknown data type with class(tmp) = ", class(tmp), "   val = ", tmp, "\n"))
          }
        }
      }
    }
  }
  #cat(file = stderr(), " end get_sumfreq\n")

  return(s1)
}


get_sum_subj<- function(data, subject){
  s1=0

  for (j in 1:length(data$subjects[[subject]]$trials)){
    for (k in 1:length(data$subjects[[subject]]$trials[[j]]$dat)){
      tmp = sum(data$subjects[[subject]]$trials[[j]]$dat[[k]])
      #cat(file = stderr(),paste0(tmp,"\n"))
      #cat(file = stderr(),paste0("class(tmp)=",tmp,"\n"))

      if (class(tmp)=="list"){s1 = s1 + 0}
      else{
        #cat(file = stderr(),paste0(tmp,"\n"))
        if (is.nan(tmp)){ s1 = s1 + 0
        } else if (is.na(tmp)){s1 = s1 + 0
        } else if (is.double(tmp)){s1 = s1 + tmp
        } else { cat(file = stderr(), paste0("unknown data type with class(tmp) = ", class(tmp), "   val = ", tmp, "\n"))
        }
      }
    }
  }
  return(s1)
}


test_that("preprocessing works",{
  datapath = file.path(".","data","MEG")
  resultspath = file.path(datapath,"test_results","Coherence")

  cat(file = stderr(), paste0("\n current working dir = ", getwd(),"\n"))

  coh_json_filename = file.path(datapath, "export_conn_coh.json")
  beh_csv_filename = file.path(datapath, "bd.csv")
  df_BD <- fread(beh_csv_filename, header = TRUE, sep = ";", check.names = FALSE)
  data <- fromJSON(file = coh_json_filename)

  cat(file = stderr(), paste0("result saved in ... resultspath =",resultspath,"\n"))

  perform_preprocessing2(resultspath, df_BD, data)


  uregion_list <- readRDS(file.path(resultspath,"uregion_list.Rda"))
  utrial_list <- readRDS(file.path(resultspath,"utrial_list.Rda"))
  ufreq_list <- readRDS(file.path(resultspath,"ufreq_list.Rda"))
  tbl_behx <- readRDS(file.path(resultspath,"df_BD.Rda"))
  Ms <- readRDS(file.path(resultspath, "matrix_data.Rda"))
  D<<-readRDS(file.path(resultspath, "D.Rda"))

  tbl_behx <- D$df_BD

  M <<- D$mdat

  expect_equal(M, Ms)
  # test the dimentions
  expect_equal(length(dim(M)),5)
  expect_equal(dim(M)[1],4)
  expect_equal(dim(M)[2],4)
  expect_equal(dim(M)[3],4)
  expect_equal(dim(M)[4],2)
  expect_equal(dim(M)[5],206)

  expect_equal(D$dimcontent[1],"sub")
  expect_equal(D$dimcontent[2],"reg")
  expect_equal(D$dimcontent[3],"reg")
  expect_equal(D$dimcontent[4],"tri")
  expect_equal(D$dimcontent[5],"fre")


  # test of get_beh_tab_data_by_group
  tmp = get_beh_tbl_data_by_group("all_groups", "Alter", D$df_BD)
  expect_equal(c(69,24,32,28), tmp)
  tmp = get_beh_tbl_data_by_group("0", "Alter", D$df_BD)
  expect_equal(c(24,32,28), tmp)
  tmp = get_beh_tbl_data_by_group("1", "Alter", D$df_BD)
  expect_equal(c(69), tmp)

  # test of get_data_group
  tmp = get_data_group(M, "all_groups", D$df_BD, method = D$method)
  expect_equal(M, tmp)
  tmp = get_data_group(M, "0", D$df_BD, method = D$method)
  expect_equal(dim(tmp)[1], 3)
  tmp = get_data_group(M, "1", D$df_BD, method = D$method)
  expect_equal(dim(tmp)[1], 1)



  ####################Test 1 ################
  # summe ueber die 2. Frequenz aller Daten

  expect_equal(sum(M[,,,,2],na.rm=T), get_sum_freq(data,2))
  expect_equal(sum(M[,,,,5],na.rm=T), get_sum_freq(data,5))
  expect_equal(sum(M[,,,,12],na.rm=T), get_sum_freq(data,12))

  expect_equal(sum(M[1,,,,],na.rm=T), get_sum_subj(data,1))
  #cat(file = stderr(), paste0("sum subj1 = ", get_sum_subj(data,1),"\n"))
  expect_equal(sum(M[2,,,,],na.rm=T), get_sum_subj(data,2))
  expect_equal(sum(M[3,,,,],na.rm=T), get_sum_subj(data,3))

  # test get_data_group_mean
  tmp = get_data_groupmean(M, "all_groups", tbl_beh = D$df_BD, method = D$method)
  expect_equal(round(tmp[1,2,1,1],3),0.792)
  tmp = get_data_groupmean(M, "0", tbl_beh = D$df_BD, method = D$method)
  expect_equal(round(tmp[1,2,1,1],3),0.760)
  tmp = get_data_groupmean(M, "1", tbl_beh = D$df_BD, method = D$method)
  expect_equal(round(tmp[1,2,1,1],3),0.856)

  # test get_data_freqmean
  v <- rep(TRUE,dim(M)[5])
  tmp = get_data_freqmean(M, v, method = D$method)
  expect_equal(round(tmp[1,1,2,1],3),0.295)
  v <- rep(FALSE,dim(M)[5])
  tmp = get_data_freqmean(M, v, method = D$method)
  expect_equal(tmp[1,1,2,1],NaN)
  v <- rep(FALSE,dim(M)[5])
  v[1:3]= TRUE
  tmp = get_data_freqmean(M, v, method = D$method)
  expect_equal(round(tmp[1,1,2,1],3),0.856)


  # test get_data_groupmean_trial_freq
  v <- rep(TRUE,dim(M)[5])
  tmp = get_data_groupmean_trial_freq(M, "all_groups",1,v, tbl_beh= D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(4,4,206))
  expect_equal(round(tmp[1,2,1],3),0.792)

  v <- rep(FALSE,dim(M)[5])
  v[1:3]= TRUE
  tmp = get_data_groupmean_trial_freq(M, "all_groups",1,v, tbl_beh= D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(4,4,3))
  expect_equal(round(tmp[1,2,1],4),0.7920)
  expect_equal(round(tmp[1,2,2],4),0.7925)
  expect_equal(round(tmp[1,2,3],4),0.7917)

  v <- rep(FALSE,dim(M)[5])
  v[1:3]= TRUE
  tmp = get_data_groupmean_trial_freq(M, "all_groups",1:2,v, tbl_beh= D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(4,4,2,3))
  expect_equal(round(tmp[1,2,1,1],4),0.7920)
  expect_equal(round(tmp[1,2,1,2],4),0.7925)
  expect_equal(round(tmp[1,2,1,3],4),0.7917)

  v <- rep(FALSE,dim(M)[5])
  v[1:3]= TRUE
  tmp = get_data_groupmean_trial_freq(M, "0",1:2,v, tbl_beh= D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(4,4,2,3))
  expect_equal(round(tmp[1,2,1,1],4),0.7601)
  expect_equal(round(tmp[1,2,1,2],4),0.7607)
  expect_equal(round(tmp[1,2,1,3],4),0.7594)

  v <- rep(FALSE,dim(M)[5])
  v[1:3]= TRUE
  tmp = get_data_groupmean_trial_freq(M, "1",1:2,v, tbl_beh= D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(4,4,2,3))
  expect_equal(round(tmp[1,2,1,1],4),0.8557)
  expect_equal(round(tmp[1,2,1,2],4),0.8559)
  expect_equal(round(tmp[1,2,1,3],4),0.8563)

  # test get_data_group_region
  region = 1
  tmp = get_data_group_region(M, "all_groups", region, tbl_beh= D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(4,4,2,206))
  expect_equal(round(tmp[2,3,2,1],4),0.8382)
  expect_equal(round(tmp[3,4,1,5],4),0.3890)
  expect_equal(tmp[4,1,2,3],NaN)
  expect_equal(tmp[2,1,2,3],NaN)

  region = 2
  tmp = get_data_group_region(M, "0", region, tbl_beh= D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(3,4,2,206))
  expect_equal(round(tmp[1,3,2,3],4),0.8106)
  expect_equal(round(tmp[2,4,1,1],4),0.5268)
  expect_equal(tmp[3,3,1,1],NaN)


  # test get_data_group_trial
  trial = 1
  tmp = get_data_group_trial(M, "all_groups", trial, tbl_beh= D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(4,4,4,206))
  expect_equal(round(tmp[2,2,3,6],4),0.7466)
  expect_equal(round(tmp[3,1,3,2],4),0.4407)
  expect_equal(tmp[4,1,2,3],NaN)
  expect_equal(tmp[2,1,1,3],NaN)

  trial = 2
  tmp = get_data_group_trial(M, "0", trial, tbl_beh= D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(3,4,4,206))
  expect_equal(round(tmp[1,2,3,3],4),0.8106)
  expect_equal(round(tmp[2,1,4,1],4),0.4245)
  expect_equal(tmp[3,3,1,1],NaN)


  # test get_data_group_freq
  v <- rep(TRUE,dim(M)[5])
  tmp = get_data_group_freq(M, "all_groups", v, tbl_beh= D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(4,4,4,2,206))
  expect_equal(round(tmp[2,2,3,1,6],4),0.7466)
  expect_equal(round(tmp[3,1,3,1,2],4),0.4407)
  expect_equal(tmp[4,1,2,1,3],NaN)
  expect_equal(tmp[2,1,1,1,3],NaN)

  v <- rep(FALSE,dim(M)[5])
  v[1:3]= TRUE
  tmp = get_data_group_freq(M, "0", v, tbl_beh= D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(3,4,4,2,3))
  expect_equal(round(tmp[1,2,3,2,3],4),0.8106)
  expect_equal(round(tmp[2,1,4,2,1],4),0.4245)
  expect_equal(tmp[3,1,1,1,1],NaN)


  # test filter_by_selfreq
  v <- rep(TRUE,dim(M)[5])
  tmp = filter_by_selfreq(M, v, method = D$method)
  expect_equal(dim(tmp),c(4,4,4,2,206))
  expect_equal(round(tmp[2,2,3,1,6],4),0.7466)
  expect_equal(round(tmp[3,1,3,1,2],4),0.4407)
  expect_equal(tmp[4,1,2,1,3],NaN)
  expect_equal(tmp[2,1,1,1,3],NaN)

  v <- rep(FALSE,dim(M)[5])
  v[1:3]= TRUE
  tmp = filter_by_selfreq(M, v, method = D$method)
  expect_equal(dim(tmp),c(4,4,4,2,3))
  expect_equal(round(tmp[2,2,3,2,3],4),0.8106)
  expect_equal(round(tmp[3,1,4,2,1],4),0.4245)
  expect_equal(tmp[4,1,1,1,1],NaN)

  # test get_freqmean
  tmp = get_freqmean(M, method = D$method)
  expect_equal(dim(tmp),c(4,4,4,2))
  expect_equal(round(tmp[2,2,3,1],4),0.7483)
  expect_equal(round(tmp[3,1,3,1],4),0.4533)
  expect_equal(tmp[4,1,2,1],NaN)
  expect_equal(tmp[2,1,1,1],NaN)

  # test get_data_group_freqmean
  v <- rep(FALSE,dim(M)[5])
  v[1:3]= TRUE
  tmp = get_data_group_freqmean(M, "all_groups",v, tbl_beh=D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(4,4,4,2))
  expect_equal(round(tmp[2,2,3,2],4),0.8117)
  expect_equal(round(tmp[3,1,3,1],4),0.4403)
  expect_equal(tmp[4,1,2,1],NaN)
  expect_equal(tmp[2,1,1,1],NaN)

  #hier weiter second test
  v <- rep(FALSE,dim(M)[5])
  v[1:3]= TRUE
  tmp = get_data_group_freqmean(M, "0",v, tbl_beh=D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(3,4,4,2))
  expect_equal(round(tmp[1,2,3,2],4),0.8117)
  expect_equal(round(tmp[2,1,3,1],4),0.4403)
  expect_equal(tmp[3,1,2,1],NaN)
  expect_equal(tmp[2,1,1,1],NaN)


  # test get_data_group_freqmean
  v <- rep(FALSE,dim(M)[5])
  v[1:3]= TRUE
  trial = 2
  tmp = get_data_group_trial_freq(M, "all_groups",trial, v, tbl_beh=D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(4,4,4,3))
  expect_equal(round(tmp[2,2,3,1],4),0.8128)
  expect_equal(round(tmp[3,1,3,2],4),0.5258)
  expect_equal(tmp[4,1,2,2],NaN)
  expect_equal(tmp[2,1,1,2],NaN)

  #hier weiter second test
  v <- rep(FALSE,dim(M)[5])
  v[1:5]= TRUE
  trial = 1
  tmp = get_data_group_trial_freq(M, "0",trial, v, tbl_beh=D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(3,4,4,5))
  expect_equal(round(tmp[2,2,3,5],4),0.5104)
  expect_equal(round(tmp[1,1,3,4],4),0.7715)
  expect_equal(tmp[3,1,2,1],NaN)
  expect_equal(tmp[2,1,1,1],NaN)



  # test get_data_group_trial_freqmean
  v <- rep(FALSE,dim(M)[5])
  v[1:3]= TRUE
  trial = 1
  tmp = get_data_group_trial_freqmean(M, "all_groups",trial, v, tbl_beh=D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(4,4,4))
  expect_equal(round(tmp[2,2,3],4),0.7455)
  expect_equal(round(tmp[3,1,3],4),0.4403)
  expect_equal(tmp[4,1,2],NaN)
  expect_equal(tmp[2,1,1],NaN)

  v <- rep(FALSE,dim(M)[5])
  v[1:5]= TRUE
  trial = 2
  tmp = get_data_group_trial_freqmean(M, "0",trial, v, tbl_beh=D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(3,4,4))
  expect_equal(round(tmp[2,2,3],4),0.5085)
  expect_equal(tmp[3,1,2],NaN)
  expect_equal(tmp[2,1,1],NaN)


  # test get_data_group_region_trial
  region = 1
  trial = 1
  tmp = get_data_group_region_trial(M, "all_groups",region, trial, tbl_beh=D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(4,4,206))
  expect_equal(round(tmp[2,2,3],4),0.9221)
  expect_equal(round(tmp[3,3,5],4),0.4440)
  expect_equal(tmp[4,1,2],NaN)
  expect_equal(tmp[2,1,1],NaN)

  region = 3
  trial = 2
  tmp = get_data_group_region_trial(M, "0",region, trial, tbl_beh=D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(3,4,206))
  expect_equal(round(tmp[2,4,3],4),0.8398)
  expect_equal(tmp[3,4,2],NaN)
  expect_equal(tmp[2,3,1],NaN)



  # test get_data_group_region_trial
  v <- rep(FALSE,dim(M)[5])
  v[1:5]= TRUE
  region = 1
  trial = 1
  tmp = get_data_group_region_trial_freq(M, "all_groups",region, trial, v, tbl_beh=D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(4,4,5))
  expect_equal(round(tmp[2,2,3],4),0.9221)
  expect_equal(round(tmp[3,3,5],4),0.4440)
  expect_equal(tmp[4,1,2],NaN)
  expect_equal(tmp[2,1,1],NaN)

  v <- rep(FALSE,dim(M)[5])
  v[1:3]= TRUE
  region = 3
  trial = 2
  tmp = get_data_group_region_trial_freq(M, "0",region, trial, v, tbl_beh=D$df_BD, method = D$method)
  expect_equal(dim(tmp),c(3,4,3))
  expect_equal(round(tmp[2,4,3],4),0.8398)
  expect_equal(tmp[3,4,2],NaN)
  expect_equal(tmp[2,3,1],NaN)








  # test get_data_groupmean_trial
  tmp <- get_data_groupmean_trial(M, "all_groups",1, tbl_beh = D$df_BD, method = D$method)
  expect_equal(round(tmp[1,2,1],3),0.792)
  tmp <- get_data_groupmean_trial(M, "0", 1, tbl_beh = D$df_BD, method = D$method)
  expect_equal(round(tmp[1,2,1],3),0.760)
  tmp <- get_data_groupmean_trial(M, "1", 1, tbl_beh = D$df_BD, method = D$method)
  expect_equal(round(tmp[1,2,1],3),0.856)

  # test that groups are correct
  Dg0 <- get_data_group(M,"0", D$df_BD, D$method)
  Dg1 <- get_data_group(M,"1", D$df_BD, D$method)
  #cat(file = stderr(), "dim(Dg0)=", dim(Dg0),"\n")
  #cat(file = stderr(), "dim(Dg1)=", dim(Dg1),"\n")
  expect_equal(dim(Dg0)[1],3)
  expect_equal(dim(Dg1)[1],1)

  # trial names

  trial_list <- c("13440", "13952")
  trial_list_named <- list("13440"=1, "13952"=2)
  expect_equal(D$utrial_list, trial_list)
  expect_equal(D$utrial_list_named, trial_list_named)

  # region names
  region_list <- c("frontal_links","frontal_rechts",  "occiptal_links",  "occiptal_rechts")
  region_list_named <- list("frontal_links"=1,"frontal_rechts"=2,  "occiptal_links"=3,  "occiptal_rechts"=4)
  expect_equal(D$uregion_list,region_list)
  expect_equal(D$uregion_list_named,region_list_named)





  v <- rep(TRUE,dim(M)[5])
  #v[1:3] = rep(TRUE,3)

  tmp<-get_currently_selected_data(M, "0", "1",1,2,v,
                                   trials = trial_list_named, regions = region_list_named,
                                   tbl_beh = tbl_behx, method = D$method)
  Dg0f <<- apply(Dg0[,,,,], c(1,2,3,4), mean)
  data1_tar <<- Dg0f[,,,1]-Dg0f[,,,2]
  expect_equal(tmp$data1, data1_tar)
  Dg1f <<- apply(Dg1, c(1,2,3,4), mean)

  data2_tar <<- Dg1f[,,,1,drop=F]-Dg1f[,,,2,drop=F]
  data2_tar = drop_except(data2_tar,1)
  expect_equal(tmp$data2, data2_tar)


  Dg0f = apply(Dg0[,,,,],c(1,2,3,4),mean)
  Dt <- Dg0[,2,,2,]
  De <- get_data_group_region_trial(M,"0",2,2,tbl_beh =D$df_BD, method = D$method)
  expect_equal(De,Dt)



  })




# mybasepath = file.path("../data", savedirname)
# saveRDS(uregion_list, file = file.path(mybasepath, "uregion_list.Rda"))
# saveRDS(utrial_list,  file = file.path(mybasepath, "utrial_list.Rda" ))
# saveRDS(ufreq_list,   file = file.path(mybasepath, "ufreq_list.Rda"  ))
# saveRDS(id_list,      file = file.path(mybasepath, "id_list.Rda"     ))
# saveRDS(tbl_beh,      file = file.path(mybasepath, "tbl_beh.Rda"     ))
# saveRDS(D,         file = file.path(mybasepath, "tbl_data.Rda"    ))
