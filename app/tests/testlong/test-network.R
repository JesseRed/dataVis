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

test_that("network works",{

  D<- list()
  D$method = "Coherence"
  D$ugroup_list = c("0","1")
  D$dimcontent = c("sub","reg","reg","tri","fre")
  D$uregion_list_named = list()
  D$uregion_list_named$Areal1 = 1
  D$uregion_list_named$Areal2 = 2
  D$uregion_list_named$Areal3 = 3
  D$uregion_list_named$Areal4 = 4
  D$uregion_list_named$Areal5 = 5
  D$uregion_list = names(D$uregion_list_named)
  D$utrial_list_named$T1 = 1
  D$utrial_list_named$T2 = 2
  D$utrial_list <- names(D$utrial_list)
  D$ufreq_list_named <- list()
  D$ufreq_list_named[['0.5']] <- 1
  D$ufreq_list_named[['1.1']] <- 2
  D$ufreq_list_named[['1.5']] <- 3
  D$ufreq_list <- names(D$ufreq_list_named)
  D$ufreq_list_num <- as.numeric(names(D$ufreq_list_named))
  D$id_list = c("Subj1", "Subj2", "Subj3")
  D$mdat <-  array(data = NA,
                  dim = c(length(D$id_list),
                          length(D$uregion_list_named),
                          length(D$uregion_list_named),
                          length(D$utrial_list_named),
                          length(D$ufreq_list_named)
                  ),
                  dimnames = list(D$id_list,
                                  D$uregion_list,
                                  D$uregion_list,
                                  D$utrial_list,
                                  D$ufreq_list
                  )
  )
  for (i in 1:dim(D$mdat)[1]){
    for (j in 1:dim(D$mdat)[2]){
      for (k in 1:dim(D$mdat)[3]){
        for (l in 1:dim(D$mdat)[4]){
          for (m in 1:dim(D$mdat)[5]){
            # nicht veraendern ... ist alles per Hand nachgerchnet
            D$mdat[i,j,k,l,m] = 1.0*(k*2+j)
          }
        }
      }
    }
  }

  new_uregion_list_named = D$uregion_list_named
  new_uregion_list_named$Areal1 <- 2
  new_uregion_list_named$Areal2 <- 5
  new_uregion_list_named$Areal3 <- 3
  new_uregion_list_named$Areal4 <- 5
  new_uregion_list_named$Areal5 <- 3

  # Loeschung lehrer Columns
  new_uregion_list_named <- remove_empty_cols(new_uregion_list_named)
  expect_equal(length(new_uregion_list_named),5)
  expect_equal(length(unique(unlist(new_uregion_list_named,use.names = F))),3)
  expect_equal(max(unique(unlist(new_uregion_list_named,use.names = F))),3)

  # mit Buchstaben versehen die neue Liste von Arelaen (A-N; 1-N)
  new_uregion_list_named_adapt <- adapt_new_network(new_uregion_list_named)

  num_regions_new = length(unique(unlist(new_uregion_list_named)))
  num_regions_old = length(unique(unlist(D$uregion_list_named)))

  # erstelle das leere Datanarray
  m_new <- create_empty_array(D, num_regions_new, new_uregion_list_named_adapt)
  expect_equal(dim(m_new)[1],dim(D$mdat)[1])
  expect_equal(dim(m_new)[2],length(unique(unlist(new_uregion_list_named))))
  expect_equal(dim(m_new)[2],num_regions_new)
  expect_equal(dim(m_new)[3],dim(m_new)[2])
  expect_equal(dim(m_new)[3],dim(D$mdat)[1])
  expect_equal(dim(m_new)[4],dim(D$mdat)[4])
  expect_equal(dim(m_new)[5],dim(D$mdat)[5])



  regions_to_mean = get_regions_to_mean(num_regions_new, D$uregion_list_named, new_uregion_list_named)



  gnew_uregion_list_named <<- new_uregion_list_named
  gD<<-D
  gregions_to_mean <<- regions_to_mean



  expect_equal(get_mymean(D$mdat, 1,1,1,1,1, regions_to_mean), 1)
  expect_equal(get_mymean(D$mdat, 1,1,2,1,1, regions_to_mean), 9)
  expect_equal(get_mymean(D$mdat, 1,1,3,1,1, regions_to_mean), 7)
  expect_equal(get_mymean(D$mdat, 1,2,1,1,1, regions_to_mean), 6)
  expect_equal(get_mymean(D$mdat, 1,2,2,1,1, regions_to_mean), 12)
  expect_equal(get_mymean(D$mdat, 1,2,3,1,1, regions_to_mean), 10)
  expect_equal(get_mymean(D$mdat, 1,3,1,1,1, regions_to_mean), 5)
  expect_equal(get_mymean(D$mdat, 1,3,2,1,1, regions_to_mean), 11)
  expect_equal(get_mymean(D$mdat, 1,3,3,1,1, regions_to_mean), 9)

  expect_equal(get_mymean(D$mdat, 1,1,1,1,1, regions_to_mean), get_mymean_fast(D$mdat[1,,,1,1], 1,1, regions_to_mean))
  expect_equal(get_mymean(D$mdat, 1,1,2,1,1, regions_to_mean), get_mymean_fast(D$mdat[1,,,1,1], 1,2, regions_to_mean))
  expect_equal(get_mymean(D$mdat, 1,1,3,1,1, regions_to_mean), get_mymean_fast(D$mdat[1,,,1,1], 1,3, regions_to_mean))
  expect_equal(get_mymean(D$mdat, 1,2,1,1,1, regions_to_mean), get_mymean_fast(D$mdat[1,,,1,1], 2,1, regions_to_mean))
  expect_equal(get_mymean(D$mdat, 1,2,2,1,1, regions_to_mean), get_mymean_fast(D$mdat[1,,,1,1], 2,2, regions_to_mean))
  expect_equal(get_mymean(D$mdat, 1,2,3,1,1, regions_to_mean), get_mymean_fast(D$mdat[1,,,1,1], 2,3, regions_to_mean))
  expect_equal(get_mymean(D$mdat, 1,3,1,1,1, regions_to_mean), get_mymean_fast(D$mdat[1,,,1,1], 3,1, regions_to_mean))
  expect_equal(get_mymean(D$mdat, 1,3,2,1,1, regions_to_mean), get_mymean_fast(D$mdat[1,,,1,1], 3,2, regions_to_mean))
  expect_equal(get_mymean(D$mdat, 1,3,3,1,1, regions_to_mean), get_mymean_fast(D$mdat[1,,,1,1], 3,3, regions_to_mean))


  mdat_new <- create_empty_array(D, num_regions_new, new_uregion_list_named_adapt)


  tmp1 <- get_myareamean(D$mdat, mdat_new, 1,1,1, num_regions_new, regions_to_mean)[1,,,1,1]
  tmp2 <- get_myareamean_fast(D$mdat[1,,,1,1], num_regions_new, regions_to_mean)
  dimnames(tmp1)<-NULL
  dimnames(tmp2)<-NULL
  expect_equal( tmp1 , tmp2 )
  tmp1 <- get_myareamean(D$mdat, mdat_new, 1,2,2, num_regions_new, regions_to_mean)[1,,,2,2]
  tmp2 <- get_myareamean_fast(D$mdat[1,,,2,2], num_regions_new, regions_to_mean)
  dimnames(tmp1)<-NULL
  dimnames(tmp2)<-NULL
  expect_equal( tmp1 , tmp2 )
  tmp1 <- get_myareamean(D$mdat, mdat_new, 2,1,2, num_regions_new, regions_to_mean)[2,,,1,2]
  tmp2 <- get_myareamean_fast(D$mdat[2,,,1,2], num_regions_new, regions_to_mean)
  dimnames(tmp1)<-NULL
  dimnames(tmp2)<-NULL
  expect_equal( tmp1 , tmp2 )

  #   expect_equal(get_myareamean(D$mdat, mdat_new, 1,2,2, num_regions_new, regions_to_mean)[1,,,1,1], get_myareamean_fast(D$mdat[1,,,2,2], num_regions_new, regions_to_mean))
  # expect_equal(get_myareamean(D$mdat, mdat_new, 2,1,2, num_regions_new, regions_to_mean)[1,,,1,1], get_myareamean_fast(D$mdat[2,,,1,2], num_regions_new, regions_to_mean))

  expect_equal(reestimate_mdat(D, new_uregion_list_named), reestimate_mdat_fast(D, new_uregion_list_named))

  # teste die GEschwindigkeit der schnellen Variante
  cat(file = stderr(), getwd())
  DX <- readRDS("./data/fMRI2/Dorg.Rda")
  new_uregion_list_namedX<- DX$uregion_list_named
  new_uregion_list_namedX[3]<-4
  new_uregion_list_namedX[6]<-7
  new_uregion_list_namedX[29]<-35
  new_uregion_list_namedX[2]<-14

  DX$mdat <- DX$mdat[1:20,,,,, drop = FALSE]
  DX1 <- DX
  start_time <- Sys.time()
  DXnew <- change_network_in_data_struct(D = DX1, new_uregion_list_named = new_uregion_list_namedX)
  proc_time1 = Sys.time()-start_time
  cat(file = stderr(), "without fast ... ", proc_time1, "\n")
  start_time <- Sys.time()
  DXnew <- change_network_in_data_struct(D = DX1, new_uregion_list_named = new_uregion_list_namedX, is_use_fast_algorithm = TRUE)
  proc_time2 = Sys.time()-start_time
  cat(file = stderr(), "with fast ... ", Sys.time()-start_time, "\n")
  cat(file = stderr(), paste0("fast algorithm was faster by ", round((as.double(proc_time2)/as.double(proc_time1))/100,2) ,"% \n"))



  Dnew <- change_network_in_data_struct(D = D, new_uregion_list_named = new_uregion_list_named)
  handmat = matrix(c(1,6,5,9,12,11,7,10, 9), ncol = 3, nrow = 3,
                   dimnames = list(names(Dnew$uregion_list_named),
                                   Dnew$uregion_list
                   ))
  for (i in 1:dim(Dnew$mdat)[1]){
#    for (j in 1:dim(Dnew$mdat)[2]){
#      for (k in 1:dim(Dnew$mdat)[3]){
        for (l in 1:dim(Dnew$mdat)[4]){
          for (m in 1:dim(Dnew$mdat)[5]){
            # nicht veraendern ... ist alles per Hand nachgerchnet
            expect_equal(Dnew$mdat[i, , ,l,m], handmat)
          }
        }
      }

  })


