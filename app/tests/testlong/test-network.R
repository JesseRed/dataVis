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

            D$mdat[i,j,k,l,m] = 1.0*j
            # if (j ==5){
            #   D$mdat[i,j,k,l,m] = 5.0
            # }
          }
        }
      }
    }
  }

  new_network = D$uregion_list_named
  new_network$Areal1 <- 2
  new_network$Areal2 <- 3
  new_network$Areal3 <- 3
  new_network$Areal4 <- 5
  new_network$Areal5 <- 5


  new_network <<- remove_empty_cols(new_network)

  expect_equal(length(new_network),5)
  expect_equal(length(unique(unlist(new_network,use.names = F))),3)
  expect_equal(max(unique(unlist(new_network,use.names = F))),3)


  })


