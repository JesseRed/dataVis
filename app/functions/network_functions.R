
# This function change the datastructure that was imported from Stefans json file
# the new network should have less regions than the old one
# It allows to estimate the relationship between sets of regions
# However, there are two possibilities to estimate these relationship
# 1. mean of all relationships between all combination of areas between two newly defined
#    networks
#    However, here we are loosing the information about the variance between the diferent
#    combinations.
#    Additionally we have some information about the intranetwork connectivity

# 2. To keep the all data, a analysis of repeated measurements would be possibile
#     However, i will keep this for later

change_network_in_data_struct<-function(D = NULL, old_network = NULL, new_network = NULL){
  if (is.null(D) | is.null(new_network) | is.null(old_network)){
    cat(file=stderr(), paste0("The function change_network_in_data_struct requiers the follwoing inputs\n"))
    cat(file=stderr(), paste0("D, old_network, new_network\n"))
  }

  # veraendert werden muessen...
  # 1. uregion_list
  # 2. uregion_list_named
  # 3. mdat
  mdat_org = D$mdat
  uregion_list_org = D$uregion_list
  uregion_list_named_org = D$uregion_list_named


  # entferne ausgelassene Columns
  new_network <- remove_empty_cols(new_network)

  D$mdat_org = D$mdat

  D$mdat <- reestimate_mdat(D, new_network)
  D$uregion_list_named = adapt_new_network(new_network)
  D$uregion_list = names(D$uregion_list_named)
  #(uregion_list == names(uregion_list_named))


  return(D)

}


# im neuen NEtzwerk stehen nummern fuer jedes alte Areal
# falls eine spalte ausgelassen wurde dann fehlt eine Zahl
# es ist aber sinnvoll zusammenhaengende aufsteigende Zahlen zu haben
# daher eliminieren wir hier leere Columns
remove_empty_cols<-function(net){
  mymax = max(unique(unlist(net)))
  network_numbers = unique(unlist(net, use.names = F))
  net_idx = 0
  for (i in 1:mymax){
    if (i %in% network_numbers){
      net_idx = net_idx + 1
      net[net==i]=net_idx
    }
  }
  return(net)

}

adapt_new_network<- function(new_network){
  u <- unique(unlist(new_network))
  n <- list()
  for (i in 1:length(u)){
    n[LETTERS[i]]=i
  }
}

reestimate_mdat<- function(D, new_network){
  m_old <- D$mdat
  n_old <- D$uregion_list_named
  n_new <- new_network
  n_new_adapt <- adapt_new_network(new_network)

  num_regions_new = length(unique(unlist(n_new)))
  num_regions_old = length(unique(unlist(n_old)))

  # lege das Datenarray mit den korrekten Dimensionen an
  m_new <-  array(data = NA,
                  dim = c(dim(m_old)[1],
                          num_regions_new,
                          num_regions_new,
                          dim(m_old)[4],
                          dim(m_old)[5]
                  ),
                  dimnames = list(D$id_list,
                                  names(n_new_adapt),
                                  names(n_new_adapt),
                                  D$utrial_list,
                                  D$ufreq_list
                  )
  )

  # erstelle eine liste in welchem fuer jede Region des neuen Netzwerkes
  # die indices des alten Netzwerkes stehen ueber die gemittelt werden soll
  # z.B. regions_to_mean[[3]]= c(3,5,7)
  regions_to_mean = list()
  for (i in 1:num_regions_new){
    regions_to_mean[[i]] = unlist(n_old[n_new==i], use.names = F)
  }



  for (s in 1:dim(m_old)[1]){
    for (t in 1:dim(m_old)[4]){
      for (f in 1:dim(m_old)[5]){
        # nun uber die Regionen als 2 schleifen
        m_new <- get_myareamean(m_old, m_new, s,t,f,num_regions_new, regions_to_mean)
      }
    }
  }

}

get_myareamean <- function(m_old, m_new, s,t,f, num_regions_new, regions_to_mean){
  # Berechnet die region x region matrix

  for (r1 in 1:num_regions_new){
    for (r2 in 1:num_regions_new){
      m_new[s,r1,r2,t,f]= get_mymean(m_old, s,r1,r2,t,f,regions_to_mean)
    }
  }
  return(m_new)
}

get_mymean <- function(m, s, r1, r2, t,f, regions_to_mean){

  # r1 vs. r2
  # r1 areale sind im orginal m[ragions_to_mean[[r1]]]
  # r2 areale sind im orginal m[ragions_to_mean[[r2]]]
  # ich mittle nun ueber alle Kombinationen dieser Areale
  if (r1==r2){ return(1.0)}

  org_regions_idx1 = regions_to_mean[[r1]]
  org_regions_idx2 = regions_to_mean[[r2]]

  tmp_mean = 0
  for (i in 1:length(org_regions_idx1)){
    idx1 = org_regions_idx1[i]
    for (j in i:length(org_regions_idx2)){
      idx2 = org_regions_idx1[j]
      #upper triangle
      if (r1<r2){
        if (idx1<idx2){
          tmp_mean = tmp_mean + m[s,idx1,idx2,t,f]
        }
      }else{
        #lower triangle
        if (idx1>idx2){
          tmp_mean = tmp_mean + m[s,idx1,idx2,t,f]
        }
      }
    }
  }
  return(tmp_mean)
}

