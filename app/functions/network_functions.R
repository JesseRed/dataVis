
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

change_network_in_data_struct<-function(D = NULL, new_uregion_list_named = NULL, is_use_fast_algorithm = FALSE){
  if (is.null(D) | is.null(new_uregion_list_named)) {
    cat(file=stderr(), paste0("The function change_network_in_data_struct requiers the follwoing inputs\n"))
    cat(file=stderr(), paste0("D, new_uregion_list_named\n"))
  }

  # veraendert werden muessen...
  # 1. uregion_list
  # 2. uregion_list_named
  # 3. mdat
  mdat_org = D$mdat
  uregion_list_org = D$uregion_list
  uregion_list_named_org = D$uregion_list_named


  # entferne ausgelassene Columns
  new_uregion_list_named <- remove_empty_cols(new_uregion_list_named)

  D$mdat_org = D$mdat

  if (is_use_fast_algorithm){
    D$mdat <- reestimate_mdat_fast(D, new_uregion_list_named)
  }
  else{
    D$mdat <- reestimate_mdat(D, new_uregion_list_named)
  }
  D$uregion_list_named = adapt_new_network(new_uregion_list_named)
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

# returns a simple named list with increasing letters and increasing numbers
adapt_new_network<- function(new_network){
  u <- unique(unlist(new_network))
  n <- list()
  for (i in 1:length(u)){
    n[LETTERS[i]]=i
  }
  return(n)
}

reestimate_mdat<- function(D, new_uregion_list_named){

  # numeriert eine Liste mit der Regionszahl mit names() = Letters
  n_new_adapt <- adapt_new_network(new_uregion_list_named)

  num_regions_new = length(unique(unlist(new_uregion_list_named)))
  num_regions_old = length(unique(unlist(D$uregion_list_named)))

  m_new <- create_empty_array(D, num_regions_new, n_new_adapt)

  regions_to_mean = get_regions_to_mean(num_regions_new, D$uregion_list_named, new_uregion_list_named)


  # nun die Schleife um die Mittlungen an der Datenmatrix durchzufuehren
  #withProgress(message = 'Making plot', value = 0, {
  for (s in 1:dim(D$mdat)[1]){
    cat(file = stderr(), paste0("estimate subject ",s,"\n"))
    for (t in 1:dim(D$mdat)[4]){
      for (f in 1:dim(D$mdat)[5]){
        # nun uber die Regionen als 2 schleifen
        m_new <- get_myareamean(D$mdat, m_new, s,t,f,num_regions_new, regions_to_mean)
      }
    }
  }

  return(m_new)
}




get_myareamean <- function(mdat_old, mdat_new, s,t,f, num_regions_new, regions_to_mean){
  # Berechnet die region x region matrix

  for (r1 in 1:num_regions_new){
    for (r2 in 1:num_regions_new){
      mdat_new[s,r1,r2,t,f]= get_mymean(mdat_old, s,r1,r2,t,f,regions_to_mean)
    }
  }
  return(mdat_new)
}






# has tests
# auf 5 x 5 Matrix per Hand nachgerechnet
get_mymean <- function(m, s, r1, r2, t,f, regions_to_mean){
  ###################################
  ################################
  # r1 vs. r2
  # r1 areale sind im orginal m[ragions_to_mean[[r1]]]
  # r2 areale sind im orginal m[ragions_to_mean[[r2]]]
  # ich mittle nun ueber alle Kombinationen dieser Areale
  # alles auf eine 5x5 Matrix per Hand nachgerechnet

  # wenn es ein diagonalelement ist das nur aus einem Areal besteht
  if ((r1==r2) && (length(regions_to_mean[[r1]])==1)){
    return(1.0)
    }

  org_regions_idx1 = regions_to_mean[[r1]]
  org_regions_idx2 = regions_to_mean[[r2]]
  #cat(file = stderr(), paste0("r1=", r1, "  r2=",r2, "\n"))
  tmp_mean = 0
  add_counter = 0
  for (i in 1:length(org_regions_idx1)){
    idx1 = org_regions_idx1[i]
    for (j in 1:length(org_regions_idx2)){
      idx2 = org_regions_idx2[j]
      #cat(file = stderr(), paste0("i=",i," idx1=", idx1, "  j=",j," idx2=",idx2, "\n"))
      if (idx1!=idx2){
        v =  m[s,idx1,idx2,t,f]
        if (is.numeric(v)){
          tmp_mean = tmp_mean + m[s,idx1,idx2,t,f]
          #cat(file = stderr(), paste0( "tmp_mean = ", tmp_mean, "   ... added ",m[s,idx1,idx2,t,f]),"\n" )
          add_counter = add_counter + 1
        }
      }
    }
  }
  #cat(file = stderr(), paste0("divide",tmp_mean,"/ ", add_counter,"\n"))
  if (add_counter==0){
    return(1.0)
  }
  result = tmp_mean/add_counter
  if (is.nan(result)){
    cat(file =stderr(), "Error in get_mymean ... NAN detected during estimation of field ", r1, "vs." , r2,"\n")
  }
  return(result)
}






reestimate_mdat_fast<- function(D, new_uregion_list_named){

  # numeriert eine Liste mit der Regionszahl mit names() = Letters
  n_new_adapt <- adapt_new_network(new_uregion_list_named)

  num_regions_new = length(unique(unlist(new_uregion_list_named)))
  num_regions_old = length(unique(unlist(D$uregion_list_named)))
  m_new <- create_empty_array(D, num_regions_new, n_new_adapt)
  regions_to_mean = get_regions_to_mean(num_regions_new, D$uregion_list_named, new_uregion_list_named)

  # nun die Schleife um die Mittlungen an der Datenmatrix durchzufuehren
  #withProgress(message = 'Making plot', value = 0, {
  for (s in 1:dim(D$mdat)[1]){
    cat(file = stderr(), paste0("estimate subject ",s,"\n"))
    for (t in 1:dim(D$mdat)[4]){
      for (f in 1:dim(D$mdat)[5]){
        # nun uber die Regionen als 2 schleifen
        m_new[s,,,t,f] <- get_myareamean_fast(D$mdat[s,,,t,f], num_regions_new, regions_to_mean)
      }
    }
  }

  return(m_new)
}

get_myareamean_fast <- function(mdat_old, num_regions_new, regions_to_mean){
  # Berechnet die region x region matrix
  mdat_new <- ones(num_regions_new, num_regions_new)
  for (r1 in 1:num_regions_new){
    for (r2 in 1:num_regions_new){
      mdat_new[r1,r2]= get_mymean_fast(mdat_old, r1,r2,regions_to_mean)
    }
  }
  return(mdat_new)
}


get_mymean_fast <- function(m, r1, r2, regions_to_mean){
  ###################################
  ################################
  # r1 vs. r2
  # r1 areale sind im orginal m[ragions_to_mean[[r1]]]
  # r2 areale sind im orginal m[ragions_to_mean[[r2]]]
  # ich mittle nun ueber alle Kombinationen dieser Areale
  # alles auf eine 5x5 Matrix per Hand nachgerechnet
  #if (r1==r2){ return(1.0)}

  # wenn es ein diagonalelement ist das nur aus einem Areal besteht
  if ((r1==r2) && (length(regions_to_mean[[r1]])==1)){
    return(1.0)
  }

  org_regions_idx1 = regions_to_mean[[r1]]
  org_regions_idx2 = regions_to_mean[[r2]]
  #cat(file = stderr(), paste0("r1=", r1, "  r2=",r2, "\n"))
  tmp_mean = 0
  add_counter = 0
  for (i in 1:length(org_regions_idx1)){
    idx1 = org_regions_idx1[i]
    for (j in 1:length(org_regions_idx2)){
      idx2 = org_regions_idx2[j]
      #cat(file = stderr(), paste0("i=",i," idx1=", idx1, "  j=",j," idx2=",idx2, "\n"))
      if (idx1!=idx2){
        v =  m[idx1,idx2]
        if (is.numeric(v)){
          tmp_mean = tmp_mean + m[idx1,idx2]
          #cat(file = stderr(), paste0( "tmp_mean = ", tmp_mean, "   ... added ",m[s,idx1,idx2,t,f]),"\n" )
          add_counter = add_counter + 1
        }
      }
    }
  }
  #cat(file = stderr(), paste0("divide",tmp_mean,"/ ", add_counter,"\n"))
  #cat(file = stderr(), paste0("divide",tmp_mean,"/ ", add_counter,"\n"))
  if (add_counter==0){
    return(1.0)
  }
  result = tmp_mean/add_counter
  if (is.nan(result)){
    cat(file =stderr(), "Error in get_mymean ... NAN detected during estimation of field ", r1, "vs." , r2,"\n")
  }
  return(result)
}


# has tests
get_regions_to_mean <- function (num_regions_new, uregion_list_named, new_uregion_list_named){
  # erstelle eine liste in welchem fuer jede Region des neuen Netzwerkes
  # die indices des alten Netzwerkes stehen ueber die gemittelt werden soll
  # [[1]]
  #  [1] 1
  #
  # [[2]]
  # [1] 2 3
  #
  # [[3]]
  # [1] 4 5
  # .... Die erste Region wird uebernommen 1=1
  # .... die zweite Region des neuen Netzwerks besteht aus der Mittlung der Regionen 2 und 3 des alten Netzwerks ...
  regions_to_mean = list()
  for (i in 1:num_regions_new){
    regions_to_mean[[i]] = unlist(uregion_list_named[new_uregion_list_named==i], use.names = F)
  }
  return(regions_to_mean)
}



create_empty_array<- function(D, num_regions_new, n_new_adapt){
  #cat(file = stderr(), paste0("in create_empty_array with num_regions_new = ", num_regions_new, "  n_new_adapt = ", n_new_adapt, "\n"))
  #cat(file = stderr(), paste0("dim(D$mdat) = ", dim(D$mdat),"\n"))
  # lege das Datenarray mit den korrekten Dimensionen an
  m_new <-  array(data = NA,
                  dim = c(dim(D$mdat)[1],
                          num_regions_new,
                          num_regions_new,
                          dim(D$mdat)[4],
                          dim(D$mdat)[5]
                  ),
                  dimnames = list(D$id_list[1:dim(D$mdat)[1]],
                                  names(n_new_adapt),
                                  names(n_new_adapt),
                                  D$utrial_list,
                                  D$ufreq_list
                  )
  )
  return(m_new)
}



