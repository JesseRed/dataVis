

create_ggplot_histogram<-function(data = NULL, colname1 = NULL, colname2=NULL, num_groups = 1){

  gdf<<- data
  if(num_groups==1){
  cat(file = stderr(), paste0("createggplot_histogram with colname2=",colname2,"\n"))
    #        p <- data %>% filter()
    p <- data %>%
       ggplot(aes_string(x = colname2))+
       geom_histogram(binwidth = 20, fill="#69b3a2", color="#e9ecef", alpha=0.9)
  }
  if(num_groups==2){
    p <- data %>%
      ggplot( aes_string(x = colname2, fill="G")) +
      geom_histogram(binwidth = 20, color="#e9ecef", alpha=0.6, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080"))
      theme_ipsum() +
      labs(fill="")
  }
  return(p)

}


create_ggplot_histogram_mean<-function(data = NULL, colname1 = NULL, colname2=NULL, num_groups = 2){

  if(num_groups==1){

    #        p <- data %>% filter()
    p <- data %>%
      ggplot(aes(x = colname1, y= colname2))+
      geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)
  }
  if(num_groups==2){
    p <- data %>%
      ggplot( aes(x=colname1, y= colname2, fill=G)) +
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080"))
    #          theme_ipsum() +
    #          labs(fill="")
  }
  return(p)

}



create_ggplot_individual_line_chart<-function(data = NULL, colname1 = NULL, colname2=NULL, num_groups = 1){
cat(file = stderr(),paste0("create_ggplot_individual_line_chart with num_groups=",num_groups,"\n"))

  if(num_groups==1){
    p<- data %>%
        ggplot( aes_string(x = colname1, y = colname2)) +
        geom_line(aes_string(x = colname1, y = colname2, group = "ID", colour = "ID"))+
        geom_point(aes_string(x = colname1, y = colname2, group = "ID", colour = "ID", shape = "ID"),size = 2)+ # shape = 21, color="black", fill="#69b3a2", size=6) +
        ggtitle(paste0(colname1," vs. ", colname2))



  }
  if(num_groups==2){
    p <- data %>%
      ggplot(aes_string(x = colname1, y = colname2, color = "G"))+ #, group = "ID", colour = "ID", shape = "ID")) +
      geom_point(size = 2)+ # shape = 21, color="black", fill="#69b3a2", size=6) +
      ggtitle(paste0(colname1," vs. ", colname2))
    #          theme_ipsum() +
    #          labs(fill="")
  }
  return(p)

}




create_ggplot_individual_subjects<-function(data = NULL, colname1 = NULL, colname2=NULL, num_groups = 2){




  if(num_groups==1){
    p<- data %>%
      ggplot( aes_string(x = colname1, y = colname2))+ #, group = "ID", colour = "ID", shape = "ID")) +
      #geom_line()+
      geom_point(aes_string(x = colname1, y = colname2, group = "ID", colour = "ID", shape = "ID"),size = 2)+ # shape = 21, color="black", fill="#69b3a2", size=6) +
      ggtitle(paste0(colname1," vs. ", colname2))

  }
  if(num_groups==2){
    p <- data %>%
      ggplot(aes_string(x = colname1, y = colname2, color = "G"))+ #, group = "ID", colour = "ID", shape = "ID")) +
        geom_point(size = 2)+ # shape = 21, color="black", fill="#69b3a2", size=6) +
        ggtitle(paste0(colname1," vs. ", colname2))
    #          theme_ipsum() +
    #          labs(fill="")
  }
  return(p)

}




add_ggplot_geomsmooth<-function(p, num_groups = 1){
  if (num_groups == 1){
    p<-p + geom_smooth(se = FALSE) +
       geom_smooth(method=lm , color="red", se=FALSE)
  }else{
    p<-p + geom_smooth(se = FALSE) +
      geom_smooth(method=lm , color="red", se=FALSE)
  }
  return(p)
}

