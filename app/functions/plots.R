
generate_histogram_plot_facet<-function(group1, group2, trial1, trial2, freq, level_x_rval, level_y_rval){
  # Erstellt einen Histogram plot von mehreren Gruppen mit gleichen Achsenskalen

  # gedacht fuer tabs in denen gruppen und trials ausgewaehlt werden
  # d$mypaired ... bool handelt es sich um einen gepaarten oder ungeparrten test
  # d$data1 ... 3D matrix ... der gruppe 1 bzw trial 1
  # d$data2 ... 3D matrix ... der gruppe 2 bzw trial 2
  # d$mat_p ... 2D matrix ... p Werte des t-tests ueber alle Regionen
  # d$mat_t ... 2D matrix ... t Werte des t-tests ueber alle Regionen
  # d$string1 ... string .... eine beschreibung des durchgefuehrten Vergleiches
  # d$color1 ... col ........ die Color palette die zu den Werten passen

  #cat(file = stderr(), "freq=",freq,"\n\n")
  #cat(file = stderr(), paste0("is.reactive(freq)=",is.reactive(freq),"\n"))
  #cat(file = stderr(), paste0("is.reactive(freq)=",is.reactive(freq),"\n"))
  #cat(file = stderr(), paste0("is.reactive(trial1)=",is.reactive(trial1),"\n"))
  #cat(file = stderr(), paste0("is.reactive(level_x_rval)=",is.reactive(level_x_rval),"\n"))
  #cat(file = stderr(), "freq()=",freq(),"\n")
  d <- get_currently_selected_data(g_data(), group1, group2, as.numeric(trial1), as.numeric(trial2), freq)
  #d <- get_data_freqmean(g_data(), freq)
  #cat(file = stderr(), "class(d)=",class(d),"\n")
  #cat(file = stderr(), paste0("\n","level_x_rval=",level_x_rval))
  #d <- curdata()
  if (trial1 == trial2) {
    #cat(file = stderr(), "trial1 == trial2\n")
    string1 = paste0(group1," vs ", group2, " in trial ", names(g_trials_named())[trial1], "\n")
    d1 = get_data_group_freqmean(g_data(), group1)
    d2 = get_data_group_freqmean(g_data(), group2)
    x = d1[,level_x_rval, level_y_rval, as.numeric(trial1)]
    y = d2[,level_x_rval, level_y_rval, as.numeric(trial1)]
    df <- data.frame(Gruppe=c(rep(group1, times=length(x)),
                              rep(group2, times=length(y))),
                     val=c(x, y))
    df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
    # means for geomline
    df_hline = data.frame(Gruppe = c(group1,group2), Means=c(mean(x), mean(y)))
    # df$val = d[,level_x_rval, level_y_rval, as.numeric(input$trial1)]
    # df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
    # dummy2 = data.frame(Gruppe = c(0,1), Means=c(0.4, 0.5))

  }else if (group1 == group2){
    string1 = paste0(trial1," vs ", trial2, "in group ", group1, "\n")
    #data1 = data_1()
    #data2 = data_2()
    data1 = d$data1
    data2 = d$data2
    x = data1[,level_x_rval, level_y_rval]
    y = data2[,level_x_rval, level_y_rval]
    df <- data.frame(Gruppe=c(rep(g_trials()[as.numeric(trial1)], times=length(x)),
                              rep(g_trials()[as.numeric(trial2)], times=length(y))),
                     val=c(x, y))
    #cat(file = stderr(), paste0("val=",c(x,y),"\n"))
    #cat(file = stderr(), paste0("df=",df,"\n"))

    df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
    # means for geomline
    df_hline = data.frame(Gruppe = c(g_trials()[as.numeric(trial1)],
                                     g_trials()[as.numeric(trial2)]),
                          Means=c(mean(x), mean(y)))
  } else{

    my_paired <- d$my_paired
    data1 <- d$data1
    data2 <- d$data2
    string1 <-d$string1
    mat_p <- d$mat_p
    mat_t <- d$mat_t

    x = data1[,level_x_rval, level_y_rval]
    y = data2[,level_x_rval, level_y_rval]

    df <- data.frame(Gruppe=c(rep(group1, times=length(x)),
                              rep(group2, times=length(y))),
                     val=c(x, y))
    df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
    # means for geomline
    df_hline = data.frame(Gruppe = c(group1, group2), Means=c(mean(x), mean(y)))

  }
  myplot<- ggplot(df, aes(num, val, fill=Gruppe)) +
    geom_bar(stat="identity") +
    facet_wrap(~Gruppe) +
    geom_hline(data = df_hline, aes(yintercept = Means))
return(myplot)
}

generate_plot_Circle<-function(mat_p, mat_t, data1, data2){
  rownames(mat_t) = g_regions()
  colnames(mat_t) = g_regions()
  x = data1[,1,2]
  y = data2[,2,3]
  z = t.test(x,y)
  df = z$parameter

  #M = 1/d$mat_p
  M = abs(log(mat_p))

  t_threshold = abs(log(g_sig()))
  rownames(M) = g_regions()
  colnames(M) = g_regions()
  M[is.nan(M)]=0
  M[upper.tri(M)]=0.001

  # cat(file=stderr(), M)
  # RdYlBu hat 11 Farbstufen daher nicht fuer diese Palette veraendern
  mycol = map2color4threshold(
    M,brewer.pal(n=11, name = "RdYlBu"),
    threshold = t_threshold,
    invert_col_map = TRUE
  )
  dim(mycol) = dim(M)

  myplot <-chordDiagram(M ,col = mycol)
  return(myplot)
}

generate_plot_Corrplot<-function(mat_p, mat_t){
  # colnames(d$mat_p) = g_regions()
  # rownames(d$mat_p) = vector(mode="character", length=length(g_regions()))
  # colnames(d$mat_t) = vector(mode="character", length=length(g_regions()))
  # rownames(d$mat_t) = g_regions()

  if (g_act_method()=="Coherence"){
  # Erstellt einen Histogram plot von mehreren Gruppen mit gleichen Achsenskalen
  #d = get_currently_selected_data(g_data(), group1, group2, trial1, trial2, freq())
    rownames(mat_p) = vector(mode="character", length=length(g_regions()))
  x1 <<- corrplot(mat_p, method="number", tl.cex = 0.9, type = "upper", is.corr = FALSE,
                  p.mat = mat_p, sig.level = g_sig(), tl.srt = 45,
                  col=colorRampPalette(c("blue","red","green"))(200))
  colnames(mat_t) = vector(mode="character", length=length(g_regions()))

  myplot_corr <<- corrplot(mat_t, add = TRUE, method="number", tl.cex = 0.9, type = "lower", is.corr = FALSE,
                  p.mat = mat_p, sig.level = g_sig(), tl.srt = 45)
  }else if (g_act_method()=="Transferentropy") {
    myplot_corr <<- corrplot(d$mat_p, method="number", tl.cex = 0.9, is.corr = FALSE,
                             p.mat = d$mat_p, sig.level = g_sig(),tl.srt = 45,
                             col=colorRampPalette(c("blue","red","green"))(200))
  }else if (g_act_method()=="Granger") {
    cat(file = stderr(), "Corrrplot Granger not implemented")
    myplot_corr = NULL
  }else if (g_act_method()=="Frequency") {
    cat(file = stderr(), "Corrplot Frequency not implemented")
    myplot_corr = NULL
  }

  return(myplot_corr)

}

generate_plot_Pheatmap<-function(mat_p, mat_t, myfontsize = g_saveImage_fontsize()){
  colnames(mat_p) = g_regions()
  rownames(mat_p) = g_regions()

  # Returns a vector of 'num.colors.in.palette'+1 colors. The first 'cutoff.fraction'
  # fraction of the palette interpolates between colors[1] and colors[2], the remainder
  # between colors[3] and colors[4]. 'num.colors.in.palette' must be sufficiently large
  # to get smooth color gradients.
  makeColorRampPalette <- function(colors, cutoff.fraction, num.colors.in.palette)
  {
    stopifnot(length(colors) == 4)
    ramp1 <- colorRampPalette(colors[1:2])(num.colors.in.palette * cutoff.fraction)
    ramp2 <- colorRampPalette(colors[3:4])(num.colors.in.palette * (1 - cutoff.fraction))
    return(c(ramp1, ramp2))
  }

  cutoff.distance <- 0.05
  cols <- makeColorRampPalette(c("red", "orange",    # distances 0 to 3 colored from white to red
                                 "gray", "black"), # distances 3 to max(distmat) colored from green to black
                               cutoff.distance / 1, #max(mat_p), #max(distmat),
                               100)

  myplot<-pheatmap(
    mat                   = mat_p,
    display_numbers       = TRUE,
    color                 = cols,
    fontsize              = myfontsize,
    main                  = "P-Values Pheatmap",
    show_rownames         = TRUE,
    show_colnames         = TRUE,
    cluster_cols          = FALSE,
    cluster_rows          = FALSE,

  )
  return(myplot)
}

open_device_for_save <- function(filename){
  if (g_saveImage_fileext()== "png"){
    png(filename, width = g_saveImage_width(),
        height = g_saveImage_height(),
        units  = "cm",
        res = g_saveImage_dpi()
    )
  }
  if (g_saveImage_fileext()== "tiff"){
    tiff(filename, width = g_saveImage_width(),
        height = g_saveImage_height(),
        units  = "cm",
        res = g_saveImage_dpi()
    )
  }
  if (g_saveImage_fileext()== "pdf"){
      pdf(filename, width = g_saveImage_width(),
          height = g_saveImage_height(),
          units  = "cm",
          res = g_saveImage_dpi()
    )

  }

}

generate_plot_ggplot_corrplot_handmade<- function(mat_p, mat_t, myfontsize = g_saveImage_fontsize()){

  rownames(mat_p)<-NULL
  colnames(mat_p)<-NULL
  rownames(mat_t)<-NULL
  colnames(mat_t)<-NULL

 # mat_p <- matrix(runif(196,0,0.1),nrow=14)
 #mat_t <- matrix(runif(196,0,10),nrow=14)

  # names <- c("frontal_right_A1",
  #            "frontal_right_B2",
  #            "frontal_right_A3",
  #            "frontal_right_A4",
  #            "frontal_right_A5",
  #            "frontal_right_A6",
  #            "frontal_right_A7",
  #            "frontal_right_A8",
  #            "frontal_right_A9",
  #            "frontal_right_A10",
  #            "frontal_right_A11",
  #            "frontal_right_A12",
  #            "frontal_right_A13",
  #            "frontal_right_A14"
  # )


  A <- mat_p
  Alt_b = lower.tri(A, diag = FALSE)
  Aut_b = upper.tri(A, diag = FALSE)
  Alt <- A[Alt_b]
  Aut <- A[Aut_b]
  df_tval <- melt(mat_t)
  df<-melt(A)
  df$t_val <- df_tval$value

  tmp <- as.character(df$value)
  tmp[df$value<3]="1"
  tmp[df$value<0.05]="2"
  tmp[df$value<0.01]="3"
  tmp[df$value<0.001]="4"
  tmp[df$value<0.0001]="5"
  # tmp[df$value<3]="pval >0.5"
  # tmp[df$value<0.05]="0.01 < pval <= 0.05"
  # tmp[df$value<0.01]="0.001 < pval <= 0.01"
  # tmp[df$value<0.001]="0.0001 < pval <= 0.001"
  # tmp[df$value<0.0001]="pval <= 0.0001"
  sig_level = tmp
  df$sig_level = tmp




  df$lt<-melt(Alt_b)
  df$ut<-melt(Aut_b)
  #dflt <- df[df[,"lt.value"]==TRUE, ]
  #df<-as_tibble(df)
  dflt <- df %>% filter(lt$value==TRUE)
  dfut <- df %>% filter(ut$value==TRUE)
  #df[df$lt.value==TRUE,]


  #dfut<-df[df$value!=0,]
  #df$V2 <- factor(df$Var2, levels = df$V2)

  #cat(file = stderr(), paste0("length(g_regions()",length(g_regions()),"\n"))


  regions<<-g_regions()

  #diagonal_elements <- df$Var1==df$Var2

  p<-ggplot(df, aes(x = Var1, y = Var2)) +
    #geom_raster(aes(fill=value)) +
    #scale_fill_gradient(low="grey90", high="red") +
    labs(x="",
         y="",
         title="Significance Matrix",
         caption="handmade with ggplot2") +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=45, vjust=0.9, hjust=1.0),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=12),
                       aspect.ratio = 1) +
    geom_point(data = dflt, aes(x = Var1, y= Var2,colour = sig_level),
               size = 12,
               alpha = 0.5,
               stat = "identity",
               #position='stack',
               show.legend = TRUE
    )+
    scale_color_manual(name = "p-value coding",
                       values = c("white", "green", "yellow", "red"   , "pink", "black"),
                       labels = c(">0.05", "<0.05", "<0.01" , "<0.001", "<0.0001", "not existent"))+
    geom_text(data = dflt, aes(label= ifelse(value<0.05, sprintf("%0.3f", round(value,digits =3)),"")),
              hjust=0.5, vjust=0.5, size = 3,
              stat = "identity")+
    geom_abline(slope = 1, intercept = 0) +
    scale_y_continuous(breaks=seq(1, length(g_regions()), 1), labels = g_regions(), minor_breaks = NULL) +
    scale_x_continuous(breaks=seq(1, length(g_regions()), 1), labels = g_regions(), minor_breaks = NULL)

  #cat(file = stderr(), paste0("length(g_regions()",length(g_regions()),"\n"))

  return(p)



}

