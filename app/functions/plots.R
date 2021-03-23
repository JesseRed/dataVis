
generate_histogram_plot_facet<-function(group1, group2, trial1, trial2, freq, level_x_rval, level_y_rval, data = NULL){
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
  cat(file = stderr(), "into generate_histogram_plot_facet\n")
  if (is.null(data)){
    cat(file = stderr(), "create new data\n")
    d <- get_currently_selected_data_long(g_data(), group1, group2, as.numeric(trial1), as.numeric(trial2), freq)
  }else{
    cat(file = stderr(), "get the old data\n")
    d<-data
  }
  x = d$data1[,level_x_rval, level_y_rval]
  y = d$data2[,level_x_rval, level_y_rval]
  df <- data.frame(Gruppe=c(rep(group1, times=length(x)),
                            rep(group2, times=length(y))),
                   val=c(x, y))
  df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
  # means for geomline
  df_hline = data.frame(Gruppe = c(group1,group2), Means=c(mean(x), mean(y)))
  myplot<- ggplot(df, aes(num, val, fill=Gruppe)) +
    geom_bar(stat="identity") +
    facet_wrap(~Gruppe) +
    geom_hline(data = df_hline, aes(yintercept = Means))
  return(myplot)

<<<<<<< HEAD
}



generate_histogram_plot_facet_long<-function(group1, group2, trial1, trial2, freq, level_x_rval, level_y_rval, data = NULL){
  # Erstellt einen Histogram plot von mehreren Gruppen mit gleichen Achsenskalen

  # output$hist <- renderPlot({
  #   glob_hist_d <<- curdata()
  #   generate_histogram_plot_facet(input$group1, input$group2,
  #                                 input$trial1, input$trial2,
  #                                 g_sel_freqs(),
  #                                 level_x_rval(), level_y_rval(),
  #                                 data = curdata())

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
  cat(file = stderr(), "into generate_histogram_plot_facet_long\n")
  if (is.null(data)){
    cat(file = stderr(), "data are needed error return create new data\n")
    return(NULL)

    #d <- get_currently_selected_data_long(g_data(), group1, group2, as.numeric(trial1), as.numeric(trial2), freq)
  }else{
    cat(file = stderr(), "get the old data\n")
    d<-data
  }

  x = d$data1[,level_x_rval, level_y_rval]
  y = d$data2[,level_x_rval, level_y_rval]
  df <- data.frame(Gruppe=c(rep(group1, times=length(x)),
                            rep(group2, times=length(y))),
                   val=c(x, y))
  df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
  # means for geomline
  df_hline = data.frame(Gruppe = c(group1,group2), Means=c(mean(x), mean(y)))
  myplot<- ggplot(df, aes(num, val, fill=Gruppe)) +
    geom_bar(stat="identity") +
    facet_wrap(~Gruppe) +
    geom_hline(data = df_hline, aes(yintercept = Means))
  return(myplot)

}


=======
#   #d <- get_data_freqmean(g_data(), freq)
#   #cat(file = stderr(), "class(d)=",class(d),"\n")
#   #cat(file = stderr(), paste0("\n","level_x_rval=",level_x_rval))
#   #d <- curdata()
#   if (trial1 == trial2) {
#     #cat(file = stderr(), "trial1 == trial2\n")
#     data1 = d$data1
#     data2 = d$data2
#     x = data1[,level_x_rval, level_y_rval]
#     y = data2[,level_x_rval, level_y_rval]
#
#     #     string1 = paste0(group1," vs ", group2, " in trial ", names(g_trials_named())[trial1], "\n")
#     # d1 = get_data_group_freqmean(g_data(), group1, freq)
#     # d2 = get_data_group_freqmean(g_data(), group2, freq)
#     # x = d1[,level_x_rval, level_y_rval, as.numeric(trial1)]
#     # y = d2[,level_x_rval, level_y_rval, as.numeric(trial1)]
#     df <- data.frame(Gruppe=c(rep(group1, times=length(x)),
#                               rep(group2, times=length(y))),
#                      val=c(x, y))
#     df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
#     # means for geomline
#     df_hline = data.frame(Gruppe = c(group1,group2), Means=c(mean(x), mean(y)))
#     # df$val = d[,level_x_rval, level_y_rval, as.numeric(input$trial1)]
#     # df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
#     # dummy2 = data.frame(Gruppe = c(0,1), Means=c(0.4, 0.5))
#
#   }else if (group1 == group2){
#     string1 = paste0(trial1," vs ", trial2, "in group ", group1, "\n")
#     #data1 = data_1()
#     #data2 = data_2()
#     data1 = d$data1
#     data2 = d$data2
#     x = data1[,level_x_rval, level_y_rval]
#     y = data2[,level_x_rval, level_y_rval]
#     df <- data.frame(Gruppe=c(rep(g_trials()[as.numeric(trial1)], times=length(x)),
#                               rep(g_trials()[as.numeric(trial2)], times=length(y))),
#                      val=c(x, y))
#     #cat(file = stderr(), paste0("val=",c(x,y),"\n"))
#     #cat(file = stderr(), paste0("df=",df,"\n"))
#
#     df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
#     # means for geomline
#     df_hline = data.frame(Gruppe = c(g_trials()[as.numeric(trial1)],
#                                      g_trials()[as.numeric(trial2)]),
#                           Means=c(mean(x), mean(y)))
#   } else{
#
#     #my_paired <- d$my_paired
#     data1 <- d$data1
#     data2 <- d$data2
#     string1 <-d$string1
#     #mat_p <- d$mat_p
#     #mat_t <- d$mat_t
#
#     x = data1[,level_x_rval, level_y_rval]
#     y = data2[,level_x_rval, level_y_rval]
#
#     df <- data.frame(Gruppe=c(rep(group1, times=length(x)),
#                               rep(group2, times=length(y))),
#                      val=c(x, y))
#     df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
#     # means for geomline
#     df_hline = data.frame(Gruppe = c(group1, group2), Means=c(mean(x), mean(y)))
#
#   }
#   myplot<- ggplot(df, aes(num, val, fill=Gruppe)) +
#     geom_bar(stat="identity") +
#     facet_wrap(~Gruppe) +
#     geom_hline(data = df_hline, aes(yintercept = Means))
# return(myplot)
}

>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
generate_plot_Circle<-function(mat_p, mat_t, data1, data2, regions = g_regions()){
  rownames(mat_t) = regions
  colnames(mat_t) = regions
  #cat(file = stderr(), paste0("Circle regions = ", regions,"\n"))
  #cat(file = stderr(), mat_t)
  x = data1[,1,2]
  y = data2[,2,3]
  z = t.test(x,y)
  df = z$parameter

  #M = 1/d$mat_p
  M = abs(log(mat_p))

  t_threshold = abs(log(g_sig()))
  rownames(M) = regions
  colnames(M) = regions
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

generate_plot_Corrplot<-function(mat_p, mat_t,
                                 myfontsize = g_saveImage_fontsize(),
                                 inline_numbers = g_visprop_inlinenumbers(),
                                 only_sig = g_visprop_onlysig(),
                                 regions = g_regions(),
                                 clustering_method = "original",
                                 num_hclust = 0){
  start_time <- Sys.time()
  # colnames(d$mat_p) = g_regions()
  # rownames(d$mat_p) = vector(mode="character", length=length(g_regions()))
  # colnames(d$mat_t) = vector(mode="character", length=length(g_regions()))
  # rownames(d$mat_t) = g_regions()
  # setting the fontsize
  #if ((myfontsize >8) & (myfontsize<12)){
  #  cex = 0.4
  #}
<<<<<<< HEAD


=======
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
  if (is.null(myfontsize)){myfontsize=14}

  cex = myfontsize /20
  cat(file = stderr(), cex)
  if (cex>1.0){
    cex = 1.0
  }
<<<<<<< HEAD


  if (only_sig){
    insig = "blank"
    #insig = "pch"
  }else if (min(mat_p, na.rm = TRUE)>g_sig()){
    insig = "p-value"
  }else{
    insig = "pch"

    insig = "n"
    insig = "label_sig"
  }
  cat(file = stderr(), "after if\n")
  if (inline_numbers && !(insig=="p-value")){
=======
  if (only_sig) {
    insig = "blank"
    #insig = "pch"
  }else{
    insig = "pch"
    insig = "p-value"
    insig = "n"
    insig = "label_sig"
  }
  if (inline_numbers){
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
    method = "number"
  } else{
    method = "square"
    method = "circle"
    method = "shade"
    method = "color"
  }
<<<<<<< HEAD
  if (insig =="label_sig"){
    multi_sig_level = c(.001, .01, g_sig())
  }else{
    multi_sig_level = g_sig()
  }
  glob_mat_p <<- mat_p
  glob_mat_t <<- mat_t
  #cat(file = stderr(), paste0("Corr regions = ", regions,"\n"))
  cat(file = stderr(), paste0("method = ", g_act_method(),"\n"))
  #cat(file = stderr(), paste0("Corr colnames = ", colnames(mat_p),"\n"))
  #cat(file = stderr(), mat_t)

  if ((g_act_method()=="Coherence") | (g_act_method()=="Connectivity")){
    cat(file = stderr(), paste0("g_act_method is =", g_act_method() ,"\n"))
    cat(file = stderr(), paste0("insig =", insig ,"\n"))
    gcorplot_matp <<- mat_p
    gcorplot_matt <<- mat_t
  # Erstellt einen Histogram plot von mehreren Gruppen mit gleichen Achsenskalen
  #d = get_currently_selected_data(g_data(), group1, group2, trial1, trial2, freq())
    rownames(mat_p) = vector(mode="character", length=length(regions))
    myplot_corr <- corrplot(mat_p, method=method,
                             type = "upper",
                             is.corr = FALSE,
                             p.mat = mat_p,
                             sig.level = multi_sig_level,
                             tl.cex = cex,
                             tl.srt = 45,
                             insig = insig,
                             pch.cex = 0.4,
                             pch.col = "white",
                             order = clustering_method,
                             addrect = num_hclust,
                             col=colorRampPalette(c("blue","red","green"))(200),
                             cl.pos = "r")
  colnames(mat_t) = vector(mode="character", length=length(regions))
  myplot_corr <- corrplot(mat_t,
                          add = TRUE,
                          method=method,
                          tl.cex = cex,
                          type = "lower",
                          is.corr = FALSE,
                          p.mat = mat_p,
                          sig.level = g_sig(),
                          insig = insig,
                          tl.srt = 45,
                          order = clustering_method,
                          addrect = num_hclust,
                          col = colorRampPalette(c("green", "yellow","black"))(200),
                          cl.pos = "b")
=======
  multi_sig_level = c(.001, .01, g_sig())
  glob_mat_p <<- mat_p
  glob_mat_t <<- mat_t
  #cat(file = stderr(), paste0("Corr regions = ", regions,"\n"))
  #cat(file = stderr(), paste0("Corr colnames = ", colnames(mat_p),"\n"))
  #cat(file = stderr(), mat_t)

  if (g_act_method()=="Coherence"){

  # Erstellt einen Histogram plot von mehreren Gruppen mit gleichen Achsenskalen
  #d = get_currently_selected_data(g_data(), group1, group2, trial1, trial2, freq())
    rownames(mat_p) = vector(mode="character", length=length(regions))
    myplot_corr <<- corrplot(mat_p, method=method, tl.cex = cex, type = "upper", is.corr = FALSE,
                  p.mat = mat_p, sig.level = multi_sig_level, tl.srt = 45,
                  insig = insig, pch.cex = 0.4, pch.col = "white",
                  order = clustering_method,
                  addrect = num_hclust,
                  col=colorRampPalette(c("blue","red","green"))(200))
  colnames(mat_t) = vector(mode="character", length=length(regions))

  myplot_corr <- corrplot(mat_t, add = TRUE, method=method, tl.cex = cex, type = "lower", is.corr = FALSE,
                  p.mat = mat_p, sig.level = g_sig(), insig = insig, tl.srt = 45)
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc


  }else if (g_act_method()=="Transferentropy") {
    rownames(mat_p) = vector(mode="character", length=length(regions))
    x1 <<- corrplot(mat_p, method=method, tl.cex = cex, type = "upper", is.corr = FALSE,
                    p.mat = mat_p, sig.level = multi_sig_level, tl.srt = 45,
                    insig = insig, pch.cex = 0.4, pch.col = "white",
                    order = clustering_method,
                    addrect = num_hclust,
                    col=colorRampPalette(c("blue","red","green"))(200))
<<<<<<< HEAD
    colnames(mat_p) = vector(mode="character", length=length(regions))

    myplot_corr <<- corrplot(mat_p, add = TRUE, method=method, tl.cex = cex, type = "lower", is.corr = FALSE,
=======
    colnames(mat_t) = vector(mode="character", length=length(regions))

    myplot_corr <<- corrplot(mat_t, add = TRUE, method=method, tl.cex = cex, type = "lower", is.corr = FALSE,
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
                             p.mat = mat_p, sig.level = g_sig(), insig = insig, tl.srt = 45)



#    myplot_corr <<- corrplot(d$mat_p, method=method, tl.cex = cex, is.corr = FALSE,
#                             p.mat = d$mat_p, sig.level = g_sig(),tl.srt = 45,
#                             insig = insig,
#                             col=colorRampPalette(c("blue","red","green"))(200))
  }else if (g_act_method()=="Granger") {
    cat(file = stderr(), "Corrrplot Granger not implemented")
    myplot_corr = NULL
  }else if (g_act_method()=="Frequency") {
    cat(file = stderr(), "Corrplot Frequency not implemented")
    myplot_corr = NULL
<<<<<<< HEAD
  }else if ((g_act_method()=="RS")){#|(g_act_method()=="Connectivity")) {
    cat(file = stderr(), "RS in corrplot\n")
    # setzte auf leer damit keine Namen in diagonalelementen angezeigt werden
=======
  }else if (g_act_method()=="RS") {
    cat(file = stderr(), "RS in corrplot\n")
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
    rownames(mat_p) = vector(mode="character", length=length(regions))
    x1 <<- corrplot(mat_p,
                    method=method,
                    tl.cex = cex,
                    type = "upper",
                    is.corr = FALSE,
                    p.mat = mat_p,
                    tl.srt = 45,
                    sig.level = multi_sig_level,
                    insig = insig,
                    pch.cex = 0.5,
                    addgrid.col = "grey",
<<<<<<< HEAD
                    order = clustering_method, #"hclust", #clustering_method,
=======
                    order = "hclust", #clustering_method,
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
                    hclust.method = "average",
                    addrect = num_hclust,
                    pch.col = "white",
                    col=colorRampPalette(c("blue","red","green"))(200))
    colnames(mat_t) = vector(mode="character", length=length(regions))

    myplot_corr <<- corrplot(mat_t,
                             add = TRUE,
                             type = "lower",
                             is.corr = FALSE,
                             method=method,
                             tl.cex = cex,
                             p.mat = mat_p,
                             sig.level = g_sig(),
                             pch.cex = 0.5,
                             order = clustering_method,
                             addrect = num_hclust,
                             insig = insig,
                             tl.srt = 45)
  }
  cat(file = stderr(), paste0("generate_plot_Corrplot duration = ", Sys.time()-start_time,"\n" ))
  return(myplot_corr)

}

generate_plot_Pheatmap<-function(mat_p,
                                 mat_t,
                                 myfontsize = g_saveImage_fontsize(),
                                 inline_numbers = g_visprop_inlinenumbers(),
                                 regions = g_regions(),
                                 sig_level = g_sig()){
  cat(file=stderr(),paste0("show inline numbers = ",inline_numbers,"\n"))
  colnames(mat_p) = regions
  rownames(mat_p) = regions

  # Returns a vector of 'num.colors.in.palette'+1 colors. The first 'cutoff.fraction'
  # fraction of the palette interpolates between colors[1] and colors[2], the remainder
  # between colors[3] and colors[4]. 'num.colors.in.palette' must be sufficiently large
  # to get smooth color gradients.
  makeColorRampPalette <- function(colors, cutoff.fraction, num.colors.in.palette=100, pmin=0.0, pmax=1.0)
  {
    num.colors.in.palette = as.integer((pmax-pmin)*100)
    cutoff.fraction_low = cutoff.fraction-pmin
    cutoff.fraction_high = pmax-cutoff.fraction_low

    stopifnot(length(colors) == 4)
    ramp1 <- colorRampPalette(colors[1:2])(num.colors.in.palette * cutoff.fraction_low)
    ramp2 <- colorRampPalette(colors[3:4])(num.colors.in.palette * cutoff.fraction_high)
    return(c(ramp1, ramp2))
  }

  cutoff.distance <- sig_level # -0.01 # die 0.01 weil sonst 0.06 auch eingefaerbt wird ... vollstaendig verstehe ich das aber nicht
  cols <- makeColorRampPalette(c("red", "orange",    # distances 0 to 3 colored from white to red
                                 "gray", "black"), # distances 3 to max(distmat) colored from green to black
                               cutoff.distance / 1, #max(mat_p), #max(distmat),
                               pmin= min(mat_p),
                               pmax = max(mat_p)
                               )

  myplot<-pheatmap(
    mat                   = mat_p,
    display_numbers       = inline_numbers,
    color                 = cols,
    fontsize              = myfontsize,
    main                  = "P-Values Pheatmap",
    show_rownames         = TRUE,
    show_colnames         = TRUE,
    cluster_cols          = FALSE,
    cluster_rows          = FALSE,
    angle_col             = 45,

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

generate_plot_ggplot_corrplot_handmade<- function(mat_p, mat_t,
<<<<<<< HEAD
                                                  mat_mean_diff = NULL,
=======
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
                                                  myfontsize = g_saveImage_fontsize(),
                                                  inline_numbers = g_visprop_inlinenumbers(),
                                                  regions = g_regions()){

  start_time <- Sys.time()

  rownames(mat_p)<-NULL
  colnames(mat_p)<-NULL
  rownames(mat_t)<-NULL
  colnames(mat_t)<-NULL


  A <- mat_p
  Alt_b = lower.tri(A, diag = FALSE)
  Aut_b = upper.tri(A, diag = FALSE)
  Alt <- A[Alt_b]
  Aut <- A[Aut_b]
  df_tval <- melt(mat_t)
  df<-melt(A)
  df$t_val <- df_tval$value

<<<<<<< HEAD
  if (is.null(mat_mean_diff)){
    mat_mean_diff = mat_t * 0
  }
  df_mean_diff <- melt(mat_mean_diff)
  df$mean_diff <- df_mean_diff$value
  df$mean_bool <- df_mean_diff$value>0
  df$mean_disc <- unlist(lapply(df$mean_bool, as.numeric))
  #df$mean_discf <- as.factor(df$mean_disc)

=======
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
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


<<<<<<< HEAD
=======


>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
  df$lt<-melt(Alt_b)
  df$ut<-melt(Aut_b)
  #dflt <- df[df[,"lt.value"]==TRUE, ]
  #df<-as_tibble(df)
  dflt <- df %>% filter(lt$value==TRUE)
  dfut <- df %>% filter(ut$value==TRUE)
<<<<<<< HEAD

  #df[df$lt.value==TRUE,]


  gdflt <<- dflt
  gdf <<- df

=======
  #df[df$lt.value==TRUE,]


>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
  #dfut<-df[df$value!=0,]
  #df$V2 <- factor(df$Var2, levels = df$V2)

  #cat(file = stderr(), paste0("length(g_regions()",length(g_regions()),"\n"))

  #diagonal_elements <- df$Var1==df$Var2
<<<<<<< HEAD
  p<-ggplot(df, aes(x = Var1, y = Var2, shape = mean_bool)) +
=======

  p<-ggplot(df, aes(x = Var1, y = Var2)) +
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
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
<<<<<<< HEAD
    scale_shape_manual(name = "Group Difference",
                       values = c(15, 16),
                       labels = c("G1<G2", "G1>G2"))+
=======
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc
    geom_text(data = dflt, aes(label= ifelse(value<0.05, sprintf("%0.3f", round(value,digits =3)),"")),
              hjust=0.5, vjust=0.5, size = 3,
              stat = "identity")+
    geom_abline(slope = 1, intercept = 0) +
<<<<<<< HEAD
    # geom_point(data = dflt, aes(x = Var1, y= Var2, colour = mean_bool),
    #            shape = 0,
    #            size = 14,
    #            #alpha = 0.8,
    #            stroke = 3,
    #            #stat = "identity",
    #            #position='stack',
    #            fill = NA,
    #            show.legend = TRUE
    # )+
    #scale_shape(solid = FALSE)+
  # scale_color_manual(name = "group-value coding",
  #                    values = c("blue", "red"),
  #                    labels = c(0,1)
  #                    )+

  #shape =21, size = 14, colour = "mediumvioletred", stroke = 3) +
  scale_y_continuous(breaks=seq(1, length(regions), 1), labels = regions, minor_breaks = NULL) +
    scale_x_continuous(breaks=seq(1, length(regions), 1), labels = regions, minor_breaks = NULL)

  cat(file = stderr(), paste0("generate_plot_ggplot_corrplot_handmade duration = ", Sys.time()-start_time,"\n" ))
=======
    scale_y_continuous(breaks=seq(1, length(regions), 1), labels = regions, minor_breaks = NULL) +
    scale_x_continuous(breaks=seq(1, length(regions), 1), labels = regions, minor_breaks = NULL)
    cat(file = stderr(), paste0("generate_plot_ggplot_corrplot_handmade duration = ", Sys.time()-start_time,"\n" ))
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc

  #cat(file = stderr(), paste0("length(regions",length(regions),"\n"))

  return(p)



}
<<<<<<< HEAD
#
# generate_plot_ggplot_corrplot_handmade<- function(mat_p, mat_t,
#                                                   myfontsize = g_saveImage_fontsize(),
#                                                   inline_numbers = g_visprop_inlinenumbers(),
#                                                   regions = g_regions()){
#
#   start_time <- Sys.time()
#
#   rownames(mat_p)<-NULL
#   colnames(mat_p)<-NULL
#   rownames(mat_t)<-NULL
#   colnames(mat_t)<-NULL
#
#
#   A <- mat_p
#   Alt_b = lower.tri(A, diag = FALSE)
#   Aut_b = upper.tri(A, diag = FALSE)
#   Alt <- A[Alt_b]
#   Aut <- A[Aut_b]
#   df_tval <- melt(mat_t)
#   df<-melt(A)
#   df$t_val <- df_tval$value
#
#   tmp <- as.character(df$value)
#   tmp[df$value<3]="1"
#   tmp[df$value<0.05]="2"
#   tmp[df$value<0.01]="3"
#   tmp[df$value<0.001]="4"
#   tmp[df$value<0.0001]="5"
#   # tmp[df$value<3]="pval >0.5"
#   # tmp[df$value<0.05]="0.01 < pval <= 0.05"
#   # tmp[df$value<0.01]="0.001 < pval <= 0.01"
#   # tmp[df$value<0.001]="0.0001 < pval <= 0.001"
#   # tmp[df$value<0.0001]="pval <= 0.0001"
#   sig_level = tmp
#   df$sig_level = tmp
#
#
#
#
#   df$lt<-melt(Alt_b)
#   df$ut<-melt(Aut_b)
#   #dflt <- df[df[,"lt.value"]==TRUE, ]
#   #df<-as_tibble(df)
#   dflt <- df %>% filter(lt$value==TRUE)
#   dfut <- df %>% filter(ut$value==TRUE)
#   #df[df$lt.value==TRUE,]
#
#
#   #dfut<-df[df$value!=0,]
#   #df$V2 <- factor(df$Var2, levels = df$V2)
#
#   #cat(file = stderr(), paste0("length(g_regions()",length(g_regions()),"\n"))
#
#   #diagonal_elements <- df$Var1==df$Var2
#
#   p<-ggplot(df, aes(x = Var1, y = Var2)) +
#     #geom_raster(aes(fill=value)) +
#     #scale_fill_gradient(low="grey90", high="red") +
#     labs(x="",
#          y="",
#          title="Significance Matrix",
#          caption="handmade with ggplot2") +
#     theme_bw() + theme(axis.text.x=element_text(size=9, angle=45, vjust=0.9, hjust=1.0),
#                        axis.text.y=element_text(size=9),
#                        plot.title=element_text(size=12),
#                        aspect.ratio = 1) +
#     geom_point(data = dflt, aes(x = Var1, y= Var2,colour = sig_level),
#                size = 12,
#                alpha = 0.5,
#                stat = "identity",
#                #position='stack',
#                show.legend = TRUE
#     )+
#     scale_color_manual(name = "p-value coding",
#                        values = c("white", "green", "yellow", "red"   , "pink", "black"),
#                        labels = c(">0.05", "<0.05", "<0.01" , "<0.001", "<0.0001", "not existent"))+
#     geom_text(data = dflt, aes(label= ifelse(value<0.05, sprintf("%0.3f", round(value,digits =3)),"")),
#               hjust=0.5, vjust=0.5, size = 3,
#               stat = "identity")+
#     geom_abline(slope = 1, intercept = 0) +
#     scale_y_continuous(breaks=seq(1, length(regions), 1), labels = regions, minor_breaks = NULL) +
#     scale_x_continuous(breaks=seq(1, length(regions), 1), labels = regions, minor_breaks = NULL)
#   cat(file = stderr(), paste0("generate_plot_ggplot_corrplot_handmade duration = ", Sys.time()-start_time,"\n" ))
#
#   #cat(file = stderr(), paste0("length(regions",length(regions),"\n"))
#
#   return(p)
#
#
#
# }
=======
>>>>>>> 2d838b8f5f94f7854c105dd6ec3a1771c6573efc

