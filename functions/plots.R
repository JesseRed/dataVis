
generate_histogram_plot_facet<-function(group1, group2, trial1, trial2, freq, level_x_rval, level_y_rval, d){
  # Erstellt einen Histogram plot von mehreren Gruppen mit gleichen Achsenskalen

  # gedacht fuer tabs in denen gruppen und trials ausgewaehlt werden
  # d$mypaired ... bool handelt es sich um einen gepaarten oder ungeparrten test
  # d$data1 ... 3D matrix ... der gruppe 1 bzw trial 1
  # d$data2 ... 3D matrix ... der gruppe 2 bzw trial 2
  # d$mat_p ... 2D matrix ... p Werte des t-tests ueber alle Regionen
  # d$mat_t ... 2D matrix ... t Werte des t-tests ueber alle Regionen
  # d$string1 ... string .... eine beschreibung des durchgefuehrten Vergleiches
  # d$color1 ... col ........ die Color palette die zu den Werten passen
  d = get_data_freqmean(g_data(), freq())
  #cat(file = stderr(), paste0("\n","level_x_rval=",level_x_rval))

  if (trial1 == trial2) {
    #cat(file = stderr(), "trial1 == trial2\n")
    string1 = paste0(group1," vs ", group2, " in trial ", names(g_trials_named())[trial1], "\n")
    d1 = get_data_group_freqmean(g_data(), group1, freq())
    d2 = get_data_group_freqmean(g_data(), group2, freq())
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
    data1 = d$data_1
    data2 = d$data_2
    x = data1[,level_x_rval, level_y_rval]
    y = data2[,level_x_rval, level_y_rval]
    df <- data.frame(Gruppe=c(rep(g_trials()[as.numeric(trial1)], times=length(x)),
                              rep(g_trials()[as.numeric(trial2)], times=length(y))),
                     val=c(x, y))
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

generate_plot_Coherence<-function(mat_p, mat_t){
  # Erstellt einen Histogram plot von mehreren Gruppen mit gleichen Achsenskalen
  #d = get_currently_selected_data(g_data(), group1, group2, trial1, trial2, freq())
  x1 <<- corrplot(mat_p, method="number", tl.cex = 0.9, type = "upper", is.corr = FALSE,
                  p.mat = mat_p, sig.level = g_sig(),
                  col=colorRampPalette(c("blue","red","green"))(200))
  colnames(d$mat_t) = vector(mode="character", length=length(g_regions))

  x2 <<- corrplot(mat_t, add = TRUE, method="number", tl.cex = 0.9, type = "lower", is.corr = FALSE,
                  p.mat = mat_p, sig.level = g_sig())
  return(x2)
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
