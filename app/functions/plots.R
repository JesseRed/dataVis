
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
    cat(file = stderr(), "get the old datax\n")
    d<-data
  }

  g_d_facetlong <<-d

  x = d$data1[,level_x_rval, level_y_rval]
  y = d$data2[,level_x_rval, level_y_rval]
  df <- data.frame(Gruppe=c(rep("A", times=length(x)),
                            rep("B", times=length(y))),
                   val=c(x, y))
  df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
  # means for geomline
  df_hline = data.frame(Gruppe = c("A","B"), Means=c(mean(x), mean(y)))
  myplot<- ggplot(df, aes(num, val, fill=Gruppe)) +
    geom_bar(stat="identity") +
    facet_wrap(~Gruppe) +
    geom_hline(data = df_hline, aes(yintercept = Means))
  return(myplot)
  # cat(file = stderr(), "into generate_histogram_plot_facet_long\n")
  # if (is.null(data)){
  #   cat(file = stderr(), "data are needed error return create new data\n")
  #   return(NULL)
  #
  #
  #   #d <- get_currently_selected_data_long(g_data(), group1, group2, as.numeric(trial1), as.numeric(trial2), freq)
  # }else{
  #   cat(file = stderr(), "get the old data\n")
  #   d<-data
  # }
  #
  # x = d$data1[,level_x_rval, level_y_rval]
  # y = d$data2[,level_x_rval, level_y_rval]
  # df <- data.frame(Gruppe=c(rep(group1, times=length(x)),
  #                           rep(group2, times=length(y))),
  #                  val=c(x, y))
  # df$num <- ave(df$val, df$Gruppe, FUN = seq_along)
  # # means for geomline
  # df_hline = data.frame(Gruppe = c(group1,group2), Means=c(mean(x), mean(y)))
  # myplot<- ggplot(df, aes(num, val, fill=Gruppe)) +
  #   geom_bar(stat="identity") +
  #   facet_wrap(~Gruppe) +
  #   geom_hline(data = df_hline, aes(yintercept = Means))
  # return(myplot)

}


generate_plot_Circle<-function(mat_p, mat_t, data1, data2, regions = g_regions(),
                               method =  g_act_method()){
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
  if (!((method == 'Transferentropy') || (method == 'Granger'))){
    M[upper.tri(M)]=0.001
  }

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
                                 show_color = g_visprop_color(),
                                 regions = g_regions(),
                                 clustering_method = "original",
                                 num_hclust = 0,
                                 sig_level = g_sig(),
                                 title = NULL){
  start_time <- Sys.time()
  red_hex = "#FF0000"
  ora_hex = "#ee7777" #FF9E00"
  yel_hex = "#eeaf77" #FFEB00"
  gre_hex = "#eed277" #D9FF00"
  hex_cols = c(red_hex, ora_hex, yel_hex, gre_hex)

  # colnames(d$mat_p) = g_regions()
  # rownames(d$mat_p) = vector(mode="character", length=length(g_regions()))
  # colnames(d$mat_t) = vector(mode="character", length=length(g_regions()))
  # rownames(d$mat_t) = g_regions()
  # setting the fontsize
  #if ((myfontsize >8) & (myfontsize<12)){
  #  cex = 0.4
  #}
  cat(file = stderr(), paste0("inline_numbers = ", inline_numbers, "\n"))
  cat(file = stderr(), paste0("only_sig = ", only_sig, "\n"))
  cat(file = stderr(), paste0("show_color = ", show_color, "\n"))


  if (is.null(title)){title = paste0("Corrplot with method ",g_act_method())}
  else{
    title = paste0(title, "method = ", g_act_method())
  }


  if (is.null(myfontsize)){myfontsize=14}

  cex = myfontsize /20
  cat(file = stderr(), cex)
  if (cex>1.0){
    cex = 1.0
  }



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
    method = "number"
  } else{
    method = "square"
    method = "circle"
    method = "shade"
    method = "color"
  }
  if (insig =="label_sig"){
    multi_sig_level = c(.001, .01, g_sig())
  }else{
    multi_sig_level = g_sig()
  }



  if (show_color){
    method = "color"
    col <- colorRampPalette(c("#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF","#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#EE9988", "#BB4444", "#EE9988", "#FFFFFF","#FFFFFF", "#FFFFFF",  "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF","#FFFFFF", "#FFFFFF","#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF" ))
    col <- colorRampPalette(c( "#BB4444", "#EE9988", "#FFFFFF","#FFFFFF", "#FFFFFF","#FFFFFF", "#FFFFFF","#FFFFFF", "#FFFFFF","#FFFFFF", "#77AADD", "#4477AA"))
    col <- colorRampPalette(c( "#BB4444", "#FFFFFF","#FFFFFF", "#FFFFFF","#FFFFFF", "#FFFFFF","#FFFFFF", "#FFFFFF","#FFFFFF", "#4477AA"))
    col_t <- colorRampPalette(c( "#BB4444", "#EE9988", "#FFFFFF","#FFFFFF", "#FFFFFF","#FFFFFF", "#FFFFFF","#FFFFFF", "#FFFFFF","#FFFFFF", "#77AADD", "#4477AA"))
  }else{
    method = "color"
    col   <- colorRampPalette(c( "#FFFFFF", "#FFFFFF", "#FFFFFF"))
    col_t <- colorRampPalette(c( "#FFFFFF", "#FFFFFF", "#FFFFFF"))
  }

  if (inline_numbers){
    number_color = "black"
  }else{
    number_color = NULL
  }

  if (only_sig){
    insig = "blank"
    multi_sig_level = sig_level
  }

  if (!only_sig){
    insig = "blank"
    multi_sig_level = 1.0
  }

  if (!only_sig && !inline_numbers){
    method = "color"
    insig = "label_sig"
    number_color = NULL
    multi_sig_level = c(.001, .01, sig_level)
  }


  glob_mat_p <<- mat_p
  glob_mat_t <<- mat_t

  #cat(file = stderr(), paste0("Corr regions = ", regions,"\n"))
  cat(file = stderr(), paste0("method = ", g_act_method(),"\n"))
  #cat(file = stderr(), paste0("Corr colnames = ", colnames(mat_p),"\n"))
  #cat(file = stderr(), mat_t)

  # M<- glob_mat_p
  # col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  # col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  # p.mat = glob_mat_p
  # myplot_corr<-corrplot(M, method="color", col=col(200),
  #          type="upper", order="hclust",
  #          addCoef.col = "black", # Add coefficient of correlation
  #          tl.col="black", tl.srt=45, #Text label color and rotation
  #          # Combine with significance
  #          p.mat = p.mat, sig.level = 0.101, insig = "blank",
  #          # hide correlation coefficient on the principal diagonal
  #          diag=FALSE
  # )
  e_method <<- method
  e_insig <<- insig
  e_clustering_method <<- clustering_method
  e_num_hclust <<- num_hclust

  e_col <<- col
  e_col_t<<-col_t


  if ((g_act_method()=="Coherence") | (g_act_method()=="Connectivity") | (g_act_method() == "RS")){
    #cat(file = stderr(), paste0("g_act_method is =", g_act_method() ,"\n"))
    #cat(file = stderr(), paste0("insig =", insig ,"\n"))
    gcorplot_matp <<- mat_p
    gcorplot_matt <<- mat_t
    #cat(file = stderr(), paste0("mat_p =", mat_p ,"\n"))

  # Erstellt einen Histogram plot von mehreren Gruppen mit gleichen Achsenskalen
  #d = get_currently_selected_data(g_data(), group1, group2, trial1, trial2, freq())
#     rownames(mat_p) = vector(mode="character", length=length(regions))
#     myplot_corr <- corrplot(mat_p, method=method,
#                              type = "upper",
#                              is.corr = FALSE,
#                              p.mat = mat_p,
#                              sig.level = multi_sig_level,
#                              tl.cex = cex,
#                              tl.srt = 45,
#                              insig = insig,
#                              pch.cex = 0.4,
#                              pch.col = "white",
#                              order = clustering_method,
#                              addrect = num_hclust,
#                             col = col(500),
# #                            col=colorRampPalette(c("blue","red","green"))(200),
#                              title = paste0("Corrplot with method ",g_act_method()),
#                              mar=c(0,0,1,0),
#                              cl.pos = "r")
#   colnames(mat_t) = vector(mode="character", length=length(regions))
#   myplot_corr <- corrplot(mat_t,
#                           add = TRUE,
#                           method=method,
#                           tl.cex = cex,
#                           type = "lower",
#                           is.corr = FALSE,
#                           p.mat = mat_p,
#                           sig.level = g_sig(),
#                           insig = insig,
#                           tl.srt = 45,
#                           order = clustering_method,
#                           addrect = num_hclust,
#                           title = paste0("Corrplot with method ",g_act_method()),
#                           mar=c(0,0,1,0),
#                           col = col(500),
#                           #col = colorRampPalette(c("green", "yellow","black"))(200),
#                           cl.pos = "b")

    rownames(mat_p) = vector(mode="character", length=length(regions))

    colnames(mat_t) = vector(mode="character", length=length(regions))

#    cat(file = stderr(), paste0("before corrpolot with mat_t"))


    # NAs in den Diagnonalelementen werden von der neuen Version von
    # corrplot nicht mehr akzeptiert
    # Error in data.frame(..., check.names = FALSE) : arguments imply differing number of rows: 55, 45
    mat_p <- replace_na(mat_p,1)
    mat_t <- replace_na(mat_t,0)
    max_abs_mat_t <- max(abs(max(mat_t)), abs(min(mat_t)))
    mat_p1023 <<- mat_p
    mat_t1023 <<- mat_t
    x1 <<- corrplot(mat_t,
                             add = FALSE,
                             type = "lower",
                             is.corr = FALSE,
                             method=method,
                             tl.cex = cex,
                             addCoef.col = number_color,
                             p.mat = mat_p,
                             sig.level = multi_sig_level,
                             insig = insig,
                             pch.col = "black",
                             pch.cex = 0.5,
                             addgrid.col = "grey",
                             order = clustering_method,
                             hclust.method = "average",
                             addrect = num_hclust,
                             title = title, #paste0("Corrplot with method ",g_act_method()),
                             col = col_t(1000),
                             col.lim = c(-1*max_abs_mat_t, max_abs_mat_t),
                             mar=c(0,0,2,2),
                             tl.srt = 45)

    myplot_corr <<- corrplot(mat_p,
                             type = "upper",
                             add = TRUE,
                             is.corr = FALSE,
                             method=method,
                             tl.cex = cex,
                             p.mat = mat_p,
                             tl.srt = 45,
                             addCoef.col = number_color,
                             sig.level = multi_sig_level,
                             insig = insig,
                             pch.col = "black",
                             pch.cex = 0.5,
                             addgrid.col = "grey",
                             order = clustering_method, #"hclust", #clustering_method,
                             hclust.method = "average",
                             addrect = num_hclust,
                             title = title, #paste0("Corrplot with method ",g_act_method()),
                             col = create_sig_colorramp(hex_cols),
                             col.lim = c(0,1),
                             #col=colorRampPalette(c("blue","red","green"))(200),
                             mar=c(0,0,2,2)
                            )

    #text(paste("Correlation:", round(cor(x, y), 2)), x = 25, y = 95)
    leg <-legend( x= "left", legend=c("<0.1", "<0.05", "<0.01", "<0.001"),
                  col=hex_cols, #inset =c(-0.0,0.3),
                  fill= hex_cols,
                  cex = 1.5,
                  pch=c(".",".", ".", ".","."))
    legend( x = (leg$rect$left + leg$rect$w) * 1.05, y = leg$rect$top,
            legend=c("<0.001", "<0.01", "<0.05", "<0.1"),
            col=hex_cols, #inset =c(0.03,0.3),
            fill= hex_cols,
            cex = 1.5,
            pch=c(".",".", ".", ".","."))




  }else if (g_act_method()=="Transferentropy") {
    rownames(mat_p) = vector(mode="character", length=length(regions))

    myplot_corr <<- corrplot(mat_p,
                    is.corr = FALSE,
                    method=method,
                    tl.cex = cex,
                    p.mat = mat_p,
                    tl.srt = 45,
                    addCoef.col = number_color,
                    sig.level = multi_sig_level,
                    insig = insig,
                    pch.col = "black",
                    pch.cex = 0.5,
                    addgrid.col = "grey",
                    order = clustering_method, #"hclust", #clustering_method,
                    hclust.method = "average",
                    addrect = num_hclust,
                    title = title, #paste0("Corrplot with method ",g_act_method()),
                    col = col(500),
                    #col=colorRampPalette(c("blue","red","green"))(200),
                    mar=c(0,0,1,1)
    )

    colnames(mat_p) = vector(mode="character", length=length(regions))


   }else if (g_act_method()=="Granger") {
     cat(file = stderr(), "Corrrplot Granger not implemented")
     myplot_corr = NULL
   }else if (g_act_method()=="Frequency") {
     cat(file = stderr(), "Corrplot Frequency not implemented")
     myplot_corr = NULL
   }
  # else if ((g_act_method()=="RS")){#|(g_act_method()=="Connectivity")) {
  #    cat(file = stderr(), "RS in corrplot\n")
  #    # setzte auf leer damit keine Namen in diagonalelementen angezeigt werden
  #    rownames(mat_p) = vector(mode="character", length=length(regions))
  #    x1 <<- corrplot(mat_p,
  #                    type = "upper",
  #                    is.corr = FALSE,
  #                    method=method,
  #                    tl.cex = cex,
  #                    p.mat = mat_p,
  #                    tl.srt = 45,
  #                    addCoef.col = number_color,
  #                    sig.level = multi_sig_level,
  #                    insig = insig,
  #                    pch.col = "black",
  #                    pch.cex = 0.5,
  #                    addgrid.col = "grey",
  #                    order = clustering_method, #"hclust", #clustering_method,
  #                    hclust.method = "average",
  #                    addrect = num_hclust,
  #                    title = paste0("Corrplot with method ",g_act_method()),
  #                    col = col(500),
  #                    #col=colorRampPalette(c("blue","red","green"))(200),
  #                    mar=c(0,0,1,1)
  #    )
  #    colnames(mat_t) = vector(mode="character", length=length(regions))
  #
  #    myplot_corr <<- corrplot(mat_t,
  #                             add = TRUE,
  #                             type = "lower",
  #                             is.corr = FALSE,
  #                             method=method,
  #                             tl.cex = cex,
  #                             addCoef.col = number_color,
  #                             p.mat = mat_p,
  #                             sig.level = multi_sig_level,
  #                             insig = insig,
  #                             pch.col = "black",
  #                             pch.cex = 0.5,
  #                             addgrid.col = "grey",
  #                             order = clustering_method,
  #                             hclust.method = "average",
  #                             addrect = num_hclust,
  #                             title = paste0("Corrplot with method ",g_act_method()),
  #                             col = col_t(500),
  #                             mar=c(0,0,1,1),
  #                             tl.srt = 45)
  #  }
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


create_sig_colorramp <- function(hex_cols){

  # color ramp
  # idee ist es 1000 Werte von 0-1 fuer die p Werte zu haben
  # <0.001 rot  - 1
  # <0.01  rosa 2-10
  # <0.05  gruen 11-50
  # <0.1   blau  51-100

  col <- colorRampPalette(c( "#BB4444", "#FFFFFF","#FFFFFF", "#FFFFFF","#FFFFFF", "#FFFFFF","#FFFFFF", "#FFFFFF","#FFFFFF", "#4477AA"))

  sig_col <- col(1000)
  sig_col[1]<- hex_cols[1]
  sig_col[2:10]<-hex_cols[2]
  sig_col[11:50]<-hex_cols[3]
  sig_col[51:100]<-hex_cols[4]
  #sig_col[101:1000]<-"#FFFFFF"
#  sig_col[2:10] <- sig_col[10]
#  sig_col[11:50] <- sig_col[50]
#  sig_col[51:100] <- sig_col[100]


  return(sig_col)
}

generate_plot_ggplot_corrplot_handmade<- function(mat_p, mat_t,
                                                  mat_mean_diff = NULL,
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

  if (is.null(mat_mean_diff)){
    mat_mean_diff = mat_t * 0
  }
  df_mean_diff <- melt(mat_mean_diff)
  df$mean_diff <- df_mean_diff$value
  df$mean_bool <- df_mean_diff$value>0
  df$mean_disc <- unlist(lapply(df$mean_bool, as.numeric))
  #df$mean_discf <- as.factor(df$mean_disc)

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


  gdflt <<- dflt
  gdf <<- df

  #dfut<-df[df$value!=0,]
  #df$V2 <- factor(df$Var2, levels = df$V2)

  #cat(file = stderr(), paste0("length(g_regions()",length(g_regions()),"\n"))

  #diagonal_elements <- df$Var1==df$Var2
  p<-ggplot(df, aes(x = Var1, y = Var2, shape = mean_bool)) +
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
    scale_shape_manual(name = "Group Difference",
                       values = c(15, 16),
                       labels = c("G1<G2", "G1>G2"))+
    geom_text(data = dflt, aes(label= ifelse(value<0.05, sprintf("%0.3f", round(value,digits =3)),"")),
              hjust=0.5, vjust=0.5, size = 3,
              stat = "identity")+
    geom_abline(slope = 1, intercept = 0) +
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

  #cat(file = stderr(), paste0("length(regions",length(regions),"\n"))

  return(p)



}
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

