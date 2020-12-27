
map2color<-function(x,pal,limits=NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}


map2color4threshold<-function(x,pal,threshold = 0.8, limits=NULL, invert_col_map = TRUE){
  xl = x
  if (invert_col_map){
    xl = xl*-1
  }
  #cat(file = stderr(), x)
  # cat(file = stderr(), paste0("x = ", x, "\n"))
  # cat(file = stderr(), paste0("xl = ", xl, "\n"))
  #cat(file = stderr(), paste0("pal = ", pal, "\n"))

  if(is.null(limits)) limits=range(xl)
  interval = findInterval(xl,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)
  # cat(file = stderr(), "\n")
  # cat(file = stderr(),  interval)
  # cat(file = stderr(), "\n")

  colstrings = pal[interval]
  for (i in 1:length(colstrings)){

    if (x[i]<threshold){
      colstrings[i] = add.alpha(colstrings[i], 0.0)
    }
    else{
      colstrings[i] = add.alpha(colstrings[i], 0.6)
    }
  }
  #cat(file = stderr(), "map2color4threshold")
  return(colstrings)
}


# map2color(0:11,rainbow(200),limits=c(1,10))
# [1] "#FF0000FF" "#FF0000FF" "#FFA800FF" "#ADFF00FF" "#05FF00FF" "#00FFA3FF"
# [7] "#00ABFFFF" "#0003FFFF" "#A600FFFF" "#FF00B0FF" "#FF0008FF" "#FF0008FF"
# map2color(0:11,rainbow(200))
# [1] "#FF0000FF" "#FF8A00FF" "#EBFF00FF" "#61FF00FF" "#00FF29FF" "#00FFB3FF"
# [7] "#00BAFFFF" "#0030FFFF" "#5900FFFF" "#E300FFFF" "#FF0091FF" "#FF0008FF"
#
#
# mypal <- colorRampPalette( c( "blue", "red" ) )( 5 )
# x <- c( 1, 9, 8.5, 3, 3.4, 6.2 )
# map2color(x,mypal)
# "#0000FF" "#FF0000" "#FF0000" "#3F00BF" "#3F00BF" "#BF003F"
