cor_tbl(extra.mat = list(mat = mat_p)) %>%
  quickcor(mapping = aes(fill = mat_p)) +
  #geom_square(aes(upper_fill = mat_p, upper_r0 =mat_p)) +
  #geom_abline(slope = -1, intercept = 12)

# cor_tbl(extra.mat = list(mat = mat)) %>%
#   +     quickcor(mapping = aes(fill = mat)) + geom_colour()
#
#
# options(scipen=999)  # turn off scientific notation like 1e+06
# library(ggplot2)
# data("midwest", package = "ggplot2")  # load the data
# # midwest <- read.csv("http://goo.gl/G1K41K") # alt source
#
# # Init Ggplot
# x<-ggplot(midwest, aes(x=area, y=poptotal))  #
# x


# funktioniert

mat_p <- matrix(runif(100),nrow=10)




df <- as.data.frame(mat_p)


# funktioniert
quickcor(mtcars, cor.test = TRUE) +
  geom_square(data = get_data(type = "lower", show.diag = FALSE)) +
  geom_mark(data = get_data(type = "upper", show.diag = FALSE), size = 2.5) +
  geom_abline(slope = -1, intercept = 12)

# funktioniert

cor_tbl(extra.mat = list(mat = mat_p)) %>%
  +   quickcor(mapping = aes(fill = mat_p)) +
  geom_square(data = get_data(type = "lower", show.diag = FALSE)) +
  geom_mark(data = get_data(type = "upper", show.diag = FALSE), size = 2.5) +
  geom_abline(slope = -1, intercept = 12)



cor_tbl(extra.mat = list(mat = mat_p)) %>%
  +   ggcor(mapping = aes(fill=mat_p))


# funktioniert
cor_tbl(extra.mat = list(mat = mat_p)) %>%
  +   quickcor(mapping = aes(fill = mat_p))
