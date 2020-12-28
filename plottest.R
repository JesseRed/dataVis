
library(reshape2)
library(ggplot2)
library(tidyverse)

mat_p <- matrix(runif(196,0,0.1),nrow=14)
mat_t <- matrix(runif(196,0,10),nrow=14)

names <- c("frontal_right_A1",
                    "frontal_right_B2",
                    "frontal_right_A3",
                    "frontal_right_A4",
                    "frontal_right_A5",
                    "frontal_right_A6",
                    "frontal_right_A7",
                    "frontal_right_A8",
                    "frontal_right_A9",
                    "frontal_right_A10",
                    "frontal_right_A11",
                    "frontal_right_A12",
                    "frontal_right_A13",
                    "frontal_right_A14"
)


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






#diagonal_elements <- df$Var1==df$Var2

ggplot(df, aes(x = Var1, y = Var2)) +
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
  geom_point(data = dflt, aes(x = dflt$Var1, y= dflt$Var2,colour = dflt$sig_level),
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
  scale_y_continuous(breaks=seq(1, length(names), 1), labels = names, minor_breaks = NULL) +
  scale_x_continuous(breaks=seq(1, length(names), 1), labels = names, minor_breaks = NULL)




 # axis(1, at=1:length(names), labels = letters[1:length(names)])
#gg<-gg + scale_x_continuous(breaks=seq(1, length(names), 1), labels = names)
#gg +
#text(seq(1, length(names), by=1), par("usr")[3] - 0.2, labels = names, srt = 45, pos =1, xpd = TRUE)
  #scale_y_discrete(limits=names)

 # guides(color = TRUE)


# p + geom_point(aes(colour = p$data$siglevel),
#                size = 12,
#                alpha = 0.8,
#                show.legend = TRUE) +
#   scale_color_manual(name = "pvalue",
#                      values = c("red", "yellow", "green", "black", "black", "black"))+
#   geom_text(aes(label= ifelse(pvalue<0.05, sprintf("%0.3f", round(pvalue,digits =3)),"")),
#             hjust=0.5, vjust=0.5, size = 3 )
  #guides(color = TRUE)



# colnames(mat_p) = names
# rownames(mat_p) = names
# ggcorrplot(mat_p, hc.order = FALSE,
#            type = "lower",
#            lab = FALSE,
#            lab_size = 3, insig = 'blank',
#            method="circle", p.mat = mat_p, sig.level = 0.05,
#            #colors = c("tomato2", "white", "springgreen3"),
#            colors = c("white"),
#            title="Significant values ",
#            #show.legend = FALSE,
#            ggtheme=theme_bw)

#
# tmp <- as.character(p$data$pvalue)
# tmp[p$data$pvalue<3]="pval >0.5"
# tmp[p$data$pvalue<0.05]="0.01 < pval <= 0.05"
# tmp[p$data$pvalue<0.01]="0.001 < pval <= 0.01"
# tmp[p$data$pvalue<0.001]="0.0001 < pval <= 0.001"
# tmp[p$data$pvalue<0.0001]="pval <= 0.0001"
# p$data$siglevel = tmp
#
#
#
#
# p + geom_point(aes(colour = p$data$siglevel),
#                size = 12,
#                alpha = 0.8,
#                show.legend = TRUE) +
#   scale_color_manual(name = "pvalue",
#           values = c("red", "yellow", "green", "black", "black", "black"))+
#  geom_text(aes(label= ifelse(pvalue<0.05, sprintf("%0.3f", round(pvalue,digits =3)),"")),
#           hjust=0.5, vjust=0.5, size = 3 ) +
#   guides(color = TRUE)
