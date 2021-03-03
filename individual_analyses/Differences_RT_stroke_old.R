library(data.table) # importiert das Paket in welchem "fread" enthalten ist
 df <- fread("C:/Users/jenny/Desktop/all_reaction_times.csv")
ids <- unique(df[, 'ID'])
mrtc1 <- c()
mrtc2 <- c()
mrtc3 <- c()
mid <- c()
mGruppe <- c()

xmrt <- c()
xmid <- c()
xGruppe <- c()
xcue <- c()

get_cue_mean_rt <- function(df, id_name, mycue, my_min, my_max) {
  df_subj <- df[ID == id_name,]
  df_cue <- df_subj[cue == mycue,]
  df_cue_filtered <- df_cue[rt >= my_min,]
  df_cue_filtered <- df_cue_filtered[rt <= my_max,]
  mean_cue <- mean(df_cue_filtered[, rt], rm.na = TRUE)
}

for (s in 1:nrow(ids)) {
  id_name <- ids[s, ID]
  rtc1 <- get_cue_mean_rt(df, id_name, 1, 100, 2000)
  rtc2 <- get_cue_mean_rt(df, id_name, 2, 100, 2000)
  rtc3 <- get_cue_mean_rt(df, id_name, 3, 100, 2000)
  group <- df[ID == ids[s], group][1]

  mrtc1 <- c(mrtc1, rtc1)
  mrtc2 <- c(mrtc2, rtc2)
  mrtc3 <- c(mrtc3, rtc3)
  mid <- c(mid, id_name)
  mGruppe <- c(mGruppe, group)

  xmrt <- c(xmrt, rtc1, rtc2, rtc3)
  xmid <- c(xmid, id_name, id_name, id_name)
  xGruppe <- c(xGruppe, group, group, group)
  xcue <- c(xcue, 1, 2, 3)
}

df_new <- na.omit(data.frame(mid, mGruppe, mrtc1, mrtc2, mrtc3))
# berechne noch die Differenzen
df_new$mrtc3m2 <- df_new$mrtc3 - df_new$mrtc2
df_new$mrtc3m1 <- df_new$mrtc3 - df_new$mrtc1
df_new$mrtc2m1 <- df_new$mrtc2 - df_new$mrtc1
# independent t-test
newModel <- t.test(mrtc3m2 ~ mGruppe, data = df_new)
print(newModel)

# independent t-test
newModel <- t.test(mrtc3m1 ~ mGruppe, data = df_new)
print(newModel)

# independent t-test
newModel <- t.test(mrtc2m1 ~ mGruppe, data = df_new)
print(newModel)

# independent 2-group Mann-Whitney U Test
newModel <- wilcox.test(mrtc3m2 ~ mGruppe, data = df_new)
print(newModel)

# shapiro test
newModel <- shapiro.test (mrtc3m2, data = df_new)
print(newModel)

