# column group was renamed to Gruppe
df = fread("G:/Programming/dataVisdata/data/Behavioral/Jenny-Reaktionszeiten-2021-01-07.csv")
ids = unique(df[,'ID'])
mrtc1   = c()
mrtc2   = c()
mrtc3   = c()
mid     = c()
mGruppe = c()

get_cue_mean_rt<- function(df, id_name, mycue, my_min, my_max){
  df_subj <- df[ID==id_name,]
  df_cue = df_subj[cue ==mycue,]
  df_cue_filtered <- df_cue[rt>=my_min,]
  df_cue_filtered <- df_cue_filtered[rt<=my_max,]
  mean_cue = mean(df_cue_filtered[,rt], rm.na = TRUE)
}

for (s in 1:nrow(ids)){
  id_name = ids[s, ID]
  mrtc1 = c(mrtc1, get_cue_mean_rt(df, id_name, 1,60, 1100))
  mrtc2 = c(mrtc2, get_cue_mean_rt(df, id_name, 2,60, 1100))
  mrtc3 = c(mrtc3, get_cue_mean_rt(df, id_name, 3,60, 1100))
  mid = c(mid, id_name)
  mGruppe = c(mGruppe, df[ID==ids[s],Gruppe][1])
}

df_new = na.omit(data.frame(mid, mGruppe,mrtc1, mrtc2, mrtc3))
# berechne noch die Differenzen
df_new$mrtc1m2 <- df_new$mrtc1 - df_new$mrtc2
df_new$mrtc1m3 <- df_new$mrtc1 - df_new$mrtc3
df_new$mrtc2m3 <- df_new$mrtc2 - df_new$mrtc3

# independent t-test
newModel <- t.test(mrtc1m2 ~ mGruppe, data = df_new)
print(newModel)

# independent t-test
newModel <- t.test(mrtc1m3 ~ mGruppe, data = df_new)
print(newModel)

# independent t-test
newModel <- t.test(mrtc2m3 ~ mGruppe, data = df_new)
print(newModel)

