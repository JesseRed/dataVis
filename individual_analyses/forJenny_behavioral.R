# column group was renamed to Gruppe
df = fread("G:/Programming/dataVisdata/data/Behavioral/Jenny-Reaktionszeiten-2021-01-07.csv")
ids = unique(df[,'ID'])
mrtc1   = c()
mrtc2   = c()
mrtc3   = c()
mid     = c()
mGruppe = c()

xmrt = c()
xmid = c()
xGruppe = c()
xcue = c()

get_cue_mean_rt<- function(df, id_name, mycue, my_min, my_max){
  df_subj <- df[ID==id_name,]
  df_cue = df_subj[cue ==mycue,]
  df_cue_filtered <- df_cue[rt>=my_min,]
  df_cue_filtered <- df_cue_filtered[rt<=my_max,]
  mean_cue = mean(df_cue_filtered[,rt], rm.na = TRUE)
}

for (s in 1:nrow(ids)){
  id_name = ids[s, ID]
  rtc1 = get_cue_mean_rt(df, id_name, 1,70, 1100)
  rtc2 = get_cue_mean_rt(df, id_name, 2,70, 1100)
  rtc3 = get_cue_mean_rt(df, id_name, 3,70, 1100)
  group = df[ID==ids[s],Gruppe][1]
  
  mrtc1 = c(mrtc1, rtc1)
  mrtc2 = c(mrtc2, rtc2)
  mrtc3 = c(mrtc3, rtc3)
  mid = c(mid, id_name)
  mGruppe = c(mGruppe, group)
  
  xmrt = c(xmrt, rtc1, rtc2, rtc3)
  xmid = c(xmid, id_name, id_name, id_name)
  xGruppe = c(xGruppe, group, group, group)
  xcue = c(xcue, 1,2,3)
}

df_new = na.omit(data.frame(mid, mGruppe,mrtc1, mrtc2, mrtc3))
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


#two way independent ANOVA
dfm = na.omit(data.frame(xmrt, xmid, xGruppe, xcue))
dfm$xcue<-factor(dfm$xcue)
dfm$xGruppe<-factor(dfm$xGruppe)
contrasts(dfm$xcue)<-cbind(c(-2,1,1), c(0,-1,1))
contrasts(dfm$xGruppe)<-c(-1,1)
aovModel <- aov(xmrt ~ xGruppe + xcue + xGruppe:xcue, data = dfm)
Anova(aovModel, type = "III")
summary.lm(aovModel)

df$cue<-factor(df$cue)
df$Gruppe<-factor(df$Gruppe)
contrasts(df$cue)<-cbind(c(-2,1,1), c(0,-1,1))
contrasts(df$Gruppe)<-c(-1,1)
aovModel <- aov(rt ~ Gruppe + cue + Gruppe:cue, data = df)
Anova(aovModel, type = "III")
summary.lm(aovModel)


 
 