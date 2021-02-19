
library(data.table)
library(reshape2)
D <- read.csv("./tests/testthat/data/Behavioral/Tipp10_Auswertung3.csv", header = TRUE, sep = ";", check.names = FALSE)


#Christianes Fragen
# Gibt es einen linearen Trend in der Anzahl der richtigen Anschläge über die Tage hinweg
#   (jeweils im Vergleich zu Lektion 1 = gain)
# Sinkt die Fehlerquote über die Tage hinweg ab?
# Gibt es ein offline learning i.S.  einer Abnahme der Fehlerquote bzw. Zunahem der richtigen Anschläge von der jeweils ersten Lektion am Tage im Vorgleich zur letzten am Vortag
#ist die Anzahl der richtigen bzw. die Fehlerquote jeweils besser nach einem Tag training (Vergleich Anfangs- zu Endlektion jeweils am Tag).



# Gibt es einen linearen Trend in der Anzahl der richtigen Anschläge über die Tage hinweg
#   (jeweils im Vergleich zu Lektion 1 = gain)
#ReihenfolgeImTag




# ID
# ToInclude                 ... Ob in die Testung einzubeziehen
# Lektion                   ... Die verwendete Lektion als Test
# ReihenfolgeImTag.X        ... der wievielte Test innerhalb des TAges fuer den Test X (1-16) da 16 Tests
# Start0Middel1Abschluss2.X ... Angabe fuer die Stellung des Tests innerhalb des Tages X
#                               0 wenn der Test innerhalb des Tages am Start war
#                               1 wenn der TEst in der Mitte der Tageslektionen war
#                               2 wenn der Test als letztes am Tag war
# Tag.X                     ... An welchem Tag wurde der Test X durchgefuert
# Aufgabe.X                 ... Eine String Beschreibung bei welcher Lektion getestet wurde % fuer die aktuelle Analyse eher uninterssant
# Dauer.X                   ... Dauer des Tests X
# Zeichen.X                 ... Gepresste Zeichen in der Lektion X
# Amin.X                    ... Anschlaege pro MInute in der Lektion X
# Tippfehler.X              ... Tippfehler in der Lektion X
# Fehlerquote.X             ... Fehlerquote in der Lektion X
# Punkte.X                  ... Punkte in der Lektion X
# Anzahl_Richtige.X         ... Anzahl der richtigen Anschlaege in der Lektion X
#

#
# # Select column whose name starts with "Petal"
# my_data %>% select(starts_with("Petal"))
#
# # Select column whose name ends with "Width"
# my_data %>% select(ends_with("Width"))
#
# # Select columns whose names contains "etal"
# my_data %>% select(contains("etal"))
#
# # Select columns whose name maches a regular expression
# my_data %>% select(matches(".t."))
#
# # selects variables provided in a character vector.
# my_data %>% select(one_of(c("Sepal.Length", "Petal.Length")))
measurement_seperator = '__'
get_columnnames_by_pattern<- function(df, pattern){
  cn = colnames(df)
  ret = grep(pattern,cn, value = TRUE)
  return(ret)
}

#select(D, starts_with("Tippfehler"))


outcome_var_pattern = "Tippfehler"
outcome_var_pattern = "Anzahl_Richtige"


df <- melt(data = D, id.vars = c("ID","ToInclude","Lektion"),
           measure.vars = get_columnnames_by_pattern(D, outcome_var_pattern)
           )

# exlude those not to include
df <- subset(df, ToInclude==1)

df <- cbind(df, colsplit(df$variable, "__", names= c(outcome_var_pattern, "Test_number")))


newModel <- lm(Test_number ~ value, data = df, na.action = na.exclude)

summary(newModel)


new_id <- c()
new_val<- c()
new_varname <-c()
new_testnumber<-c()
outcome_var_pattern_to_add = "Start0Middel1Abschluss2"
cn = colnames(df)
#Offline learning
for (row in 1:nrow(df)){
    test_num = df$Test_number[row]
    myid = df$ID[row]
    mycolname = paste0(outcome_var_pattern_to_add,"__", test_num)
    # was steht in der Start0Middel1Abchluss2__X des entsprechenden Tests in der Orginaltabelle?
    sma = D[which(D$ID == myid), mycolname]
    if (sma == 2){
        Tag_end_value = df[row, "value"]
    }
    # wenn es sich um den ersten Test des Tages handelt und es nicht der erste Test ist
    if (sma==0 && test_num>1){
      Tag_start_value = df[row, "value"]
      offline_learning = Tag_start_value - Tag_end_value
      new_id        <- c(new_id, myid)
      new_val       <- c(new_val, offline_learning)
      new_varname   <- c(new_varname, df[row, "var_names"])
      new_testnumber<- c(new_testnumber, test_num)
    }
    # wir wollen offline learning d.h. wir wollen v=0 und ziehen davon v2 des vortages ab

}
new_df <- data.frame(new_id, new_val, new_varname, new_testnumber)

