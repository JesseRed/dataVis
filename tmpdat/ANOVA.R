
library(ggplot2)
library(car)
library(MASS)
library(combinat)
library(stargazer)
filename = "df_main_spm_results_red.csv"
filename = "csv_all_komma.csv"
df <- read.csv(filename, header = TRUE, sep = "|", dec = ",")
df[,1]<-NULL

df$NIHSS1M2 <- df$NIHSS1-df$NIHSS2
df$NIHSS2M3 <- df$NIHSS2-df$NIHSS3
df$NIHSS1M3 <- df$NIHSS1-df$NIHSS3

df <- subset(df, Gruppe == 1)


# Fit the ANOVA model with interaction term
myanova <- aov(max_nSeq ~ NIHSS1 + NIHSS2+ NIHSS3 + Age + Gruppe + Gender1m2w + NIHSS1M2, data = df)
myanova <- aov(max_nSeq ~ NIHSS1M2 +  MOTRL + MOTRR + MOTLL+ DMNRL + DMNRR + DMNLL + MOT+ DMN+Age +Gender1m2w + Chads2Vasc + MoCA , data = df)
myanova <- aov(max_nSeq ~ NIHSS1M2 + NIHSS3 + MOTRL + DMNRL + DMNRR  + MOT+ DMN+Age +Gender1m2w + Chads2Vasc + MoCA , data = df)

#Anova(myanova, type="III")

# View the ANOVA table
myanova <- aov(BlockSlope ~ NIHSS1M2 + NIHSS1M3 + NIHSS2+ NIHSS3 + Age + Gruppe + Gender1m2w + NIHSS1M2, data = df)
summary(myanova)
myanova <- aov(max_nSeq ~ NIHSS1M2 + NIHSS1M3 + NIHSS2+ NIHSS3 + Age + Gruppe + Gender1m2w + NIHSS1M2, data = df)
summary(myanova)

add_vars = c("NIHSS1M2", "NIHSS1M3","NIHSS3",
             "MOTRL", "DMNRL", "DMNRR", "MOT", "DMN", "Gruppe", "Age", "Gender1m2w")
add_vars = c("Gender1m2w", "Age", "Chads2Vasc","MoCA", "NIHSS1",
             "NIHSS2", "NIHSS3", "mRs1", "mRs2", "mRs3", "Barthel", "Fugl_Meyer_A_D", "Fugl_Meyer_H", "Fugl_Meyer_J1", "Fugl_Meyer_J2", "MOTRL", "MOTRR", "MOTLL", "DMNRL", "DMNRR", "DMNLL", "MOT", "DMN", "NIHSS1M2", "NIHSS2M3", "NIHSS1M3")
add_vars = c("Gender1m2w", "Age", "Chads2Vasc","MoCA", "NIHSS1",
             "NIHSS2", "NIHSS3", "mRs1", "mRs2", "mRs3", "Barthel", "Fugl_Meyer_A_D", "Fugl_Meyer_H", "Fugl_Meyer_J1", "MOTRL", "MOTRR", "MOTLL", "DMNRL", "DMNRR", "DMNLL", "MOT", "DMN", "NIHSS1M2", "NIHSS2M3", "NIHSS1M3")
formula1 <- as.formula(paste0("max_nSeq ~ ", paste(add_vars, collapse = "+")))
model <- aov(formula1, data = df)
summary(model)

add_vars = c("Gender1m2w", "Age", "Chads2Vasc","MoCA", "NIHSS1M2",
             "NIHSS2", "NIHSS3", "mRs1", "mRs2", "mRs3", "Barthel", "Fugl_Meyer_A_D", "Fugl_Meyer_H", "Fugl_Meyer_J1", "MOTRL", "MOTRR", "MOTLL", "DMNRL", "DMNRR", "DMNLL", "MOT", "DMN")
formula1 <- as.formula(paste0("max_nSeq ~ ", paste(add_vars, collapse = "+")))
model <- aov(formula1, data = df)
summary(model)

add_vars = c("Gender1m2w", "Age", "Chads2Vasc","MoCA", "NIHSS1M2",
             "NIHSS2", "NIHSS3", "mRs1", "mRs2", "mRs3", "Fugl_Meyer_A_D", "Fugl_Meyer_H", "Fugl_Meyer_J1", "MOTRL", "MOTRR", "MOTLL", "DMNRL", "DMNRR", "DMNLL", "MOT", "DMN")
formula1 <- as.formula(paste0("max_nSeq ~ ", paste(add_vars, collapse = "+")))
model <- aov(formula1, data = df)
summary(model)



add_vars = c("Gender1m2w", "Age", "Chads2Vasc","MoCA", "NIHSS1M2",
             "NIHSS2", "NIHSS3", "mRs1", "mRs2", "mRs3", "Fugl_Meyer_J1", "MOTRL", "MOTRR", "MOTLL", "DMNRL", "DMNRR", "DMNLL", "MOT", "DMN")
formula1 <- as.formula(paste0("max_nSeq ~ ", paste(add_vars, collapse = "+")))
model <- aov(formula1, data = df)
summary(model)


add_vars = c("Gender1m2w", "Age", "Chads2Vasc","MoCA", "NIHSS1M2",
             "mRs1", "mRs2", "mRs3", "Fugl_Meyer_J1", "MOTRL", "MOTRR", "MOTLL", "DMNRL", "DMNRR", "DMNLL", "MOT", "DMN")
formula1 <- as.formula(paste0("max_nSeq ~ ", paste(add_vars, collapse = "+")))
model <- aov(formula1, data = df)
summary(model)




add_vars = c("Gender1m2w", "Age", "Chads2Vasc", "NIHSS1M2",
             "mRs1", "mRs2", "Fugl_Meyer_J1", "MOTRL", "MOTRR", "MOTLL", "DMNRL", "DMNRR", "DMNLL", "MOT", "DMN")
formula1 <- as.formula(paste0("max_nSeq ~ ", paste(add_vars, collapse = "+")))
model <- aov(formula1, data = df)
summary(model)



add_vars = c("Gender1m2w", "Age", "Chads2Vasc", "NIHSS1", "NIHSS2", "NIHSS3",
             "mRs1", "Fugl_Meyer_J1", "MOTRL", "MOTRR", "MOTLL", "DMNRL", "DMNRR", "DMNLL", "MOT", "DMN")
formula1 <- as.formula(paste0("max_nSeq ~ ", paste(add_vars, collapse = "+")))
model <- aov(formula1, data = df)
summary(model)

add_vars = c("MOT", "DMN", "MOTRL", "DMNRL", "DMNRR","Gender1m2w", "Age", "Chads2Vasc", "NIHSS1", "NIHSS2", "NIHSS3",
             "mRs1")
formula1 <- as.formula(paste0("max_nSeq ~ ", paste(add_vars, collapse = "+")))
model <- aov(formula1, data = df)
summary(model)


add_vars = c("MOT", "DMN", "MOTRL", "DMNRL", "DMNRR","Gender1m2w",  "NIHSS1", "NIHSS2", "NIHSS3",
             "mRs1","Age", "Chads2Vasc")
formula1 <- as.formula(paste0("max_nSeq ~ ", paste(add_vars, collapse = "+")))
model <- aov(formula1, data = df)
summary(model)


add_vars = c("MOT", "DMN", "MOTRL", "DMNRL", "DMNRR","Gender1m2w",  "NIHSS1", "NIHSS2", "NIHSS3",
             "mRs1","Age", "Chads2Vasc")
formula1 <- as.formula(paste0("max_nSeq ~ ", paste(add_vars, collapse = "+")))
model <- aov(formula1, data = df)
summary(model)


add_vars = c("MOT", "DMN", "MOTRL", "DMNRL", "DMNRR",  "NIHSS1", "NIHSS2", "NIHSS3",
             "mRs1","Age", "Chads2Vasc","Gender1m2w")
formula1 <- as.formula(paste0("max_nSeq ~ ", paste(add_vars, collapse = "+")))
model <- aov(formula1, data = df)
summary(model)
# Get the summary of the model
model_summary <- summary(model)

# Generate all possible combinations of the variables
perms1 <- permn(add_vars[1:6])
perms2 <- permn(add_vars[7:12])
perms3 <- permn(add_vars[9:12])

all_perms <- lapply(seq_along(perms1), function(i) c(perms1[[i]], perms2[[i]], perms3[[i]]))
all_perms <- lapply(seq_along(perms1), function(i) c(perms1[[i]], perms2[[i]]))

x <- 0
p_old = 1
for (p in all_perms) {
  #print(p)
  formula1 <- as.formula(paste0("max_nSeq ~ ", paste(p, collapse = "+")))
  model <- aov(formula1, data = df)
  model_summary <- summary(model)
  position_NIHSS1 <- which( p == "NIHSS1")
  p_value_NIHSS1 <- model_summary[[1]]$`Pr(>F)`[which( p == "NIHSS1")]
  if (p_value_NIHSS1<p_old){
    list_string <- paste(p, collapse = ", ")
    cat(paste(p_value_NIHSS1 , " <- " , list_string , " \n "))
    p_old <- p_value_NIHSS1
  }
  if (x>10000){
    break
  }
}







# perform initial ANOVA model with only intercept
model <- lm(max_nSeq ~ 1, data = df)
model <- aov(max_nSeq ~ NIHSS1M2, data = df)
add_vars = c("Gruppe", "Included", "Gruppe_old", "ohtp1chronic2helathyold3acute4helathyyoung", "Gender1m2w", "Age", "Chads2Vasc",
             "MoCA", "BSQ", "HADS_A", "HADS_D", "NHPT_Dom", "NHPT_NonDom", "NHPT_Rationichtdomdom", "NHPTratiokrankgesund", "DDB_TotalM", "DDB_RatioFehlerr", "NIHSS1",
             "NIHSS2", "NIHSS3", "mRs1", "mRs2", "mRs3", "Barthel", "Fugl_Meyer_A_D", "Fugl_Meyer_H", "Fugl_Meyer_J1", "Fugl_Meyer_J2", "MSTslopes_allID",
             "max_nSeq", "max_nSeq_BlockID", "MOTRL", "MOTRR", "MOTLL", "DMNRL", "DMNRR", "DMNLL", "MOT", "DMN", "NIHSS1M2", "NIHSS2M3", "NIHSS1M3")
add_vars = c("Gruppe", "Gender1m2w", "Age", "Chads2Vasc","MoCA", "NIHSS1", "NIHSS2", "NIHSS3", "Fugl_Meyer_A_D", "Fugl_Meyer_H",
             "MOTRL", "MOTRR", "MOTLL", "DMNRL", "DMNRR", "DMNLL", "MOT", "DMN", "NIHSS1M2", "NIHSS2M3", "NIHSS1M3")
add_vars = c("Gruppe", "Gender1m2w", "Age", "Chads2Vasc","MoCA",
             "MOTRL", "MOTRR", "MOTLL", "DMNRL", "DMNRR", "DMNLL", "MOT", "DMN", "NIHSS1M2", "NIHSS2M3", "NIHSS1M3")
add_vars = c("NIHSS1M2","NIHSS3", "Gruppe" , "Gender1m2w", "Age", "Chads2Vasc","MoCA",
             "MOTRL", "MOTRR", "MOTLL", "DMNRL", "DMNRR", "DMNLL", "MOT", "DMN", "DMNRR*DMN")

add_vars = c("NIHSS1M2", "NIHSS1M3","NIHSS3",
             "MOTRL", "DMNRL", "DMNRR", "MOT", "DMN", "Gruppe", "Age", "Gender1m2w")

indep_vars = paste(add_vars, collapse = "+")

formula <- as.formula(paste0("max_nSeq ~ ", paste(add_vars, collapse = "+")))
model <- aov(formula, data = df)
summary(model)


modelg1 <- aov(formula, data = dfg1)
summary(modelg1)

step_model <- stepAIC(model, trace = TRUE, direction= "both")
#step_model <- stepAIC(model, trace = TRUE, direction= "forward")
#step_model <- stepAIC(model, trace = TRUE, direction= "backward")
stargazer(model, step_model, type = "text")
summary(step_model)


model <- lm(max_nSeq ~ 1, data = df)

print(model)

# use stepwise Akaike information criterion (AIC) approach to add independent variables to model
while (TRUE) {
  current_AIC <- AIC(model)
  print(current_AIC)
  next_model <- NULL
  for (col in add_vars) {
    formula <- as.formula(paste0("max_nSeq ~ ", paste(c(names(model)[-1], col), collapse = "+")))
    print(formula)
    temp_model <- lm(formula, data = df)
    print(AIC(temp_model))
    if (is.null(next_model) | AIC(temp_model) < AIC(next_model)) {
      next_model <- temp_model
    }
  }
  if (AIC(next_model) < current_AIC) {
    model <- next_model
  } else {
    break
  }
}




# perform initial ANOVA model with only intercept
model <- lm(max_nSeq ~ 1, data = df)

while (TRUE) {
  current_AIC <- AIC(model)
  next_model <- NULL
  for (col in add_vars) {
    formula <- as.formula(paste0("max_nSeq ~ ", paste0(setNames(c(names(model)[-1], col), c(names(model))), collapse = "+")))
    print(formula)
    temp_model <- lm(formula, data = df)
    if (is.null(next_model) | AIC(temp_model) < AIC(next_model)) {
      next_model <- temp_model
    }
  }
  if (AIC(next_model) < current_AIC) {
    model <- next_model
  } else {
    break
  }
}

# view the final model
summary(model)


# use stepwise Akaike information criterion (AIC) approach to add independent variables to model
while (TRUE) {
  current_AIC <- AIC(model)
  next_model <- NULL
  for (col in names(df)[-1]) {
    formula <- as.formula(paste0("max_nSeq ~ ", paste0(names(df)[-1], collapse = "+"), "+", col))
    print(formula)
    temp_model <- lm(formula, data = df)
    if (is.null(next_model) | AIC(temp_model) < AIC(next_model)) {
      next_model <- temp_model
    }
  }
  if (AIC(next_model) < current_AIC) {
    model <- next_model
  } else {
    break
  }
}

# view the final model
summary(model)











filename = "beer_googles.csv"
df <- read.csv(filename, header = TRUE, sep = "|")


# Create the boxplot
ggplot(data = df, aes(x = alcohol, y = attractiveness)) +
  geom_boxplot() +
  facet_wrap(~ gender)

leveneTest(df$attractiveness, df$gender, center = median)
leveneTest(df$attractiveness, df$alcohol, center = median)
leveneTest(df$attractiveness, interaction(df$alcohol, df$gender), center = median)


df$gender <- factor(df$gender, levels = c("Female", "Male"))
df$alcohol <- factor(df$alcohol, levels =  c("None", "2 Pints", "4 Pints"))

contrasts(df$alcohol) <-cbind(c(-2,1,1), c(0,-1,1))
contrasts(df$gender)  <-c(-1,1)


df$alcohol

# Fit the ANOVA model with interaction term
myanova <- aov(attractiveness ~ gender + alcohol + alcohol*gender, data = df)

Anova(myanova, type="III")

# View the ANOVA table
summary(myanova)

