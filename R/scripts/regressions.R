########################################################################
# Project Seminar Applied Data Analysis
#
# R Script "regressions"
# by: D. Kalmbach
# Date of this version: August, 2024
########################################################################

# Libraries
library(here) #this is your project directory (where the .Rproj file is located)
library(readr)
#library(plyr) # !conflict with package here()
library(dplyr)
library(modelsummary)
options("modelsummary_format_numeric_latex" = "plain")
library(cartogramR)
library(mgcViz) #gam

# helper functions
source(here("scripts", "helpers.R"))

# Load the data
# =============
load(here("data","df.Rda")) # to load df
load(here("data","Industry.Rda")) # to load df

# Variablenbenennung lt. Paper
cm_tex <- c('Skills' = 'Skills 1790s',
            'Wage' = 'Wage 1760s',
            'Potential' = 'Mkt Potential 1750',
            'Coal_Distance'="Distance to Coal",
            'Tax_1707'='Land Tax 1707',
            'Water_Flow'='Water Power', 
            'Literate'='Literacy c1800',
            'Density'='Pop Density 1700',
            'Banks'='County Banks 1796',
            'Lawyers'='Lawyers 1730',
            'Booksellers'='Booksellers 1761',
            'X'='Long',
            'Y'='Lat')

# 1 #
###############################################
# Tabelle 1 aus Papier replizieren ✅
###############################################

tex1=gam(Textiles_1831~Skills+Wage+Potential+SHP+s(X,Y),data=df) # R2=0.684, AIC=131.1
tex2=gam(Textiles_1831~Skills+Wage+Potential+Coal_Distance+SHP+s(X,Y),data=df) # R2=0.695, AIC=130.6
tex3=gam(Textiles_1831~Skills+Wage+Potential+Water_Flow+SHP+s(X,Y),data=df) # R2=0.676, AIC=132.9
tex5=gam(Textiles_1851~Skills+Wage+Potential+STF+s(X,Y),data=df) # R2=0.813, AIC=88.7
tex6=gam(Textiles_1851~Skills+Wage+Potential+Coal_Distance+STF+s(X,Y),data=df) # R2=0.810, AIC=89.5
tex7=gam(Textiles_1851~Skills+Wage+Potential+Water_Flow+STF+s(X,Y),data=df) # R2=0.865, AIC=74.3

models_tex=list(tex1,tex2,tex3,tex5,tex6,tex7)

modelsummary(models_tex,
             parametric=T,
             coef_map = cm_tex,
             #output="latex",
             title="Determinants of Textile Employment",
             gof_omit = "^(?!R2|Num|AI|s\\()"
)
# this modelsummary with p values and s.e.
modelsummary(models_tex,
             parametric=T,
             coef_map = cm_tex,
             #output="latex",
             title="Determinants of Textile Employment without Outliers",
             statistic = c("s.e. = {std.error}",
                           "p = {p.value}"),
             gof_omit = "^(?!R2|Num|AI|s\\()"
)

# nochmal auf andere Art (dafür mit Intercepts)
a<-summary(mgcv::gam(formula=Textiles_1831~Skills+Wage+Potential+SHP+s(X,Y), 
                  data = df, method = "GCV.Cp", gamma = 1))
# print intercept (incl. coeff)
a$p.coeff + a$p.coeff

# Print Regression Output with *, **, *** for significances
tex1_sum <- summary(mgcv::gam(formula=Textiles_1831~Skills+Wage+Potential+SHP+s(X,Y), data = df))
tex2_sum <- summary(mgcv::gam(Textiles_1831~Skills+Wage+Potential+Coal_Distance+SHP+s(X,Y),data=df))
tex3_sum <- summary(mgcv::gam(Textiles_1831~Skills+Wage+Potential+Water_Flow+SHP+s(X,Y),data=df))
tex5_sum <- summary(mgcv::gam(Textiles_1851~Skills+Wage+Potential+STF+s(X,Y),data=df))
tex6_sum <- summary(mgcv::gam(Textiles_1851~Skills+Wage+Potential+Coal_Distance+STF+s(X,Y),data=df))
tex7_sum <- summary(mgcv::gam(Textiles_1851~Skills+Wage+Potential+Water_Flow+STF+s(X,Y),data=df))
tex1_sum
tex2_sum
tex3_sum
tex5_sum
tex6_sum
tex7_sum
              

###############################################
# Tabelle 1 aus Papier ohne Outliers replizieren ✅
###############################################
# 1831 
######

# remove outliers for Textiles_1831
x <- remove_outlier(df, c('Textiles_1831_abs'))
# remove outliers for Wage
x <- remove_outlier(x, c('wage.1767')) 
# remove outliers for Skills
x <- remove_outlier(x, c('Skills_exp'))
# remove outliers for Potential
x <- remove_outlier(x, c('Potential_exp'))
# Hinweis: remove_outlier() und detect_outlier() sind in helpers.R definiert

# run regression for 1831 without outliers
tex1_no_outliers=gam(Textiles_1831~Skills+Wage+Potential+SHP+s(X,Y),data=x)
tex2_no_outliers=gam(Textiles_1831~Skills+Wage+Potential+Coal_Distance+SHP+s(X,Y),data=x)
tex3_no_outliers=gam(Textiles_1831~Skills+Wage+Potential+Water_Flow+SHP+s(X,Y),data=x)

# 1851 
######

# remove outliers for Textiles_1851
x <- remove_outlier(df, c('Textiles_1851_abs'))
# remove outliers for Wage
x <- remove_outlier(x, c('wage.1767')) 
# remove outliers for Skills
x <- remove_outlier(x, c('Skills_exp'))
# remove outliers for Potential
x <- remove_outlier(x, c('Potential_exp'))

# run regression #5 without outliers
tex5_no_outliers=gam(Textiles_1851~Skills+Wage+Potential+STF+s(X,Y),data=x)
tex6_no_outliers=gam(Textiles_1851~Skills+Wage+Potential+Coal_Distance+STF+s(X,Y),data=x)
tex7_no_outliers=gam(Textiles_1851~Skills+Wage+Potential+Water_Flow+STF+s(X,Y),data=x)

# run regressions
models_tex_no_outliers=list(tex1_no_outliers,
                            tex2_no_outliers,
                            tex3_no_outliers,
                            tex5_no_outliers,
                            tex6_no_outliers,
                            tex7_no_outliers)

modelsummary(models_tex_no_outliers,
             parametric=T,
             coef_map = cm_tex,
             #output="latex",
             title="Determinants of Textile Employment without Outliers",
             gof_omit = "^(?!R2|Num|AI|s\\()"
)
# this modelsummary with p values and s.e.
modelsummary(models_tex_no_outliers,
             parametric=T,
             coef_map = cm_tex,
             #output="latex",
             title="Determinants of Textile Employment without Outliers",
             statistic = c("s.e. = {std.error}",
                           "p = {p.value}"),
             gof_omit = "^(?!R2|Num|AI|s\\()"
)

# Plot Geo Splines
plot(tex1)
plot(tex2)
plot(tex3)
plot(tex5) 
plot(tex7) 
# with residuals
plot(tex7, residuals=TRUE, pch=1, cex=1) 

###############################################
# ENDE  Tabelle 1 replizieren ⬆️
###############################################



###############################################
# LOGISTISCHE REGRESSION ✅
###############################################
# a) without Geodaten

# 1831
df$industr_1831 <- ifelse(df$Textiles_1831_exp > 5, yes = 1, no = 0)
# label industr_1831 1="yes", 0="no"
df$industr_1831 <- factor(df$industr_1831, levels = c(0, 1), labels = c("no", "yes"))
# table of df$industr_1831
table(df$industr_1831)

log_model <- glm(industr_1831 ~ Skills + Wage + Potential + Water_Flow + STF, data = df, family = "binomial")
summary(log_model)

# 1851
df$industr_1851 <- ifelse(df$Textiles_1851_exp > 5, yes = 1, no = 0)
df$industr_1851 <- factor(df$industr_1851, levels = c(0, 1), labels = c("no", "yes"))
table(df$industr_1851)

log_model <- glm(industr_1851 ~ Skills + Wage + Potential + Water_Flow + STF, data = df, family = "binomial")
summary(log_model)

# b) with Geodaten

# Logitgam
# 1831
logitgam1=gam(I(Textiles_1831_exp>5)~Skills+Wage+Potential+Coal_Distance+STF+s(X,Y),data=df)
plot(logitgam1,se=T)
summary(logitgam1)
# 1851
logitgam1=gam(I(Textiles_1851_exp>5)~Skills+Wage+Potential+Coal_Distance+STF+s(X,Y),data=df)
plot(logitgam1,se=T)
summary(logitgam1)
###############################################
# ENDE  Logistic Regression ⬆️
###############################################


################################
# Regressionen ohne Geodaten  ️✍️
################################
# Regr. aus Tabelle 1 ohne Geodaten 
################################

# run regression table1 without s(x,y)
lm1=lm(Textiles_1831~Skills+Wage+Potential+SHP,data=df) # R2=0.655, AIC=131.4
lm2=lm(Textiles_1831~Skills+Wage+Potential+Coal_Distance+SHP,data=df) # R2=0.647, AIC=133.2
lm3=lm(Textiles_1831~Skills+Wage+Potential+Water_Flow+SHP,data=df) # R2=0.652, AIC=132.5
lm5=lm(Textiles_1851~Skills+Wage+Potential+STF,data=df) # R2=0.613, AIC=110.8
lm6=lm(Textiles_1851~Skills+Wage+Potential+Coal_Distance+STF,data=df) # R2=0.603, AIC=112.6
lm7=lm(Textiles_1851~Skills+Wage+Potential+Water_Flow+STF,data=df) # R2=0.602, AIC=112.7

models_lm=list(lm1,lm2,lm3,lm5,lm6,lm7)

modelsummary(models_lm,
             parametric=T,
             coef_map = cm_tex,
             #output="latex",
             title="Determinants of Textile Employment NEW",
             gof_omit = "^(?!R2|Num|AI|s\\()"
)

#alternative way to print the summary statistics
for (i in 1:length(models_lm)) {
  print(summary(models_lm[[i]]))
}


# Regressions from the paper
tex1=gam(Textiles_1831~Skills+Wage+Potential+SHP+s(X,Y),data=Industry)
tex2=gam(Textiles_1831~Skills+Wage+Potential+Coal_Distance+SHP+s(X,Y),data=Industry)
tex3=gam(Textiles_1831~Skills+Wage+Potential+Water_Flow+SHP+s(X,Y),data=Industry)
tex5=gam(Textiles_1851~Skills+Wage+Potential+STF+s(X,Y),data=Industry)
tex6=gam(Textiles_1851~Skills+Wage+Potential+Coal_Distance+STF+s(X,Y),data=Industry)
tex7=gam(Textiles_1851~Skills+Wage+Potential+Water_Flow+STF+s(X,Y),data=Industry)
models_tex=list(tex1,tex2,tex3,tex5,tex6,tex7)

plot(tex5)
plot(tex6)
plot(tex7)

modelsummary(models_tex) # auch mse

modelsummary(models_tex,
             parametric=T,
             coef_map = cm_tex,
             #output="latex",
             title="Regression mit t- und p-Werten. Abh. Variable: Textilbeschäftigung 1831 (1-3) und 1851 (4-6)",
             statistic = c(
               "t = {statistic}",
               "p = {p.value}"), 
             gof_omit = "^(?!R2|Num|AI|s\\()"
)
modelsummary(models_tex,
             parametric=T,
             coef_map = cm_tex,
             #output="latex",
             title="Determinants of Textile Employment 1851",
             statistic = c("s.e. = {std.error}",
                           "p = {p.value}"),
             gof_omit = "^(?!R2|Num|AI|s\\()"
)

modelsummary(models_tex,
             parametric=T,
             coef_map = cm_tex,
             #output="latex",
             title="Determinants of Textile Employment",
             statistic = c("s.e. = {std.error}",
                           "p = {p.value}"),
             gof_omit = "^(?!R2|Num|AI|s\\()"
)

# Regressions without geo spline
tex1_nogeo=lm(Textiles_1831~Skills+Wage+Potential+SHP,data=Industry)
tex2_nogeo=lm(Textiles_1831~Skills+Wage+Potential+Coal_Distance+SHP,data=Industry)
tex3_nogeo=lm(Textiles_1831~Skills+Wage+Potential+Water_Flow+SHP,data=Industry)
tex5_nogeo=lm(Textiles_1851~Skills+Wage+Potential+STF,data=Industry)
tex6_nogeo=lm(Textiles_1851~Skills+Wage+Potential+Coal_Distance+STF,data=Industry)
tex7_nogeo=lm(Textiles_1851~Skills+Wage+Potential+Water_Flow+STF,data=Industry)
models_tex_nogeo=list(tex1_nogeo,tex2_nogeo,tex3_nogeo,tex5_nogeo,tex6_nogeo,tex7_nogeo)

modelsummary(models_tex_nogeo,
             parametric=T,
             coef_map = cm_tex,
             #output="latex",
             statistic = c("s.e. = {std.error}",
                           "p = {p.value}"),
             title="Regression ohne Geodaten",
             gof_omit = "^(?!R2|Num|AI|s\\()"
)

# run regression table1 with X and Y
new7_geo = lm(Textiles_1851~Skills+Wage+Potential+Water_Flow+STF+X+Y,data=Industry)
models_tex_new=list(new7_geo)

modelsummary(models_tex_new,
             parametric=T,
             coef_map = cm_tex,
             #output="latex",
             title="Determinants of Textile Employment with Longitude and Latitude",
             gof_omit = "^(?!R2|Num|AI|s\\()"
)
summary(new7_geo) # R2 = 0.6117, X & Y not sign.

#Industry <- subset(Industry, select = -c(long,lat)) #delete long, lat

# simple linear regression
simp <- lm(Textiles_1851 ~ Textiles_1831, data = Industry)
summary(simp)
plot(simp) #residual vs fits plot
plot(Industry$Textiles_1831, Industry$Textiles_1851)
abline(simp)
# das gleiche als GAM
gimp <- gam(Textiles_1851 ~ Textiles_1831 + s(X,Y), data = Industry)
summary(gimp)
plot(gimp)
simp$residuals
gam.check(gimp)

#heatmap of counties according to abweichung von der regression linie -> passt das mit spline zusammen?
carto_1830=cartogramR(england,"agg_inc_1830")
carto_1760=cartogramR(england,"agg_inc_1760")
ggplot(data=carto_1830$cartogram)+
  geom_sf(aes(fill=abs(simp$residuals)),color="black")+ 
  theme_bw()+map_theme + 
  theme(legend.title=element_blank())+
  #scale_fill_grey(start = 0.9, end = 0.2,na.value = "white")+
  labs(title="Absolute Residuals of Textiles_1851 ~ Textiles_1831")

#########################


########################
# CROSS-VAL GAM 🧐🍮️⬇️
########################
CVgam <-
  function (formula, data, nfold = 5, debug.level = 0, method = "GCV.Cp",
            printit = TRUE, cvparts = NULL, gamma = 1, seed = 29)
  {
    if (is.null(cvparts)) {
      set.seed(seed)
      cvparts <- sample(1:nfold, nrow(data), replace = TRUE) #Reihe von Zahlen von 1 bis nfold, nrow(data) mal
    }
    folds <- unique(cvparts) # z.B. 5 4 2 3 1
    khat <- hat <- numeric(nrow(data))
    scale.gam <- summary(mgcv::gam(formula, data = data, method = method))$scale
    for (i in folds) {
      trainrows <- cvparts != i
      testrows <- cvparts == i
      elev.gam <- mgcv::gam(formula, data = data[trainrows, ], method = method,
                            gamma = gamma)
      #print(elev.gam)
      hat[testrows] <- predict(elev.gam, newdata = data[testrows,
      ], select = TRUE)
      res <- residuals(elev.gam)
      print(res)
    }
    y <- eval(formula[[2]], envir = as.data.frame(data))
    res <- y - hat
    cvscale <- sum(res^2)/length(res)
    print(paste0("MSE: ", mean(res^2))) #mean square error
    prntvec <- c(GAMscale = scale.gam, `CV-mse-GAM ` = cvscale)
    if (printit)
      print(round(prntvec, 4))
    invisible(list(fitted = hat, resid = res, cvscale = cvscale,
                   scale.gam = scale.gam))
  }

# Run cross validation for every model
CVgam(Textiles_1831~Skills+Wage+Potential+SHP+s(X,Y),data=Industry,printit = TRUE) #tex1
CVgam(Textiles_1831~Skills+Wage+Potential+Coal_Distance+SHP+s(X,Y),data=Industry,printit = FALSE) #tex2
CVgam(Textiles_1831~Skills+Wage+Potential+Water_Flow+SHP+s(X,Y),data=Industry,printit = FALSE) #tex3

CVgam(Textiles_1851~Skills+Wage+Potential+STF+s(X,Y),data=Industry,printit = FALSE) #tex5
CVgam(Textiles_1851~Skills+Wage+Potential+Coal_Distance+STF+s(X,Y),data=Industry,printit = FALSE) #tex6
CVgam(Textiles_1851~Skills+Wage+Potential+Water_Flow+STF+s(X,Y),data=Industry,printit = FALSE) #tex7

# ohne Geodaten:

#CVgam(Textiles_1831~Skills+Wage+Potential+SHP,data=Industry,printit = TRUE) #tex1
#CVgam(Textiles_1831~Skills+Wage+Potential+Coal_Distance+SHP,data=Industry,printit = FALSE) #tex2
#CVgam(Textiles_1831~Skills+Wage+Potential+Water_Flow+SHP,data=Industry,printit = FALSE) #tex3

#CVgam(Textiles_1851~Skills+Wage+Potential+STF,data=Industry,printit = FALSE) #tex5
#CVgam(Textiles_1851~Skills+Wage+Potential+Coal_Distance+STF,data=Industry,printit = FALSE) #tex6
#CVgam(Textiles_1851~Skills+Wage+Potential+Water_Flow+STF,data=Industry,printit = FALSE) #tex7


# cool: now compare these mse with the mse from the cross validation:

# print mse for the regressions from the paper
for (i in 1:length(models_tex)) {
  mse <- mean(models_tex[[i]]$residuals^2)
  print(mse)
}
# print mse for the regressions from the paper (other wqy)
mse <- mean((predict(tex1) - Industry$Textiles_1831)^2)
mse 



###############################################
# ENDE  CROSS VAL ⬆️
###############################################
