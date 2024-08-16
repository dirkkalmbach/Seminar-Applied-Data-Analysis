########################################################################
# Project Seminar Applied Data Analysis:
#
# R Script "descriptives"
# by: D. Kalmbach
# Date of this version: August, 2024
########################################################################

# Libraries
# =========
library(here)
library(readr)
library(dplyr)
library(xtable)
here()
# 🧐 the following line of code solved a potential conflict between dplyr and here
#conflicts_prefer(dplyr::rename)


# LOAD THE DATA
# =============

path_to_data = here("R", "data", "Industry.csv")
#industry <- read_csv(path_to_data) # to load industry.csv
load(here("data","Industry.Rda")) # to load industry.Rda
load(here("data","england_map.Rda")) # to load industry.Rda

# helper functions
source(here("scripts", "helpers.R"))

# merge datasets Industry.Rda and england_map.Rda
england = rename(england, Code = code)
df <- merge(Industry, england, by = "Code")

########################################################################
# DESCRIPTIVE STATISTICS
# ======================
summary(Industry)

## show countie codes
x <- df[,c("Code","NAME")]
x 
### sorted by countie code
x[order(x$Code),]

# print this as table in latex
xtable(x[order(x$Code),])

## Missing values
sum(is.na(Industry)) # -> 0

# PLOTTING
## plot long, lat with 5 biggest cities and population size in 1760)
plot(Industry$X, Industry$Y, xlab='long', ylab='lat', main="England (Counties Coordinates)")
points(0, 51.5, col='blue', pch=19, cex=2) # London 
points(-2.5, 51.5, col='yellow', pch=19) # Bristol 
points(-2, 52.5, col='brown', pch=19) # Birmingham 
points(-3, 53.5, col='red', pch=19) # Liverpool 
points(-2.25, 53.5, col='green', pch=19) # Manchaster 

## plot with Countie Code
plot(x = Industry$X, y = Industry$Y, type = "n", xlab='long', ylab='lat', main="England (Counties Code)")
text(x = Industry$X, y = Industry$Y,labels = Industry$Code, cex=0.5)

## plot high Textile X,Y
plot(df$X, df$Y, , main="Counties with highest Textile Industry 1831",
     sub="5th quantile of Textile Industry 1831",
     xlab='long', ylab='lat',
     pch = 19,
     col = factor(df$textiles=="Q5" ))


# Deskriptive Statistik der wichtigsten Variablen
# =================================================
# abhängige Variablen
df$Textiles_1831_exp <- exp(df$Textiles_1831)
df$Textiles_1851_exp <- exp(df$Textiles_1851)

summary(df$Textiles_1831_exp)
summary(df$Textiles_1851_exp)

hist(df$Textiles_1831_exp, main="Anteil Beschäftigter Textilindustrie 1831",
     ylab="Höufigkeit", xlab="%", sub="männl. Beschäfigte >20 J.(N=41)")
hist(df$Textiles_1831_exp, main="Anteil Beschäftigter Textilindustrie 1851", 
     ylab="Häufigkeit", xlab="%", sub="männl. Beschäftigte 20-29 J. (N=41)")

# Visualisierung mit Boxplots
par(mfrow=c(1,2))
hist(df$Textiles_1831_exp, 
        main = "1831", ylab="Höufigkeit",
        xlab="%", sub="männl. Beschäfigte >20 J.(N=41)")
hist(df$Textiles_1851_exp,
        main = "1851", ylab="Häufigkeit",
        xlab="%", sub="männl. Beschäftigte 20-29 J. (N=41)")
par(mfrow=c(1,1))
title("Beschäftigungsanteile in der Textilindustrie", line = -1, outer = TRUE)

# unabhängige Variablen
# Population
summary(exp(df$Population))
cat("Population: ", sum(exp(Industry$Population)))
df$Population_abs <- exp(df$Population)

# Wage
summary(exp(df$Wage))
boxplot(exp(df$Wage))# -> 3 Ausreisser
df$Wage_exp <- exp(df$Wage)

# Skills
summary(exp(df$Skills))
boxplot(exp(df$Skills))#-> 2 Ausreisser
df$Skills_exp <- exp(df$Skills)

# Coal_Distance
summary(exp(df$Coal_Distance))
boxplot(exp(df$Coal_Distance))# -> 0 Ausreisser
# TODO sum(exp(df$Coal_Distance)==20)

# Water_Flow
summary(exp(df$Water_Flow))
boxplot(exp(df$Water_Flow)) # -> 1 Ausreisser
df$Water_Flow_exp <- log(df$Water_Flow)

# Market Potential
summary(exp(df$Potential))
boxplot(exp(df$Potential)) # -> 1 Ausreisser
df$Potential_exp <- exp(df$Potential)

# explorative data analysis of Apprencice_Cost
summary(exp(df$Apprentice_Cost))
boxplot(exp(df$Apprentice_Cost)) # -> 4 Ausreisser
df$Apprentice_Cost_exp <- exp(df$Apprentice_Cost)
  

########################################################################Textile
cat("Einwohnerzahl 1831: ", sum(exp(Industry$Population)))

# Wieviele Arbeiten wirklich im Textilsektor?
#Lt. Paper:
#Textiles_1831 = employment share of textiles among all males over 20 in 1831 
#Textiles_1851 = employment share of textiles among males aged 20–29 in 1851

sex_anteil <- 0.5 # vereinfacht auf 50% gesetzt
age_20_29 <- 9.3+8.2 # 1851, aus: https://www.histpop.org/ohpr/servlet/PageBrowser?path=Browse/Census%20(by%20date)/1851/England&active=yes&mno=1&tocstate=expandnew&display=sections&display=tables&display=pagetitles&pageseq=1
age_20plus <- 100-9.8-10.7-11.7-13 # 1831

# 1831
df["Textiles_1831_exp"] <- exp(Industry$Textiles_1831)
df[,c("Code", "Textiles_1831_exp")][order(-df$Textiles_1831_exp),] #Anteil Beschäftigter in der Textilindustrie 1831
df["Textiles_1831_abs"] <- round(df["Textiles_1831_exp"]/100 * exp(df$Population) * sex_anteil * age_20plus/100, digit=0)
df[,c("Code", "Textiles_1831_abs")][order(-df$Textiles_1831_abs),] #abs. Beschäftigte in der Textilindustrie 1831

# 1851
df["Textiles_1851_exp"] <- exp(Industry$Textiles_1851)
df[,c("Code", "Textiles_1851_exp")][order(-df$Textiles_1851_exp),] #Anteil Beschäftigter in der Textilindustrie 1851
df["Textiles_1851_abs"] <- round(df["Textiles_1851_exp"]/100 * exp(df$Population) * sex_anteil * age_20_29/100, digit=0)
df[,c("Code", "Textiles_1851_abs")][order(-df$Textiles_1851_abs),] #abs. Beschäftigte in der Textilindustrie 1851

# Visualisierung mit Boxplots
par(mfrow=c(1,2))
boxplot(df$Textiles_1831_abs, 
        main = "1831", 
        ylab="Beschäftigte",xlab="N=41")

boxplot(df$Textiles_1851_abs,
        main = "1851", 
        xlab="N=41")
par(mfrow=c(1,1))
title("Beschäftigte in der Textilindustrie", line = -1, outer = TRUE)

# Visualisierung mit Histograms
hist(df$Textiles_1831_abs, main="Beschäftigte in der Textilindustrie 1831", 
     ylab="Häufigkeit",xlab="N=41", sub="männl. Beschäfigte >20 J.")
hist(df$Textiles_1851_abs, main="Beschäftigte in der Textilindustrie 1851", 
     ylab="Häufigkeit",xlab="N=41", sub="männl. Beschäfigte 20-29 J.")

# höchste absolute Beschäftigung
head(df[c("Code","NAME", "Population_abs","Textiles_1851_exp", "Textiles_1831_abs", "Textiles_1851_abs")][order(-df$Textiles_1851_abs),])

# niedrigste absolute Beschäftigung
tail(df[c("Code","NAME", "Textiles_1831", "Textiles_1851_abs")][order(-df$Textiles_1831_abs),])

# beides als Latex Tabelle ausdrucken
x <- head(df[c("Code","NAME", "Textiles_1831_abs", "Textiles_1851_abs")][order(-df$Textiles_1831_abs),])
colnames(x) <- c("Code", "Name", "Textiles 1831", "Textiles 1851")
y <- tail(df[c("Code","NAME", "Textiles_1831_abs", "Textiles_1851_abs")][order(-df$Textiles_1831_abs),])
colnames(y) <- c("Code", "Name", "Textiles 1831", "Textiles 1851")
# print this as table in latex without rownames
xtable(x, digits = 0)
xtable(y, digits = 0)
#############################################end of Textile

# Histograms
#The United Kingdom used a triple system with all prices in pounds, shillings and pence (abbreviated as £, s and d). The basis of this triple system was 12 pennies to the shilling, and 20 shilling to the pound.
hist(exp(Industry$Apprentice_Cost))
hist(exp(Industry$Wage))

# save df
save(df, file = here("data", "df.Rda"))


# Sonstige Deskriptive Statistiken
# ================================
# Density
summary(exp(df$Density))
sum(detect_outlier(exp(df$Density))) # 4 Outliers
hist(exp(df$Density), breaks=c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150 , 800)
     main="")

# Apprentice Cost
summary(exp(df$Apprentice_Cost))
sum(detect_outlier(exp(df$Apprentice_Cost))) # 4 Outliers
hist(exp(df$Density), breaks=c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150 , 800), main="Apprentice Cost")

# Apprentice Fee


# Vergleich Textilbeschäftigung 1831 und 1851
# ===========================================
# compare textile employment 1831 and 1851
# relative change in textile employment
df$textile_change <- round(df$Textiles_1851_exp - df$Textiles_1831_exp,1)
df$textile_change 

# absolute change in textile employment
df$textile_change_abs <- round(df$Textiles_1851_abs - df$Textiles_1831_abs,0)
sum(df$textile_change_abs)

# Check Correlations
# plot correlation matrix of all variables
cor(df[,c("Textiles_1831_exp", "Textiles_1851_exp", "Population_abs", "Wage_exp", "Skills_exp", "Coal_Distance", "Water_Flow_exp", "Potential_exp", "Apprentice_Cost_exp")])

# create exponential of log variables and store it in a new variable with the ending _exp
df$Textiles_1831_exp <- exp(df$Textiles_1831)
df$Textiles_1851_exp <- exp(df$Textiles_1851)


# Wage, Apprentice_Cost, Apprentice_Wage, Coal_Distance, Water_Flow, Tax_1707, Banks, Potential, Textiles_1831, Literate, Lawyers, Booksellers, Density, Metal_Mfg, Textiles_1851, Food, Garments, Shoes, Wood, Metal_Prods, Ships, Sheet_Metal, Metal, Heat, Skills, 