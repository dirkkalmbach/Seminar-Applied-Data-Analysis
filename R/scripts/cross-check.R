########################################################################
# Project Seminar Applied Data Analysis
#
# R Script "cross-check"
# by: D. Kalmbach
# Date of this version: AUGUST, 2024
########################################################################

# LIBRARIES
# =========
library(here)
library(readr)
library(dplyr)
here()

# LOAD THE DATA
# =============
load(here("data","df.Rda")) # to load df
load(here("data","Industry.Rda")) # to load df

########################################################################

# POPULATION 📓
############
# 3 population variables: pop.1761, pop.1831, Population with different data type
typeof(df$pop.1761) # -> double
typeof(df$pop.1831) # -> integer
typeof(df$Population) # -> double

# What is df$Population?
df$pop.1761 == exp(df$Population) # -> FALSE
df$pop.1831 == as.integer(exp(df$Population)) # -> TRUE
#convert df$Population to double

as.double(df$pop.1831)
exp(df$Population)

typeof(df$pop.1761)
typeof(df$pop.1831)
df$pop.1831
exp(df$Population)


# CHECK QUANTILES 📓
#################

# Quantiles in Textile 1831
# replicate quantile variables
df$textiles_CHECK <- ntile(df$Textiles_1831, 5)
df$textiles_CHECK = factor(df$textiles_CHECK, 
                         levels = c(1, 2, 3, 4, 5), 
                         labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
df$textiles_CHECK == df$textiles # -> one county is different!
# print countie with different quantile
#correct the next line

df[df$textiles_CHECK != df$textiles, c("Code", "NAME", "Textiles_1831", "textiles_CHECK")] # -> YNR

# check how much YNR is different in quantile 🤓
# print boundaries of ntile
quantile(df$Textiles_1831, probs = c(0, 0.25, 0.5, 0.75, 1))
#print wage.1767 for DRH,LCS,NHP,RTL,YNR
df[df$Code %in% c("YNR"), c("Code", "NAME", "Textiles_1831")]

# delete wage_CHECK
#df$textiles_CHECK <- NULL


# Quantiles in Wage
# 1760
df$wage_CHECK <-  ntile(df$wage.1767, 4)
df$wage_CHECK
df$wage_CHECK = factor(df$wage_CHECK, 
                    levels = c(1, 2, 3, 4), 
                    labels = c("Q1", "Q2", "Q3", "Q4"))
df$wage_CHECK == df$wage_1760 # ✅
# print countie with different quantile
df[wage_CHECK != df$wage_1760, c("Code", "NAME", "wage_1760", "wage_CHECK")] # -> DRH,LCS,NHP,RTL,YNR
# delete wage_CHECK
df$wage_CHECK <- NULL

# nochmal die Größe der Abweichung anschauen 🤓
# print boundaries of ntile
quantile(df$wage.1767, probs = c(0, 0.25, 0.5, 0.75, 1))
#print wage.1767 for DRH,LCS,NHP,RTL,YNR
df[df$Code %in% c("DRH", "LCS", "NHP", "RTL", "YNR"), c("Code", "NAME", "wage.1767")]

# 1831
wage_CHECK <-  ntile(df$wage.1833, 4)
wage_CHECK = factor(wage_CHECK, 
                        levels = c(1, 2, 3, 4), 
                        labels = c("Q1", "Q2", "Q3", "Q4"))
wage_CHECK == df$wage_1830 # ✅


# CHECK THE VAR wage.1767 AND WAGE 📓✅
##################################
# wage
df$Wage == log(df$wage.1767) # -> TRUE bis auf 1 Wert
df[df$Wage != log(df$wage.1767), c("Code", "NAME", "Wage", "wage.1767")] # -> HRF
exp(4.394449) # -> 81 d.h. deutlich anders als 70!


# CHECK THE VAR SHP ✅
# SHP
df[df$SHP == TRUE, c("Code", "NAME", "SHP")] # -> SHP

# CHECK AGGR INCOME ✅
df$agg_inc_1760 == df$pop.1761 * df$wage.1767
df$agg_inc_1830 == df$pop.1831 * df$wage.1833





######## verify Marshall 1833
text <- 11064
#df$Textiles_1831 for Middlesex MX
df[df$Code == "MSX", c("Code", "NAME", "Textiles_1831_abs")] # 15805
15805-11064 # 4741
agric <- 13417
