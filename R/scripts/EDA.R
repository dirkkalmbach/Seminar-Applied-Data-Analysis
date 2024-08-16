########################################################################
# Project Seminar Applied Data Analysis:
# ".."
#
# R Script "EDA (Exploratory Data Analysis)"
# by: D. Kalmbach
# Date of this version: April, 2024
########################################################################

# Libraries
library(here)
library(dplyr)
library(modelsummary)
library(mapview)
library(maps)
here()

# Load the data
path_to_data = here("data", "Industry.csv")
#industry <- read_csv(path_to_data) # to load industry.csv
load(here("data","Industry.Rda")) # to load industry.Rda
load(here("data","england_map.Rda")) # to load industry.Rda

# merge datasets Industry and England
england = rename(england, Code = code)
df <- merge(Industry, england, by = "Code")

# Correlation Matrix
x=Industry[0:5]
plot(x)
cor(x$Wage,x$Coal_Distance) # why high correlation? 🧐 0.79
cor(x$Wage,x$Water_Flow) #-0.43

# Build var textile_increase
Industry$textile_increase = exp(Industry$Textiles_1851)-exp(Industry$Textiles_1831)

# try Regression
new7 = lm(textile_increase~Skills,data=Industry)
summary(new7) # R2 = 0.32
#count(Industry$textile_increase_dummy=0)

# plot counties with textile increase
Industry$textile_increase_dummy=ifelse(Industry$textile_increase>0,1,0)
carto_1830=cartogramR(england,"agg_inc_1830")

ggplot(data=carto_1830$cartogram) + geom_sf(aes(fill = Industry$textile_increase_dummy)) + theme(legend.position = "none")


# Plot England with ggplot
# https://bookdown.org/yann_ryan/r-for-newspaper-data/mapping-with-r-geocode-and-map-the-british-librarys-newspaper-collection.html
worldmap = map_data('world')
ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, 
                   group = group), 
               fill = 'gray90', 
               color = 'black') + 
  coord_fixed(ratio = 1.3, 
              xlim = c(-10,3), 
              ylim = c(50, 59))

plot(df$X, df$Y,
     pch = 19,
     col = factor(df$textile=="Q5" ))



