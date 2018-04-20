# Taylor Diagram

rm(list = ls())
graphics.off()

data <- read.csv(file = "data/Sample.Data.csv")

library(ggplot2)

P = Taylor(ref = data$Ref, batch = data$Mod1, add = FALSE, couleur = "red", normalise = TRUE)
Taylor(ref = data$Ref, batch = data$Mod2, add = TRUE, couleur = "blue", normalise = TRUE)
Taylor(ref = data$Ref, batch = data$Mod3, add = TRUE, couleur = "green", normalise = TRUE)
Taylor(ref = data$Ref, batch = data$Mod4, add = TRUE, couleur = "black", normalise = TRUE)
Taylor(ref = data$Ref, batch = data$Mod6, add = TRUE, couleur = "khaki3", normalise = TRUE)
