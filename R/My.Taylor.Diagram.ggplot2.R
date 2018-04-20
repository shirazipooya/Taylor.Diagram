# # Taylor Diagram
# 
# rm(list = ls())
# graphics.off()
# 
# data <- read.csv(file = "data/Sample.Data.csv")
# ref <- data$Ref
# batch <- data$Mod7
# normalise = TRUE
# couleur = "red"
# 
# library(ggplot2)

Taylor <- function(ref, batch, add = FALSE, couleur = "red", normalise = TRUE) {
  
  x <- ref
  y <- batch
  
  grad.corr.full <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99, 1)
  grad.corr.lines <- c(0.2, 0.4, 0.6, 0.8, 0.9)
  
  R <- cor(x = x, y = y, use = "complete.obs")
  
  sd.r <- sd(x = x, na.rm = TRUE)
  sd.f <- sd(x = y, na.rm = TRUE)
  
  if (normalise) {
    
    sd.f <- sd.f / sd.r
    sd.r <- 1
  }
  
  if (add == FALSE) {
    
    maxray <- round(1.5 * max(sd.f, sd.r))
    
    P <- ggplot(data = NULL, mapping = aes(x = c(0, maxray), y = c(0, maxray))) + 
      coord_fixed(ratio = 1) +
      xlab(label = "Standard Deviation") +
      ylab(label = "Standard Deviation")
    
    discrete <- seq(from = 90, to = 0, by = -1)
    
    listepoints <- NULL
    
    for (i in discrete) {
      
      listepoints <- cbind(listepoints,
                           maxray * cos(i*pi/180),
                           maxray * sin(i*pi/180))
      
    }
    
    listepoints <- matrix(data = listepoints,
                          nrow = 2,
                          ncol = length(x = listepoints)/2)
    
    
    listepoints <- t(x = listepoints)
    
    P <- P + geom_line(mapping = aes(x = listepoints[,1], y = listepoints[,2])) +
      geom_line(mapping = aes(x = c(0, maxray), y = c(0, 0))) +
      geom_line(mapping = aes(x = c(0, 0), y = c(0, maxray)))
      
    
    for (i in grad.corr.lines) {
      
      P <- P + geom_line(mapping = aes_string(x = c(0, maxray * i), y = c(0, maxray * sqrt(1 - i^2))), color = "gray70")
      
    }
    
    for (i in grad.corr.full) {
      
      P <- P + geom_text(mapping = aes_string(x = 1.05 * maxray * i, y = 1.05 * maxray * sqrt(1 - i^2), label = i))
      
    }
    
    
    seq.sd <- seq.int(from = -2 * maxray,
                      to = 2 * maxray,
                      by = ((maxray)/4))
    
    for (i in seq.sd) {
      
      xcircle <- sd.r + (cos(discrete*pi/180) * i)
      
      if (i < 0) {
        
        ycircle <- -1 *sin(discrete*pi/180) * i
        xycircle <- xcircle^2 + ycircle^2
        
        P <- P + geom_line(mapping = aes_string(x = xcircle[xycircle < (maxray^2) & xcircle >= 0], y = ycircle[xycircle < (maxray^2) & xcircle >= 0]), color = "goldenrod4", linetype = "dashed")
        
      } else {
        
        ycircle <- sin(discrete*pi/180) * i
        xycircle <- xcircle^2 + ycircle^2
        
        P <- P + geom_line(mapping = aes_string(x = xcircle[xycircle < (maxray^2) & xcircle >= 0], y = ycircle[xycircle < (maxray^2) & xcircle >= 0]), color = "goldenrod4", linetype = "dashed")
      }
      
    }
    
    seq.sd <- seq.int(from = 1,
                      to = maxray,
                      length.out = 2)
    
    for (i in seq.sd) {
      
      xcircle <- cos(discrete*pi/180) * i
      ycircle <- sin(discrete*pi/180) * i
      
      P <- P + geom_line(mapping = aes_string(x = xcircle, y = ycircle), color = "black", linetype = "dashed")
        
    }
    
    P <- P + geom_text(mapping = aes(x = 0.8 * maxray, y = 0.8 * maxray, label = "Correlation Coefficient", angle = -45)) +
      geom_text(mapping = aes(x = 0.9 * maxray, y = maxray, label = "Centered RMS Error"), color = "goldenrod4")
    
    P <- P + geom_point(mapping = aes(x = sd.r, y = 0, size = 2), shape = 15, color = "gray45") + scale_shape_identity() + theme(legend.position="none")
    P <- P + geom_text(mapping = aes(x = sd.r + 0.1, y = 0.1, label = "Ref"), color = "gray45")

  }
  
  P <- P + geom_point(mapping = aes_string(x = sd.f * cos(acos(R)), y = sd.f * sin(acos(R)), size = 2), shape = 19, color = couleur) + theme(legend.position = "none")
  
}