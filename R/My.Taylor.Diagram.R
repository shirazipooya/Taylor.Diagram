# Taylor Diagram

library(ggplot2)

Taylor <- function(ref, batch, add = F, couleur = "red") {
  
  x <- ref
  y <- batch
  
  grad.corr.full <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99, 1)
  grad.corr.lines <- c(0.2, 0.4, 0.6, 0.8, 0.9)
  
  R <- cor(x = x, y = y, use = "complete.obs")
  
  sd.r <- sd(x = x, na.rm = TRUE)
  sd.f <- sd(x = y, na.rm = TRUE)
  
  if (add = FALSE) {
    
    maxray <- 1.5 * max(sd.f, sd.r)
    
    plot(x = c(-maxray, maxray), 
         y = c(0, maxray),
         type = "n",
         asp = 1,
         bty = "n",
         xaxt = "n",
         yaxt = "n",
         xlab = "",
         ylab = "",
         main = "Taylor Diagram")
    
    discrete <- seq(from = 180, to = 0, by = -1)
    
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
    
    lines(x = listepoints[,1],
          y = listepoints[,2])
    
    lines(x = c(-maxray, maxray),
          y = c(0, 0))
    
    lines(x = c(0, 0),
          y = c(0, maxray))
    
    for (i in grad.corr.lines) {
      
      lines(x = c(0, maxray * i), 
            y = c(0, maxray * sqrt(1 - i^2)),
            lty = 3)
      
      lines(x = c(0, -maxray * i),
            y = c(0, maxray * sqrt(1 - i^2)),
            lty = 3)
      
    }
    
    for (i in grad.corr.full) {
      
      text(x = 1.05 * maxray * i,
           y = 1.05 * maxray * sqrt(1 - i^2),
           labels = i,
           cex = 0.6)
      
      text(x = -1.05 * maxray * i,
           y = 1.05 * maxray * sqrt(1 - i^2),
           labels = -i,
           cex = 0.6)
      
    }
    
    seq.sd <- seq.int(from = 0,
                      to = 2 * maxray,
                      by = (maxray/10))
    
    for (i in seq.sd) {
      
      xcircle <- sd.r + (cos(discrete*pi/180) * i)
      ycircle <- sin(discrete*pi/180) * i
      
      for (j in 1:length(xcircle)) {
        
        if ((xcircle[j]^2 + ycircle[j]^2) < (maxray^2)) {
          
          points(x = xcircle[j],
                 y = ycircle[j],
                 col = "darkgreen",
                 pch=".")
          if (j == 10) {
            
            text(x = xcircle[j],
                 y = ycircle[j],
                 labels = signif(i,2),
                 cex = 0.5,
                 col = "darkgreen")
            
          }
        }
      }
    }
    
    seq.sd <- seq.int(from = 0,
                      to = maxray,
                      length.out = 5)
    
    for (i in seq.sd) {
      
      xcircle <- cos(discrete*pi/180) * i
      ycircle <- sin(discrete*pi/180) * i
      
      lines(x = xcircle,
            y = ycircle,
            lty = 3,
            col = "blue")
      
      text(x = min(xcircle),
           y = -0.03 * maxray,
           labels = signif(i,2),
           cex = 0.5,
           col = "blue")
      
      text(x = max(xcircle),
           y = -0.03 * maxray,
           labels = signif(i,2),
           cex = 0.5,
           col = "blue")
      
    }
    
    text(x = 0,
         y = -0.08 * maxray,
         labels = "Standard Deviation",
         cex = 0.7,
         col = "blue")
    
    text(x = 0,
         y = -0.12 * maxray,
         labels = "Centered RMS Difference",
         cex = 0.7,
         col = "darkgreen")
    
    points(x = sd.r,
           y = 0,
           pch = 22,
           bg = "darkgreen",
           cex = 1.1)
    
    text(x = 0,
         y = 1.1 * maxray,
         labels = "Correlation Coefficient",
         cex = 0.7)
    
  }
  
  points(x = sd.f * cos(acos(R)),
         y = sd.f * sin(acos(R)),
         pch = 21,
         bg = couleur,
         cex = 0.8)
  
}