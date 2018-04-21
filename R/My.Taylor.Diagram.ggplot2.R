# Taylor Diagram

Taylor <- function(data, nRef = 2, nModel = 3, model.color = c("red", "blue", "green"), ref.lable = c(1:2), name.plot = "plot.png") {
  
  sd.data <- apply(X = data, MARGIN = 2, sd, na.rm = TRUE)
  sd.Ref.max <- max(sd.data[seq.int(from = 1, to = length(data), by = nModel + 1)])
  sd.Model.max <- max(sd.data[-seq.int(from = 1, to = length(data), by = nModel + 1)])
  
  sd.Model.max <- sd.Model.max / sd.Ref.max
  sd.Ref.max <- 1
  
  maxray <- round(1.5 * max(sd.Model.max, sd.Ref.max))
  
  P <- ggplot(data = NULL, mapping = aes(x = c(0, maxray), y = c(0, maxray))) + 
    coord_fixed(ratio = 1) +
    xlab(label = "Standard Deviation (Normalized)") +
    ylab(label = "Standard Deviation (Normalized)")
  
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
  
  grad.corr.lines <- c(0.2, 0.4, 0.6, 0.8, 0.9)
  
  for (i in grad.corr.lines) {
    P <- P + geom_line(mapping = aes_string(x = c(0, maxray * i),
                                            y = c(0, maxray * sqrt(1 - i^2))),
                       color = "gray70")
  }
  
  grad.corr.full <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 1)
  
  for (i in grad.corr.full) {
    P <- P + geom_text(mapping = aes_string(x = 1.05 * maxray * i,
                                            y = 1.05 * maxray * sqrt(1 - i^2),
                                            label = i))
  }
  
  for (i in grad.corr.full) {
    if (i != 0.95 & i != 0.99) {
      P <- P + geom_line(mapping = aes_string(x = c(0.97 * maxray * i, 1.00 * maxray * i),
                                              y = c(0.97 * maxray * sqrt(1 - i^2), 1.00 * maxray * sqrt(1 - i^2))), 
                         color = "black")
    } else {
      P <- P + geom_line(mapping = aes_string(x = c(0.99 * maxray * i, 1.00 * maxray * i),
                                              y = c(0.99 * maxray * sqrt(1 - i^2), 1.00 * maxray * sqrt(1 - i^2))), 
                         color = "black")
    }
  }
  
  seq.sd <- seq.int(from = -2 * maxray,
                    to = 2 * maxray,
                    by = ((maxray)/4))
  
  xText <- yText <- NULL
  
  for (i in seq.sd) {
    xcircle <- sd.Ref.max + (cos(discrete*pi/180) * i)
    ycircle <- abs(-1 *sin(discrete*pi/180) * i)
    xycircle <- xcircle^2 + ycircle^2
    P <- P + geom_line(mapping = aes_string(x = xcircle[xycircle <= (maxray^2) & xcircle >= 0],
                                            y = ycircle[xycircle <= (maxray^2) & xcircle >= 0]),
                       color = "goldenrod4", linetype = "dashed")
  }
  
  P <- P + geom_text(mapping = aes_string(x = c(1, 0.75, 0.5, 0.25)),
                                          y = c(0.5 + 0.03, 1 , 1.5 - 0.04, 2 - 0.1),
                                          label = c(0.5, 1, 1.5, 2),
                     color = "goldenrod4", size = 4)
  
  seq.sd <- seq.int(from = 1,
                    to = maxray,
                    length.out = 2)
  
  for (i in seq.sd) {
    xcircle <- cos(discrete*pi/180) * i
    ycircle <- sin(discrete*pi/180) * i
    P <- P + geom_line(mapping = aes_string(x = xcircle,
                                            y = ycircle),
                       color = "black", linetype = "dashed")
  }
  
  P <- P + geom_text(mapping = aes(x = 0.8 * maxray,
                                   y = 0.8 * maxray,
                                   label = "Correlation Coefficient",
                                   angle = -45)) +
    geom_text(mapping = aes(x = 0.9 * maxray,
                            y = maxray,
                            label = "Centered RMS Error"),
              color = "goldenrod4")
  
  P <- P + geom_point(mapping = aes(x = sd.Ref.max,
                                    y = 0,
                                    size = 2),
                      shape = 15, color = "gray45") +
    scale_shape_identity()
  
  P <- P + geom_text(mapping = aes(x = sd.Ref.max + 0.1,
                                   y = 0.05,
                                   label = "Ref"),
                     color = "gray45")
  
  P <- P + theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, colour = "black"),
    axis.text.y = element_text(size = 12, colour = "black"),
    legend.position = "none"
  )
  
  ######################################################################
  
  n <- length(data) / nRef

  for (i in 1:nRef) {
    TD.data <- data[, ((n * i - n + 1):(n * i))]
    for (j in 1:nModel) {
      R <- cor(x = TD.data[,1], y = TD.data[, (j + 1)], use = "complete.obs")
      sd.r <- sd(x = TD.data[,1], na.rm = TRUE)
      sd.f <- sd(x = TD.data[, (j + 1)], na.rm = TRUE)
      sd.f <- sd.f / sd.r
      sd.r <- 1
      
      P <- P + geom_point(mapping = aes_string(x = sd.f * cos(acos(R)),
                                               y = sd.f * sin(acos(R)), 
                                               size = 2), 
                          shape = 19, color = model.color[j])
      
      if (nRef != 1) {
        P <- P + geom_text(mapping = aes_string(x = sd.f * cos(acos(R)),
                                                y = sd.f * sin(acos(R)) + 0.08,
                                                label = ref.lable[i]),
                           color = model.color[j])
      }
    }
  }

  ggsave(filename = name.plot, plot = P, width = 6, height = 6, units = "in", dpi = 1200)
  
  return(P)
}