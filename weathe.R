yplot <- function(years, pres = FALSE, hum = FALSE){
  ## Get data of the year and render plots
  day.col <- "red"
  night.col <- "blue"
  pres.col <- "green"
  hum.col <- "grey"
  
  par(mfrow = c(length(years), 1), mai = c(1.2, 1, 0.7, 1))
  
  for(year in years){
    data <- read.csv(sprintf("data/rp5_vladimir%d.csv", year),
                     colClasses = "character", sep = ";")
    data[,1] <- as.POSIXct(data[,1], format = "%d.%m.%Y %H:%M")
    data[,2] <- as.numeric(data[,2])
    data[,4] <- as.numeric(data[,4]) ## pressure
    data[,6] <- as.numeric(data[,6]) ## humidity
    
    nt <- data[grep("01:00", data[,1]),]
    dt <- data[grep("13:00", data[,1]),]
    
    plot(nt[,1], nt[,2], type = "l", col = night.col, ylim = c(-30, 40), xaxt = "n",
         main = sprintf("Temperature for %d Year", year),
         xlab = "", ylab = "Temperature")
    points(dt[,1], dt[,2], type = "l", col = day.col)
    
    ## legends
    leg.names <- c("Night", "Day")
    leg.lty <- c(1, 1)
    leg.col <- c(night.col, day.col)
    if(pres) {
      leg.names <- c(leg.names, "Pressure")
      leg.lty <- c(leg.lty, 1)
      leg.col <- c(leg.col, pres.col)
  }
    if(hum) {
      leg.names <- c(leg.names, "Humidity")
      leg.lty <- c(leg.lty, 1)
      leg.col <- c(leg.col, hum.col)
    }
    legend("topright", leg.names, lty = leg.lty, col = leg.col)
    
    ## horizontal line 10C
    abline(h = 10, lty = 2, lheight = 0.5)
    
    ## vertical lines at the begginning of months
    nm <- data[grep("-01 13:00", data[,1]),][,1]
    nm <- c(nm, data[1,1]) ## add last entry of december
    for(d in nm) {
      abline(v = d, lty = 2, lwd = 0.2)
    }
    
    ## axis
    ms <- split(data, months(data[,1]))
    months <- names(ms)
    at <- c()
    for(m in ms){
      at <- c(at, mean(m[,2], na.rm = TRUE))
    }
    bymonths <- data.frame(month = months, temp = at)
    
    axis(1, at = lapply(ms, function(x) x[nrow(x)/2,1]), 
         labels = sprintf("%s (%.1f)", bymonths[,1], round(bymonths[,2], 1)),
         las = 2, cex.axis = 0.8)
    
    ## pressure
    if(pres) {
      par(new = TRUE)
      plot(data[,1], data[,4], type = "l", col = pres.col, ylab = "", xlab = "",
           ylim = c(700, 800), axes = FALSE, xaxt = "n")
      
      axis(4, col = pres.col)
      mtext("Pressure", 4)
    }
    
    ## humidity
    if(hum) {
      ## try to computate mean for every day
      days <- format(seq(data[,1][nrow(data)], data[,1][1], "day"), "%Y-%m-%d")
      means <- c()
      for(x in days) {
        means <- c(means, mean(data[grep(x, data[,1]), 6], na.rm = TRUE))
      }
      dh <- data.frame(date = days, means = means)
      
      par(new = TRUE)
      plot(dh[,1], dh[,2], type = "l", col = hum.col, ylab = "", xlab = "",
           ylim = c(20, 100), axes = FALSE, xaxt = "n")
      
      if(pres) {
        axis(4, col = hum.col, line = 2)
        mtext("Humidity", 4, line = 2)
      } else {
        axis(4, col = hum.col)
        mtext("Humidity", 4)  
      }
    }    
  }
}
