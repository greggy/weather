yplot <- function(years, pres = FALSE){
  ## Get data of the year and render plots
  
  par(mfrow = c(length(years), 1), mai = c(1.2, 1, 0.7, 1))
  
  for(year in years){
    data <- read.csv(sprintf("data/rp5_vladimir%d.csv", year),
                     colClasses = "character", sep = ";")
    data[,1] <- as.POSIXct(data[,1], format = "%d.%m.%Y %H:%M")
    data[,2] <- as.numeric(data[,2])
    data[,3] <- as.numeric(data[,3])
    
    nt <- data[grep("01:00", data[,1]),]
    dt <- data[grep("13:00", data[,1]),]
    
    plot(nt[,1], nt[,2], type = "l", col = "blue", ylim = c(-30, 40), xaxt = "n",
         main = sprintf("Temperate for %d Year", year),
         xlab = "", ylab = "Temperate")
    legend("topright", c("Night", "Day", "Pressure"), lty = c(1, 1, 1),
           col = c("blue", "red", "green"))
    points(dt[,1], dt[,2], type = "l", col = "red")
    
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
      plot(data[,1], data[,3], type = "l", col = "green", ylab = "", xlab = "",
           ylim = c(710, 780), axes = FALSE, xaxt = "n")
      
      axis(4, pretty(seq(710, 780, 5)), col = "green")
      mtext("Pressure", 4, cex = 0.7)
    }
  }
}
