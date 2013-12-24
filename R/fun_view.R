

bathyview <- function(bathy, xrange, yrange)
{
  isob <- c(-20, -50, -100, -200, -500, -1000, -1500)
  lon <- unique(as.numeric(rownames(bathy$data)))
  lat <- unique(as.numeric(colnames(bathy$data)))
  icol <- rgb(0,0, seq(250,20, len = length(isob)), maxColorValue = 255)
  contour(lon, lat, bathy$data, add=TRUE,
          #lwd=0.5,
          col=icol,
          ylim = yrange, xlim = xrange,
          levels = c(-20, -50, -100, -200, -500, -1000, -1500),
          drawlabel=TRUE) 
}
####################

trackview2 <- function(track, trackn, bathy = "", xrange, yrange)
{
  span <- 0.25

  if(diff(xrange) != 0 & diff(yrange) != 0)
  {
    map2plot <- try(map("worldHires",fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), plot = F), silent = T)
    if(class(map2plot) != "try-error")
    {
      map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0))
      if(bathy$path != ""){
        bathyview(bathy, xrange, yrange)
        map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), add = TRUE)
      }
      map.axes()
      title(main = paste("Vessel: ", track[1,1],"  -  Track: ",trackn, sep = ""), line = 0.3)
      title(xlab = "Lon", ylab = "Lat", line = 2)
      points(cbind(track[which(track["W_HARB"] == 1),"LON"], track[which(track["W_HARB"] == 1),"LAT"]), col = "yellow", bg = "blue", pch = 25, cex = 1)
      textn <- 0
      for (i in 1:(nrow(track)-1))
      {
        lines(cbind(c(track[i,"LON"], track[i+1,"LON"]), c(track[i,"LAT"], track[i+1,"LAT"])), lty = "dashed", cex = 2)
        if((track[i,"W_HARB"] != 1) & (trackn != textn))
        {
          text(track[i,"LON"], track[i,"LAT"], labels = track[i,"T_NUM"])
          textn <- trackn
        }
      }
      points(cbind(track[,"LON"], track[,"LAT"]), pch = 19, cex = 0.5) #col = colsca[ceiling(track[,"SPE"])],
      points(cbind(track[which(track["W_HARB"] == 1),"LON"], track[which(track["W_HARB"] == 1),"LAT"]), col = 2, pch = 19, cex = 0.5)
    }else{
      while(class(map2plot) == "try-error")
      {
        span <- span + span
        xrange <- extendrange(x = track["LON"], f = span)
        yrange <- extendrange(x = track["LAT"], f = span)
        map2plot <- try(map("worldHires",fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), plot = F), silent = T)
      }
      map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0))
      if(bathy$path != ""){
        bathyview(bathy, xrange, yrange)
        map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), add = TRUE)
      }
      map.axes()
      title(main = paste("Vessel: ", track[1,1],"  -  Track: ",trackn, sep = ""), line = 0.3)
      title(xlab = "Lon", ylab = "Lat", line = 2)
      points(cbind(track[which(track["W_HARB"] == 1),"LON"], track[which(track["W_HARB"] == 1),"LAT"]), col = "yellow", bg = "blue", pch = 25, cex = 1)
      textn <- 0
      for (i in 1:(nrow(track)-1))
      {
        lines(cbind(c(track[i,"LON"], track[i+1,"LON"]), c(track[i,"LAT"], track[i+1,"LAT"])), lty = "dashed", cex = 2)
        if((track[i,"W_HARB"] != 1) & (trackn != textn))
        {
          text(track[i,"LON"], track[i,"LAT"], labels = track[i,"T_NUM"])
          textn <- trackn
        }
      }            
      points(cbind(track[,"LON"], track[,"LAT"]), pch = 19, cex = 0.5) #col = colsca[ceiling(track[,"SPE"])],
      points(cbind(track[which(track["W_HARB"] == 1),"LON"], track[which(track["W_HARB"] == 1),"LAT"]), col = 2, pch = 19, cex = 0.5)
      span <- 0.25
    }
  }
}

####################

intrpview <- function(vessel, bathy = "", xrange, yrange)
{
  span <- 0.25

  if(diff(xrange) != 0 & diff(yrange) != 0 )
  {
    map2plot <- try(map("worldHires",fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), plot = F), silent = T)
    if(class(map2plot) != "try-error")
    {
      map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0))
      if(bathy$path != ""){
        bathyview(bathy, xrange, yrange)
        map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), add = TRUE)
      }
      map.axes()
      title(main = paste("Vessel ", vessel[1,1]," with ", length(unique(vessel[,"T_NUM"]))," tracks" , sep = ""), line = 0.3)
      title(xlab = "Lon", ylab = "Lat", line = 2)
      points(cbind(vessel[which(vessel["W_HARB"] == 1),"LON"], vessel[which(vessel["W_HARB"] == 1),"LAT"]), col = "yellow", bg = "blue", pch = 25, cex = 1)
      colsca <- rainbow(max(vessel[, "T_NUM"]))
      textn <- 0
      for (i in 1:(nrow(vessel)-1))
      {
        trackn <- vessel[i,"T_NUM"]
        if(trackn != 0)
        {
          if(trackn == vessel[i+1,"T_NUM"])
          {
            lines(cbind(c(vessel[i,"LON"],vessel[i+1,"LON"]), c(vessel[i,"LAT"], vessel[i+1,"LAT"])), col = colsca[vessel[i,"T_NUM"]], lty = "dashed", cex = 2)
            if((vessel[i,"W_HARB"] != 1) & (trackn != textn))
            {
              text(vessel[i,"LON"], vessel[i,"LAT"], col = colsca[vessel[i,"T_NUM"]], labels = vessel[i,"T_NUM"])
              textn <- trackn
            }
          }
        }
      }
      points(cbind(vessel[,"LON"], vessel[,"LAT"]), col = colsca[vessel[,"T_NUM"]], pch = 19, cex = 0.5)
      points(cbind(vessel[which(vessel["W_HARB"] == 1),"LON"], vessel[which(vessel["W_HARB"] == 1),"LAT"]), col = 2, pch = 19, cex = 0.5)
    }else{
      map2plot <- try(map("worldHires",fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), plot = F), silent = T)
      while(class(map2plot) == "try-error")
      {
        span <- span + span
        xrange <- extendrange(x = vessel["LON"], f = span)
        yrange <- extendrange(x = vessel["LAT"], f = span)
        map2plot <- try(map("worldHires",fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(0, 0, 0, 0), plot = F), silent = T)
      }
      map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0))
      if(bathy$path != ""){
        bathyview(bathy, xrange, yrange)
        map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), add = TRUE)
      }
      map.axes()
      title(main = paste("Vessel ", vessel[1,1]," with ", length(unique(vessel[,"T_NUM"]))," tracks" , sep = ""), line = 0.3)
      title(xlab = "Lon", ylab = "Lat", line = 2)
      points(cbind(vessel[which(vessel["W_HARB"] == 1),"LON"], vessel[which(vessel["W_HARB"] == 1),"LAT"]), col = "yellow", bg = "blue", pch = 25, cex = 1)
      colsca <- rainbow(max(vessel[, "T_NUM"]))
      textn <- 0
      for (i in 1:(nrow(vessel)-1))
      {
        trackn <- vessel[i,"T_NUM"]
        if(trackn != 0)
        {
          if(trackn == vessel[i+1,"T_NUM"])
          {
            lines(cbind(c(vessel[i,"LON"],vessel[i+1,"LON"]), c(vessel[i,"LAT"], vessel[i+1,"LAT"])), col = colsca[vessel[i,"T_NUM"]], lty = "dashed", cex = 2)
            if((vessel[i,"W_HARB"] != 1) & (trackn != textn))
            {
              text(vessel[i,"LON"], vessel[i,"LAT"], col = colsca[vessel[i,"T_NUM"]], labels = vessel[i,"T_NUM"])
              textn <- trackn
            }
          }
        }
      }            
      points(cbind(vessel[,"LON"], vessel[,"LAT"]), col = colsca[vessel[,"T_NUM"]], pch = 19, cex = 0.5)
      points(cbind(vessel[which(vessel["W_HARB"] == 1),"LON"], vessel[which(vessel["W_HARB"] == 1),"LAT"]), col = 2, pch = 19, cex = 0.5)
      span <- 0.25
    }
  }
}

####################

intrpview2 <- function(track, trackn, bathy = "", fishi = FALSE, xrange, yrange)
{
  
  span <- 0.25
  
  if(diff(xrange) != 0 & diff(yrange) != 0 )
  {
    map2plot <- try(map("worldHires",fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), plot = F), silent = T)
    if(class(map2plot) != "try-error")
    {
      map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0))
      if(bathy$path != ""){
        bathyview(bathy, xrange, yrange)
        map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), add = TRUE)
      }
      points(cbind(track[which(track["W_HARB"] == 1),"LON"], track[which(track["W_HARB"] == 1),"LAT"]), col = "yellow", bg = "blue", pch = 25, cex = 1)
      map.axes()
      title(main = paste("Vessel: ", track[1,1],"  -  Track: ",trackn, sep = ""), line = 0.3)
      title(xlab = "Lon", ylab = "Lat", line = 2)
      #               colsca <- heat.colors(ceiling(max(track[,"SPE"])))
      textn <- 0
      
      for (i in 1:(nrow(track)-1))
      {
        lines(cbind(c(track[i,"LON"], track[i+1,"LON"]), c(track[i,"LAT"], track[i+1,"LAT"])), lty = "dashed", cex = 2)
        
        if((track[i,"W_HARB"] != 1) & (trackn != textn))
        {
          text(track[i,"LON"], track[i,"LAT"], labels = track[i,"T_NUM"])
          textn <- trackn
        }
        
        if(track[i,"P_INT"] == 0)
        {
          text(track[i,"LON"], track[i,"LAT"], labels = i, pos = 2, cex = 0.8)
        }
        
      }
      points(cbind(track[,"LON"], track[,"LAT"]), pch = 19, cex = 0.5) #col = colsca[ceiling(track[,"SPE"])],
      points(cbind(track[which(track["P_INT"] == 0),"LON"], track[which(track["P_INT"] == 0),"LAT"]), pch = 19, cex = 0.5, col = 2)
      if(fishi == TRUE) {points(cbind(track[which(track["FISH"] == 1),"LON"], track[which(track["FISH"] == 1),"LAT"]), pch = 19, cex = 0.5, col = "goldenrod1")}
      points(cbind(track[which(track["W_HARB"] == 1),"LON"], track[which(track["W_HARB"] == 1),"LAT"]), col = 2, pch = 20, cex = 0.5)
    }else{
      while(class(map2plot) == "try-error")
      {
        #svalue(stats) <- "Extending Range"
        span <- span + span
        xrange <- extendrange(x = track["LON"], f = span)
        yrange <- extendrange(x = track["LAT"], f = span)
        map2plot <- try(map("worldHires",fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(0, 0, 0, 0), plot = F), silent = T)
      }
      map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0))
      if(bathy$path != ""){
        bathyview(bathy, xrange, yrange)
        map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), add = TRUE)
      }
      map.axes()
      title(main = paste("Vessel: ", track[1,1],"  -  Track: ",trackn, sep = ""), line = 0.3)
      title(xlab = "Lon", ylab = "Lat", line = 2)
      points(cbind(track[which(track["W_HARB"] == 1),"LON"], track[which(track["W_HARB"] == 1),"LAT"]), col = "yellow", bg = "blue", pch = 25, cex = 1)
      #               colsca <- heat.colors(ceiling(max(track[,"SPE"])))
      textn <- 0
      
      for (i in 1:(nrow(track)-1))
      {
        lines(cbind(c(track[i,"LON"], track[i+1,"LON"]), c(track[i,"LAT"], track[i+1,"LAT"])), lty = "dashed", cex = 2)
        
        if((track[i,"W_HARB"] != 1) & (trackn != textn))
        {
          text(track[i,"LON"], track[i,"LAT"], labels = track[i,"T_NUM"])
          textn <- trackn
        }
        
        if(track[i,"P_INT"] == 0)
        {
          text(track[i,"LON"], track[i,"LAT"], labels = i, pos = 2, cex = 0.8)
        }
        
      }
    }
    
    points(cbind(track[,"LON"], track[,"LAT"]), pch = 19, cex = 0.5) #col = colsca[ceiling(track[,"SPE"])],
    points(cbind(track[which(track["P_INT"] == 0),"LON"], track[which(track["P_INT"] == 0),"LAT"]), pch = 19, cex = 0.5, col = 2)
    if(fishi == TRUE) {points(cbind(track[which(track["FISH"] == 1),"LON"], track[which(track["FISH"] == 1),"LAT"]), pch = 19, cex = 0.5, col = "goldenrod1")}
    points(cbind(track[which(track["W_HARB"] == 1),"LON"], track[which(track["W_HARB"] == 1),"LAT"]), col = 2, pch = 20, cex = 0.5)
    span <- 0.25
  }
  
}

####################

trackview <- function(vessel, bathy = "", xrange, yrange)
{
  span <- 0.25

  if(diff(xrange) != 0 & diff(yrange) != 0 )
  {
    map2plot <- try(map("worldHires",fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), plot = F), silent = T)
    if(class(map2plot) != "try-error")
    {
      map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0))
      if(bathy$path != ""){
        bathyview(bathy, xrange, yrange)
        map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), add = TRUE)
      }
      map.axes()
      title(main = paste("Vessel ", vessel[1,1]," with ", length(unique(vessel[,"T_NUM"]))," tracks" , sep = ""), line = 0.3)
      title(xlab = "Lon", ylab = "Lat", line = 2)
      points(cbind(vessel[which(vessel["W_HARB"] == 1),"LON"], vessel[which(vessel["W_HARB"] == 1),"LAT"]), col = "yellow", bg = "blue", pch = 25, cex = 1)
      colsca <- rainbow(max(vessel[, "T_NUM"]))
      textn <- 0
      for (i in 1:(nrow(vessel)-1))
      {
        trackn <- vessel[i,"T_NUM"]
        if(trackn != 0)
        {
          if(trackn == vessel[i+1,"T_NUM"])
          {
            lines(cbind(c(vessel[i,"LON"],vessel[i+1,"LON"]), c(vessel[i,"LAT"], vessel[i+1,"LAT"])), col = colsca[vessel[i,"T_NUM"]], lty = "dashed", cex = 2)
            if((vessel[i,"W_HARB"] != 1) & (trackn != textn))
            {
              text(vessel[i,"LON"], vessel[i,"LAT"], col = colsca[vessel[i,"T_NUM"]], labels = vessel[i,"T_NUM"])
              textn <- trackn
            }
          }
        }
      }
      points(cbind(vessel[,"LON"], vessel[,"LAT"]), col = colsca[vessel[,"T_NUM"]], pch = 19, cex = 0.5)
      points(cbind(vessel[which(vessel["W_HARB"] == 1),"LON"], vessel[which(vessel["W_HARB"] == 1),"LAT"]), col = 2, pch = 19, cex = 0.5)
    }else{
      map2plot <- try(map("worldHires",fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), plot = F), silent = T)
      while(class(map2plot) == "try-error")
      {
        span <- span + span
        xrange <- extendrange(x = vessel["LON"], f = span)
        yrange <- extendrange(x = vessel["LAT"], f = span)
        map2plot <- try(map("worldHires",fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(0, 0, 0, 0), plot = F), silent = T)
      }
      map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0))
      if(bathy$path != ""){
        bathyview(bathy, xrange, yrange)
        map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), add = TRUE)
      }
      map.axes()
      title(main = paste("Vessel ", vessel[1,1]," with ", length(unique(vessel[,"T_NUM"]))," tracks" , sep = ""), line = 0.3)
      title(xlab = "Lon", ylab = "Lat", line = 2)
      points(cbind(vessel[which(vessel["W_HARB"] == 1),"LON"], vessel[which(vessel["W_HARB"] == 1),"LAT"]), col = "yellow", bg = "blue", pch = 25, cex = 1)
      colsca <- rainbow(max(vessel[, "T_NUM"]))
      textn <- 0
      for (i in 1:(nrow(vessel)-1))
      {
        trackn <- vessel[i,"T_NUM"]
        if(trackn != 0)
        {
          if(trackn == vessel[i+1,"T_NUM"])
          {
            lines(cbind(c(vessel[i,"LON"],vessel[i+1,"LON"]), c(vessel[i,"LAT"], vessel[i+1,"LAT"])), col = colsca[vessel[i,"T_NUM"]], lty = "dashed", cex = 2)
            if((vessel[i,"W_HARB"] != 1) & (trackn != textn))
            {
              text(vessel[i,"LON"], vessel[i,"LAT"], col = colsca[vessel[i,"T_NUM"]], labels = vessel[i,"T_NUM"])
              textn <- trackn
            }
          }
        }
      }            
      points(cbind(vessel[,"LON"], vessel[,"LAT"]), col = colsca[vessel[,"T_NUM"]], pch = 19, cex = 0.5)
      points(cbind(vessel[which(vessel["W_HARB"] == 1),"LON"], vessel[which(vessel["W_HARB"] == 1),"LAT"]), col = 2, pch = 19, cex = 0.5)
      span <- 0.25
    }
  }
}

####################
pingview <- function(vessel, bathy, xrange, yrange)
{
  span <- 0.25
  
  if(diff(xrange) != 0 & diff(yrange) != 0 )
  {
    map2plot <- try(map("worldHires",fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), plot = F), silent = T)
    if(class(map2plot) != "try-error")
    {
      map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0))
      if(bathy$path != ""){
        bathyview(bathy, xrange, yrange)
        map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), add = TRUE)
      }
      points(cbind(vessel[,"LON"], vessel[,"LAT"]), col = "firebrick", pch = 19, cex = 0.7)
      map.axes()
      title(main = paste("Vessel ", vessel[1,1]," with ", nrow(vessel)," pings" , sep = ""), line = 0.3)
      title(xlab = "Lon", ylab = "Lat", line = 2)
    }else{
      map2plot <- try(map("worldHires",fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), plot = F), silent = T)
      while(class(map2plot) == "try-error")
      {
        #svalue(stats) <- "Extending Range"
        span <- span + span
        xrange <- extendrange(x = vessel["LON"], f = span)
        yrange <- extendrange(x = vessel["LAT"], f = span)
        map2plot <- try(map("worldHires",fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), plot = F), silent = T)
        map.axes()
        
      }
      map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0))
      if(bathy$path != ""){
        bathyview(bathy, xrange, yrange)
        map("worldHires", fill=T,col="darkgreen",bg="cornflowerblue", ylim = yrange, xlim = xrange, mar = c(6, 6, 0, 0), add = TRUE)
      }
      points(cbind(vessel[,"LON"], vessel[,"LAT"]), col = "firebrick", pch = 19, cex = 0.6)
      map.axes()
      #title("Ping Viewer", line = 1)
      title(main = paste("Vessel ", vessel[1,1]," with ", nrow(vessel)," pings" , sep = ""), line = 0.3)
      title(xlab = "Lon", ylab = "Lat", line = 2)
      span <- 0.25
    }
  }
}