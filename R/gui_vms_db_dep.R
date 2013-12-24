
#' VMS DB Depth Assignment GUI
#'  
#' 
#' The \code{gui_vms_db_dep} function implements the graphical user interface for the
#'  VMS Assign Depth routine.
#' 
#' This function, with a VMS database,
#'  assigns depths to VMS interpolated points according to positions on the 3D spline 
#'  of XYZ bathymetry data downloaded from NOAA servers.
#'   
#' @param vms_db_name The path of a VMS DataBase
#' 
#'   
#' @return This function does not return a value. 
#' 
#' 
#' @usage gui_vms_db_dep(vms_db_name = "")
#' 
#' @export gui_vms_db_dep
#'
#'
#'@references free text reference Pointers to the literature related to this object.




gui_vms_db_dep <- function(vms_db_name = "")
{
  vms_DB <- vms_DB$new()
  vms_DB$db <- vms_db_name
  
  vms_db_dep_win <- gwindow("VMS Data Enhancer Utility - Depth", visible = FALSE)
  
  # Assign depth
  dep_g <- gframe(horizontal = FALSE, container = vms_db_dep_win)
  dep_g2 <- ggroup(horizontal = TRUE, container = dep_g)
  addSpring(dep_g2)
  gimage(system.file("ico/go-down-3.png", package = "vmsbase"), container = dep_g2)
  proglab_dep <- glabel("Assign Depth" , container = dep_g2)
  addSpring(dep_g2)
  
  
  dep_g3 <- ggroup(horizontal = TRUE, container = dep_g)
  
  #################
  addSpring(dep_g3)
  vms_db_f <- gframe(text = "VMS DB file", horizontal = TRUE, container = dep_g3)
  addSpring(vms_db_f)
  sel_vms_f <- glabel("Select VMS DB file", container = vms_db_f)
  addSpring(vms_db_f)
  gimage(system.file("ico/folder-blue.png", package="vmsbase"), container = vms_db_f,
         handler = function(h,...){
           vms_DB$db <- gfile(text = "Select VMS DataBase file",
                              type = "open",
                              filter = list("VMS DB file" = list(patterns = c("*.vms.sqlite"))))
           svalue(sel_vms_f) <- strsplit(vms_DB$db, "/")[[1]][length(strsplit(vms_DB$db, "/")[[1]])]
           enabled(start_b) <- TRUE
         })
  gimage(system.file("ico/application-exit-5.png", package="vmsbase"), container = vms_db_f,
         handler = function(h,...){
           vms_DB$db <- ""
           enabled(start_b) <- FALSE
           svalue(sel_vms_f) <- "Select VMS DB file"
         })
  addSpring(dep_g3)
  ################à
  
  res_g <- ggroup(horizontal = TRUE, container = dep_g3)
  addSpring(res_g)
  glabel("Set Resolution", container = res_g)
  use_res <- gdroplist(1:4,
                       selected = 2, container = res_g) 
  addSpring(res_g)
  
  addSpring(dep_g)
  infolab_dep <- glabel("\n" , container = dep_g)
  addSpring(dep_g)
  
  theplot <- ggraphics(container = dep_g, width = 350, height = 350)
  
  addSpring(dep_g)
  
  start_b <- gbutton("Start\nDepth Annotation", container = dep_g, handler = function(h,...)
  {
    enabled(res_g) <- FALSE
    enabled(vms_db_f) <- FALSE
    enabled(start_b) <- FALSE
    
    if(sqldf("select count(*) from intrp", dbname = vms_DB$db) > 0)
    {
      
      sqldf("drop table if exists p_depth", dbname = vms_DB$db)
      
      sqldf("CREATE TABLE p_depth(i_id INT, vess_id INT, DEPTH REAL)", dbname = vms_DB$db)
      
      thebo <- as.numeric(sqldf("select max(LON), min(LON), max(LAT), min(LAT) from track", dbname = vms_DB$db))
      xmax <- thebo[1]
      xmin <- thebo[2]
      ymax <- thebo[3]
      ymin <- thebo[4]
      
      xrange = extendrange(c(xmax, xmin), f = 0.05)
      yrange = extendrange(c(ymax, ymin), f = 0.05)
      
      l_x <- xrange[2] - xrange[1]
      l_y <- yrange[2] - yrange[1]
      
      area <- l_x * l_y
      
      max_dim <- 0.25
      
      l_xp <- sqrt(max_dim/(l_y / l_x))
      l_yp <- sqrt(max_dim/(l_x / l_y))
      
      xblock <- ceiling(l_x / l_xp)
      yblock <- ceiling(l_y / l_yp)
      
      cat("\n---   Area divided in ", xblock * yblock, " blocks   ---\n")
      cou <- 1
      for(m in 1:yblock)
      {
        for(n in 1:xblock)
        {
          new_xmin <- xmin + (l_xp * (n - 1))
          new_xmax <- xmin + (l_xp * n)
          new_ymin <- ymin + (l_yp * (m - 1))
          new_ymax <- ymin + (l_yp * m)
          
          pings <- fn$sqldf("select ROWID, * from intrp where LON > `new_xmin` and LON < `new_xmax` and LAT > `new_ymin` and LAT < `new_ymax`", dbname = vms_DB$db)
          
          if(nrow(pings) == 0)
          {
            #cat("\nSkipped block [", m, ",", n, "]\n", sep = "")
            svalue(infolab_dep) <- paste("Skipped block [", m, ",", n, "]\n",
                                         "N. ",cou, " of " ,xblock * yblock, " blocks", sep = "")
            cou = cou + 1
            next
          }
          cat("\nAnalyzing block [", m, ",", n, "]\n", sep = "")
          svalue(infolab_dep) <- paste("Analyzing block [", m, ",", n, "]\n",
                                       "N. ",cou, " of " ,xblock * yblock, " blocks", sep = "")
          
          bat_blo <- getNOAA.bathy(new_xmin-0.1,
                                   new_xmax+0.1,
                                   new_ymin-0.1,
                                   new_ymax+0.1, resolution = svalue(use_res))
          plot(bat_blo, image = T)
          points(pings[,"LON"], pings[,"LAT"], pch = 20, col = "firebrick")
          
          xlon <- rep(as.numeric(rownames(bat_blo)),length(as.numeric(colnames(bat_blo))))
          ylat <- rep(as.numeric(colnames(bat_blo)),each=length(as.numeric(rownames(bat_blo))))
          zdep <- as.numeric(bat_blo)
          cat("Calculating Spln\n", sep = "")
          SplineD <- Tps(cbind(xlon, ylat), zdep, lon.lat=TRUE)
          rm(bat_blo, zdep, xlon, ylat)
          cat("Predicting dpt", sep = "")
          if(nrow(pings)<= 10000){
            cat(" - ", sep = "")
            dept <- as.numeric(predict(SplineD, pings[,c("LON","LAT")]))
            dep_v <- as.data.frame(cbind(pings[,"rowid"], pings[,"I_NCEE"], dept))
            sqldf("insert into p_depth select * from `dep_v`", dbname = vms_DB$db)
            rm(dept, dep_v)
          }else{
            nPin <- ceiling(nrow(pings)/10000)
            for(pi in 1:nPin)
            {
              cat(".", sep = "")
              r1 <- 10000*(pi-1)+1
              r2 <- min(nrow(pings),r1+10000-1)
              dept <- as.numeric(predict(SplineD, pings[r1:r2,c("LON","LAT")]))
              dep_v <- as.data.frame(cbind(pings[r1:r2,"rowid"], pings[r1:r2,"I_NCEE"], dept))
              sqldf("insert into p_depth select * from `dep_v`", dbname = vms_DB$db)
              rm(dept, dep_v)
            }            
          }
          cat(" - Completed!\n", sep = "")
          rm(SplineD)
          cou <- cou + 1
        }
      }
      cat("\n\n   ---   End Assign Depth   ---\n", sep = "")
      
      gconfirm("VMS DB Depth Annotation Completed!",
               title = "Confirm",
               icon = "info",
               parent = vms_db_dep_win,
               handler = function(h,...){dispose(vms_db_dep_win)})
    }else{
      gconfirm("Interpolated track data not available\n\nRun Interpolation first!",
               title = "Error",
               icon = "error",
               parent = vms_db_dep_win,
               handler = function(h,...){dispose(vms_db_dep_win)})
    }  
  })
  enabled(start_b) <- FALSE
  
  if(vms_DB$db != "")
  {
    svalue(sel_vms_f) <- strsplit(vms_DB$db, "/")[[1]][length(strsplit(vms_DB$db, "/")[[1]])]    
    enabled(start_b) <- TRUE
  } 
  
  visible(vms_db_dep_win) <- TRUE
}