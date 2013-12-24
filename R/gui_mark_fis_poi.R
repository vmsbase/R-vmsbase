
#' Mark Fishing Points GUI
#'  
#' 
#' The \code{gui_mark_fis_poi} function implements the graphical user interface for the
#'  Fishing Point Marking routine.
#' 
#' This function, with a VMS database and a shape file with harbours points, performs a filtered search over the whole
#'  db assigning fishing status to the vms interpolated data.
#'   
#' @param vms_db_name The path of a VMS DataBase
#' @param harb_file_name The path of a shape file with harbours point data
#' 
#'
#' @return This function does not return a value. 
#' 
#' 
#' @usage gui_mark_fis_poi(vms_db_name = "", harb_file_name = "")
#' 
#' @export gui_mark_fis_poi  
#'
#'
#'@references free text reference Pointers to the literature related to this object.



gui_mark_fis_poi <- function(vms_db_name = "", harb_file_name = "")
{
  
  vms_DB <- vms_DB$new()
  vms_DB$db <- vms_db_name
  harb <- harbCoo$new()
  harb$path = harb_file_name
  
  sta_met <- read.table(file = system.file("extdata/EU_MET_DATA.csv", package="vmsbase"),
                        sep = ";",
                        header = TRUE)
  
  
  mark_fis_poi_win <- gwindow(title = "Mark Fishing Points Tool",
                              visible = FALSE,
                              width = 600,
                              height = 350)
  
  big_g <- ggroup(horizontal = FALSE, container = mark_fis_poi_win)
  
  up_g <- ggroup(horizontal = TRUE, container = big_g)
  addSpring(up_g)
  lay_win <- glayout(container = up_g, spacing = 5)
  
  lay_win[1,1, anchor = 0] <- "Metier"
  lay_win[1,2, anchor = 0] <- "Min Vel"
  lay_win[1,3, anchor = 0] <- "Max Vel"
  lay_win[1,4, anchor = 0] <- "Min Depth"
  lay_win[1,5, anchor = 0] <- "Max Depth"
  lay_win[1,6, anchor = 0] <- "Harb Dist"
  
  for(i in 1:nrow(sta_met))
  {
    
    lay_win[i+1,1] <- as.character(sta_met[i,1])
    lay_win[i+1,2] <- gedit(text = sta_met[i,2], width = 10, container = lay_win)
    lay_win[i+1,3] <- gedit(text = sta_met[i,3], width = 10, container = lay_win)
    lay_win[i+1,4] <- gedit(text = sta_met[i,4], width = 10, container = lay_win)
    lay_win[i+1,5] <- gedit(text = sta_met[i,5], width = 10, container = lay_win)
    lay_win[i+1,6] <- gedit(text = sta_met[i,6], width = 10, container = lay_win)
    
  }
  addSpring(up_g)
  g_input <- ggroup(horizontal = TRUE, container = big_g)
  g_go <- ggroup(horizontal = TRUE, container = big_g)
  
  addSpring(g_input)
  gri_g3f4 <- ggroup(horizontal = TRUE, container = g_input)
  addSpring(gri_g3f4)
  dat_sel_f <- gframe(text = "Metier Data Source", horizontal=TRUE, container = gri_g3f4) 
  met_da_su <- gdroplist(c("VMS-LB Match", "NN Prediction"), selected = 1, container = dat_sel_f) 
  addSpring(gri_g3f4)
  enabled(gri_g3f4) <- FALSE
  
  ## VMS DB file
  vms_db_f <- gframe(text = "VMS DB file", horizontal = TRUE, container = g_input)
  addSpring(vms_db_f)
  sel_vms_f <- glabel("Select VMS DB file", container = vms_db_f)
  addSpring(vms_db_f)
  gimage(system.file("ico/folder-blue.png", package="vmsbase"), container = vms_db_f,
         handler = function(h,...){
           vms_DB$db <- gfile(text = "Select VMS DataBase file",
                              type = "open",
                              filter = list("VMS DB file" = list(patterns = c("*.vms.sqlite"))))
           
           ######################
           if(vms_DB$db != "")
           {
             svalue(sel_vms_f) <- strsplit(vms_DB$db, "/")[[1]][length(strsplit(vms_DB$db, "/")[[1]])]
             nn_tab <- as.numeric(sqldf("SELECT count(*) FROM sqlite_master WHERE type='table' AND name='nn_clas'", dbname = vms_DB$db))
             if(nn_tab == 1)
             {enabled(gri_g3f4) <- TRUE}
             
             if(harb$path != "")
             {
               enabled(b_mark_fis_poi) <- TRUE
             }
           }
         })
  gimage(system.file("ico/application-exit-5.png", package="vmsbase"), container = vms_db_f,
         handler = function(h,...){
           vms_DB$db <- ""
           enabled(b_mark_fis_poi) <- FALSE
           enabled(gri_g3f4) <- FALSE
           svalue(sel_vms_f) <- "Select VMS DB file"
         })
  addSpring(g_input)
  
  ##Harbours file
  cus_har_g <- gframe(text = "Harbours Shape File", horizontal = TRUE, container = g_input)
  addSpring(cus_har_g)
  cus_har_lab <- glabel("Select Harbours Shape File", container = cus_har_g)
  addSpring(cus_har_g)
  gimage(system.file("ico/folder-man.png", package="vmsbase"), container = cus_har_g,
         handler = function(h,...){
           harb$path <- gfile(text = "Select ShapePoints map",
                              type = "open",
                              filter = list("shp data" = list(patterns = c("*.shp"))))
           svalue(cus_har_lab) <- paste("Harbour: ", strsplit(harb$path, "/")[[1]][length(strsplit(harb$path, "/")[[1]])], sep = "")
           if(harb$path != "" & vms_DB$db != "")
           {
             enabled(b_mark_fis_poi) <- TRUE
           }
         })
  gimage(system.file("ico/application-exit-5.png", package="vmsbase"), container = cus_har_g,
         handler = function(h,...){
           harb$path <- ""
           svalue(cus_har_lab) <- "Select Harbours Shape File"
           enabled(b_mark_fis_poi) <- FALSE
         })
  addSpring(g_input)
  
  addSpring(g_go)
  b_mark_fis_poi <- gbutton(text = "Start", container = g_go, handler = function(h,...)
  {
    enabled(big_g) <- FALSE
    
    harbs <- readShapePoints(harb$path)
    
    cat("\n\n   ---   Fishing Point Analysis Started   ---\n", sep = "")
    
    sqldf("drop table if exists p_fish", dbname = vms_DB$db)
    
    sqldf("CREATE TABLE p_fish(i_id INT, F_SPE INT, F_DEP INT, F_DIS INT, FISH INT)", dbname = vms_DB$db)
    
    incee_vms <- sqldf("select distinct I_NCEE from intrp", dbname = vms_DB$db)
    
    num_vess <- nrow(incee_vms)
    
    cat("\n   -     Metier Data Source: ", svalue(met_da_su),"     -\n", sep = "")
    
    for(i in 1:num_vess)
    {
      
      cat("\nVessel: ", incee_vms[i,1], " - N.", i, " of ", num_vess, sep = "")

      if(svalue(met_da_su) == "VMS-LB Match")
      {
        match <- fn$sqldf("select vessel, track, met_des from vms_lb where vessel = `incee_vms[i,1]`", dbname = vms_DB$db)
      }else{
        match <- fn$sqldf("select I_NCEE, T_NUM, met_des from nn_clas where I_NCEE = `incee_vms[i,1]`", dbname = vms_DB$db)
        colnames(match) <- c("vessel", "track", "met_des")
      } 
      
      num_track <- nrow(match)
      
      if(num_track == 0)
      {
        
        cat(" - Skipped, VMS-LogBook Match not found!")
        next
        
      }
      
      for(k in 1:num_track)
      {
        #cat("\nTrack: ", k, " of ", num_track, sep = "")
        cat(".", sep = "")
        eff_tra <- match[k,"track"]
        sin_tra <- fn$sqldf("select * from intrp, p_depth where I_NCEE = `incee_vms[i,1]` and T_NUM = `eff_tra` and intrp.rowid = i_id", dbname = vms_DB$db)
        
        if(nrow(sin_tra) == 0){
          cat("-", sep = "")
          next
        }
        
        tra_met <- match[k,"met_des"]
        sta_met_num <- which(sta_met[,"metier"] == tra_met)
        
        sin_tra <- cbind(sin_tra, 0, 0, 0, 0)
        colnames(sin_tra)[(ncol(sin_tra)-3):ncol(sin_tra)] <- c("F_SPE", "F_DEP", "F_DIS", "FISH")
        
        #cat(" - Checking Speed... ", sep = "")
        sin_tra[which((sin_tra[,"SPE"]*0.539956803456) > sta_met[sta_met_num, "min_vel_kn"] & (sin_tra[,"SPE"]*0.539956803456) < sta_met[sta_met_num, "max_vel_kn"]), "F_SPE"] <- 1
        #cat("Depth... ", sep = "")
        sin_tra[which(sin_tra[,"DEPTH"] < sta_met[sta_met_num, "min_depth_mt"] & sin_tra[,"DEPTH"] > sta_met[sta_met_num, "max_depth_mt"]), "F_DEP"] <- 1
        #cat("Distance... ", sep = "")
        for(j in 1:nrow(sin_tra))
        {
          nea_har <- which(spDistsN1(harbs, as.numeric(sin_tra[j,c("LON","LAT")]), longlat = TRUE) > (3*1.85200))
          if(length(nea_har) != 0)
          {
            sin_tra[j, "F_DIS"] <- 1
          }
        }
        
        fis_poi <- which(sin_tra[,"F_SPE"] == 1 & sin_tra[,"F_DEP"] == 1 &  sin_tra[, "F_DIS"] == 1)
        if(length(fis_poi) != 0)
        {
          cat("+", sep = "")
          sin_tra[fis_poi, "FISH"] <- 1
        }
        
        result <- sin_tra[,c("i_id", "F_SPE", "F_DEP", "F_DIS", "FISH")]
        sqldf("insert into p_fish select * from `result`", dbname = vms_DB$db)
      }
      #       }else{
      #         cat(" - Skipped, VMS-LogBook Match not found!")
      #       }
    }
    
    cat("\n\n   ---   END Fishing Point Analysis   ---\n\n", sep = "")
    
    gconfirm("Fishing Point Analysis complete!",
             title = "Confirm",
             icon = "info",
             parent = mark_fis_poi_win,
             handler = function(h,...){dispose(mark_fis_poi_win)})
  })
  addSpring(g_go)
  enabled(b_mark_fis_poi) <- FALSE
  
  if(vms_DB$db != "")
  {
    svalue(sel_vms_f) <- strsplit(vms_DB$db, "/")[[1]][length(strsplit(vms_DB$db, "/")[[1]])]
    nn_tab <- as.numeric(sqldf("SELECT count(*) FROM sqlite_master WHERE type='table' AND name='nn_clas'", dbname = vms_DB$db))
    if(nn_tab == 1)
    {
      enabled(gri_g3f4) <- TRUE
    }
  }
  if(harb$path != "")
  {
    svalue(cus_har_lab) <- paste("Harbour: ", strsplit(harb$path, "/")[[1]][length(strsplit(harb$path, "/")[[1]])], sep = "")
  }
  if(harb$path != "" & vms_DB$db != "")
  {
    enabled(b_mark_fis_poi) <- TRUE
  }
  visible(mark_fis_poi_win) <- TRUE
}
