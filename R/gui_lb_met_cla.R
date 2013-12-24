
#' LogBook Metier Classification GUI
#' 
#' The \code{gui_lb_met_cla} function implements the graphical user interface
#'  for the LogBook metier classification
#' 
#' This function, with a LogBook DB and a Metier Discovery object
#'  (see \code{\link{gui_lb_met_dis}}), computes a fuzzy classification of the logbooks
#'   in the seleted db according to the data provided from the Metier Discovery object.
#'
#' @param lb_db_name The path of a LogBook DataBase
#' 
#' @return This function does not return a value.
#'  After the execution the computed result will be added to the logbook database
#'   in the 'lb_cla' table.
#' 
#' @usage gui_lb_met_cla(lb_db_name = "")
#' 
#' @export gui_lb_met_cla
#'
#'
#'@references free text reference Pointers to the literature related to this object.
#'@seealso \code{\link{gui_lb_met_dis}}


gui_lb_met_cla <- function(lb_db_name = "")
{
  lb_DB <- log_DB$new()
  lb_DB$db <- lb_db_name
  lb_CLU <- log_Cla$new()
  cla_file <- ""
  
  lb_met_cla_win <- gwindow(title = "LogBook Metier Classification Tool",
                            visible = FALSE)
  
  big_g <- ggroup(horizontal = FALSE, container = lb_met_cla_win)
  
  new_g <- ggroup(horizontal = TRUE, container = big_g)
  addSpring(new_g)
  lb_db_f <- gframe(text = "LogBook DB file", horizontal = TRUE, container = new_g)
  addSpring(lb_db_f)
  sel_lb_f <- glabel("Select LB DB file", container = lb_db_f)
  addSpring(lb_db_f)
  gimage(system.file("ico/folder-orange.png", package="vmsbase"), container = lb_db_f,
         handler = function(h,...){
           lb_DB$db <<- gfile(text = "Select LB DataBase file",
                              type = "open",
                              filter = list("LB DB file" = list(patterns = c("*.lb.sqlite"))))
           svalue(sel_lb_f) <- strsplit(lb_DB$db, "/")[[1]][length(strsplit(lb_DB$db, "/")[[1]])]
           enabled(g_input) <- TRUE
           enabled(b_met_cla) <- TRUE
         })
  gimage(system.file("ico/application-exit-5.png", package="vmsbase"), container = lb_db_f,
         handler = function(h,...){
           lb_DB$db <<- ""
           enabled(b_met_cla) <- FALSE
           enabled(g_input) <- FALSE
           svalue(sel_lb_f) <- "Select LB DB file"
         })
  addSpring(new_g)
  
  g_input <- ggroup(horizontal = TRUE, container = big_g)
  addSpring(g_input)
  met_c_f <- gframe(text = "Metier file", horizontal = FALSE, container = g_input)
  me_cl_st_g <- ggroup(horizontal = TRUE, container = met_c_f)
  glabel("Use Standard\nClassification?", container = me_cl_st_g)
  sta_cla_sel <- gradio(c("Yes", "No"), container = me_cl_st_g, horizontal = FALSE,
                        handler = function(h,...)
                          {enabled(cus_cla_g) <- !enabled(cus_cla_g)
                            enabled(b_met_cla) <- !enabled(b_met_cla)
                           if(lb_DB$db == "")
                           {
                             enabled(b_met_cla) <- FALSE
                           }
                        })
  
  cus_cla_g <- ggroup(horizontal = TRUE, container = met_c_f)
  sta_cla_lab <- glabel("Select Edited\nMetier File", container = cus_cla_g)
  gimage(system.file("ico/address-book-new-4.png", package="vmsbase"), container = cus_cla_g,
         handler = function(h,...){
           enabled(b_met_cla) <- FALSE
           cla_file <- gfile(text = "Select Metier file",
                             type = "open",
                             filter = list("Metier data" = list(patterns = c("*.rData"))))
           svalue(sta_cla_lab) <- paste(strsplit(cla_file, "/")[[1]][length(strsplit(cla_file, "/")[[1]])])
           if(lb_DB$db != "")
           {
             enabled(b_met_cla) <- TRUE
           }
         })
  enabled(cus_cla_g) <- FALSE
  
  addSpring(g_input)  
  met_c_f <- gframe(text = "Metric", horizontal = FALSE, container = g_input)
  met_cla_sel <- gradio(c("euclidean", "manhattan", "bray-curtis"), container = met_c_f, horizontal = FALSE)
  addSpring(g_input)
  addSpring(big_g)
  
  info_lab <- glabel("", container = big_g)
  
  addSpring(big_g)
  b_met_cla <- gbutton(text = "Start\nClassification", container = big_g, handler = function(h,...)
  {
    enabled(g_input) <- FALSE
    enabled(b_met_cla) <- FALSE
    
    sqldf("drop table if exists lb_cla", dbname = lb_DB$db)
    query <- "CREATE TABLE lb_cla(vessel INT, log_num INT, met_fo INT, met_des CHAR)"
    sqldf(query, dbname = lb_DB$db)
    vess <- sqldf("select distinct vessUE from elobo", dbname = lb_DB$db)
    
    svalue(info_lab) <- paste("LogBook Metier Classification Started!", sep = "")
    cat("\n\n   ---   LogBook Metier Classification Started!   ---\n\n", sep = "")
    
    risultato <- data.frame()
    num_vess <- nrow(vess)
    
    if(svalue(sta_cla_sel) == "No")
    {
      lb_CLU <- readRDS(cla_file)
      medoids <- lb_CLU$data$medoids
      options <- lb_CLU$options
    }else{
      sta_cla_fl <- read.csv2(system.file("extdata/Sta_met", package="vmsbase"))
      medoids <- sta_cla_fl[,2:ncol(sta_cla_fl)]
      colnames(medoids) = paste("FAO_", colnames(medoids), sep = "")
      options <- as.character(sta_cla_fl[,1])
    }
    
    for(i in 1:num_vess)
    {
      
      cat("\nClassifying vessel: ", vess[i,1], " - N.", i, " of ", num_vess, sep = "")
      svalue(info_lab) <- paste("Classifying vessel: ", vess[i,1], " - N.", i, " of ", num_vess, sep = "")
      lb_data <- fn$sqldf("select ROWID, * from elobo where vessUE = `vess[i,1]`", dbname = lb_DB$db)
      
      if(nrow(lb_data) > 0)
      {
      same_col <- which(colnames(lb_data) %in% colnames(medoids))
      same_col2 <- which(colnames(medoids) %in% colnames(lb_data))
      
      
      same_dat <- lb_data[,same_col]
      
      num_lb <- nrow(same_dat)    
      
      res_diss <- data.frame(vessel = vess[i,1],
                             log_num = numeric(num_lb),
                             met_fo = numeric(num_lb),
                             met_des = character(num_lb))
      
      res_diss[,4] <- NA
      #                            res_diss[1:num_lb,2] <- 1:num_lb
      
      fuzz <- 2
      
      cat("\n", num_lb, " logbooks - Calculating... ", sep = "")
      for(k in 1:num_lb)
      {
        
        if(sum(same_dat[k,]) == 0)
        {next}
        
        diss_dat <- rbind(same_dat[k,], medoids[,same_col2])
        
        diss_res <- distance(diss_dat, method = svalue(met_cla_sel))
        
        mat_res <- as.matrix(diss_res)[,1]
        
        vec_res <- as.numeric(mat_res[-1]) 
        
        memb <- numeric(length(vec_res))
        
        for(h in 1:length(vec_res))
        {
          memb[h] <- 1/sum((vec_res[h]/vec_res)^(2/(fuzz-1)))
        }
        res_diss[k,2] <- lb_data[k,"rowid"]
        res_diss[k,3] <- which.max(memb)
        if(length(options) != 0 & res_diss[k,3] != 0)
        {
          res_diss[k,4]  <- options[res_diss[k,3]]
        }
      }
      
      cat("Complete!\n\n", sep = "")
      
      sqldf("INSERT INTO lb_cla SELECT * FROM `res_diss`", dbname = lb_DB$db)
      }else{
        
        cat(" - Skipped \n", sep = "")
        
      }
    }
    svalue(info_lab) <- paste("End LogBook Metier Classification", sep = "")
    cat("\n   ---   End LogBook Metier Classification   ---\n\n", sep = "")
    
    gconfirm("LogBook Classification Complete!",
             title = "Confirm",icon = "info",
             handler = dispose(lb_met_cla_win))
    
  })
  #addSpring(g_go)
  enabled(g_input) <- FALSE
  enabled(b_met_cla) <- FALSE
  
  if(lb_DB$db != "")
  {
    svalue(sel_lb_f) <- strsplit(lb_DB$db, "/")[[1]][length(strsplit(lb_DB$db, "/")[[1]])]
    enabled(b_met_cla) <- TRUE
    enabled(g_input) <- TRUE
  }
  visible(lb_met_cla_win) <- TRUE
}