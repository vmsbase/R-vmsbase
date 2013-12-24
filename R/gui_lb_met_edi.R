
#' Metier Editing GUI
#' 
#' The \code{gui_lb_met_edi} function implements the graphical user interface for the
#'  editing of a LogBook Metier file
#' 
#' In this gui, with a LogBook Metier file, the user can change the simple discovery
#'  ordering of the clusters to a corresponding international metier code.
#'
#' @return This function does not return a value. 
#' After the execution, the edited names will be added to the LogBook Metier file.
#' 
#' @usage gui_lb_met_edi()
#' 
#' @export gui_lb_met_edi
#'
#'@references free text reference Pointers to the literature related to this object.





gui_lb_met_edi <- function(){
  
  lb_CLA <- log_Cla$new()
  
  cla_file <- gfile(text = "Select Metier file",
                    type = "open",
                    filter = list("Metier data" = list(patterns = c("*.rData"))))
  
  lb_CLA <- readRDS(cla_file)
  
  # temp_med <- as.data.frame(lb_CLA$data$medoids[, -which(apply(lb_CLA$data$medoids ,2,sum)==0)])
  temp_med <- as.data.frame(lb_CLA$data$medoids)
  num_clu <- nrow(temp_med)
  
  if(length(lb_CLA$options) == 1)
  {
  temp_med <- cbind(1:num_clu, 1:num_clu, temp_med)
  }else{
  temp_med <- cbind(1:num_clu, lb_CLA$options, temp_med)
  }
  colnames(temp_med)[1] <- "Cluster"
  colnames(temp_med)[2] <- "Metier"
  col_zer <- as.numeric(which(apply(temp_med[,3:ncol(temp_med)], 2, sum) == 0))+2
  temp_med <- temp_med[,-col_zer]
  fao_spe <- sub("FAO_", "", colnames(temp_med))
  
  sta_met <- read.table(file = system.file("extdata/EU_CODES_MET.csv", package="vmsbase"),
                        header = FALSE)
  
  met_edi_win <- gwindow("Metier Editing Tool", 
                         width = 700, height = 500,
                         horizontal= FALSE,
                         visible = FALSE)
  
  big_g <- ggroup(horizontal = FALSE,
                  use.scrollwindow = TRUE,
                  container = met_edi_win,
                  expand = TRUE)
  
  
  
  g_top <- ggroup(horizontal = FALSE, container = big_g, expand = TRUE)
  g_bot <- ggroup(horizontal = FALSE, container = big_g)
    
  tab_dat <- gtable(items = temp_med,
                    name = "Metier data",
                    filter.column = NULL,
                    expand = TRUE,
                    container = g_top)
  
  #font(tab_dat) <- list(size = 14)
  colnames(tab_dat) <- fao_spe
  
  #new_dro <- vector(mode = "integer", length = num_clu)
  
  for(i in 1:num_clu)
  {
    if(((i-1) %% 4) == 0 )
    {
      new_gr <- paste("g_met_", i, sep = "")
      assign(new_gr, ggroup(horizontal = TRUE, container = g_bot))
      addSpring(get(new_gr))
    }
    glabel(text = paste("Group ", i, sep = ""), container = get(new_gr))
    new_dro <- paste("d_met_", i, sep = "")
    
    assign(new_dro, 
           gdroplist(as.character(sta_met[,1]), 
                     horizontal = TRUE, 
                     container = get(new_gr)))
    
    if(length(lb_CLA$options) != 1)
    {
      aho <- get(new_dro)
      svalue(aho) <- lb_CLA$options[i]
    }
    
    addSpring(get(new_gr))

  }
  
  new_name <- array(1:nrow(temp_med))
  
  gbutton(text = "Assign New Names", container = g_bot, handler = function(h,...){
    
    for(k in 1:num_clu)
    {
      
      new_name[k] <<- as.character(svalue(get(paste("d_met_", k, sep = ""))))
      
      temp_med[k,2] <<- new_name[k]
      
      tab_dat[] <- temp_med
      
    }
  })
  
  gbutton(text = "Save Editing", container = g_bot, handler = function(h,...){
    
    lb_CLA$options <- new_name
    
    res_file <- gfile(text = "Save Metier Names Editing",
                      type = "save")
    
    saveRDS(lb_CLA, file = paste(res_file, "_ed.rData", sep = ""))
    
    gconfirm("Metier Name Editing saved",
             title = "Confirm",
             parent = met_edi_win,
             handler = function(h,...){dispose(met_edi_win)})
    
  })
  
  visible(met_edi_win) <- TRUE
  
}