
#' Predict Metier GUI
#'  
#' 
#' The \code{gui_vms_met_pred} function implements the graphical user interface for the
#'  Metier Prediction
#' 
#' This function,  with a VMS database and a shape file with harbours points, performs a neural network prediction over the whole
#'  db assigning metier data to vms tracks based on a training with existing vms-lb match data
#'   given by the database.
#'   
#' @param vms_db_name The path of a VMS DataBase
#' 
#'
#' @return This function does not return a value. 
#' 
#' 
#' @usage gui_vms_met_pred(vms_db_name = "")
#' 
#' @export gui_vms_met_pred  
#'
#'
#'@references free text reference Pointers to the literature related to this object.



gui_vms_met_pred <- function(vms_db_name = "")
{
  
  vms_DB <- vms_DB$new()
  vms_DB$db <- vms_db_name
  
  
  main_win <- gwindow(title = " Metier Prediction - Interactive Interface", visible = FALSE,
                      width = 800, height= 500)
  main_g <- ggroup(horizontal = TRUE, container = main_win)
  rigth_g <- gframe(horizontal = FALSE, use.scrollwindow = FALSE, container = main_g)
  addSpring(main_g)
  left_g <- ggroup(horizontal = FALSE, use.scrollwindow = FALSE, container = main_g)
  
  #addSpring(left_g)
  one_g <- ggroup(horizontal = TRUE, container = rigth_g)
  addSpring(one_g)
  ##VMS DataBase file
  vms_db_f <- gframe(text = "VMS DB file", horizontal = TRUE, container = one_g)
  addSpring(vms_db_f)
  sel_vms_f <- glabel("Select VMS DB file", container = vms_db_f)
  addSpring(vms_db_f)
  gimage(system.file("ico/folder-blue.png", package="vmsbase"), container = vms_db_f,
         handler = function(h,...){
           vms_DB$db <- gfile(text = "Select VMS DataBase file",
                              type = "open",
                              filter = list("VMS DB file" = list(patterns = c("*.vms.sqlite"))))
           if(!is.na(vms_DB$db))
           {
             svalue(sel_vms_f) <- strsplit(vms_DB$db, "/")[[1]][length(strsplit(vms_DB$db, "/")[[1]])]
             n_ntr <- as.numeric(sqldf("select count(*) from intrp", dbname = vms_DB$db))
             if(n_ntr > 0)
             {
               svalue(n_vess) <- paste("   N. of Vessels:  ", as.numeric(sqldf("select count(distinct I_NCEE) from intrp", dbname = vms_DB$db)), sep = "")
               svalue(n_trck) <- paste("    N. of tracks:  ", as.numeric(sqldf("select count(*) from (select distinct I_NCEE, T_NUM from track)", dbname = vms_DB$db)), sep = "")
               svalue(n_matc) <- paste(" N. VMS-LB match:  ", as.numeric(sqldf("select count(*) from vms_lb", dbname = vms_DB$db)), sep = "")
               svalue(n_ping) <- paste("     N. of Pings:  ", n_ntr, sep = "")
               enabled(start_ba) <- TRUE
               enabled(two_b_g) <- TRUE
               nn_tab <- as.numeric(sqldf("SELECT count(*) FROM sqlite_master WHERE type='table' AND name='pre_nn'", dbname = vms_DB$db))
               if(nn_tab == 1)
               {
                 enabled(start_bb) <- TRUE
                 enabled(two_c_g) <- TRUE
               }
             }else{
               cat("\n\n  VMS DB error - Interpolated Pings not found!\n\n", sep = "")
             }
           }
         })
  gimage(system.file("ico/application-exit-5.png", package="vmsbase"), container = vms_db_f,
         handler = function(h,...){
           vms_DB$db <- ""
           svalue(sel_vms_f) <- "Select VMS DB file"
           enabled(start_bb) <- FALSE
           enabled(start_ba) <- FALSE
           enabled(two_b_g) <- FALSE
           enabled(two_c_g) <- FALSE
         })
  addSpring(one_g)
  addSpring(rigth_g)
  
  two_g <- gframe("Data", horizontal = FALSE, container = rigth_g)
  n_vess <- glabel("N. of Vessels:   ---", container = two_g)
  n_trck <- glabel(" N. of Tracks:   ---", container = two_g)
  n_matc <- glabel(" VMS-LB match:   ---", container = two_g)
  n_ping <- glabel("  N. of Pings:   ---", container = two_g)
  #addSpring(rigth_g)
  
  two_b_g <- gframe("Classes", horizontal = FALSE, container = rigth_g)
  bg21 <- ggroup(horizontal = TRUE, container = two_b_g)
  addSpring(bg21)
  n_clas_s <- glabel("Speed", container = bg21)
  addSpring(bg21)
  spe_sel <- gspinbutton(from = 2, to = 30, by = 1, value = 2, container = bg21)
  bg22 <- ggroup(horizontal = TRUE, container = two_b_g)
  addSpring(bg22)
  n_clas_d <- glabel("Depth", container = bg22)
  addSpring(bg22)
  dep_sel <- gspinbutton(from = 2, to = 30, by = 1, value = 2, container = bg22)
  bg23 <- ggroup(horizontal = TRUE, container = two_b_g)
  addSpring(bg23)
  n_clas_h <- glabel("Heading", container = bg23)
  addSpring(bg23)
  hea_sel <- gspinbutton(from = 2, to = 30, by = 1, value = 2, container = bg23)
  start_ba <- gbutton(text = "\nClassify Data\n", container = two_b_g, handler = function(h,...)
  {
    enabled(start_bb) <- FALSE
    enabled(start_ba) <- FALSE
    enabled(vms_db_f) <- FALSE
    enabled(two_b_g) <- FALSE
    enabled(two_c_g) <- FALSE
    
    cat("\n\n   ---   Start Data Classification   ---\n\n", sep = "")
    cat("\n   -     Configuring...     \n", sep = "")
    svalue(sup_rep) <- "Parameters\nConfiguration..."
    spe_data <- sqldf("select max(SPE), min(SPE) from track", dbname = vms_DB$db)
    dep_data <- sqldf("select max(DEPTH), min(DEPTH) from p_depth", dbname = vms_DB$db)
    
    max_spe <- ceiling(spe_data[1])
    min_spe <- 0
    
    max_dep <- 0
    min_dep <- floor(dep_data[2])
    
    vect_spe <- seq(min_spe, as.numeric(max_spe), length = svalue(spe_sel)+1)
    vect_dep <- seq(as.numeric(min_dep), max_dep, length = svalue(dep_sel)+1)
    vect_hea <- seq(-360, 360, length = svalue(hea_sel)+1)
    svalue(sup_rep) <- "Loading\nFleet Data..."
    
    poi <- sqldf("select distinct I_NCEE, T_NUM from intrp", dbname = vms_DB$db)
    numpoi <- nrow(poi)
    
    to_out <- data.frame("I_NCEE" = numeric(numpoi),
                         "T_NUM" = numeric(numpoi))
    
    to_out[,1:2] <- poi
    for(s in 1:svalue(spe_sel))
    {
      to_out <- cbind(to_out, 0)
      colnames(to_out)[ncol(to_out)] <- paste("SPE_", s, sep = "")
    }
    
    for(d in 1:svalue(dep_sel))
    {
      to_out <- cbind(to_out, 0)
      colnames(to_out)[ncol(to_out)] <- paste("DEP_", d, sep = "")
    }
    
    for(h in 1:svalue(dep_sel))
    {
      to_out <- cbind(to_out, 0)
      colnames(to_out)[ncol(to_out)] <- paste("HEA_", h, sep = "")
    }
    
    to_out <- cbind(to_out, 0)
    colnames(to_out)[ncol(to_out)] <- "M_LAT"
    to_out <- cbind(to_out, 0)
    colnames(to_out)[ncol(to_out)] <- "M_LON"
    to_out <- cbind(to_out, 0)
    colnames(to_out)[ncol(to_out)] <- "MET"
    
    incee <- sqldf("select distinct(I_NCEE) from intrp", dbname = vms_DB$db)
    cat("   -     Analyzing     -\n", sep = "")
    svalue(sup_rep) <- "Analysis\nStarted..."
    for(v in 1:nrow(incee))
    {
      cat("\n   -     Vessel: ", incee[v,1]," - ", v, " of ", nrow(incee), sep = "")
      vessel <- fn$sqldf("select * from intrp, p_depth where intrp.ROWID = i_id and I_NCEE = `incee[v,1]` order by DATE", dbname = vms_DB$db)
      svalue(sup_rep) <- paste("Analyzing...\n   ", round((100/nrow(incee))*v,1), "%", sep = "")
      if(nrow(vessel) > 0)
      {
        
        spee_t <- vessel[,"SPE"]
        spee_t <- spee_t[which((spee_t >= min_spe)&(spee_t <as.numeric(max_spe)))]
        if(length(spee_t) == 0){
          cat("\nSkipped no data!", sep = "")
          next
        }
        spe_int <- hist(spee_t, breaks = vect_spe, plot = FALSE)$count
        spe_out <- spe_int/length(spee_t)
        
        deep_t <- vessel[,"DEPTH"]
        deep_t <- deep_t[which((deep_t > as.numeric(min_dep))&(deep_t <= as.numeric(max_dep)))]
        if(length(deep_t) == 0){
          cat("\nSkipped no data!", sep = "")
          next
        }
        dee_int <- hist(deep_t, breaks = vect_dep, plot = FALSE)$count
        dep_out <- dee_int/length(deep_t)
        
        head_t <- vessel[,"HEA"]
        hea_int <- hist(head_t, breaks = vect_hea, plot = FALSE)$count
        hea_out <- hea_int/length(head_t)
        to_tr <- which(to_out[,"I_NCEE"] == incee[v,1])
        to_out[to_tr, 3:(2+svalue(spe_sel))] <- spe_out
        to_out[to_tr, (3+svalue(spe_sel)):(2+svalue(spe_sel)+svalue(dep_sel))] <- dep_out
        to_out[to_tr, ((3+svalue(spe_sel)+svalue(dep_sel))):((2+svalue(spe_sel)+svalue(dep_sel)+svalue(hea_sel)))] <- hea_out
        trks <- unique(vessel[,"T_NUM"])
        for(t in trks)
        {
          cat(".", sep = "")
          to_out[to_tr, "M_LON"] <- median(vessel[which(vessel[,"T_NUM"] == t),"LON"])
          to_out[to_tr, "M_LAT"] <- median(vessel[which(vessel[,"T_NUM"] == t),"LAT"])
        }
        metier <- fn$sqldf("select * from vms_lb where vessel = `incee[v,1]`", dbname = vms_DB$db)
        if(length(metier) > 0)
        {
          go_in <- which(to_out[,"I_NCEE"] == incee[v,1])
          whi_1 <- which(to_out[go_in,"T_NUM"] %in% metier[,2])
          if(length(whi_1) > 0 )
          {
            whi_3 <- which(metier[,"track"] %in% to_out[go_in[whi_1], 2])
            if(length(whi_3) > 0)
            {
              to_out[go_in[whi_1], "MET"] <- as.character(metier[whi_3, "met_des"])
            }
          }
        }
        
      }else{
        cat("\nNo Vessel Data - Skipping", sep = "")
      }
      rm(vessel)
    }
    
    svalue(sup_rep) <- "Updating\nDataBase..."
    
    sqldf("drop table if exists pre_nn", dbname = vms_DB$db)
    
    sqldf("CREATE TABLE pre_nn AS SELECT * FROM `to_out`", dbname = vms_DB$db)
    
    cat("\n\n   ---   Vessel Data Classification Complete!   ---\n\n", sep = "")
    
    enabled(start_bb) <- TRUE
    enabled(start_ba) <- TRUE
    enabled(vms_db_f) <- TRUE
    enabled(two_b_g) <- TRUE
    enabled(two_c_g) <- TRUE
  })
  #addSpring(rigth_g)
  
  two_c_g <- gframe("NN parameters", horizontal = FALSE, container = rigth_g)
  bgc1 <- ggroup(horizontal = TRUE, container = two_c_g)
  addSpring(bgc1)
  n_thr_h <- glabel("Metier Abundance\nThreshold", container = bgc1)
  addSpring(bgc1)
  thr_sel <- gspinbutton(from = 0.01, to = 0.1, by = 0.01, value = 0.05, container = bgc1)
  bgc2 <- ggroup(horizontal = TRUE, container = two_c_g)
  addSpring(bgc2)
  n_nHf_h <- glabel("nHf", container = bgc2)
  addSpring(bgc2)
  nHf_sel <- gspinbutton(from = 1, to = 3, by = 0.5, value = 1.5, container = bgc2)
  bgc3 <- ggroup(horizontal = TRUE, container = two_c_g)
  addSpring(bgc3)
  n_trs_h <- glabel("Tr size", container = bgc3)
  addSpring(bgc3)
  trs_sel <- gspinbutton(from = 50, to = 70, by = 1, value = 60, container = bgc3)
  bgc4 <- ggroup(horizontal = TRUE, container = two_c_g)
  addSpring(bgc4)
  n_va_h <- glabel("VA size", container = bgc4)
  addSpring(bgc4)
  va_sel <- gspinbutton(from = 10, to = 29, by = 1, value = 15, container = bgc4)
  bgc5 <- ggroup(horizontal = TRUE, container = two_c_g)
  addSpring(bgc5)
  n_step_h <- glabel("n. Step", container = bgc5)
  addSpring(bgc5)
  step_sel <- gspinbutton(from = 100, to = 1000, by = 100, value = 100, container = bgc5)
  bgc6 <- ggroup(horizontal = TRUE, container = two_c_g)
  addSpring(bgc6)
  n_show_h <- glabel("n. Show", container = bgc6)
  addSpring(bgc6)
  show_sel <- gspinbutton(from = 100, to = 1000, by = 100, value = 100, container = bgc6)
  bgc7 <- ggroup(horizontal = TRUE, container = two_c_g)
  addSpring(bgc7)
  n_mfac_h <- glabel("Min Fac", container = bgc7)
  addSpring(bgc7)
  mfac_sel <- gspinbutton(from = 1, to = 5, by = 1, value = 2, container = bgc7)
  addSpring(rigth_g)
  sup_rep <- glabel("\n\n", container = rigth_g)
  addSpring(rigth_g)
  g_go <- ggroup(horizontal = TRUE, container = rigth_g)
  addSpring(g_go)
  start_bb <- gbutton(text = "\nStart Analysis\n", container = g_go, handler = function(h,...)
  {
    enabled(start_bb) <- FALSE
    enabled(start_ba) <- FALSE
    enabled(vms_db_f) <- FALSE
    enabled(two_b_g) <- FALSE
    enabled(two_c_g) <- FALSE
    
    cat("\n\n   ---   Metier Prediction Started!   ---", sep = "")
    
    cat("\n   -     Configuring Neural Network     -", sep = "")
    svalue(sup_rep) <- "Neural Network\nConfiguration..."

    #Set parametri
    thr <- svalue(thr_sel)
    nHf <- svalue(nHf_sel)
    nTr <- svalue(trs_sel)
    nVa <- svalue(va_sel)
    ##########
    nTe <- 100-(svalue(trs_sel) + svalue(va_sel))
    nStep <- svalue(step_sel)
    nShow <- svalue(show_sel)
    minfac <- svalue(mfac_sel)
    
    LBdata <- sqldf("select * from pre_nn", dbname = vms_DB$db)
    
    if(nrow(LBdata) > 0)
    {
    #Seleziono dati per training
    tdata <- LBdata[which(LBdata[,ncol(LBdata)]!=0),3:ncol(LBdata)]
    #Quanti met, quali met
    lmet <- unique(tdata[,ncol(tdata)])
    nmet <- length(lmet)
    mets <- tdata[,ncol(tdata)]
    nvms <- nrow(tdata)
    
    #Bilancia i mestieri
    selmet <- names(table(mets))[which(table(mets)>(thr*length(mets)))]
    minrec <- minfac*min(table(mets[which(mets %in% selmet)]))
    tdatab <- tdata[0,]
    for(i in 1:length(selmet))
      tdatab <- rbind(tdatab,tdata[sample(which(mets==selmet[i]), minrec, replace = T),])
    
    mets <- tdatab[,ncol(tdatab)]
    nvms <- nrow(tdatab)
    lmet <- unique(mets)
    nmet <- length(lmet)
    
    
    #Pulizia
    #Elimina colonne nulle
    c0 <- which(apply(tdatab[,1:(ncol(tdatab)-1)], 2, sum)==0)
    if(length(c0)>0) tdatab <- tdatab[,-c0]
    #Standardizzo
    tdatab[,1:(ncol(tdatab)-1)] <- StandardizeByCol(tdatab[,1:(ncol(tdatab)-1)])
    
    #Genero Training, Validation, Test dataset
    Tr <- Va <- Te <- numeric(0)
    for(ij in 1:nmet){
      ijr <- which(tdatab[,ncol(tdatab)]==selmet[ij])
      Tr <- c(Tr,sample(ijr,floor(length(ijr)*nTr/100), replace = F))
      Va <- c(Va,sample(setdiff(ijr,Tr),floor(length(ijr)*nVa/100), replace = F))
      Te <- c(Te,setdiff(ijr,c(Tr,Va)))
    }
    
    #Genero matrici x BP
    metmat <- matrix(0,nvms,nmet)
    colnames(metmat) <- selmet
    for(ij in 1:nvms) metmat[ij,which(colnames(metmat)==mets[ij])]=1
    
    nInput <- ncol(tdatab)-1
    nOUT <- nmet
    nH <- floor(nInput*nHf)
    
    svalue(sup_rep) <- "TR - VA - TE\nConfiguration..."
    #Individua le matrici di Training
    Tr_DATA=as.matrix(tdatab[Tr,1:nInput])
    Tr_OUT=as.matrix(metmat[Tr,])
    #Individua le matrici di Validazione
    Va_DATA=as.matrix(tdatab[Va,1:nInput])
    Va_OUT=as.matrix(metmat[Va,])
    #Individua le matrici di Test
    Te_DATA=as.matrix(tdatab[Te,1:nInput])
    Te_OUT=as.matrix(metmat[Te,])
    
    
    net.start = newff(n.neurons=c(nInput,nH,nOUT),learning.rate.global=1e-2, 
                      momentum.global=0.5, error.criterium="LMS",
                      hidden.layer="sigmoid", output.layer="sigmoid", 
                      method="ADAPTgd")
    cat("\n   -     Neural Network Calibration     -\n", sep = "")
    svalue(sup_rep) <- "Neural Network\nTraining..."
    net = train(net.start, P=Tr_DATA, 
                T=Tr_OUT, 
                Pval=Va_DATA, 
                Tval=Va_OUT, 
                error.criterium="LMS", 
                report=FALSE, show.step=nStep, n.shows=nShow)
    cat("\n   -     Neural Network Prediction     -", sep = "")
    
    svalue(sup_rep) <- "Neural Network\nPredicting..."
    # Previsione
    Te_PRED <- sim(net$net,Te_DATA)
    Te_PRED <- selmet[apply(Te_PRED,1,which.max)]
    Te_OUT <- as.factor(mets[Te])
    ConfMat <- xtabs(~.,cbind(Te_PRED,Te_OUT))
    rownames(ConfMat) <- colnames(ConfMat) <- selmet
    Dg <- sum(diag(ConfMat))/sum(ConfMat)
    
    
    #Assegnazione finale
    pdata <- LBdata[which(LBdata[,ncol(LBdata)]==0),c(3:(ncol(LBdata)-1))]
    if(length(c0)>0) pdata <- pdata[,-c0]
    #Standardizzo
    pdata[,1:(ncol(pdata)-1)] <- StandardizeByCol(pdata[,1:(ncol(pdata)-1)])
    
    
    Met_PRED <- sim(net$net,pdata)
    Met_PRED <- selmet[apply(Met_PRED,1,which.max)]
    Out_Pred <- numeric(nrow(LBdata))
    Out_Pred[which(LBdata[,ncol(LBdata)]!=0)] <- LBdata[which(LBdata[,ncol(LBdata)]!=0),ncol(LBdata)]
    Out_Pred[which(Out_Pred==0)] <- Met_PRED
    Out_BP <- cbind(LBdata[,c(1:2)],Out_Pred)
    
    plotNet(net,Dg,ConfMat)
    
    cat("\n   -     DataBase Update     -", sep = "")
    
    svalue(sup_rep) <- "Updating\nDataBase..."
    
    sqldf("drop table if exists nn_clas", dbname = vms_DB$db)
    
    sqldf("CREATE TABLE nn_clas(I_NCEE INT, T_NUM INT, met_des CHAR)", dbname = vms_DB$db)
    
    sqldf("INSERT INTO nn_clas SELECT * FROM `Out_BP`", dbname = vms_DB$db)
    
    cat("\n   -     Perfect Prediction in the ", round(Dg*100,2), "% of test set     -", sep = "")
    
    cat("\n\n   ---   Metier Prediction Complete!   ---\n\n", sep = "")
    svalue(sup_rep) <- paste("Perfect Prediction\nin the ", round(Dg*100,2), "% of test set", sep = "")
    
    }else{
      
      cat("\n\n   ---   Error no Pre_NN data!   ---", sep = "")
      
    }
    
    enabled(vms_db_f) <- TRUE
    enabled(two_b_g) <- TRUE
    enabled(two_c_g) <- TRUE
    enabled(start_bb) <- TRUE
    enabled(start_ba) <- TRUE
    
  })
  enabled(start_bb) <- FALSE
  enabled(two_b_g) <- FALSE
  enabled(two_c_g) <- FALSE
  
  addSpring(g_go)
  addSpace(rigth_g, 30, horizontal = FALSE)
  
  addSpring(left_g)
  theplot <- ggraphics(width = 600, height = 400, container = left_g)
  addSpring(left_g)
  
  
  if(vms_DB$db != "")
  {
    svalue(sel_vms_f) <- strsplit(vms_DB$db, "/")[[1]][length(strsplit(vms_DB$db, "/")[[1]])]
    n_ntr <- as.numeric(sqldf("select count(*) from intrp", dbname = vms_DB$db))
    if(n_ntr > 0)
    {
      svalue(n_vess) <- paste("   N. of Vessels:  ", as.numeric(sqldf("select count(distinct I_NCEE) from intrp", dbname = vms_DB$db)), sep = "")
      svalue(n_trck) <- paste("    N. of tracks:  ", as.numeric(sqldf("select count(*) from (select distinct I_NCEE, T_NUM from track)", dbname = vms_DB$db)), sep = "")
      svalue(n_matc) <- paste(" N. VMS-LB match:  ", as.numeric(sqldf("select count(*) from vms_lb", dbname = vms_DB$db)), sep = "")
      svalue(n_ping) <- paste("     N. of Pings:  ", n_ntr, sep = "")
      enabled(start_ba) <- TRUE
      enabled(two_b_g) <- TRUE
      nn_tab <- as.numeric(sqldf("SELECT count(*) FROM sqlite_master WHERE type='table' AND name='pre_nn'", dbname = vms_DB$db))
      if(nn_tab == 1)
      {
        enabled(start_bb) <- TRUE
        enabled(two_c_g) <- TRUE
      }
    }else{
      cat("\n\n  VMS DB error - Interpolated Pings not found!\n\n", sep = "")
    }
  }
  
  visible(main_win) <- TRUE
  
}