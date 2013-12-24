
#' @title VMS File Class
#'
#' @description  VMS File reference Class
#'\itemize{
#'\item{\code{path} character path of the vms file}
#'\item{\code{data} data.frame data from the vms file}
#'}
#'
#' @details \code{vms_File} class is a reference class for the VMS file.
#'
#' @docType class
#' @aliases vms_File-class
#'
#' @name vms_File

vms_File <- setRefClass("vms_File", 
                        fields = list(path = "character",
                                      data = "data.frame"))


#' @title VMS DataBase class 
#'
#' @description VMS DataBase reference Class
#'
#'\itemize{
#'\item{\code{dir} character path of the VMS DataBase}
#'\item{\code{db} character name of the VMS DataBase}
#'\item{\code{tab} character tables in the VMS DataBase}
#'
#'}
#' @details \code{vms_DB} class is a reference class for the VMS DataBase.
#'
#' @docType class
#' @aliases vms_DB-class
#' 
#' @name vms_DB

vms_DB <- setRefClass("vms_DB", 
                     fields = list(dir = "character",
                                   db = "character",
                                   tab = "character"))


#' @title VMS DataBase Query class 
#'
#' @description VMS DataBase Query reference Class
#'
#'\itemize{
#'\item{\code{dir} character path of the VMS DataBase}
#'\item{\code{db} character name of the VMS DataBase}
#'\item{\code{que} character query for the VMS DataBase}
#'
#'}
#' @details \code{que_vms_DB} class is a reference class for the VMS DataBase.
#'
#' @docType class
#' @aliases que_vms_DB-class
#' 
#' @name que_vms_DB

que_vms_DB <- setRefClass("que_vms_DB", 
                      fields = list(dir = "character",
                                    db = "character",
                                    que = "character"))


#' @title LogBook File Class
#'
#' @description LogBook File reference Class
#' 
#'\itemize{
#'\item{\code{path} character path of the logbook file}
#'\item{\code{data} data.frame data from the logbook file}
#'
#'}
#' @details \code{log_File} class is a reference class for the LogBook file.
#'
#' @docType class
#' @aliases log_File-class
#'
#'
#' @name log_File

log_File <- setRefClass("log_File", 
                        fields = list(path = "character", 
                                      data = "data.frame"))


#' @title LogBook DataBase class 
#'
#' @description LogBook DataBase reference Class
#' 
#'\itemize{
#'\item{\code{dir} character path of the LogBook DataBase}
#'\item{\code{db} character name of the LogBook DataBase}
#'\item{\code{tab} character tables in the LogBook DataBase}
#'
#'}
#' @details \code{log_DB} class is a reference class for the Logbook DataBase.
#'
#' @docType class
#' @aliases log_DB-class
#'
#' 
#' @name log_DB
#' 

log_DB <- setRefClass("log_DB", 
                      fields = list(dir = "character", 
                                    db = "character",
                                    tab = "character"))


#' @title LogBook Clustering class 
#'
#' @description LogBook Clustering rData file reference Class
#' 
#'\itemize{
#'\item{\code{data} character The Clustering result data}
#'\item{\code{options} character Metier manual annotation}
#'
#'}
#' @details \code{log_Cla} class is a reference class for the Logbook Clustering rData file.
#'
#' @docType class
#' @aliases log_Cla-class
#'
#' 
#' @name log_Cla
#' 

log_Cla <- setRefClass("log_Cla", fields = c("data", "options"))


#' @title Harbours Coordinates Shape File class 
#'
#' @description Harbours coordinates Shape File reference Class
#' 
#'\itemize{
#'\item{\code{path} character The Path to the harbours coordinates shape file}
#'\item{\code{data} SpatialPointsDataFrame The loaded harbours coordinates data}
#'
#'}
#' @details \code{harbCoo} class is a reference class for the Harbours Coordinates shape file.
#'
#' @docType class
#' @aliases harbCoo-class
#' 
#' @name harbCoo
#' 

harbCoo <- setRefClass("harbCoo",
                       fields = list(path = "character",
                                     data = "SpatialPointsDataFrame"))


#' @title Land Map Shape File class 
#'
#' @description Land Map Shape File reference Class
#' 
#'\itemize{
#'\item{\code{path} character The Path to the Land Map shape file}
#'\item{\code{data} SpatialPolygonsDataFrame The loaded Land Map data}
#'
#'}
#' @details \code{polymap} class is a reference class for the Land Map shape file.
#'
#' @docType class
#' @aliases polymap-class
#'
#' @param path character The Path to the Land Map shape file
#' @param data SpatialPolygonsDataFrame The loaded Land Map data
#' 
#' @name polymap
#' 

polymap <- setRefClass("polymap",
                       fields = list(path = "character",
                                     data = "SpatialPolygonsDataFrame"))


#' @title Bathymetry rData File class 
#'
#' @description Bathymetry rData File reference Class
#' 
#'\itemize{
#'\item{\code{path} character The Path to the Bathymetry rData file}
#'\item{\code{data} XYZ-matrix The loaded Bathymetry data}
#'
#'}
#' @details \code{bathymetry} class is a reference class for the Bathymetry rData file.
#'
#' @docType class
#' @aliases bathymetry-class
#'
#' 
#' @name bathymetry
#' 

bathymetry <- setRefClass("bathymetry", fields = c("path", "data"))

