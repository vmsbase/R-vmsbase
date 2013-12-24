
#' Turn Widget On - Internal Function
#'  
#' 
#' The \code{turn_wdgt_on} is the internal function that implements the
#'  routine to switch on all the widgets in the list.
#' 
#' This function,  with a widgets list,
#'  turns on all the widgets in the list.
#'   
#' 
#' @param widget_list A list of widgets
#' 
#' @usage turn_wdgt_on(widget_list)
#' @seealso \code{\link{turn_wdgt_off}}
#' @name turn_wdgt_on

turn_wdgt_on <- function(widget_list)
{
  
  lapply(widget_list , FUN = function(h) {enabled(h) <- T})
  
}

#' Turn Widget Off - Internal Function
#'  
#' 
#' The \code{turn_wdgt_off} is the internal function that implements the
#'  routine to switch off all the widgets in the list.
#' 
#' This function,  with a widgets list,
#'  turns off all the widgets in the list.
#'   
#' 
#' @param widget_list A list of widgets
#' 
#' @usage turn_wdgt_off(widget_list)
#' @seealso \code{\link{turn_wdgt_on}}
#' @name turn_wdgt_off

turn_wdgt_off <- function(widget_list)
{
  
  lapply(widget_list , FUN = function(h) {enabled(h) <- F})
  
}

#' Get Widget Values - Internal Function
#'  
#' 
#' The \code{get_wdgt_vals} is the internal function that implements the
#'  routine to get the current value from all the widgets in the list.
#' 
#' This function, with a widgets list,
#'  get the current value from all the widgets in the list as a vector of characters.
#'   
#' 
#' @param widget_list A list of widgets
#' 
#' @usage get_wdgt_vals(widget_list)
#' @seealso \code{\link{set_wdgt_vals}}; \code{\link{set_wdgt_sel}}
#' @name get_wdgt_vals

get_wdgt_vals <- function(widget_list)
{
  
  as.character(sapply(widget_list, FUN = svalue))
  
}

#' Set Widget Selection - Internal Function
#'  
#' 
#' The \code{set_wdgt_sel} is the internal function that implements the
#'  routine to set the current value to all the widgets in the list.
#' 
#' This function, with a widgets list and a vector of characters,
#'  set the current selected value to all the widgets in the list,
#'  according to the order in the character vector provided.
#'   
#' 
#' @param widget_list A list of widgets
#' @param col_vals A vector of chararcters
#' 
#' @usage set_wdgt_sel(widget_list, col_vals)
#' @seealso \code{\link{get_wdgt_vals}}; \code{\link{set_wdgt_vals}}
#' @name set_wdgt_sel

set_wdgt_sel <- function(widget_list, col_vals)
{
  
  for(i in 1:length(col_vals))
  {
    svalue(widget_list[[i]]) <- col_vals[i]
  }
  
}

#' Set Widget Values - Internal Function
#'  
#' 
#' The \code{set_wdgt_vals} is the internal function that implements the
#'  routine to set available values to all the widgets in the list.
#' 
#' This function, with a widgets list and a vector of characters,
#'  assigns the vector of character values to all the widgets in the list
#'   
#' 
#' @param widget_list A list of widgets
#' @param col_vals A vector of chararcters
#' 
#' @usage set_wdgt_vals(widget_list, col_vals)
#' @seealso \code{\link{get_wdgt_vals}}; \code{\link{set_wdgt_sel}}
#' @name set_wdgt_vals

set_wdgt_vals <- function(widget_list, col_vals)
{
  
  sapply(widget_list, FUN = function(h) {h[] <- col_vals})
  
}