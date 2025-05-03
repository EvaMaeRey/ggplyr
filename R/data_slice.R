#' @export
data_slice <- function(keep, .by) {
  
  structure(list(keep_specification = rlang::enquo(keep), 
                 by_specification = rlang::enquo(.by)), 
            class = "slice_obs")
  
}

#' @export
ggplot_add.slice_obs <- function(object, plot, object_name, .by) {
  
  new_data <- dplyr::slice(plot$data, 
                           !! object$keep_specification, 
                           .by = !! object$by_specification)
  plot$data <- new_data
  plot

  }
