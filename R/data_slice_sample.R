#' @export
data_slice_sample <- function(n = 10, by) {
  
  structure(list(n_specification = n, 
                 by_specification = rlang::enquo(by)),
                 class = "slice_sample_obs")
  
}

ggplot_add.slice_sample_obs <- function(object, plot, object_name) {
  
  new_data <- dplyr::slice_sample(plot$data, 
                               n = object$n_specification, 
                               by = !! object$by_specification)
  plot$data <- new_data
  plot

  }




