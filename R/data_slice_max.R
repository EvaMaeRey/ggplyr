#' @export
data_slice_max <- function(ordered_by, n = 10, by) {
  
  structure(list(ordered_by_specification = rlang::enquo(ordered_by),
                 n_specification = n, 
                 by_specification = rlang::enquo(by)),
            class = "slice_max_obs")
  
}

#' @export
ggplot_add.slice_max_obs <- function(object, plot, object_name) {
  
  new_data <- dplyr::slice_max(plot$data, 
                               order_by = !! object$ordered_by_specification, 
                               n = object$n_specification, 
                               by = !! object$by_specification)
  plot$data <- new_data
  plot

  }
