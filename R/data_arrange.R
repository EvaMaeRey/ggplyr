#' @export
data_arrange <- function(arrange) {
  structure(list(arrange_specification = rlang::enquo(arrange)), 
            class = "arrange_obs")
}

ggplot_add.arrange_obs <- function(object, plot, object_name) {
  
  new_data <- dplyr::arrange(plot$data, 
                            !! object$arrange_specification)
  plot$data <- new_data
  plot

}
