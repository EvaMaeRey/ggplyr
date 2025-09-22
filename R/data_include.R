#' @export
data_include <- function(.include, .by) {
  structure(list(include_specification = rlang::enquo(.include), 
                 by_specification = rlang::enquo(.by)), 
            class = "includeobs")
}

#' @export
ggplot_add.includeobs <- function(object, plot, object_name) {
  
  plot$unfiltered_data <- plot$unfiltered_data %||% plot$data
  
  include_data <- dplyr::filter(plot$data, 
                            !!object$include_specification, 
                            .by = !!object$by_specification)
  
  plot$data <- plot$data |> bind_rows(include_data)
  plot

}



