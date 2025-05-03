#' @export
#' 
library(ggplot2)
data_filter <- function(.keep, .by) {
  structure(list(keep_specification = rlang::enquo(.keep), 
                 by_specification = rlang::enquo(.by)), 
            class = "filterobs")
}

#' @export
ggplot_add.filterobs <- function(object, plot, object_name) {
  
  plot$unfiltered_data <- plot$unfiltered_data %||% plot$data
  
  new_data <- dplyr::filter(plot$data, 
                            !!object$keep_specification, 
                            .by = !!object$by_specification)
  plot$data <- new_data
  plot

}

data_unfilter <- function(){
    structure(list(), 
            class = "unfilterobs")
  
}


#' @export
ggplot_add.unfilterobs <- function(object, plot, object_name) {
  
  plot$data <- plot$unfiltered_data %||% plot$data
  
  plot

}

