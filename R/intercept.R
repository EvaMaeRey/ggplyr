#' @export
#' 
library(ggplot2)
intercept <- function(plot_name = NULL) {

  structure(
    list(plot_name_specification = plot_name), 
    class = "intercept"
    )

}

ggplot_add.intercept <- function(object, plot, object_name) {
  
  assign(x = object$plot_name_specification, 
         value = plot, envir = .GlobalEnv)
  plot

  }
