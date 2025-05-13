#' @export
#' 
library(ggplot2)
tag <- function(plot_name = NULL) {

  structure(
    list(plot_name_specification = plot_name), 
    class = "tag"
    )

}

ggplot_add.tag <- function(object, plot, object_name) {
  
  assign(x = object$plot_name_specification, 
         value = plot + labs(tag = object$plot_name_specification), 
         envir = .GlobalEnv)
  plot 

  }
