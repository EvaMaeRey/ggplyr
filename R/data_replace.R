#' @export
data_replace <- function(data = NULL) {

  structure(
    list(new_data_specification = data), 
    class = "df_replace"
    )

}

#' @export

ggplot_add.df_replace <- function(object, plot, object_name) {
  
  plot$data <- object$new_data_specification
  plot

}


