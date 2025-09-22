#' @export
aes_from_data <- function() {

  structure(
    list(data_specification = data), 
    class = "aes_from_data"
    )

}

#' @import ggplot2
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.aes_from_data <- function(object, plot, object_name) {
  
  plot + aes_all(plot$data |> names())

}
