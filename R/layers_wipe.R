#' @export
layers_wipe <- function(i = NULL) {

  structure(
    list(wipe_which = i), 
    class = "layers_wipe"
    )

}

#' @import ggplot2
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.layers_wipe <- function(object, plot, object_name) {
  
  if(is.null(object$wipe_which)){
   
    plot$layers <- list()
     
  }else{
    
  plot$layers[object$wipe_which] <- NULL

  }
  
  plot
  
}
