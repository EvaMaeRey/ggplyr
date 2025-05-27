#' @export
intercept <- function (plot_name = NULL) {
    structure(list(plot_name_specification = plot_name), class = "intercept")
}

#' @export
ggplot_add.intercept <- function (object, plot, object_name) {
  
  objects <- ls(envir = .GlobalEnv)
  
  is_ggplot2 <- c()
  
  for (i in 1:length(objects)){
    
    is_ggplot2[i] <- is.ggplot(get(objects[i])) 
    
  }
  
  count <- sum(is_ggplot2) + 1
  
  plot_name <- paste0("p", count)

  if(!is.null(object$plot_name_specification)){plot_name <- object$plot_name_specification}
  
    assign(x = plot_name, value = plot, 
           envir = .GlobalEnv)
    
    message(plot_name)
    
    plot
}


