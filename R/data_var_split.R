#' @export
data_var_split <- function(var, sep = "; ", levels = NULL) {
  
  structure(list(var_specification = rlang::enquo(var),
                 var_name = deparse(substitute(var)),
                 levels = levels,
                 sep_specification = sep),
            class = "data_var_split")
  
}

#' @export
ggplot_add.data_var_split <- function(object, plot, object_name) {
  

  plot$data  <- plot$data  |> 
    dplyr::mutate("{ object$var_name }" := 
             stringr::str_split(!! object$var_specification,
                                object$sep_specification)) |>
    tidyr::unnest(!! object$var_specification)
  
  if(!is.null(object$levels)){
    
    plot$data <- plot$data |>
      dplyr::mutate("{ object$var_name }" := 
             factor(!! object$var_specification,
                                levels = object$levels))
    
  }
  
  plot

}
