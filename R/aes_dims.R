# use in plot specification
#' @export
dims0 <- function(...) {
  
  varnames <- as.character(ensyms(...))
  vars <- list(...)
  listvec <- asplit(do.call(cbind, vars), 1)
  structure(listvec, varnames = varnames)

}

# use in Stat specification?
#' @export
dims_unpack <- function(x) {
  dim_reduction_vars <- x
  df <- do.call(rbind, dim_reduction_vars)
  colnames(df) <- attr(dim_reduction_vars, "varnames")
  as.data.frame(df)
  
}


#' @export
aes_dims <- function(vars) {

  structure(
    list(data_spec = data,
         vars_spec = rlang::enquo(vars)), 
    class = "aes_dims"
    )

}




#' @import ggplot2
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.aes_dims <- function(object, plot, object_name) {
  
  selected_names <- plot$data |> 
    dplyr::select(!!rlang::quo_get_expr(object$vars_spec)) |>
    names()
  
  var_syms <- rlang::syms(selected_names)

  plot + 
    aes(dims = dims0(!!!var_syms))

}

