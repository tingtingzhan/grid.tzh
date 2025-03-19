

# ?grid:::print.gList
# cannot understand why write such a function...





#' @title Plot \link[grid]{gList} Object
#' 
#' @param x a \link[grid]{gList}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @return 
#' Function [plot.gList()] does not have a returned value.
#' 
#' @note
#' We don't have `grid:::plot.gList`, as of R version 4.4.3.
#' 
#' @importFrom grid grid.newpage grid.draw
#' @export plot.gList
#' @export
plot.gList <- function(x, ...) {
  grid.newpage()
  x |> grid.draw()
}



