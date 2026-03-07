
# missing pieces for \link[grid]{gList} Object


# ?grid:::print.gList
# cannot understand why write such a function hahaha

# does not exist in \pkg{grid} as of R version 4.5.
#' @export
plot.gList <- function(x, ...) {
  grid.newpage()
  x |> grid.draw()
}



