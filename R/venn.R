


#' @title Venn Diagram via \CRANpkg{VennDiagram}
#' 
#' @description
#' 
#' Minor modifications to function \link[VennDiagram]{venn.diagram}.
#' 
#' @param object see **Usage**
#' 
#' @param palette \link[base]{character} scalar, color palette to use.
#' Default `ggplot2::ggplot` uses the default color palette \link[scales]{pal_hue}.
#' Other options are `grDevices::rainbow`.
#' 
#' @param ind,lty,fill,alpha,cex,cat.col,cat.cex,cat.fontface,print.mode,cat.default.pos,... see function \link[VennDiagram]{draw.pairwise.venn}
#' 
#' @details
#' Workhorse of function [venn()] is one of 
#' \link[VennDiagram]{draw.single.venn}, 
#' \link[VennDiagram]{draw.pairwise.venn}, 
#' \link[VennDiagram]{draw.triple.venn}, 
#' \link[VennDiagram]{draw.quad.venn}, 
#' or
#' \link[VennDiagram]{draw.quintuple.venn}.
#' 
#' @returns 
#' Function [venn()] returns a \link[grid]{gList} object. 
#' 
#' @seealso 
#' Function \link[VennDiagram]{venn.diagram} does handle \link[base]{list} input, but not as elegantly as 
#' function [venn.matrix()].
#'
#' @examples 
#' x = list(
#'   A = state.name[1:20], 
#'   B = state.name[2:21], 
#'   C = state.name[3:22],
#'   D = state.name[4:23])
#' x |> venn() |> plot()
#' x |> venn(palette = 'rainbow') |> plot()
#' @importFrom VennDiagram draw.single.venn draw.pairwise.venn draw.triple.venn draw.quad.venn draw.quintuple.venn
#' @importFrom stats setNames
#' @name venn
#' @export
venn <- function(object, ...) {
  if (!length(object)) return(invisible())
  UseMethod(generic = 'venn')
}

#' @rdname venn
#' @export venn.list
#' @export
venn.list <- function(object, ...) {
  typ <- vapply(object, FUN = typeof, FUN.VALUE = '')
  if (!all(duplicated.default(typ)[-1L])) stop('all elements of `object` must be the same typeof')
  obj <- switch(typ[1L], logical = {
    ns <- lengths(object, use.names = FALSE)
    if (!all(duplicated.default(ns)[-1L])) stop('all \'logical\' elements of `object` must be of same length')
    do.call(cbind, args = object)
  }, character =, integer =, double = { 
    # 'character'
    # 'integer', typeof \link[base]{factor}
    # 'double', some `ptid` are stored as \link[base]{numeric} 
    if (anyNA(object, recursive = TRUE)) stop('each element of `object` must not contain NA')
    if (!length(nm <- names(object)) || !all(nzchar(nm))) stop('`object` must be fully named')
    do.call(cbind, args = lapply(object, FUN = `%in%`, x = unique.default(unlist(object, use.names = FALSE))))
  }, stop(sQuote(typ[1L]), ' not supported'))
  venn.matrix(obj, ...)
}

#' @rdname venn
#' @importFrom stats complete.cases
#' @export venn.data.frame
#' @export
venn.data.frame <- function(object, ...) {
  obj <- object[complete.cases(object), ]
  venn.list(as.list.data.frame(obj), ...)
}

#' @rdname venn
#' @importFrom grDevices rainbow
#' @importFrom scales pal_hue
#' @importFrom utils combn
#' @export venn.matrix
#' @export
venn.matrix <- function(
    object,
    palette = c('ggplot', 'rainbow'),
    ind = FALSE, 
    lty = 'blank',
    fill = switch(match.arg(palette), ggplot = pal_hue(), rainbow = rainbow)(n = n_cat),
    alpha = .25,
    cex = 1,
    cat.col = fill,
    cat.fontface = 'bold',
    cat.cex = 1.2,
    print.mode = c('percent', 'raw'),
    cat.default.pos = 'outer',
    ...
) {
  if (anyNA(object)) stop('do not allow missing in \'matrix\' input for Venn diagram')
  if (typeof(object) != 'logical') stop('`object` must be binary/logical matrix')
  if (!length(object)) return(invisible())
  
  rid <- (rowSums(object) == 0) # all-FALSE rows
  if (all(rid)) return(invisible())
  if (any(rid)) {
    message('Remove ', sum(rid), ' all-FALSE row(s)')
    # must perform *before* removing all-TRUE columns
    object <- object[!rid, , drop = FALSE]
  }
  
  if (dim(object)[2L] > 1L) {
    cid <- (colSums(object) == 0) # all-FALSE columns
    if (all(cid)) return(invisible())
    if (any(cid)) {
      message('Remove all-FALSE columns ', paste(sQuote(colnames(object)[cid]), collapse = ', '))
      object <- object[, !cid, drop = FALSE]
    }
  }
  
  dimy <- dim(object)
  n_cat <- dimy[2L]
  
  area <- .colSums(object, m = dimy[1L], n = n_cat, na.rm = FALSE)
  
  ag1 <- c(
    list(category = paste0(colnames(object), '\n(', area, ')')),
    setNames(as.list.default(area), nm = if (n_cat == 1L) 'area' else paste0('area', seq_len(n_cat)))
  )
  ag2 <- if (n_cat == 1L) {
    NULL 
  } else if (n_cat == 2L) {
    list(cross.area = sum(rowSums(object) == dimy[2L])) 
  } else {
    cbs <- lapply(2:n_cat, FUN = combn, x = n_cat, simplify = FALSE) # all [c]om[b]ination indexe[s]
    fcbs <- do.call(what = c, args = cbs) # make '[f]lat'
    names(fcbs) <- paste0('n', vapply(fcbs, FUN = paste, collapse = '', FUN.VALUE = ''))
    lapply(fcbs, FUN = function(i) sum(rowSums(object[, i, drop = FALSE]) == length(i)))
  }
  
  ret0 <- do.call(what = switch(
    n_cat, 
    '1' = draw.single.venn, 
    '2' = draw.pairwise.venn, 
    '3' = draw.triple.venn,
    '4' = draw.quad.venn, 
    '5' = draw.quintuple.venn,
    stop('cannot draw Venn diagram for 6 or more categories')
  ), args = c(ag1, ag2, list(
    print.mode = print.mode,
    ind = ind, lty = lty, 
    fill = fill, 
    alpha = alpha, cex = cex, 
    cat.default.pos	= cat.default.pos, cat.col = cat.col, cat.fontface = cat.fontface, cat.cex = cat.cex, ...)))
  
  ret <- ret0 |> gsub_text_label.gList(
    pattern = '^0$|^0\\%$|^0\n\\(0\\%\\)$|^0\\%\n\\(0\\)$', 
    # '0%' # print.mode = 'percent'
    # '0' # print.mode = 'raw'
    # '0%\n(0)' # print.mode = c('percent', 'raw')
    # '0\n(0%) # print.mode = c('raw', 'percent')
    # these `pattern`s are determined by \CRANpkg{VennDiagram}
    replacement = ''
  )
  
  attr(ret, which = 'text') <- paste0('`', colnames(object), '`', collapse = ', ') |>
    sprintf(fmt = 'Venn diagram of %s is created using <u>**`R`**</u> package <u>**`VennDiagram`**</u>.')
  
  return(ret)
  
}












