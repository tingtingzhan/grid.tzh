


#' @title Venn Diagram via \CRANpkg{VennDiagram}
#' 
#' @description
#' 
#' Minor modifications to function \link[VennDiagram]{venn.diagram}.
#' 
#' @param object see **Usage**
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
#' Function \link[VennDiagram]{venn.diagram} does handle \link[base]{list} input, 
#' but not as elegantly as function [venn.matrix()].
#'
#' @examples 
#' m = list(
#'   A = state.name[1:20], 
#'   B = state.name[2:21], 
#'   C = state.name[3:22],
#'   D = state.name[4:23]) |> venn() 
#' m |> plot()
#' library(rmd.tzh); list(
#'   '`venn`' = m
#' ) |> render_(file = 'gList')
#' @keywords internal
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
#' @importFrom scales pal_hue
#' @importFrom utils combn bibentry
#' @export venn.matrix
#' @export
venn.matrix <- function(
    object,
    ind = FALSE, 
    lty = 'blank',
    fill = pal_hue()(n = n),
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
  n <- dimy[2L]
  
  area <- .colSums(object, m = dimy[1L], n = n, na.rm = FALSE)
  
  ag1 <- c(
    list(category = paste0(colnames(object), '\n(', area, ')')),
    setNames(as.list.default(area), nm = if (n == 1L) 'area' else paste0('area', seq_len(n)))
  )
  ag2 <- if (n == 1L) {
    NULL 
  } else if (n == 2L) {
    list(cross.area = sum(rowSums(object) == dimy[2L])) 
  } else {
    fcbs <- (2:n) |>
      lapply(FUN = combn, x = n, simplify = FALSE) |> # all [c]om[b]ination indexe[s]
      do.call(what = c) # make '[f]lat'
    names(fcbs) <- paste0('n', vapply(fcbs, FUN = paste, collapse = '', FUN.VALUE = ''))
    fcbs |>
      lapply(FUN = \(i) sum(rowSums(object[, i, drop = FALSE]) == length(i)))
  }
  
  ret <- c(
    ag1, ag2, list(
      print.mode = print.mode,
      ind = ind, lty = lty, 
      fill = fill, 
      alpha = alpha, cex = cex, 
      cat.default.pos	= cat.default.pos, cat.col = cat.col, 
      cat.fontface = cat.fontface, 
      cat.cex = cat.cex, 
      ...
    )
  ) |>
    do.call(what = switch(
      n, 
      '1' = draw.single.venn, 
      '2' = draw.pairwise.venn, 
      '3' = draw.triple.venn,
      '4' = draw.quad.venn, 
      '5' = draw.quintuple.venn,
      stop('cannot draw Venn diagram for 6 or more categories')
  )) |> 
    gsub_text_label.gList(
      pattern = '^0$|^0\\%$|^0\n\\(0\\%\\)$|^0\\%\n\\(0\\)$', 
      # '0%' # print.mode = 'percent'
      # '0' # print.mode = 'raw'
      # '0%\n(0)' # print.mode = c('percent', 'raw')
      # '0\n(0%) # print.mode = c('raw', 'percent')
      # these `pattern`s are determined by \CRANpkg{VennDiagram}
      replacement = ''
    )
  
  txt <- paste0('`', colnames(object), '`', collapse = ', ') |>
    sprintf(fmt = 'Venn diagram [@Venn1880] of %s is created using <u>**`R`**</u> package <u>**`VennDiagram`**</u>.')
  
  attr(txt, which = 'bibentry') <- bibentry(
    bibtype = 'article', key = 'Venn1880',
    author = 'John Venn',
    title = 'I. On the diagrammatic and mechanical representation of propositions and reasonings',
    journal = 'The London, Edinburgh, and Dublin Philosophical Magazine and Journal of Science',
    volume = '10',
    number = '59',
    pages = '1--18',
    year = '1880',
    publisher = 'Taylor & Francis',
    doi = '10.1080/14786448008626877'
  )
  
  attr(ret, which = 'text') <- txt
  
  return(ret)
  
}













