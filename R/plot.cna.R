
#' Create hypergraphs from solution objects of \code{\link[cna]{cna}()} and related functions
#' 
#' These \code{plot()} methods draw causal hypergraphs from the solution formulas included 
#' in the objects of the following classes
#' from package \CRANpkg{cna}:
#' \itemize{
#' \item{class \dQuote{cna}, the output of \code{\link[cna]{cna}()};}
#' \item{class \dQuote{condTbl}, the output of \code{\link[cna]{condTbl}()}, 
#' \code{\link[cna]{csf}()}, \code{\link[cna]{asf}()} and \code{\link[cna]{msc}()};}
#' \item{class \dQuote{condList}, the output of \code{\link[cna]{condition}()}.}
#' }
#' 
#' @param x An object of an appropriate class.
#' @param outcome Character vector of factor names. Only solutions with one of these factors as outcome 
#' will be plotted. If \code{NULL} (default), all solutions are plotted.
#' @param what One of \code{"csf"} (default), \code{"asf"}, \code{"msc"}, determining which type of solution to 
#' extract from the \dQuote{cna} object.
#' @param \dots Additional arguments in \code{plot.condTbl()} are passed to 
#' \code{\link[causalHyperGraph]{causalHyperGraph}()}, in particular \code{show_formula}. 
#' Additional arguments in \code{plot.cna()} and \code{plot.condition()} 
#' are passed to \code{plot.condTbl()}, e.g. \code{n}, \code{outcome}, \code{ask}.
#' @inheritParams causalHyperGraph
#' 
#' @return These \code{plot()} methods return an object of class \dQuote{\link[causalHyperGraph]{causalHyperGraph}}.
#' 
#' @examples
#' library(cna)
#' 
#' ana <- cna(d.educate)
#' pl <- plot(asf(ana))
#' pl
#' 
#' plot(csf(ana))
#' plot(csf(ana), ask = FALSE)
#' 
#' plot(ana)
#' plot(ana, show_formula = TRUE)
#' plot(ana, what = "msc", n = 5)
#' 
#' plot(msc(ana), outcome = "E")
#' plot(ana, what = "msc", outcome = "E")  # same as previous
#' 
#' @name plot.cna
NULL

#' @rdname plot.cna
#' @importFrom utils head
#' @export
plot.condTbl <- function(x, outcome = NULL, n = 10, ask = nrow(x)>1, ...){
  if (!is.null(outcome) && "outcome" %in% names(x)){
    x <- x[x$outcome %in% outcome, ]
  }
  sol <- x$condition
  causalHyperGraph(sol, ask = ask, n = n, ...)
}

#' @rdname plot.cna
#' @importFrom cna csf asf msc 
#' @export
plot.cna <- function(x, what = c("csf", "asf", "msc"), ...){
  what <- match.arg(what)
  fn <- get(what, getNamespace("cna"))
  plot(fn(x), ...)
}

#' @rdname plot.cna
#' @importFrom cna as.condTbl
#' @export
plot.condList <- function(x, ...){
  plot(as.condTbl(x), ...)
}

