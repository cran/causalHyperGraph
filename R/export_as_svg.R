
#' Export causal hypergraphs in svg format
#'
#' \code{export_as_svg()} exports the hypergraphs to svg files for editing with svg software. 
#' 
#' @param x A graph generated with \code{causalHyperGraph()}.
#' @param file Character vector specifying file names for the graphs in \code{x}, optionally with a path.
#' The file names must not include the extension ".svg", which will be appended automatically. 
#' \code{file} can be of length 1 or \code{length(x)}. If \code{length(file)<length(x)}, the name(s) in \code{file} 
#' is (are) recycled to \code{length(x)} and adjusted with \code{\link{make.unique}()}.
#' @param verbose Logical; if \code{TRUE} (default), additional information about the file names
#' is printed in the console. If \code{FALSE}, no information at all is printed.
#' @param sep Separator passed to \code{make.unique()}.
#' 
#' @details
#' Although \code{causalHyperGraph()} optimizes the placement of the nodes and edges 
#' (using \CRANpkg{DiagrammeR}) as much as possible,
#' the resulting graph may still need manual adjustments to enhance its readability. 
#' In case of multi-value graphs, for example, factor values may be packed too closely or even printed
#' on top of each other, calling for manual disentangling. Once the graphs have been exported via
#' \code{export_as_svg()} such adjustments can be made with any svg software, such as Inkscape, Adobe Illustrator, 
#' or Apache Batik. Moreover, svg files can easily be converted to pdf or other formats.
#' 
#' @return Returns the names of the generated file(s) invisibly.
#' 
#' @examples
#' library(cna)  # required for randomCsf() and allCombs()
#' 
#' # Manual crisp-set example.
#' x <- "(X+Y<->S)*(D+s*B<->E)*(A+S*B<->C)*(C+E<->F)"
#' gr1 <- causalHyperGraph(x)
#' export_as_svg(gr1, file.path(tempdir(), "csGraph"))
#' 
#' # Random multi-value example.
#' y <- randomCsf(allCombs(c(3,4,3,5,3,4)))
#' gr2 <- chg(y)
#' export_as_svg(gr2, file.path(tempdir(), "mvGraph"))
#' 
#' file.remove(file.path(tempdir(), c("csGraph.svg", "mvGraph.svg")))
#' 
#' 
#' @importFrom DiagrammeRsvg export_svg
#' @seealso \code{\link{causalHyperGraph}}, and the methods \code{\link{plot.cna}}.
#' @export
export_as_svg <- function(x, file, sep = "", verbose = TRUE){
	n <- length(x)
	filenms <- paste0(make.unique(rep(file, length.out = n), sep = sep), ".svg")
	if (verbose) msgs <- paste0(format(extractCondition(x)), "  ==>  file ", sQuote(filenms))
	for (i in seq_len(n)){
		if (verbose) message(msgs[i])
		write(export_svg(x[[i]]$graph), file = filenms[[i]])
	}
	invisible(filenms)
}

