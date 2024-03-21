# graph plotting tool for CNA solution formulas.  #
# by Christoph Falk                               #
#                                                 #
# September 2023                                  #
# #################################################

#' Draw Causal Hypergraphs
#' 
#' @description
#' \code{causalHyperGraph()} draws causal hypergraphs based on solution formulas of configurational comparative methods 
#' as \CRANpkg{cna} or \CRANpkg{QCA}. In contrast to a directed acyclic graph (DAG), edges of a hypergraph
#' can connect more than two nodes. \code{chg()} is a short form/alias of \code{causalHyperGraph()}.
#' 
#' @details
#' The most common type of graph representing causal structures is
#' the directed acyclic graph (DAG) (cf. e.g. Spirtes et al. 2000).
#' Edges in DAGs connect exactly two nodes, and the edges
#' indicate the direction of causation. In contrast, 
#' edges in hypergraphs can connect more than two nodes and, thereby, represent more
#' than just the direction of causation. Causal hypergraphs represent causal complexity, that is, 
#' they depict the fact that often many causes need to be instantiated jointly, i.e. as a bundle,
#' in order for an effect to occur (which is known as \emph{causal conjunctivity}) and 
#' that different bundles of causes can bring about an effect independently of one another on alternative
#' causal routes (\emph{causal 
#' disjunctivity}). Hypergraphs are particularly useful for representing causal structures in the vein
#' of Mackie's (1974) INUS theory of causation, and modern variants thereof (Baumgartner & Falk 2023).
#' 
#' The methods designed to trace causal complexity in data are configurational 
#' comparative methods (CCMs), such as \CRANpkg{cna} (Baumgartner & Ambühl 2020) and \CRANpkg{QCA}
#' (Ragin 2008). Accordingly, the first argument of \code{causalHyperGraph()} is a character vector of  
#' CCM solution formulas in crisp-set (binary) or multi-value standard form (asf or csf).
#' CCM solution formulas are conjunctions of
#' biconditionals with minimized disjunctive normal forms (DNF) on the left-hand sides and single effects 
#' on the right-hand sides. Conjunction is expressed by "\eqn{*}", disjunction by "+", negation by changing upper case into lower case letters
#' and vice versa, conditional by "\eqn{\rightarrow}", and biconditional by "\eqn{\leftrightarrow}". Examples are \code{(A*b + c*B <-> D)*(D + E <-> F)} or 
#' \code{(A=2*B=4 + A=3*B=1 <-> C=2)*(C=2*D=3 + C=1*D=4 <-> E=3)}.
#' 
#' If the hypergraphs drawn by \code{causalHyperGraph()} have crisp-set (or fuzzy-set) factors only,
#' upper case "X" means X=1, lower case "x" stands for X=0, and "\eqn{\diamond}" expresses 
#' that the value of the factor at the tail of the edge is negated. In case of hypergraphs with
#' multi-value factors, the relevant values of the factors are displayed at the tails and heads of the directed edges.
#' In all graphs, nodes whose exiting edges are
#' joined by "\eqn{\bullet}" form a conjunction, and the tails of edges 
#' with the same head node (so-called "colliders") constitute a disjunction.
#' 
#' The arguments \code{show_formula}, \code{formula_font}, and \code{formula_spaces} control the display
#' of the solution formula below the graph. \code{show_formula} 
#' determines whether the formula is printed, and \code{formula_font} specifies a font for the formula.
#' The argument \code{formula_spaces} identifies characters that are displayed with a space around them.
#' For example,  \code{formula_spaces = c("+", "<->")} displays "+" and "\eqn{\leftrightarrow}" with a space around them.
#' \code{formula_font} and \code{formula_spaces} only have an effect if \code{show_formula} is set to its 
#' non-default value \code{TRUE}. 
#'  
#' The argument \code{n} specifies the maximal number of graphs to render. If the number
#'  of graphs is larger than \code{n},
#' only the first \code{n} graphs are drawn.
#' By means of the argument \code{ask} the rendering of the graphs can be paused. 
#' If \code{ask=TRUE}, the user is asked to hit <Return> before a new graph is drawn. 
#' If \code{ask=FALSE}, all \code{n} graphs are drawn at once.
#' 
#' Formally, \code{causalHyperGraph()} returns a list of graphs of class 
#' \dQuote{causalHyperGraph} produced using the \CRANpkg{DiagrammeR} package. 
#' Such a list contains one or more graphs.
#' 
#' The class \dQuote{causalHyperGraph} has the following methods: \code{plot()} for rendering the graphs, 
#' \code{print()} for printing the solution formulas to the console and, optionally, graph rendering,
#'  \code{c()} for concatenating several \dQuote{causalHyperGraph} objects, and
#'  \code{[]}/\code{subset()} for subsetting. By 
#'  contrast, extraction of a single list element with \code{[[]]} or \code{$} does not return anything useful.
#' 
#' Hint: Use \code{length(x)} to query the number of graphs in an object of class \dQuote{causalHyperGraph}.
#'  
#' @return \code{causalHyperGraph()} returns a list of class \dQuote{causalHyperGraph} containing one or several graphs.
#' 
#' @param x A character vector containing atomic or complex solution formulas (asf or csf) in crisp-set (binary) 
#' or multi-value standard form.
#' @param show_formula Logical; if \code{TRUE}, the formula is printed below the graph.
#' @param formula_font Character string; specifies a font for the formula. The name of any available 
#' systemfont can be used.
#' The argument is only relevant if \code{show_formula=TRUE}.
#' @param formula_spaces Character vector; the characters in this vector will be displayed with a space around them
#' in the formula. The argument is only relevant if \code{show_formula=TRUE}.
#' @param ask Logical; if \code{TRUE}, the user is asked to hit <Return> before a new graph is drawn.
#' 
#' @references
#' Baumgartner, Michael and Christoph Falk. 2023. Boolean Difference-Making: A Modern
#' Regularity Theory of Causation. \emph{The British Journal for the Philosophy of Science} 74(1):171–197.
#' 
#' Baumgartner, Michael and Mathias Ambühl. 2020. Causal Modeling with Multi-Value and Fuzzy-Set Coincidence Analysis.
#'  \emph{Political Science Research and Methods} 8:526–542.
#' 
#' Mackie, John L. 1974. \emph{The Cement of the Universe: A Study of Causation.} Oxford: Oxford University Press.
#'
#' Ragin, Charles C. 2008. \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond.} Chicago: University of Chicago Press.
#'
#' Spirtes, Peter, Clark Glymour and Richard Scheines. 2000. \emph{Causation, Prediction, and Search}. 2 ed. Cambridge: MIT Press.


#' @examples
#' library(cna)  # required for randomAsf(), randomCsf(), and allCombs()
#' 
#' x <- "(A+B<->C)*(B+D<->E)"
#' causalHyperGraph(x)
#' chg(x)
#' 
#' # Input of length > 1
#' x <- c("(A*b+a*B<->C)*(C+f<->E)", "(A*B+a*b<->C)*(C+F<->E)")
#' gr1 <- causalHyperGraph(x)
#' gr1
#' # Suppress plotting.
#' print(gr1, plot = FALSE)
#' 
#' # Outcomes can be negated.
#' chg("(A+B<->c)*(c+F<->E)")
#' 
#' # Negative outcomes that appear positively downstream are rendered
#' # with double negation in the resulting hypergraph.
#' chg("(A+B<->c)*(C+F<->E)")
#' 
#' # Random formula.
#' x <- randomCsf(6, n.asf = 2)
#' chg(x, show_formula = TRUE)
#' # Change the font of the formula.
#' chg(x, show_formula = TRUE, formula_font = "arial")
#' # Change the spacing.
#' chg(x, show_formula = TRUE, formula_spaces = c("+", "<->"))
#' 
#' # Multi-value formula.
#' x <- "(C=1*G=0 + T=1*A=0 + T=2*G=3 <-> P=1)*(P=1*M=0 + F=1 <-> D=1)"
#' causalHyperGraph(x)
#' 
#' # Random multi-value formula with 3 outcomes.
#' x <- randomCsf(allCombs(c(3,3,3,3,3,3)), n.asf = 3)
#' gr2 <- causalHyperGraph(x)
#' gr2
#' 
#' # Random multi-value formula with a random number of outcomes.  
#' y <- randomCsf(allCombs(c(3,4,3,5,3,4)))
#' gr3 <- chg(y, show_formula = TRUE)
#' gr3
#' 
#' # Concatenation.
#' gr4 <- c(gr1,gr2,gr3)
#' plot(gr4)
#' 
#' # Subsetting.
#' gr5 <- gr4[c(1,4)]
#' plot(gr5)
#' 
#' # Longer factor names.
#' x <- paste("(Test1=1*Test2=3+Test3=2 <-> Out1=2)", 
#'            "(Out1=1*TestN=5 <-> Out2=3)", 
#'            "(TestN=4+TestK=1*Out2=1 <-> Out3=5)", 
#'            sep = "*")
#' chg(x)
#' 
#' @seealso \code{\link{export_as_svg}}, and the methods described in \code{\link{plot.cna}}.
#' @name causalHyperGraph
NULL

#' @rdname causalHyperGraph
#' @importFrom cna noblanks extract_asf hstrsplit
#' @importFrom stringr str_detect
#' @importFrom DiagrammeR grViz
#' @export
causalHyperGraph <- function(x, show_formula = FALSE, 
														 formula_font = "Courier", formula_spaces = NULL, 
														 n = 10, ask = length(x) > 1){
  x <- noblanks(x)
	out <- vector("list", length(x))
	oldopt <- options(spaces = formula_spaces)
	on.exit(options(oldopt))
  for (i in seq_along(x)){
  	xi <- structure(x[[i]], class = "condString")
  	mv <- str_detect(xi, "\\=")
  	if (!check_condition(xi)) next
	  # Detect type of graph to be plotted:
	  if (mv){
	    # Translate multi-value solution formula into a string written in DOT:
	    formula_in_DOT <- mv_formula_in_DOT(xi) 
	  } else {
	    # Translate a crisp-set solution formula into a string written in DOT:
	    formula_in_DOT <- cs_formula_in_DOT(xi) 
	  }
	  # Optional: Display the input formula xi as figure caption:
	  if(show_formula == FALSE){
	    content_to_plot <- paste(
	      "digraph {graph [layout = dot] tooltip=","'", xi, "'",
	      formula_in_DOT, "}")
	  } else {
	    content_to_plot <- paste(
	    	"digraph {graph [layout = dot] tooltip=","'", xi, "'",
	    	"label=","'", format(xi), "'", 
	    	if (length(formula_font)) paste0(" fontname= '", formula_font, "'"),
	    	formula_in_DOT, "}")
	  }
	  out[[i]] <- list(
	  	condition = xi,
	  	graph = grViz(content_to_plot), 
	  	dot = content_to_plot)
  }
	names(out) <- x
  class(out) <- "causalHyperGraph"
  attr(out, "ask") <- ask
  attr(out, "n") <- n
	ok <- vapply(out, is.list, FUN.VALUE = logical(1))
	if (any(!ok)){
		warning("Improper input(s) are dropped: ", paste0(x[!ok], collapse = ", "), 
						call. = FALSE)
		out <- out[ok]
	}
  out
}

#' @rdname causalHyperGraph
#' @export
chg <- causalHyperGraph


#' @rdname causalHyperGraph
#' @param n Positive integer; specifies the maximal number of graphs to render.
#' @param print Logical; if \code{TRUE}, \code{x} is printed to the console.
#' @param \dots Arguments passed to methods.
#' @importFrom utils head
#' @export
plot.causalHyperGraph <- function(x, n = attr(x, "n"), ask = attr(x, "ask"), 
																	print = TRUE, ...){
  x0 <- x
  ask <- isTRUE(ask) && interactive()
  x <- head(x0, n)
  n0 <- length(x0)
  n1 <- length(x)
  if (n1 < n0) message("Displaying only ", n1, " of ", n0, " graphs. ",
                       "Choose a higher value of 'n' to see more graphs")
  if (print){
  	print(x, n = n, ask = ask, plot = FALSE)
  }
  for (i in seq_along(x)){
    if (ask){
    	readline(paste0("Press <Return> to see graph ", i, "/", n1, " ", sep = ""))
    }
    print(x[[i]]$graph, plot = FALSE)
  }
  invisible(x0)
}

#' @rdname causalHyperGraph
#' @param plot Logical; if \code{TRUE}, \code{x} is rendered graphically (in addition to being printed to the console).
#' @importFrom utils head
#' @export
print.causalHyperGraph <- function(x, n = attr(x, "n"), ask = attr(x, "ask"), 
																	 plot = TRUE, ...){
	x0 <- x
  ask <- isTRUE(ask) && interactive()
	x <- head(x, n)
  n0 <- length(x0)
  n1 <- length(x)
  if (n1 < n0) message("Displaying only ", n1, " of ", n0, " graphs. ",
                       "Choose a higher value of 'n' to see more graphs")
	if (length(x) == 1){
		cat("Causal graph of", sQuote(format(x[[1]]$condition)), "\n")
	} else {
		l <- length(x)
		cat(l, "causal graphs:  \n")
		if (l>0) cat(paste0("  ", format(seq_len(l), justify = "r"), ":  ", 
												format(extractCondition(x)), "\n"), sep = "")
	}
	if (plot) plot(x, n = n, ask = ask, print = FALSE, ...)
	invisible(x0)
}

#' @rdname causalHyperGraph
#' @param i A vector (integer, character or logical) indicating elements to select.
#' @export
`[.causalHyperGraph` <- function(x, i, ask = length(out)>1){
	out <- NextMethod()
	attr(out, "ask") <- ask
	attr(out, "n") <- attr(x, "n")
	class(out) <- "causalHyperGraph"
	out
}

#' @rdname causalHyperGraph
#' @export
`c.causalHyperGraph` <- function(..., n = 10, ask = length(out)>1){
	stopifnot(sapply(list(...), inherits, "causalHyperGraph"))
	out <- NextMethod()
	attr(out, "ask") <- ask
	attr(out, "n") <- n
	class(out) <- "causalHyperGraph"
	out
}

extractCondition <- function(x){
	l <- length(x)
	out <- structure(character(l), class = "condString")
	out[] <- vapply(x, "[[", "condition", FUN.VALUE = character(1), USE.NAMES = FALSE)
	out
}

