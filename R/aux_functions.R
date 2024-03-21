# Aux Functions causal plot
###########################


#' @importFrom cna extract_asf lhs noblanks rhs
#' @importFrom useful upper.case
#' @importFrom stringr str_split
list_of_causes <- function(x, standardize_notation = TRUE){
  # Input x is an atomic or complex solution formula.
  # Create a named list whose elements are lists of complex causes of each outcome. 
  # If standardize_notation = TRUE, labels are written in upper or lower case depending on the first letter of each factor value label.
  
  asfs <- unlist(extract_asf(noblanks(x)))
  complete_causes <- split(lhs(asfs), rhs(asfs))
  causes <- unlist(lapply(complete_causes, function(y) str_split(y,"\\+")), recursive = F)
  
  if(standardize_notation == FALSE) return(causes)
  
  causes_standardized_notation <- lapply(causes, function(x) unlist(lapply(x, function(y) 
    paste0(unlist(lapply(str_split(y, "\\*"), function(z) ifelse(upper.case(substr(z,1,1)),toupper(z),tolower(z)))), collapse="*"))))
  outcomes <- names(causes)
  outcomes_standardized_notation <- unlist(lapply(outcomes, function(x) ifelse(upper.case(substr(x,1,1)),toupper(x),tolower(x))))
  names(causes_standardized_notation) <- outcomes_standardized_notation
  
  causes_standardized_notation
  
}

#' @importFrom useful lower.case upper.case
#' @importFrom stringr str_split str_detect
cs_formula_in_DOT <- function(x, switching_factors = c()){
  # Translate a crisp-set solution formula x into a string written in DOT.
  # Elements from the list "switching_factors" create dashed nodes with tooltip label "Switching Factor".
  # If the formula contains a negative outcome literal x, x is used as a node. Otherwise, positive literals are generally used as nodes.
  
  causes <- list_of_causes(x, standardize_notation = TRUE)
  outcomes <- names(causes)
  
  lower_case_outcomes <- list()
  lower_case_outcomes <- unlist(lapply(outcomes, function(y) if(lower.case(substr(y,1,1))){c(lower_case_outcomes,y)}))
  
  formula_in_DOT <- causes
  
  for(i in 1:length(causes)){
    for(j in 1:length(causes[[i]])){
      cause <- causes[[i]][[j]]
      
      if(str_detect(cause,"\\*")){
        # Create directed hyperedge in case of complex causes:
        outcome_ID <- paste0("'", cause, " -> ", outcomes[[i]],"'")
        factor_values <- unlist(str_split(cause, "\\*"))
        upper_edge <- unlist(lapply(factor_values, function(z)
          if(upper.case(substr(z,1,1)) && !(tolower(z) %in% lower_case_outcomes)){
            paste(z, "->", outcome_ID, "[dir = none, tooltip =", outcome_ID, "]")
          }else if (lower.case(substr(z,1,1)) && !(z %in% lower_case_outcomes)){
            paste(toupper(z), "->", outcome_ID, "[dir = both, arrowtail = ediamond, arrowhead = none, tooltip =", outcome_ID, "]")
          }else if (upper.case(substr(z,1,1)) && (tolower(z) %in% lower_case_outcomes)){
            paste(tolower(z), "->", outcome_ID, "[dir = both, arrowtail = ediamond, arrowhead = none, tooltip =", outcome_ID, "]")
          }else{
            paste(z, "->", outcome_ID, "[dir = none, tooltip =", outcome_ID, "]")
          }))
        upper_edge <- paste(upper_edge, collapse = " ")
        lower_edge <- paste(outcome_ID, "->", outcomes[[i]], "[tooltip =",outcome_ID, "]")
        conjunction_node <- paste(outcome_ID, "[shape = point]")
        formula_in_DOT[[i]][[j]] <- paste(conjunction_node, upper_edge, lower_edge)
        
      }else{
        # Create directed edge in case of single factor value causes:
        edge_label <- paste0("'", cause, "->", outcomes[[i]],"'")
        edge <- if(upper.case(substr(cause,1,1)) && !(tolower(cause) %in% lower_case_outcomes)){
          paste(cause, "->", outcomes[[i]], "[tooltip =", edge_label, "]")
        }else if(lower.case(substr(cause,1,1)) && !(cause %in% lower_case_outcomes)){
          paste(toupper(cause), "->", outcomes[[i]], "[dir = both, arrowtail = ediamond, tooltip =", edge_label, "]")
        }else if(upper.case(substr(cause,1,1)) && (tolower(cause) %in% lower_case_outcomes)){
          paste(tolower(cause), "->", outcomes[[i]], "[dir = both, arrowtail = ediamond, tooltip =", edge_label, "]")
        }else{
          paste(cause, "->", outcomes[[i]], "[tooltip =", edge_label, "]")
        }
        formula_in_DOT[[i]][[j]] <- edge
      }
    }
  }
  
  formula_in_DOT <- c("node [fontname = 'Times-Italic']", formula_in_DOT)
  formula_in_DOT <- paste(unlist(formula_in_DOT), collapse = " ")
  
  if(is.null(switching_factors)) return(formula_in_DOT)
  
  # Display the nodes of elements from the list "switching_factors" with dashed lines and tooltip label "Switching Factor":
  switch_standardized <- unlist(lapply(switching_factors, function(s) ifelse(tolower(s) %in% lower_case_outcomes, tolower(s), toupper(s))))
  switch_nodes <- paste(unlist(lapply(switch_standardized, function(n) 
    paste(n, "[fontname = 'Times-Italic', style=dashed, tooltip='Switching Factor", n, "']"))), collapse = " ")
  formula_in_DOT <- paste(switch_nodes, formula_in_DOT, collapse = " ")

  formula_in_DOT
  
}

#' @importFrom stringr str_split str_detect str_extract
mv_formula_in_DOT <- function(x, switching_factors = c()){
  # Translate a multi-value solution formula x into a string written in DOT.
  # Factors from the list "switching_factors" are highlighted as dashed nodes and given the tooltip label "Switching Factor".
  
  causes <- list_of_causes(x, standardize_notation = FALSE)
  outcomes <- names(causes)
  
  formula_in_DOT <- causes
  
  for(i in 1:length(causes)){
    for(j in 1:length(causes[[i]])){
      cause <- causes[[i]][[j]]
      
      if(str_detect(cause,"\\*")){
        # Create directed hyperedge in case of complex causes:
        outcome_ID <- paste0("'", cause, " -> ", outcomes[[i]],"'")
        factor_values <- unlist(str_split(cause, "\\*"))
        upper_edge <- unlist(lapply(factor_values, function(z) 
          paste(str_extract(z, "^.*(?==)"),"->",outcome_ID, 
                "[dir = none, taillabel=", 
                str_extract(z, "(?<==)(.*)"), 
                "labeldistance = 0.7, 
                fontsize = 12,
                fontcolor = black, 
                color = grey, 
                tooltip = ",outcome_ID,
                ",tailtooltip = ",outcome_ID,
                "]")))
        upper_edge <- paste(upper_edge, collapse = " ")
        lower_edge <- paste(outcome_ID, "->", str_extract(outcomes[[i]],"^.*(?==)"), 
                            "[headlabel = ", str_extract(outcomes[[i]], "(?<==)(.*)"),
                            "labeldistance = 0.7,
                            fontsize = 12,
                            fontcolor = black, 
                            color = grey, 
                            tooltip = ",outcome_ID,
                            ",headtooltip = ",outcome_ID,
                            "]")
        conjunction_node <- paste(outcome_ID, "[shape = point]")
        formula_in_DOT[[i]][[j]] <- paste(conjunction_node, upper_edge, lower_edge)
        
      } else {
        # Create directed edge in case of single factor value causes:
        edge_label <- paste0("'", cause, " -> ", outcomes[[i]],"'")
        edge <- paste(str_extract(cause, "^.*(?==)"),"->", str_extract(outcomes[[i]],"^.*(?==)"),
                      "[taillabel = ", str_extract(cause, "(?<==)(.*)"), ", 
                      headlabel = ", str_extract(outcomes[[i]], "(?<==)(.*)"), 
                      "labeldistance = 0.7,
                      fontsize = 12,
                      fontcolor = black, 
                      color = grey, 
                      tooltip = ",edge_label,
                      ",tailtooltip = ",edge_label,
                      ",headtooltip = ",edge_label,
                      "]")
        formula_in_DOT[[i]][[j]] <- edge
      }
    }
  }
  
  formula_in_DOT <- paste(unlist(formula_in_DOT), collapse = " ")
  
  if(is.null(switching_factors)) return(formula_in_DOT)
  
  # Display factors from the list "switching_factors" as dashed nodes with tooltip label "Switching Factor":
  switch_nodes <- paste(unlist(lapply(switching_factors, function(n) 
    paste(n, "[style=dashed, tooltip='Switching Factor", n, "']"))), collapse = " ")
  formula_in_DOT <- paste(switch_nodes, formula_in_DOT, collapse = " ")
  
  formula_in_DOT
  
}


# Two auxiliary functions used in causalHyperGraph()
check_condition <- function(xi){
	stopifnot(length(xi) == 1)
	xi <- noblanks(xi)
	mv <- str_detect(xi, "\\=")
	asfs <- unlist(extract_asf(xi))
	outcomes <- rhs(asfs)
	if (mv) outcomes <- unique(sub("=[0-9]+$", "", outcomes))
	if (!all(check_names(outcomes))) return(FALSE)
	lhsfactors <- unique(toupper(hstrsplit(lhs(asfs), c("+", "*"), relist = FALSE)))
	if (mv) lhsfactors <- unique(sub("=[0-9]+$", "", lhsfactors))
	if (!all(check_names(lhsfactors))) return(FALSE)
	TRUE
}
check_names <- function(nms){
	nms <- as.character(nms)
	nms == make.names(nms, unique = TRUE) & 
		!grepl("[[:punct:][:space:]]", gsub("[\\._]+", "", nms))
}
