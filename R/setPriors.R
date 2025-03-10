#' Set up prior values for different categories of nodes
#'
#' Generates a list of custom specifications of priors for parameters in the model.
#' It is possible to set priors for a category of parameters (e.g., intercept,
#' coefficient, sd, factor, continuous) or to set a prior for a specific
#' parameter name (optionally including brackets with indices).
#'
#' @name setPriors
#' @author Ken Kellner
#' 
#' @param intercept Prior specification for intercepts
#' @param coefficient Prior specfication for slope coefficients
#' @param sd Prior specification for random effects SDs
#' @param factor Prior specifications for slope coefficients corresponding
#'  to factor data
#' @param continuous Prior specifications for slope coefficients corresponding
#'  to continuous data
#' @param lkjShape Value of shape parameter for LKJ distribution prior, used for
#'  correlation matrix in correlated random slope and intercept models 
#' @param ... Specific parameters, optionally with brackets/indices
#'
#' @return A named list of prior specifications to be passed to the \code{priors}
#'  argument of other macros in the package, such as \code{LINPRED}.
#'
#' @seealso [nimble::dlkj_corr_cholesky] for more on the LKJ distribution
#'
#' @details
#'
#' Exact name matches including brackets/indices are used first, followed by
#' name matches without indices, followed by data type (factor/continuous)
#' followed by parameter type (intercept/coefficient/sd).
#' Arguments can be supplied as quoted code, a character string, or
#' as a list of prior components. For example, to specify the prior
#' \code{dnorm(0, sd = 10)} you could specify \code{quote(dnorm(0, sd = 10))},
#' or \code{"dnorm(0, sd = 10)"}, or \code{list("dnorm", 0, sd = 10)}.
#' 
#' @author Ken Kellner
#'
#' @examples
#' # Set a prior for intercept terms using quoted code
#' setPriors(intercept = quote(dunif(-5,5)))
#' # Instead using a character string
#' setPriors(intercept = "dunif(-5,5)")
#' # Set prior for slopes associated with factor covariates
#' setPriors(factor = quote(dnorm(0, sd = 2.5)))
#' # Set prior for a specific coefficient
#' setPriors('alpha[1]' = "dnorm(0, 3)")
#'
#' @export
setPriors <- function(intercept = quote(dnorm(0, sd = 1000)),
                      coefficient = quote(dnorm(0, sd = 1000)),
                      sd = quote(dunif(0, 100)),
                      factor = NULL,
                      continuous = NULL,
                      lkjShape = 1,
                      ...){
  # Get specific prior names
  extra <- list(...)
  # Combine everything
  out <- c(list(intercept = intercept, coefficient = coefficient, sd = sd,
                factor = factor, continuous = continuous), 
           extra)

  # Check if any outputs are numeric (indicating they were passed without quote())
  lapply(1:length(out), function(i) if(is.numeric(out[[i]])){
    stop("setPriors: Prior code for ", names(out)[i], " should be wrapped in quote()", call.=FALSE)
  })

  # Remove any null values
  out <- out[sapply(out, function(x) !is.null(x))]
  
  # Convert prior provided as list to quoted code
  out <- lapply(out, convertListToPrior)

  # Convert characters to code
  out <- lapply(out, function(x){
                  if(is.character(x)) return(str2lang(x)) else return(x)
                      })

  # Add eta/shape parameter (adding here because we don't want to check it above)
  out$lkjShape <- lkjShape

  out
}

# Convert a list of prior components into a call
# e.g. list(quote(dnorm), 0, sd = 10) results in quote(dnorm(0, sd = 10))
convertListToPrior <- function(input){
  if(!is.list(input)) return(input)
  if(is.function(input[[1]])){
    stop("convertListToPrior: Function name must be a character string or wrapped in quote()", call.=FALSE)
  }
  
  out <- input
  # Convert character strings to quoted code
  out <- lapply(out, function(x) if(is.character(x)) str2lang(x) else x)
  
  as.call(out)
}

# Remove bracket from node
removeBracket <- function(node){
  if(!hasBracket(node)) return(node)
  node[[2]] 
}

#' Match a prior from a list of prior settings
#'
#' Attempts to determine which prior to put on a parameter based on a list of settings,
#' such as the output from \code{setPriors()}. The function follows the following
#' search pattern: (1) looks for an exact match to the parameter name including brackets;
#' (2) a match to the parameter name without brackets; (3) goes through each value
#' supplied to \code{...} in order and looks for a match in the names of the
#' settings list. Once a match is found the function returns the corresponding
#' prior value.
#'
#' @name matchPrior
#' @author Ken Kellner
#' 
#' @param parName Parameter to get a prior for, as quoted code/name, possibly
#'  including brackets/indices
#' @param ... Character strings that categorize the parameter and match
#'  the names of elements in \code{priors}. The order is important:
#'  the first match found is used.
#' @param priors A named list of prior settings, e.g., as generated by
#'  \code{setPriors}
#'
#' @return NIMBLE code for the matching prior.
#'
#' @author Ken Kellner
#'
#' @examples
#' pr <- setPriors(intercept = quote(dunif(-3, 3)), 'alpha' = quote(dunif(0,1)), 
#'                 'alpha[2]' = "dnorm(0, 3)")
#' matchPrior(quote(alpha), priors=pr)
#' matchPrior(quote(alpha[2]), priors=pr)
#' matchPrior(quote(intercept), priors=pr)
#'
#' @export
matchPrior <- function(parName, ..., priors){
  
  if(is.call(priors) | is.name(priors)){
    priors <- eval(priors)
  }

  # 1. If exact prior name is specified in priors
  par_char <- safeDeparse(parName)
  possible_prior <- priors[[par_char]]
  if(!is.null(possible_prior)){
    checkValidPrior(possible_prior)
    return(possible_prior)
  }

  # 2. If prior name without brackets is specified
  par_nobracks <- safeDeparse(removeBracket(parName))
  possible_prior <- priors[[par_nobracks]]
  if(!is.null(possible_prior)){
    checkValidPrior(possible_prior)
    return(possible_prior)
  }
  
  # 3. Other possible categories in ..., in order
  par_types <- list(...)
  for (i in par_types){
    if(is.null(i) || is.na(i)) next
    if(i %in% names(priors)){
      if(is.null(priors[[i]])) next
      checkValidPrior(priors[[i]])
      return(priors[[i]])
    }
  }
  stop("matchPrior: Unable to set prior for parameter: ", parName, call.=FALSE)
}

checkValidPrior <- function(prior){
  if(!(is.call(prior) | is.name(prior))) stop("Invalid prior: ", prior, call.=FALSE)
  invisible()
}
