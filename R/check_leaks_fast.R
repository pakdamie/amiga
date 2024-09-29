#' Check leaks in your system of ODEs fast
#'
#' If you have a system of equations that you know should
#' add up to 0: this is the simplest function for it. If there is a leak,
#' will not tell you where. Also not useless if the total population
#' is not constant.
#'
#' @param deSolve_function
#'
#' @return A message saying your equation is fine or not
#' @export
#'
#' @examples check_leaks_fast(SIR_model)
check_leaks_fast <- function(deSolve_function){

  ###Extract the necessary system of equations
  ryacas_list <- extract_system_equations(deSolve_function)
  adj_mat <- create_adjacency_matrix(ryacas_list)
  print(graph_adjacency_matrix(adj_mat))

  ###The easiest thing to do is by simply by adding all the
  ### equations together.
  summation_bucket <- 0

  for (k in seq(1: length(ryacas_list[[2]]))){
    summation_bucket  <- summation_bucket + ryacas_list[[2]][[k]]
  }

  summation_bucket <- simplify(summation_bucket)

  if(class(as_r(summation_bucket)) == 'expression'){
    cat("There's a leak- individuals unaccounted for")
  }else{
    cat("No leak - you're good!")
  }

}

