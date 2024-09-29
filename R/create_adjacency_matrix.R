#' Creates an adjacency matrix of the system of equations
#'
#' The adjacency matrix shows where the state variables are across
#' the compartmental model
#'
#' @param yacas_list List with the first element being the state variables and
#' the second element being the systems of equations in ryacas form.
#'
#' @return A matrix object
#' @export
#'
#' @examples
create_adjacency_matrix <- function(yacas_list){

  state_variable <- yacas_list[[1]]
  system_equations <- yacas_list[[2]]

  ### If n is the number of state variables, then this is a nxn matrix
  state_matrix <- matrix(0,
                         nrow =length(state_variable),
                         ncol = length(state_variable))


  rownames(state_matrix) = colnames(state_matrix) = state_variable

  for (state in seq(1, length(state_variable))){

  #Of the state that we're interested in
  state_interest <- state_variable[state]

  #Where does it appear in the system of equations
  is_there_adjacency <- lapply(system_equations ,
                                function(x)
                                    str_detect(x, state_interest))
  #If the state appears in this equation, then enter it into the matrix
  state_matrix[,state] <- c(ifelse(is_there_adjacency == TRUE, 1 ,0))

  }
 diag(state_matrix) <- 0 #No self loop!

 return(state_matrix)

}





