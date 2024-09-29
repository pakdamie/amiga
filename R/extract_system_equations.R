
#' Extract the system equations from a deSolve function
#'
#' By feeding in a deSolve function, you can then retrieve the state-variables
#' of interest as well as the corresponding equations related to their derivative.
#' Note: Because this function using `{ryacas}`, it automatically assumes that I
#' is a complex number. This is very annoying if you're doing SIR, so I automatically
#' change I to little i.
#'
#'
#' @param deSolve_function

#' @return A list element with the first element being the state variables
#' and the second element being a list of the equations.
#' @export
#'
#' @examples
extract_system_equations <- function(deSolve_function){



 if(inherits(deSolve_function,"function") == FALSE){
   stop("Ensure that you are giving me a function!")
 }

 #Take the deSolve function and make each line by line a list element
 #remove spaces and ensure that we use equal signs only
 deparsed_equations <- deparse(deSolve_function)
 deparsed_equations <- lapply(deparsed_equations, function(x) gsub(" ", "", x))
 deparsed_equations <- lapply(deparsed_equations, function(x) gsub("<-", "=", x))
 deparsed_equations <- lapply(deparsed_equations, function(x) gsub("I", "i", x))


 #Find the return equation
 return_equation <- lapply(deparsed_equations, function(x)
                    str_extract(x, "(?<=return\\(list\\(c\\()[^\\)]+"))

 if(length(return_equation) == 0){
   stop("Cannot find the return() of state-variables, check!")
 }

 #What line is the return equation in.
 deparsed_return_index <-which(!is.na(unlist(return_equation)))

 #Pull out the state-variables from the return equation
 state_variables <- return_equation[[deparsed_return_index]]
 state_variables_vector <- unlist(strsplit(state_variables, split = ","))

 #All equations excluding the return
 deparsed_equations_noreturn <- deparsed_equations[-deparsed_return_index]

 ### Pulls out the equations from the deSolve model
 equations_list <- NULL
 for (state in 1:length(state_variables_vector)){
   state_var <- state_variables_vector[[state]] #state variable of interest

   ###which index matches
   equation_index <- which(unlist(lapply(deparsed_equations_noreturn, function(x)
                                  str_detect(x, state_var)) == TRUE))



   #return the equation for the derivative of the state variable
   tmp_equation <- deparsed_equations_noreturn[[equation_index]]

   print(tmp_equation)
   symb_equation <- ysym(str_extract(tmp_equation, "(?<=\\=).*"))

   equations_list[[state]] <- symb_equation

 }

 if(length(state_variables_vector) != length(equations_list)) {
   stop("The number of equations and the number of state variables are not equal!")
 }

 return(list(state_variables_vector,
             equations_list))
}

