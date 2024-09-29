
extract_system_equations <- function(deSolve_function,
                                    user_params,
                                    user_times){

 if(inherits(deSolve_function,"function") == FALSE){
   stop("Ensure that you are giving me a function!")
 }

 #Take the deSolve function and make each line by line a list element
 #remove spaces and ensure that we use equal signs only
 deparsed_equations <- deparse(deSolve_function)

 ###

 #Pull out the state-variables from the equations
 state_variables <- lapply(deparsed_equations, function(x)
                          str_extract(x, "(?<=return\\(list\\(c\\()[^\\)]+")) |>
                          unlist(use.names = FALSE)


 state_variables <- as.character(state_variables[!is.na(state_variables)])


 if(is.na(state_variables) == TRUE){
   stop("Cannot find the state-variables, check!")
 }

 state_variables_list <- as.list(unlist(strsplit(state_variables, ",")))

 for (state in 1:length(state_variables_list)){
   state_var <- state_variables_list[[state]]

   print(state_var)
   ###which index matches
   equation_index <- which(unlist(lapply(deparsed_equations, function(x)
                              str_detect(x, state_var)) == TRUE))


   return(deparsed_equations)

 }



}
