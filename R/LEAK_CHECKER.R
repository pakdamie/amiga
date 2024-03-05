LEAK_CHECKER_ODE <- function(ode_FUNC, user_states, user_params, user_times){

        state_names <- c(names(user_states)) ###Gives me the state names
        deparsed <- deparse(ode_FUNC) #deparse the ode_FUNCN

        ###get rid of all the spaces
        deparsed <- lapply(deparsed, function(x) gsub(" ", "", x))
        ###turns all <- into =
        deparsed <- lapply(deparsed, function(x) gsub("<-", "=", x))

        deparsed_return <- lapply(deparsed, function(x) str_extract(x, "return\\(.*?\\(([^(),]+),([^(),]+)\\)\\)"))
        deparsed_return_index <- which((is.na(unlist(deparsed_return ))!= FALSE) == FALSE)
        return_string <- unlist(deparsed_return[!sapply(deparsed_return, function(x) all(is.na(x)))])

        return_string <-  gsub("^return\\(.*?c\\(|\\))$", "", return_string)

        ###Here are the derivative names
        deriv_vectors <- unlist(strsplit(return_string, ","))

        deparsed_equations_only <- deparsed[-deparsed_return_index]

        equation_list = NULL
        for (i in seq(1,length(deriv_vectors))){

                deriv_interest <- deriv_vectors[i] #the derivative of interest
                ###which index matches
                equation_index <- which(unlist(lapply(deparsed_equations_only,
                       function(x)
                               str_detect(x, deriv_interest))) == TRUE)

                tmp_equation<-  unlist(deparsed_equations_only[equation_index])
                symb_equation <- ysym(str_extract(tmp_equation, "(?<=\\=).*"))
                equation_list[[i]] <-symb_equation

        }



        state_matrix <- matrix(0, nrow =length(state_names), ncol = length(state_names))
        rownames(state_matrix) = colnames(state_matrix) = state_names

                for (state in seq(1, length(state_names))){
                    state_interest <-  state_names[state]
                     true_adjacency <- unlist(lapply(equation_list,
                              function(x)
                              str_detect(x, state_of_interest)))

                     state_matrix[,state] <- c(ifelse(true_adjacency == TRUE, 1 ,0))

                     }
               diag(state_matrix) <- 0


        igraph_df <- graph_from_adjacency_matrix(state_matrix)

         plot( as.undirected(igraph_df))

         summation_bucket <- 0
         for (k in seq(1, length( equation_list))){
                 summation_bucket  =   summation_bucket +  equation_list[[k]]

         }

         if( class(as_r(summation_bucket))=='expression'){
                 cat("IT's LEAKING")
         }else{
                 cat("IT'S FINE ")
         }
}


