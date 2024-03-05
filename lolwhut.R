#


LEAK_CHECKER_ODE(Out_In_ODE,user_states,
                 user_params,user_times
                 )

##Simple differential equation
ode_FUNC<- Out_In_ODE <- function(t, y, params){
   with(as.list(c(y,parms)),{
        a = params['a']
        dx =  -a* x
        dy =   (a *x)
        return(list(c(dx, dy)))})
}

user_states, user_params, user_times
user_states=states = c(x = 5, y = 5)
user_params= parms <- c(a = 0.5)
user_times=times <- seq(0, 10, 1)
out <- ode( states, times,  Out_In_ODE, parms)


###OK I can get the states out from the states
state_names <- c(names(states))

### Ok I just want to pull out equations...

### All functions should have the names of the derivatives in the return(list)
### so i want to get that out.

string_desolve <- paste(capture.output(Out_In_ODE),collapse="")
extraction_1 <- str_extract(string_desolve,
        "return\\(list\\(c\\([^()]+\\)\\)")

extraction_2 <- str_extract(extraction_1,
                            "(?<=c\\()[^\\(\\)]+")


extraction_2 <- gsub(" ", "", extraction_2 , fixed = TRUE)
str_splitted <- strsplit(extraction_2 ,",")[[1]]

###OK GOT THE DERIVES
str_splitted_derivs <- as.list(unlist(strsplit(extraction_2, ",")))


### NOW LET'S GET THE EQUATION FROM THE FUNCTION

deparsed <- deparse(Out_In_ODE)

for (k in seq(1,nrow(str_splitted_derivs ))){

        state_of_interest <- str_splitted_derivs[[k]]

get_rids_white <- lapply(deparsed,function(x) gsub(" ", "", x , fixed = TRUE))


Equations_1 <- lapply(get_rids_white,
                 function(x)
                         str_extract(x, "dx(.*)"))

Equations_2 <- lapply(get_rids_white,
                     function(x)
                             str_extract(x, "dy(.*)"))


Equation1 <- Equations_1[[5]]
Equation2 <- Equations_2[[6]]


###Okay the order of the list elements and the state parameters have to be IN ORDER.


equation1_df <- cbind.data.frame(equation = Equation1, state = names(states)[1] )
equation2_df <- cbind.data.frame(equation = Equation2, state = names(states)[2] )

equation_list <- list(equation1_df,equation2_df)

###Somehow turn into a matrix?

###Idea find a way to split up all the equation and then match it across everyone
x <- ysym(state_names[1])
y<- ysym(state_names[2])
a <- ysym(names(parms) [1])

###Ok one way to check is to add up all the equations...

Equations_1_Term <-ysym((str_extract(Equation1 , "(?<=\\=).*")))
Equations_2_Term <-ysym((str_extract(Equation2 , "(?<=\\=).*")))


Equation_3 <- Equations_1_Term + Equations_2_Term

matrix <- matrix(0,ncol =2, nrow = 2)

matrix[2,1] <- 0
matrix[1,2] <- 1

rownames(matrix) = colnames(matrix) <- c("x","y")

network <- graph_from_adjacency_matrix(matrix)
plot(network, size2 = 30)
a <- fortify(network)
ggnetwork::ggnetwork(a, layout = with_fr())
