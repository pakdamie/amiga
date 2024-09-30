
<!-- README.md is generated from README.Rmd. Please edit that file -->

# amiga

<img src="man/figures/logo.jpeg" align="right" height="120" alt="" />

The goal of `amiga` is to check that your deSolve functions
(specifically, your equations) do not have leaks! For example,if you
write your equations wrong, you can lose individuals in the system.

## Installation

You can install the development version of amiga from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("pakdamie/amiga")
```

## Example

Let’s look at this simple SIR model. No individuals should be created or
removed from the system. Susceptibles are infected and will eventually
recover.

$$
\frac{dS}{dt} = -\beta S I
$$ $$
\frac{dI}{dt} = \beta S I - \gamma I
$$ $$
\frac{dR}{dt} =  \gamma I
$$

The first function is correct. The second function is not because we
forgot a term! The third one is a valid equation, but it’s still a WIP!

``` r
library(amiga)

SIR_Model_correct <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    dS <- -beta*S*I
    dI <- beta*S*I-gamma*I
    dR <- gamma*I
    return(list(c(dS,dI,dR)))})
  }

SIR_Model_Incorrect <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    dS <- -beta*S*I
    dI <- beta*S*I
    dR <- gamma*I
    return(list(c(dS,dI,dR)))})
}

SIR_Model_Next_Step <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    dS <- -B + beta*S*I
    dI <- beta*S*I-gamma*I
    dR <- gamma*I
    return(list(c(dS,dI,dR)))})
}
```

What does my package have right now? It can check for very simple leaks
and my plan is to give you a visualization of where the leak is.

``` r
check_leaks_fast(SIR_Model_correct)
#> No leak - you're good!
```

Ideally, we want to create automatic visualization of the compartment
models.

``` r
 extract_system_equations(SIR_Model_correct)|>
  create_adjacency_matrix()|>
  graph_adjacency_matrix()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
check_leaks_fast(SIR_Model_Incorrect)
#> There's a leak- individuals unaccounted for
```

``` r
check_leaks_fast(SIR_Model_Next_Step)
#> There's a leak- individuals unaccounted for
```

This is my first package. So bear with me.