normal_function <- function(x){
        div <- 7

        x_mod <- max((x - min(x,9)),0)

        (x_mod - (x_mod %% div))/div

}

initial_function <- function(x){
        x_mod <- max(min(x, 9),0)
        div <- 9

        (x_mod - (x_mod %% div))/div

}

repro_function <- function(x){

        1 + initial_function(x) + normal_function(x)

}
repro_function_with_recursion <- function(x){

        if (x > 1) {
                child_mod <- repro_function_with_recursion(x - 9)
        } else {
                child_mod <- 0
        }

        initial_function(x) + normal_function(x) + child_mod

}

offset_repro_function <- function(.offset){
        function(x){
                repro_function_with_recursion(x + .offset)
        }
}

determine_population <- function(x, .lifecycle_functions){
        pop_output <- .lifecycle_functions |>
                map_dbl(meta_functional_application, x) |>
                sum()

        length(.lifecycle_functions) + pop_output
}
