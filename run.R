#!/usr/bin/env Rscript
library(glue)
source("primer.R")

find_large_random_prime <- function() {
    large_number <- trunc(runif(1, 0, 100 * 1000 * 1000 * 1000))
    print(glue("Is {large_number} prime?"))
    result = is_prime(large_number)
    print(result$result)
}

find_this_many_primes <- function(count, knowledge=empty_knowledge()) {
    n <- count
    last_prime <- 2
    while (n > 0) {
        result <- get_next_prime(last_prime, knowledge)
        last_prime <- result$next_prime
        knowledge <- result$knowledge
        n <- n - 1
        print(glue(last_prime))
    }
}

find_this_many_primes(2000)

