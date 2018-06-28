library(glue)

empty_knowledge <- function() {
    return(list(
        "primes" = c(2),
        lower_bound = 3,
        all = hashmap::hashmap(2, TRUE)
    ))
}
modify_knowledge <- function(knowledge, is_not_prime=NA, is_prime=NA, new_prime=NA, lower_bound=NA) {
    if (!is.na(is_not_prime)) {
        knowledge$all[[is_not_prime]] = FALSE
    }
    if (!is.na(is_prime)) {
        knowledge$all[[is_prime]] = TRUE
    }
    if (!is.na(new_prime)) {
        knowledge$primes = c(knowledge$primes, new_prime)
        knowledge$lower_bound = new_prime + 1
    }
    if (!is.na(lower_bound)) {
        knowledge$lower_bound = lower_bound
    }
    return(knowledge)
}

get_next_prime <- function(prime, knowledge=empty_knowledge()) {
    return(get_next_prime_smaller_than(prime, upper_bound=.Machine$integer.max, knowledge=knowledge))
}

get_next_prime_smaller_than <- function(prime, upper_bound, knowledge=empty_knowledge()) {
    index <- match(prime, knowledge$primes)
    index <- index + 1
    if (index <= length(knowledge$primes)) {
        # print(knowledge$primes)
        return(list("next_prime"=knowledge$primes[[index]], "knowledge"=knowledge))
    } else {
        # print(glue("No next prime known, trying to find one..."))
        candidate <- knowledge$lower_bound
        while (candidate < upper_bound) {
            x = is_prime(candidate, knowledge)
            if (x$result) {
                new_prime <- candidate
                # print(glue("Found next prime: {new_prime}"))
                return(list("next_prime"=new_prime, "knowledge"=modify_knowledge(knowledge, new_prime=new_prime)))
            } else {
                candidate <- candidate + 1
                knowledge <- modify_knowledge(knowledge, lower_bound=candidate)
            }
        }
        return(list("next_prime"=NULL, "knowledge"=knowledge))
    }
}

is_prime <- function(number, knowledge=empty_knowledge()) {
    # print(glue("Is {number} prime?"))
    if (knowledge$all$has_key(number)) {
        result <- knowledge$all[[number]]
        if (result == TRUE) {
            print(glue("We already know that {number} is prime"))
        }
        # else {
        #     print(glue("We already know that {number} is not prime"))
        # }
        return(list("result"=result, "knowledge"=knowledge))
    }

    possible_factor <- knowledge$primes[[1]]
    while (!is.null(possible_factor)) {
        # print(glue("Is {possible_factor} a factor of {number}?"))
        if (number %% possible_factor == 0) {
            # print(glue("Yes, so it's not prime"))
            return(list("result"=FALSE, "knowledge"=modify_knowledge(knowledge, is_not_prime=number)))
        } else if (possible_factor < number - 1) {
            # print(glue("No, finding next prime"))
            next_prime <-get_next_prime_smaller_than(possible_factor, upper_bound=number, knowledge=knowledge)
            knowledge = next_prime$knowledge
            possible_factor = next_prime$next_prime
        } else {
            possible_factor = NULL
        }
    }
    # print(glue("No more possible factors, so {number} must be prime"))
    return(list("result"=TRUE, "knowledge"=modify_knowledge(knowledge, is_prime=number)))
}
