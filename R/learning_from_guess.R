#' Learn from the guess
#'
#' @param testing_vec named character vector; output of \code{test_guess}
#'   function.
#' @param allowed list of lists with all previously learned information about
#'   possible letters at given position. Starting with all letters available
#'   at all positions. Information learned from new guess saved as new element.
#'   For more details see Examples.
#' @param scan_for character vector, subset of \code{letters}.
#'
#' @return Named list with updated \code{allowed} and \code{scan_for} objects.
#' @export
#'
#' @examples
#' test_vec <- test_guess("aloft", "float")
#' # this is first guess, hence no prior knowledge
#' allowed_letters <- list(lapply(1:5, function(i) letters))
#' scan_for <- c()
#'
#' learn_from_guess(test_vec, allowed_letters, scan_for)
learn_from_guess <- function(testing_vec, allowed, scan_for) {

  # tests
  stopifnot(is.list(allowed),
            is.vector(scan_for) | is.null(scan_for),
            unique(testing_vec) %in% c("no", "almost", "exact"),
            length(allowed[[1]]) == length(testing_vec),
            all(unlist(allowed) %in% letters))

  n_letters <- length(testing_vec)

  new_allowed <- last(allowed)

  for (i in 1:n_letters) {
    let <- names(testing_vec)[i]

    if (testing_vec[i] == "no") {
      # if the letter is not found in the word, remove it from allowed,
      # except for any exacts
      new_allowed <-
          lapply(new_allowed, function(allowed_letters) {
          if (length(allowed_letters) > 1) {
            allowed_letters <- setdiff(allowed_letters, let)
          }
          allowed_letters
        })
    } else if (testing_vec[i] == "almost") {
      # if it is found, but not at this spot, remove it from allowed
      # in this position
      new_allowed[[i]] <- setdiff(new_allowed[[i]], let)
      # also, we need to actively search for that letter in other positions
      scan_for <-
        c(scan_for, let) %>%
        unique()
    } else { # testing_vec[i] == "exact"
      # we guessed letters exact position
      new_allowed[[i]] <- let
    }
  }

  allowed[[length(allowed) + 1]] <- new_allowed

  list(allowed = allowed,
       scan_for = scan_for)
}
