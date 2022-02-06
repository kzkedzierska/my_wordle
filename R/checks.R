#' Check for exact match between a guess and correct answer
#'
#' @param guess string Guess for wordle
#' @param correct string Correct answer
#' @return The logical vector corresponding to position-wise exact matches between the guess and correct answer. The vector is named with respective letters of a guess, and keeps the order of the characters in the string.
#' @examples
#' check_for_exact("stop", "start")
#' check_for_exact("aloft", "float")
#' check_for_exact("test", "test")
check_for_exact <- function(guess, correct) {
  # tests
  stopifnot(is.character(guess),
            is.character(correct),
            length(guess) == 1,
            length(correct) == 1,
            nchar(guess) == nchar(correct))

  # split guess and correct
  guess_letters <- str_split(guess, "")[[1]]
  correct_letters <- str_split(correct, "")[[1]]

  # compare letters position-wise
  result_lgl <-
    guess_letters == correct_letters

  # name the results
  names(result_lgl) <- guess_letters

  # return logical vector with letters as names
  result_lgl
}


#' Test a guess against correct answer
#'
#' @param guess A string.
#' @param correct A string.
#' @return The character vector corresponding to position-wise feedback. "no" means the letter is not found in the word, "almost" suggest a different position and "exact" confirms that a letter is in it's right position. The vector is named with respective letters of a guess, and keeps the order of the characters in the string.
#' @examples
#' test_guess("stops", "start")
#' test_guess("aloft", "float")
#' test_guess("test", "test")
test_guess <- function(guess, correct) {
  # tests
  stopifnot(is.character(guess),
            is.character(correct),
            length(guess) == 1,
            length(correct) == 1,
            nchar(guess) == nchar(correct))

  # get the number of letters
  n <- nchar(guess)

  # check for exact matches
  result_exact <- check_for_exact(guess, correct)

  # split for letters
  guess_letters <- str_split(guess, "")[[1]]
  correct_letters <- str_split(correct, "")[[1]]

  # remove exact matches
  correct_letters <- correct_letters[!result_exact]

  result_almost <- rep(FALSE, n)
  for (i in 1:n) {
    # if the letter is in it's correct position do nothing
    if (result_exact[i]) {
      next
    }
    if (guess_letters[i] %in% correct_letters) {
      # if a letter is found in the word, but not at this position, return TRUE
      result_almost[i] <- TRUE
      # then, remove just one occurrence in case there are many
      which_to_remove <- which(guess_letters[i] == correct_letters)[1]
      correct_letters <- correct_letters[-which_to_remove]
    }
  }

  # compile result vector with no, almost and exact results
  result <- rep("no", n)
  result[result_almost] <- "almost"
  result[result_exact] <- "exact"

  # return the vector named
  names(result) <- guess_letters

  result
}
