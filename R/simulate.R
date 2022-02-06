#' Simulate one game
#'
#' @param words_for_wordle_df tibble with words
#' @param n_letters integer with the choice of the size of words, default: 5
#' @param n_guesses integer with the number of guesses allowed, default: 6
#'
#' @return tibble with results of the simulation
#' @export
#'
#' @import dplyr
#' @import logger
#' @importFrom tibble tibble
#'
#'
#' @examples
#' simulate(words_for_wordle_df, 3)
simulate <- function(words_for_wordle_df, n_letters = 5, n_guesses = 6) {

  # select a word
  correct <-
    words_for_wordle_df %>%
    filter(word_length == n_letters) %>%
    slice_sample(n = 1) %>%
    pull(word)

  log_debug("Simulation looking for `{correct}`")

  # create a tibble with words, that will be consecutively filtered out
  # to narrow guessing space
  words_df <-
    words_for_wordle_df %>%
    filter(word_length == n_letters)

  # initialize allowed letters, this will be used for regular expressions
  # to narrow down the guessing space
  allowed <- list(lapply(1:n_letters, function(i) letters))

  # initialize vector for storing letters to scan for,
  # those that are in the word, but were guessed at wrong position
  scan_for <- c()

  # initializing tibble to report results
  results_df <- tibble()

  # mark outcome simulations
  success <- FALSE


  for (i in 1:n_guesses) {

    # guess the word
    if (i == 1) {
      guess <- guess_first(words_df)
    } else {
      # create regular expression based on learned information
      word_pattern <-
        lapply(last(allowed),
               function(lets) {
                 paste0("[", paste(lets, collapse = ""), "]{1}")
               }) %>%
        paste(collapse = "")

      # create pattern to screen for (that includes all almost letters)
      scanning_pattern <-
        scan_for %>%
        paste(collapse = "|")

      updated <- guess_word(words_df,
                            previous_guess = guess,
                            regexpr_word = word_pattern,
                            regexpr_letters = scanning_pattern)

      words_df <- updated$words_df
      guess <- updated$guess
    }

    # check the guess
    testing_vec <- test_guess(guess, correct)

    # record the outcome of the guess
    try_df <-
      tibble(x = 1:n_letters,
             attempt = i,
             letters = names(testing_vec),
             result = testing_vec) %>%
      mutate(letters = toupper(letters))

    results_df <-
      bind_rows(results_df,
                try_df)

    # if all letters are guessed - success!
    if (all(testing_vec == "exact")) {
      success <- TRUE
      break
    }

    # update knowledge abut the guess
    learned <- learn_from_guess(testing_vec, allowed, scan_for)

    allowed <- learned$allowed
    scan_for <- learned$scan_for

  }

  results_df %>%
    mutate(success = success,
           correct_answer = correct)
}
