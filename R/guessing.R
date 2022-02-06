#' Guess word and narrow down the guessing space
#'
#' @param words_df tibble with pre-selected (for example based on length) words
#' @param previous_guess string with last guess
#' @param regexpr_word string with regular expression combining knowledge
#'   from previous guesses has
#' @param regexpr_letters string with regular expression with screened letters;
#'   for example to screen for "a" and "c" the string would look like this "a|c"
#'
#' @return named list with:
#'   filtered tibble with possible guesses left and
#'   string with current guess.
#' @export
#'
#' @import dplyr
#' @import logger
#' @importFrom stringr str_detect
#'
#' @examples
#'
#' words_df <- words_for_wordle_df %>% filter(word %in% c("cat", "man", "can"))
#' previous_guess <- "can"
#' regexpr_word <- "[c]{1}[a]{1}[abcdefghijklmopqrstuvwxyz]{1}"
#' regexpr_letters <- ""
#' guess_word(words_df, previous_guess, regexpr_word, regexpr_letters)
guess_word <- function(words_df,
                       previous_guess,
                       regexpr_word,
                       regexpr_letters) {
  words_df <-
    words_df %>%
    # remove previous guess
    filter(word != previous_guess) %>%
    # filter out the words that don't match the learned pattern
    filter(str_detect(word, regexpr_word)) %>%
    # search for the letters found in word, but not at position
    filter(str_detect(word, regexpr_letters))

  log_debug("After guessing '{previous_guess}' {nrow(words_df)} ",
            "word(s) left in the guessing space.")

  guess <-
    words_df %>%
    # prioritize as many unique characters
    slice_max(n = 1, order_by = uniq_characters) %>%
    # take just one
    slice_sample(n = 1) %>%
    pull(word)

  if (length(guess) < 1) {
    log_error("Something went wrong - no guess left after filtering!")
    stop("No guess found")
  }

  log_debug("New guess: {guess}")
  list(words_df = words_df,
       guess = guess)
}

#' Select first word
#'
#' @param words_df tibble with preselected (for example based on length) words
#'
#' @return String with a guess word
#' @export
#'
#' @examples
#' data(words_for_wordle)
#' guess_first(words_for_wordle)
guess_first <- function(words_df) {

  words_df %>%
    filter(starters) %>%
    slice_sample(n = 1) %>%
    pull(word)

}
