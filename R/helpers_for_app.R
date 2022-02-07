#' Check guess
#'
#' @param word string
#' @param words_df tibble with words for wordle, must have word character column
#'
#' @return logical if word is among the words
#'
#' @examples
#' check_guess("start") # TRUE
#' check_guess("yolo") # FALSE
#'
check_guess <- function(word, words_df = words_for_wordle_df) {
  word %in% words_df$word
}

#' Praise successful solution
#'
#' @param i integer, successful try
#'
#' @return string praise for successfully solving wordle
#'
#' @examples
#' praise_success(2)
praise_success <- function(i) {

  stopifnot(is.numeric(i))

  praises <-
    list(c("Yowza!", "Legendary!"),
         c("Fantastic!", "Epic!"),
         c("Splendid!", "Ace!", "Awesome!"),
         c("Well done!", "Solid!"),
         c("Good job!", "Neatly done"),
         c("Good!", "Success!"))

  i <- min(i, length(praises))

  print(sample(praises[[i]], 1))

}

#' Run wordle guess, check it, store results and plot the viz
#'
#' @param testing_vec named character vector, output of \code{test_guess}
#'   function
#' @param i numeric, guess number
#' @param guesses_df tibble, saved previous guesses
#' @param n_letters numeric, length of guess and answer
#' @param n_tries numeric, maximum number of tries
#'
#' @return list with updated guesses_df (if \code{testing_vec} provided),
#'   animation (if \code{gganimate} installed) and a static plot
#'
#' @examples
#' run_wordle_guess(n_letters = 5, n_tries = 6)
run_wordle_guess <- function(testing_vec = NULL,
                             i = 1,
                             guesses_df = tibble(),
                             n_letters = NULL,
                             n_tries = NULL) {


  if (!is.null(testing_vec)) {
    n_letters <- length(testing_vec)

    try_df <-
      tibble(x = 1:n_letters,
             attempt = i,
             letters = names(testing_vec),
             result = testing_vec) %>%
      mutate(letters = toupper(letters))

    guesses_df <-
      bind_rows(guesses_df, try_df)

    plt <-
      guesses_df %>%
      group_by(attempt) %>%
      mutate(transition = ifelse(attempt == i, 1:n(), 0)) %>%
      plot_wordle(n_tries = n_tries)

  } else {
    stopifnot(is.numeric(n_letters),
              is.numeric(n_tries))

    plt <- plot_wordle(n_letters = n_letters,
                       n_tries = n_tries)

  }

  if (!requireNamespace("gganimate", quietly = TRUE)) {

    log_warn("No `gganimate` package, cannot produce annimation.")

    anim_plt <- NULL

  } else {
    if (!is.null(testing_vec)) {
      anim_plt <- plt +
        gganimate::transition_manual(transition, cumulative = TRUE)
    } else {

      anim_plt <- plt + gganimate::transition_null()
    }

  }
  list(guesses_df = guesses_df,
       static_plot = plt,
       animation = anim_plt)
}

#' Plot keyboard outline with selected "grayed" out letters
#'
#' @param used_letters character vector with letters to grey out
#'
#' @return ggplot with keyboard outline
#' @export
#'
#' @examples
#' plot_keybord(c()) # all green
#' plot_keybord(c("a", "b", "t")) # a, b and t grayed out
plot_keybord <- function(used_letters) {

  stopifnot(toupper(used_letters) %in% LETTERS)

  used_letters <- toupper(used_letters)

  row_one <- strsplit("qwertyuiop", "") %>% unlist()
  row_two <- strsplit("asdfghjkl", "") %>% unlist()
  row_three <- strsplit("zxcvbnm", "") %>% unlist()

  keybord_outline_df <-
    tibble(letter = c(row_one, row_two, row_three),
           row_n = c(rep(1, length(row_one)),
                     rep(2, length(row_two)),
                     rep(3, length(row_three)))) %>%
    group_by(row_n) %>%
    mutate(order = 1:n(),
           letter = toupper(letter))

  avail_letters_df <-
    tibble(letter = LETTERS) %>%
    full_join(keybord_outline_df) %>%
    mutate(used = letter %in% used_letters)

  avail_letters_df %>%
    group_by(row_n) %>%
    mutate(order2 = (10 - max(order))/2 + order) %>%
    ggplot(aes(order2,
               row_n)) +
    geom_tile(aes(fill = used),
              width = 0.9,
              height = 0.9) +
    geom_text(aes(label = letter),
              color = "white", size = 7,
              family = "URWHelvetica", fontface = "bold") +
    scale_fill_manual(values = c("darkgreen", "grey60")) +
    theme_void() +
    theme(strip.text = element_blank(), legend.position = "none",
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent")) +
    coord_equal() +
    scale_y_reverse()
}
