library(dplyr)
library(ggplot2)
library(gganimate)

#' Plot wordle guesses
#'
#' @param df tibble with wordle results, must contain x & y numeric columns,
#'   and result, letters and attempt character columns. For details see example.
#' @param facet logical, default: \code{FALSE}; 
#'   if multiple attempts provided in the df, allows to plot them in panels; 
#'   do not use when animating.
#'
#' @return ggplot2 plot with visualized guesses
#'
#' @examples
#' 
#' first_try_df <- 
#'   tibble(x = rep(1:3), 
#'          letters = c("M", "A", "C"), 
#'          result = c("no", "exact", "almost"), 
#'          attempt = 1)
#' 
#' second_try_df <-
#'   tibble(x = 1:3, 
#'          letters = c("C", "A", "T"), 
#'          result = rep("exact", 3), 
#'          attempt = 2)
#' 
#' example_df <-
#'   bind_rows(first_try_df,
#'             second_try_df)
#' 
#' plot_wordle(first_try_df)
#' plot_wordle(example_df)
plot_wordle <- function(df, n_tries = 6) {
  
  # tests
  stopifnot(c("x", "result", "letters", "attempt") %in% colnames(df),
            is.numeric(df$x),
            is.character(df$result),
            is.character(df$letters),
            is.numeric(df$attempt))
  
  wordle_palette <- 
    c("no" = "#939598", 
      "exact" = "#538d4e", 
      "almost" = "#b59f3b", 
      "empty" = "#212121")
  
  n_letters <- max(df$x)
  
  empty_df <-
    tibble(x = rep(1:n_letters, n_tries),
           attempt = rep(1:n_tries, each = n_letters))
  
  plt <- 
    ggplot() +
    geom_tile(data = empty_df, 
              aes(x, attempt),
              fill = "#212121", width = 0.9,
              height = 0.9, color = "grey60") +
    geom_tile(data = df, 
              aes(x, attempt, fill = result),
              width = 0.9, height = 0.9, color = "grey60") +
    geom_text(data = df, 
              aes(x, attempt, label = letters),
              color = "white", size = 7, 
              family = "URWHelvetica", fontface = "bold") +
    theme_void() + 
    theme(legend.position = "none", 
          plot.background = element_rect( fill = "#212121"),
          panel.background = element_rect(fill = "transparent")) + 
    scale_fill_manual(values = wordle_palette) +
    coord_equal() +
    scale_y_reverse()
  
  plt
}

#' Title
#'
#' @param df tibble with wordle results, must contain x & attempt numeric columns,
#'   and result & letters character columns. For details see example.
#' @param n_tries Number of tries to create the empty canvas.
#'
#' @return
#' @export
#'
#' @examples
#' first_try_df <- 
#'   tibble(x = rep(1:3), 
#'          letters = c("M", "A", "C"), 
#'          result = c("no", "exact", "almost"), 
#'          attempt = 1)
#' 
#' second_try_df <-
#'   tibble(x = 1:3, 
#'          letters = c("C", "A", "T"), 
#'          result = rep("exact", 3), 
#'          attempt = 2)
#' 
#' example_df <-
#'   bind_rows(first_try_df,
#'             second_try_df)
#' 
#' animate_wordle(example_df)
animate_wordle <- function(df, n_tries = 6) {
  
  # tests
  stopifnot(c("x", "result", "letters", "attempt") %in% colnames(df),
            is.numeric(df$x),
            is.character(df$result),
            is.character(df$letters),
            is.numeric(df$attempt))
  
  if (n_tries < max(df$attempt)) {
    warning("More attempts than max tries, increasing number of tries.")
    n_tries <- max(df$attempt)
  }
  
  df <-
    df %>%
    mutate(n_times = case_when(x == max(x) ~ 3,
                               TRUE ~ 1)) %>%
    uncount(n_times) %>%
    mutate(transition = 1:n())
  
  animation_plt <-
    plot_wordle(df, n_tries = n_tries) +
    transition_manual(transition, cumulative = TRUE)
  
  animate(animation_plt, end_pause = 5, duration = 2*max(df$attempt))
}
