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
      plot_wordle()

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

#' Run wordle Shiny App
#'
#' @return Shiny App Wordle implementation
#' @export
#' @import shiny
#' @import gganimate
#'
#' @examples
#' # run shiny app
#' run_wordle()
run_wordle <- function() {
  dark <- bslib::bs_theme(version = 4, bootswatch = "solar")
  light <- bslib::bs_theme(version = 4, bootswatch = "minty")

  ui <- fixedPage(titlePanel("Shiny App Wordle implementation"),
                  theme = light,
                  column(width = 12,
                         fixedRow(
                           column(3,
                                  fixedRow(wellPanel(
                                    checkboxInput("dark_mode", "Dark mode", value = FALSE),
                                    selectInput("n_letters", "Word size", choices = 2:6,
                                                multiple = FALSE, selected = 5),
                                    numericInput("n_tries", "Number of guessess",
                                                 min = 3, max = 10,
                                                 value = 6, step = 1),
                                    actionButton("new_game", "New game",
                                                 icon = icon("sync"))
                                  ))),
                           column(6, offset = 2,
                                  fixedRow(plotOutput("wordle_outcome", width = 400)),
                                  fixedRow(plotOutput("letters", width = 400, height = 200)),
                                  fixedRow(column(width = 3, h3("Your guess: ")),
                                           column(width = 4, textInput("guess", "",
                                                                       value = "")),
                                           column(width = 2, actionButton("submit", "",
                                                                          icon = icon("play"))))
                           )))

                   )

  # Define server function
  server <- function(input, output, session) {

    words_df <- words_for_wordle_df

    values <- reactiveValues()

    observe(session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) dark else light
    ))

    # set up clean game
    observeEvent(input$new_game, {

      logger::log_debug("Setting clean game")
      values$success <- FALSE

      values$init <- TRUE

      values$n_letters <-
        as.integer(input$n_letters)

      values$n_tries <-
        input$n_tries

      values$correct <-
        words_df %>%
        filter(word_length == input$n_letters) %>%
        slice_sample(n = 1) %>%
        pull(word)

      values$guesses_df <- tibble()

      values$i <- 1

      values$used_letters <- c()

      values$plt <-
        plot_wordle(n_tries = values$n_tries,
                    n_letters = values$n_letters)

      showNotification(
        ui = "New game started.",
        type = "message",
        duration = 5
      )


    }, ignoreNULL = FALSE)


    guess_outcome <- eventReactive({
      input$submit
      input$new_game
      }, {

      if (values$init) {
        values$init <- FALSE
        return(values$plt)
      }

      logger::log_debug("Evaluating outcome of the guess")

      # for compatibility with word_df data frame
      guess <- tolower(input$guess)

      in_dictionary <- guess %in% words_df$word
      correct_length <- nchar(guess) == nchar(values$correct)

      logger::log_debug("Current guess: {guess}. ",
                        "In dictionary? {in_dictionary}. ",
                        "Correct length? {correct_length}")

      if (in_dictionary & correct_length) {

        testing_vec <- test_guess(guess, values$correct)

        if (all(testing_vec == "exact")) {
          values$success <- TRUE
        }

        output_guess <-
          run_wordle_guess(testing_vec,
                           i = values$i,
                           guesses_df = values$guesses_df,
                           n_letters = values$n_letters,
                           n_tries = values$n_tries)

        values$plt <-
          output_guess$static_plot

        values$guesses_df <-
          output_guess$guesses_df

        values$i <- values$i + 1

        values$used_letters <-
          c(values$used_letters,
            names(testing_vec)) %>%
          unique()

        plt <- output_guess$static_plot
        values$plt <- plt

      } else {
        if (!in_dictionary & guess != "") {
          showNotification("Word not found in dictionary", type = "warning")
        }

        if (!correct_length & guess != "") {
          showNotification("Incorrect size of guess", type = "warning")
        }
        logger::log_debug("This should be empty guess: {guess}")
        plt <- values$plt
      }

      #output_guess$animation
      plt
    }, ignoreNULL = FALSE)

    output$letters <- renderPlot({

      plot_keybord(values$used_letters)

    })

    output$wordle_outcome <- renderPlot({

        guess_outcome()

      })


    observe({

      if (values$success) {
        showModal(modalDialog(
          title = "Success!",
          glue::glue("{praise_success(values$i)}"),
          easyClose = TRUE,
          footer = NULL
        ))
      }

      if (!values$success & values$i == values$n_tries + 1) {
        showModal(modalDialog(
          title = "No success :(",
          glue::glue("Correct answer: {toupper(values$correct)}"),
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })

    output$old_wordle_outcome <- renderImage({

      outfile <- tempfile(fileext = '.gif')
      anim_save(
        "outfile.gif",
        animate(guess_outcome(),
                duration = 2,
                renderer = gifski_renderer(loop = FALSE)), bg = 'transparent')



      list(src = "outfile.gif",
           contentType = 'image/gif')


    }, deleteFile = TRUE)



  }

  thematic::thematic_shiny()
  # Create Shiny object
  shinyApp(ui = ui, server = server)
}
run_wordle()
