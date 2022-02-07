#' Run wordle Shiny App
#'
#' @return Shiny App Wordle implementation
#' @export
#' @import shiny
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
                                    checkboxInput("dark_mode", "Dark mode",
                                                  value = FALSE),
                                    selectInput("n_letters", "Word size",
                                                choices = 2:6,
                                                multiple = FALSE, selected = 5),
                                    numericInput("n_tries", "Number of guesses",
                                                 min = 3, max = 10,
                                                 value = 6, step = 1),
                                    actionButton("new_game", "New game",
                                                 icon = icon("sync"))
                                  ))),
                           column(6, offset = 2,
                                  fixedRow(plotOutput("wordle_outcome",
                                                      width = 400)),
                                  fixedRow(plotOutput("letters",
                                                      width = 400,
                                                      height = 200)),
                                  fixedRow(column(width = 3,
                                                  h3("Your guess: ")),
                                           column(width = 4,
                                                  textInput("guess", "",
                                                            value = "")),
                                           column(width = 2,
                                                  actionButton("submit", "",
                                                               icon = icon("play"))))
                           )
                         )
                  )
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
