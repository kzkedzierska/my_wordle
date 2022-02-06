#' List of English Words from the Scrabble Dictionary adapted from \code{words}
#'  package.
#'
#' List of english scrabble words as listed in the OTCWL2014
#' <https://www.scrabbleplayers.org/w/Official_Tournament_and_Club_Word_List_2014_Edition>.
#' Words are collated from the 'Word Game Dictionary' <https://www.wordgamedictionary.com/word-lists/>.
#'
#' @format A data frame with 175,393 rows and 4 variables:
#' \describe{
#'   \item{word}{english word, lower case}
#'   \item{word_length}{length of the word}
#'   \item{uniq_characters}{number of unique letters in the word}
#'   \item{starters}{whether to priotise the word for first guess, so far words with all unique characters are selected}
#'
#' }
#' @source \url{https://www.wordgamedictionary.com/word-lists/}
"words_for_wordle_df"
