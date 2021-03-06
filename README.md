
# My wordle implementation

This is my implementation of Wordle using R and R Shiny.

The [7 figures worth](https://www.bbc.co.uk/news/business-60208463)
original can be found here:
[powerlanguage.co.uk/wordle/](https://www.powerlanguage.co.uk/wordle/).

I implemented Wordle in R to eventually add Polish version, but also
because I wanted to play around with data exploration. Also, I was
interesting how good can a simple regexp solver be, and if how, I humble
human, fare in comparison with the machine.

## Dictionaries

As a source of words for my wordle I use the `words::words` data frame.
`words` is the package with list of English Words from the Scrabble
Dictionary. You can find more details here:
[condwanaland/words](https://github.com/condwanaland/words).

## Installation

You can install the package from github.

``` r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

devtools::install_github("kzkedzierska/my_wordle")
```
