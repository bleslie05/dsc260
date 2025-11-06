## Functions, nesting, mapping with purrr::map.

## Here is a simple example of a function that takes an input and multiplies by 10.
mult <- function(my_value){
  10 * my_value
}

mult(20)

## Now use purrr::map() to iterate this function over values in a list or vector
## Generate a data vector; in fact an atomic numerical vector
vec <- c(1:10)

# Now iterate using purrr::map()
map(vec, mult)          ## Returns a list
map_dbl(vec, mult)      ## Returns a numeric vector

### Exercise: Write a function that takes an input vector and finds the correlation 
### with the mpg column of the mtcars data set.

mpg_cor <- function(x) {
  cor(mtcars$mpg, x)
}

mpg_cor(mtcars$cyl)

## Now use map() to apply this function to all columns of mtcars
map(mtcars, mpg_cor)       ## Returns a list
map_df(mtcars, mpg_cor)    ## Returns a one-row data frame

## Exercise: Write a function which takes a data frame as an argument
## and fits a linear model cty ~ displ. Use it on the mpg dataset.

library(broom)
library(tidyverse)

mod <- function(df) {
  lm(cty ~ displ, data = df)
}

## Test it
mod(mpg)
tidy(mod(mpg))

## Anonymous function examples
map_dbl(vec, ~10 * .)
map_dbl(vec, ~10 * .x)

## Nesting the mpg dataset by class
by_class <- mpg %>% group_by(class) %>% nest()

## Explore nested data
by_class$data       ## List of data frames
by_class$data[[2]]  ## Access 2nd class’s data

## Map the model function to each group
mod_list <- map(by_class$data, ~lm(cty ~ displ, data = .x))

## Get tidy output from each model
tidy_list <- map_df(mod_list, tidy)

## Or chain everything together:
by_class %>%
  mutate(model = map(data, ~lm(cty ~ displ, data = .x))) %>%
  mutate(coeffs = map(model, tidy)) %>%
  unnest(coeffs)
