#' @title Apply a filter to a data.frame
#'
#' @param df data.frame
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export


#3. Create and document a function that generates a density plot
#   for a specific variable in a data.frame

ggDensity <- function(x = SamplingDist(50)){
  
  plot(density(SamplingDist))
}


#4. Create and document a new customer ggplot theme function
# (Hint: Look at an existing theme and modify it)

Newcust <- function(x = SamplingDist){
  ggplot(aes(x = sampleMeans, fill = sampleSize))+
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())}

#4b.Create a function that provides the top n number of a grouped variable by counts
#  (Hint: use dplyr and for the group by use this line of code so you can pass
#  in a character vector "groupings" for the group by) group_by_at(vars(one_of(groupings)))

Top <- function(SamplingDist){
  group_by_at(vars(one_of(groupings)))%>%
    arrange(~desc(vars))%>%
    filter(top_n())}

#5  Create 8 other general functions (you can use the ones from lecture and/or create your own)
#  (i.e. power analysis, bootstrap, other dplyer or ggplot)

function(sample_size = 100){
  x = rnorm(100)
  TT <- t.test(x)
  TT$conf.int}



function(nSamples = 10000)
  Ps = rep(NA, nSamples)

for(i in 1:nSamples){
  x <- rnorm(100)
  TT <- t.test(x)
  Ps[i] = TT$p.value
}
mean(Ps <= .05)



norm <- function(x) sqrt(x%*%x)
norm(1:4)



randomplot <- function(SamplingDist){}
p <- ggplot(SamplingDist,
            aes(fill = condition,
                y = means,
                x = condition))
p + geom_bar(position = "dodge", stat = "identity")




sum.of.squares <- function(x,y) {
  x^2 + y^2
}



input_1 = 20
mySum <- function(input_1, input_2 = 10) {
  output <- input_1 + input_2
  return(output)
}



fahrenheit_to_kelvin <- function(temp_F) {
  temp_K <- ((temp_F - 32) * (5 / 9)) + 273.15
  return(temp_K)
}



center <- function(mtcars, mpg) {
  new_data <- (mtcars - mean(mpg))
  return(new_data)
}

#6 Add testthat with 4 specific tests


test_that("str_length is number of characters based on specific characters", {
  expect_equal(str_length("a"), 1)
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
})


test_that("str_length is number of characters in a distribution", {
  ss <- SamplingDist(samples =50)
  expect_equal(length(ss), 50)
})



test_that("The length does not match", {
  ss <- SamplingDist(samples = 51)
  expect_equal(length(ss), 51)
})


test_that("The deviations are extreme", {
  ss <- SamplingDist(samples = 5)
  expect_true(all(abs(ss) <= 5))
})

