# Simulation --------------------------------------------------------------- 
# clear workspace
rm(list = ls())

# generate the carddeck, C=clubs,S=spades,D=diamonds,H=hearts
carddeck <- as.vector(outer(1:13, c("C", "S", "D", "H"), paste, sep = ""))

# Give each player 5 new cards when function is called with the deck vector
dealtcards <- function(deck) {
  ret <- (split(sample(deck, 30), c(1, 2, 3, 4, 5, 6)))
  return(ret)
}

# define functions to get right and left chars usage: strright(dealtcards,1) to get all the suits
strright <- function(x, n) {
  sapply(x, function(y) substr(y, (nchar(y) - n + 1), nchar(y)))
}

# usage: strrightinv(dealtcards,1) to get all the ranks
strrightinv <- function(x, n) {
  sapply(x, function(y) substr(y, 1, nchar(y) - n))
}

# 3a) hand consisting of a single suit only Define the function which checks if a single suit for each player is
# true
singlesuit <- function(cards) {
  # extract suits, unlist, check uniqueness and see how long the vector is
  ret <- (length(unique(unlist(strright(cards, 1)))) == 1)
  return(ret)
}

# Simulation
sumsuits <- 0  # initialize numbers of player 1 decks with suits
n <- 10000  # set number of simulatons
pb <- txtProgressBar(style = 3, max = n)  # initialize progress bar
# Simulation loop
for (i in 1:n) {
  sumsuits <- sumsuits + sum(as.integer(lapply(dealtcards(carddeck), singlesuit)))  # update counter
  setTxtProgressBar(pb, i)  # update progress bar
}
close(pb)  # close progress bar
sumsuits <- sumsuits/(6 * n)  # calculate empirical probability

# theoretically probability
sumsuitstheo <- (choose(13, 5) * 4)/choose(52, 5)

# compare probabilities
cat(sumsuits, sumsuitstheo)

## 3b) Full house define fullhouse function
fullhouse <- function(cards) {
  # extract ranks and make it table to see how many times
  tab <- table(strrightinv(cards, 1))
  ret <- (length(tab) == 2 & ((tab[1] == 3) | (tab[2] == 3)))
  return(ret)
}


# Simulation
sumfullhouse <- 0  # initialize numbers of player 1 decks with suits
n <- 10000  # set number of simulatons
pb <- txtProgressBar(style = 3, max = n)  # initialize progress bar
# Simulation loop
for (i in 1:n) {
  sumfullhouse <- sumfullhouse + sum(as.integer(lapply(dealtcards(carddeck), fullhouse)))  # update counter
  setTxtProgressBar(pb, i)  # update progress bar
}
close(pb)  # close progress bar
sumfullhouse <- sumfullhouse/(n * 6)  # calculate empirical probability

# theoretically probability
sumfullhousetheo <- (choose(13, 1) * choose(4, 3) * choose(12, 1) * choose(4, 2))/choose(52, 5)
# compare probabilities
cat(sumfullhouse, sumfullhousetheo)


# 3c) Check for two fullhouses
sumtwofullhouse <- 0  # initialize numbers of player 1 decks with suits
n <- 50000  # set number of simulatons
pb <- txtProgressBar(style = 3, max = n)  # initialize progress bar
# Simulation loop
for (i in 1:n) {
  testfh <- sum(as.integer(lapply(dealtcards(carddeck), fullhouse)))
  sumtwofullhouse <- sumtwofullhouse + ifelse(testfh == 2, testfh, 0)  # update counter
  setTxtProgressBar(pb, i)  # update progress bar
}
close(pb)  # close progress bar
sumtwofullhouse <- sumtwofullhouse/n  # calculate empirical probability

# print probabilities
cat(sumtwofullhouse)

# 3d) Probability for a royal flush test with: cards <- list(c('12H','11H','10H','13H','9H'))
royalflush <- function(cards) {
  # check for royal flush
  ret <- ((length(unique(unlist(strright(cards, 1)))) == 1) & (sum(as.integer(strrightinv(cards, 1))) == 55))
  return(ret)
}

sumroyalflush <- 0  # initialize numbers of player 1 decks with suits
n <- 50000  # set number of simulatons
pb <- txtProgressBar(style = 3, max = n)  # initialize progress bar
# Simulation loop
for (i in 1:n) {
  sumroyalflush <- sumroyalflush + sum(as.integer(lapply(dealtcards(carddeck), royalflush)))  # update counter
  setTxtProgressBar(pb, i)  # update progress bar
}
close(pb)  # close progress bar
sumroyalflush <- sumroyalflush/(6 * n)  # calculate empirical probability

# theoretically probability
sumroyalflushtheo <- 4/choose(52, 5)
# compare probabilities
cat(sumroyalflush, sumroyalflushtheo) 
