# Five Card Draw
# Add money to bank using `deposit()`
# Start game using `play()`
# Draw cards using `draw()`

library(R6)
options(scipen = 999)

suit <- c("s", "h", "d", "c")
value <- c("A", 2:10, "J", "Q", "K")
cards <- paste0(rep(value, 4), " ", suit)


# source: https://advanced-r-solutions.rbind.io/r6.html
ShuffledDeck <- R6Class(
  classname = "ShuffledDeck",
  public = list(
    deck = NULL,
    initialize = function(deck = cards) {
      self$deck <- sample(deck)
    },
    reshuffle = function() {
      self$deck <- sample(cards)
      invisible(self)
    },
    n = function() {
      length(self$deck)
    },
    draw = function(n = 1) {
      if (n > self$n()) {
        stop("Only ", self$n(), " cards remaining.", call. = FALSE)
      }
      
      output <- self$deck[seq_len(n)]
      self$deck <- self$deck[-seq_len(n)]
      output
    }
  )
)

play <- function(cards = 5, bet = 0) {

  if (bet > 0) {

    if (!exists("bank")) bank <- 0
    if (bet > bank) return("Insufficient funds, deposit money")
    
    bet  <<- bet
    bank <<- bank - bet
    
  }
  
  deck <- ShuffledDeck$new()
  deck <<- deck$reshuffle()
  
  hand <<- deck$draw(cards)
  print(hand)
  
}

draw <- function(cards = c()) {
  
  if (length(cards) < 1) return(eval(hand));
  
  hand[cards] <<- deck$draw(length(cards))
  print(hand)
  
}

eval <- function(hand) {
  
  if ((!exists("bet"))) bet <- 0
  if ((!exists("bank"))) bank <- 0
  
  
  hand <- as.character(hand)
  hand_split <- strsplit(hand, " ")
  
  hand_list <- list(card = c(), suit = c())
  for (i in 1:length(hand_split)) {
    
    hand_list$card[i] <- hand_split[[i]][1]
    hand_list$suit[i] <- hand_split[[i]][2]
    
  }
  
  hand_list$card[hand_list$card == "K"] <- "13"
  hand_list$card[hand_list$card == "Q"] <- "12"
  hand_list$card[hand_list$card == "J"] <- "11"
  
  aces <- hand_list$card == "A" 
  hand_list$card[aces] <- if (
    length(hand_list$card[!aces]) == 4 && 
    all(sort(as.numeric(hand_list$card[!aces])) == 2:5)
  ) "1" else "14"
  
  hand_list$card <- as.numeric(hand_list$card)
  
  straight <- length(min(hand_list$card):max(hand_list$card)) == 5 &&
    all(sort(hand_list$card) == min(hand_list$card):max(hand_list$card))
  
  flush <- length(unique(hand_list$suit)) == 1
  
  if (straight | flush) {
    
    if (straight && max(hand_list$card) == 14) {
      
      bank <<- bank + bet * 5000 
      return(paste0("Royal Flush", ifelse(bet > 0, paste0(" you win ", bet * 5000, " and your bank is ", bank), "")))
    
    }
    
    if (flush) { 
      
      if (straight) {
      
        bank <<- bank + bet * 500
        return(paste0("Straight Flush", ifelse(bet > 0, paste0(" you win ", bet * 500, " and your bank is ", bank), ""))) 
    
      } else {
      
        bank <<- bank + bet * 25
        return(paste0("Flush", ifelse(bet > 0, paste0(" you win ", bet * 25, " and your bank is ", bank), ""))) 
      
      }
    }
    
    bank <<- bank + bet * 12.5
    return(paste0("Straight", ifelse(bet > 0, paste0(" you win ", bet * 12.5, " and your bank is ", bank), ""))) 
    
  }
  
  freq <- as.data.frame(table(hand_list$card))
  freq <- freq[order(-freq$Freq),]
  vals <- c("A", 2:10, "J", "Q", "K", "A")
  
  if (max(freq$Freq) == 4) {
    
    bank <<- bank + bet * 75
    return(paste0("Four of a Kind", ifelse(bet > 0, paste0(" you win ", bet * 75, " and your bank is ", bank), ""))) 
    
  }
  
  if (max(freq$Freq) == 3) {
    
    if (min(freq$Freq) == 2) {
      
      bank <<- bank + bet * 37.5
      return(paste0("Full House", ifelse(bet > 0, paste0(" you win ", bet * 37.5, " and your bank is ", bank), ""))) 
      
    } else {
      
      bank <<- bank + bet * 2.5
      return(paste0("Three of a Kind", ifelse(bet > 0, paste0(" you win ", bet * 2.5, " and your bank is ", bank), ""))) 
      
    }
    
  }
  
  if (max(freq$Freq) == 2) {
    
    if (freq$Freq[2] == 2) {
      
      bank <<- bank + bet * 1.5
      return(paste0("Two Pair", ifelse(bet > 0, paste0(" you win ", bet * 1.5, " and your bank is ", bank), ""))) 
      
      
    } else {
      
      bank <<- bank + bet
      return(paste0(paste("Pair of", vals[as.numeric(as.character(freq$Var1))[1]], "s"), ifelse(bet > 0, paste0(" you win ", bet, " and your bank is ", bank), ""))) 
  
    }
  }
    
  return(paste(vals[max(hand_list$card)], "High, you lose your bet"));

}

deposit <- function(x) {
  
  if (exists("bank")) bank <<- bank + x
  
  bank <<- x
  
}

checkBank <- function() {
  
  if (!exists("bank")) bank <- 0
  bank
  
}

# Sample
deposit(100)
play(bet = 100)
draw()
