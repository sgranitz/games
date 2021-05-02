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

Player <- R6Class(
  classname = "Player",
  public = list(
    bank = 0,
    deposit = function(dollars) {
      self$bank <- self$bank + dollars
      invisible(self)
    },
    deck = NULL,
    hand = NULL,
    new_game = function(ante) {
      
      if (ante > self$bank) stop("Please add funds to play")
      
      self$bank <- self$bank - ante
      self$deck <- shuffle()
      self$hand <- deal(self$deck)
      
      print(paste0("$", ante, " paid, hand dealt"))
      print("Your hand is:")
      print(self$hand)
      print("Make your bet")
      
      invisible(self)
    },
    has_bet = FALSE,
    bet = 0,
    make_bet = function(dollars) {
      
      if (dollars > self$bank) stop("Bet exceeds balance")
      
      self$has_bet <- TRUE
      self$bet <- dollars
      self$bank <- self$bank - dollars
      
      invisible(self)
    },
    draw = function(c1=FALSE,c2=FALSE,c3=FALSE,c4=FALSE,c5=FALSE) {
      
      if (!self$has_bet) stop("Bet before drawing")
      req <- c(c1,c2,c3,c4,c5)
      has_draw <- any(req)
      
      if (has_draw) {
        new_cards <- draw(self$deck, sum(req))
        self$hand[req] <- new_cards
        print(self$hand)
      }
      
      win <- eval(self$hand, self$bet)
      self$bank <- self$bank + win
      if (win > 0) print(paste0("You won $", win))
      print(paste0("Your bank is now $", self$bank))
      
      self$has_bet <- FALSE
      self$bet <- 0
      self$deck <- NULL
      self$hand <- NULL
      
      invisible(self)
    },
    balance = function() {
      print(self$bank)
    }
  )
)

shuffle <- function() {
  
  deck <- ShuffledDeck$new()
  deck$reshuffle()
  
}

deal <- function(deck) {
  
  deck$draw(5)
  
}

draw <- function(deck, cards) {
  
  deck$draw(cards)
  
}

eval <- function(hand, bet) {
  
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
      
      print("Royal Flush")
      return(bet * 5000)
    
    }
    
    if (flush) { 
      
      if (straight) {
        
        print("Straight Flush")
        return(bet * 500) 
    
      } else {
      
        print("Flush")
        return(bet * 25) 
      
      }
      
    }
    
    print("Straight")
    return(bet * 12.5) 
    
  }
  
  freq <- as.data.frame(table(hand_list$card))
  freq <- freq[order(-freq$Freq),]
  vals <- c("A", 2:10, "J", "Q", "K", "A")
  
  if (max(freq$Freq) == 4) {
    
    print("Four of a Kind")
    return(bet * 75) 
    
  }
  
  if (max(freq$Freq) == 3) {
    
    if (min(freq$Freq) == 2) {
      
      print("Full House")
      return(bet * 37.5) 
      
    } else {
      
      print("Three of a Kind")
      return(bet * 2.5) 
      
    }
    
  }
  
  if (max(freq$Freq) == 2) {
    
    if (freq$Freq[2] == 2) {
      
      print("Two Pair")
      return(bet * 1.5) 
      
    } else if (as.numeric(as.character(freq$Var1))[1] > 10) {
      
      print(paste0("Pair of ", vals[as.numeric(as.character(freq$Var1))[1]], "s"))
      return(bet) 
  
    } else {
      
      print(paste0("Pair of ", vals[as.numeric(as.character(freq$Var1))[1]], "s"))
      return(0) 
      
    }
  }
   
  print(paste(vals[max(hand_list$card)], "High"))
  return(0)
}
