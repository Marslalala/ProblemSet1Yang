###Problem 3


## Part a

#  The output by ChatGPT
#  Here are two R functions, one for identifying the name of a 5-card stud poker hand, 
#  and the other for simulating a round of poker with a specified number of players:

#  Function to identify the name of a 5-card stud poker hand
identify_poker_hand <- function(suits, ranks) {
  # Check for a flush
  is_flush <- length(unique(suits)) == 1
  
  # Check for a straight
  is_straight <- length(unique(ranks)) == 5 && (max(ranks) - min(ranks)) == 4
  
  # Check for pairs, three of a kind, and four of a kind
  rank_counts <- table(ranks)
  has_pair <- any(rank_counts == 2)
  has_three_of_a_kind <- any(rank_counts == 3)
  has_four_of_a_kind <- any(rank_counts == 4)
  
  # Determine the hand name
  if (is_flush && is_straight) {
    return("Straight Flush")
  } else if (has_four_of_a_kind) {
    return("Four of a Kind")
  } else if (has_three_of_a_kind && has_pair) {
    return("Full House")
  } else if (is_flush) {
    return("Flush")
  } else if (is_straight) {
    return("Straight")
  } else if (has_three_of_a_kind) {
    return("Three of a Kind")
  } else if (sum(rank_counts == 2) == 2) {
    return("Two Pair")
  } else if (has_pair) {
    return("One Pair")
  } else {
    return("High Card")
  }
}

#  Function to simulate dealing a round of poker with a specified number of players
deal_poker_round <- function(num_players) {
  # Create a deck of cards
  suits <- rep(c("Hearts", "Diamonds", "Clubs", "Spades"), each = 13)
  ranks <- rep(2:10, times = 4)
  ranks <- c(ranks, rep(c("Jack", "Queen", "King", "Ace"), each = 4))
  
  # Shuffle the deck
  deck <- data.frame(Suit = suits, Rank = ranks)
  deck <- deck[sample(nrow(deck)), ]
  
  # Deal cards to players
  player_hands <- vector("list", length = num_players)
  for (i in 1:num_players) {
    start <- (i - 1) * 5 + 1
    end <- i * 5
    player_hands[[i]] <- deck[start:end, ]
  }
  
  # Identify and display each player's hand
  for (i in 1:num_players) {
    cat("Player", i, "Hand:\n")
    print(player_hands[[i]])
    hand_name <- identify_poker_hand(player_hands[[i]]$Suit, player_hands[[i]]$Rank)
    cat("Hand Name:", hand_name, "\n\n")
  }
}

#  Example usage: simulate a round of poker with 4 players
deal_poker_round(4)
#  This code defines two functions: identify_poker_hand to identify the name of a 5-card 
#  stud poker hand, and deal_poker_round to simulate dealing a round of poker with a specified 
#  number of players. The example at the end demonstrates how to use the deal_poker_round 
#  function to simulate a round of poker with 4 players and display their hands along with their 
#  hand names.


## Part b

#  Check the first function with a range of inputs
test1_suits <- c('Spades', 'Spades', 'Diamonds', 'Clubs', 'Hearts')
test1_ranks <- c(5, 4, 5, 5, 'Jack')
test2_suits <- c('Spades', 'Spades', 'Spades', 'Spades', 'Spades')
test2_ranks <- c('Ace', 2, 3, 4, 5)
test3_suits <- c('Hearts', 'Hearts', 'Hearts', 'Hearts', 'Hearts')
test3_ranks <- c('Jack', 'Queen', 2, 7, 7)
test4_suits <- c('Diamonds', 'Hearts', 'Diamonds', 'Clubs', 'Hearts')
test4_ranks <- c('King', 'King', 'King', 'Ace', 'Ace')
test5_suits <- c('Spades', 'Clubs', 'Spades', 'Clubs', 'Clubs')
test5_ranks <- c(2, 2, 7, 7, 6)
identify_poker_hand(test1_suits, test1_ranks)
identify_poker_hand(test2_suits, test2_ranks)
identify_poker_hand(test3_suits, test3_ranks)
identify_poker_hand(test4_suits, test4_ranks)
identify_poker_hand(test5_suits, test5_ranks)

#  Check the second function with a range of inputs
deal_poker_round(2)
deal_poker_round(3)
deal_poker_round(4)
deal_poker_round(5)
deal_poker_round(6)

#  Give a range of inputs ,we can see that both functions occasionally return the same error: 
#  Error in max(ranks) - min(ranks) : 
#    non-numeric argument to binary operator
#  It is quite clear. In functions max() and min(), only values with the same type, i.e all 
#  values are numeric or strings, can be operated. Since in our "rank" argument, values "Jack", 
#  "Queen", "King" and "Ace" are allowed, we cannot compare such strings with an integer. Hence,
#  whenever a Straight comes out, the functions would report errors.

#  Now fix the issue
identify_poker_hand <- function(suits, ranks) {
  # Check for a flush
  is_flush <- length(unique(suits)) == 1
  
  # Check for a straight         * Fixed
  if ('Ace' %in% ranks){         # Change all the strings to numbers
    ranks[ranks == 'Ace'] <- 1
  }
  if ('Jack' %in% ranks){
    ranks[ranks == 'Jack'] <- 11
  }
  if ('Queen' %in% ranks){
    ranks[ranks == 'Queen'] <- 12
  }
  if ('King' %in% ranks){
    ranks[ranks == 'King'] <- 13
  }
  ranks <- as.numeric(ranks)    # Make ranks argument numerical then carry out the calculation
  is_straight <- length(unique(ranks)) == 5 && (max(ranks) - min(ranks)) == 4
  
  # Check for pairs, three of a kind, and four of a kind
  rank_counts <- table(ranks)
  has_pair <- any(rank_counts == 2)
  has_three_of_a_kind <- any(rank_counts == 3)
  has_four_of_a_kind <- any(rank_counts == 4)
  
  # Determine the hand name
  if (is_flush && is_straight) {
    return("Straight Flush")
  } else if (has_four_of_a_kind) {
    return("Four of a Kind")
  } else if (has_three_of_a_kind && has_pair) {
    return("Full House")
  } else if (is_flush) {
    return("Flush")
  } else if (is_straight) {
    return("Straight")
  } else if (has_three_of_a_kind) {
    return("Three of a Kind")
  } else if (sum(rank_counts == 2) == 2) {
    return("Two Pair")
  } else if (has_pair) {
    return("One Pair")
  } else {
    return("High Card")
  }
}
#  The code now runs without any errors but it still cannot produce accurate results when
#  this ranks are given: 10, Jack, Queen, King, Ace.


## Part c

#  Explain the code line-by-line

#  Function to identify the name of a 5-card stud poker hand
identify_poker_hand <- function(suits, ranks) {
  # Check for a flush; unique() returns a vector that drops all repeated values in the argument given
  # length() returns a numerical value which is the number of elements in the argument given, so the 
  # code means that is_flush is true if the unique elements in "suits" is 1.
  is_flush <- length(unique(suits)) == 1           
  
   
  if ('Ace' %in% ranks){          # Change character "Ace" to numerical value 1; x %in% y means element
                                  # x is in the vector y
    ranks[ranks == 'Ace'] <- 1    # Replace the element "Ace" with 1 if "Ace" is in ranks
  }
  if ('Jack' %in% ranks){         # Change character "Jack" to numerical value 11
    ranks[ranks == 'Jack'] <- 11  # Replace the element "Jack" with 11 if "Jack" is in ranks
  }
  if ('Queen' %in% ranks){        # Change character "Queen" to numerical value 12
    ranks[ranks == 'Queen'] <- 12 # Replace the element "Queen" with 12 if "Queen" is in ranks
  }
  if ('King' %in% ranks){         # Change character "King" to numerical value 13
    ranks[ranks == 'King'] <- 13  # Replace the element "King" with 13 if "King" is in ranks
  }
  ranks <- as.numeric(ranks)      # Make ranks argument as numeric to carry out the calculation below
  # Check for a straight; is_Straight is true when there are no repeated elements in "ranks" and the
  # difference between the largest and the smallest number in "rank" is 4. e.g. 4, 5, 6, 7, 8
  is_straight <- length(unique(ranks)) == 5 && (max(ranks) - min(ranks)) == 4
  
  # Check for pairs, three of a kind, and four of a kind
  rank_counts <- table(ranks)     # Use table() to find the frequency of each elements in ranks
  has_pair <- any(rank_counts == 2)     # any() returns a logical value if at least one value is true in
  # its arguments, so has_pair is true if there are any elements in
  # ranks with frequency equals 2
  has_three_of_a_kind <- any(rank_counts == 3)     # has_three_of_a_kind is true if there are any elements
  # in ranks with frequency equals 3
  has_four_of_a_kind <- any(rank_counts == 4)     # has_four_of_a_kind is true if there are any elements
  # in ranks with frequency equals 4
  
  # Determine the hand name
  if (is_flush && is_straight) {                      # Hand name is "Straight Flush" when both is_flush
    return("Straight Flush")                          # and is_straight are true
  } else if (has_four_of_a_kind) {                    # Hand name is "Four of a Kind" when 
    return("Four of a Kind")                          # has_four_of_a_kind is true
  } else if (has_three_of_a_kind && has_pair) {       # Hand name is "Full House" when has_three_of_a_kind
    return("Full House")                              # and has_pair is true
  } else if (is_flush) {                              # Hand name is "Flush" when is_flush is true
    return("Flush")
  } else if (is_straight) {                           # Hand name is "Straight" when is_straight is true
    return("Straight")
  } else if (has_three_of_a_kind) {                   # Hand name is "Three of a Kind" when
    return("Three of a Kind")                         # has_three_of_a_kind is true
  } else if (sum(rank_counts == 2) == 2) {            # Hand name is "Two Pair" when there are at least 2 
    return("Two Pair")                                # elements with frequency 2 in rank
  } else if (has_pair) {                              # Hand name is "One Pair" when has_pair is true
    return("One Pair")
  } else {
    return("High Card")                               # In other cases, the hand name is "High Card"
  }
}

#  Function to simulate dealing a round of poker with a specified number of players
deal_poker_round <- function(num_players) {
  # Create a deck of cards
  # rep() replicates the values in the given vector. There are 4 different suits and we want 13 cards for
  # each suits in a deck, so each = 13.
  suits <- rep(c("Hearts", "Diamonds", "Clubs", "Spades"), each = 13)
  # In a deck, 2 to 10 are represented by numbers, and we want 4 cards for each number, so times = 4. The
  # only difference between each and times here is the order of elements in the created vectors.
  ranks <- rep(2:10, times = 4)
  # In a deck, "Jack", "Queen", "King", "Ace" are represented by characters. Add to the deck here.
  ranks <- c(ranks, rep(c("Jack", "Queen", "King", "Ace"), each = 4))
  
  # Shuffle the deck; make sure all players do not always get consecutive ranks of card for each deal.
  # Create a data frame to form the deck. Each card has a suit and a rank
  deck <- data.frame(Suit = suits, Rank = ranks)
  # sample() generates a random permutation of the given data frame. The default size is the size of the 
  # data frame. We can see the code only changes the order of rows. nrow() gives the row number of deck. 
  deck <- deck[sample(nrow(deck)), ]
  
  # Deal cards to players
  player_hands <- vector("list", length = num_players)   # vector("list", ) creates an empty list for all
  # players hands to be added. The length of the 
  # list is the number of players.
  for (i in 1:num_players) {                             # Create a for loop to add each of the 5 cards
    # for each player.
    start <- (i - 1) * 5 + 1                             # Set the starting card from the deck for each 
    # player by arithmetic method.
    end <- i * 5                                         # Set the ending card.
    player_hands[[i]] <- deck[start:end, ]               # Add the 5 cards row-by-row from start to end
    # for each player to the list player_hands.
  }
  
  # Identify and display each player's hand
  for (i in 1:num_players) {                         # Create a for loop to identify and display each 
    # player's hand
    cat("Player", i, "Hand:\n")                      # \n means start the next text from a new line.
    print(player_hands[[i]])                         # Display each player's hand
    # Identify the name of each player's hand by using the first function identify_poker_hand()
    hand_name <- identify_poker_hand(player_hands[[i]]$Suit, player_hands[[i]]$Rank)
    cat("Hand Name:", hand_name, "\n\n")
  }
}


## Part d

#  Carry out some tests
set.seed(7)
deal_poker_round(2)
deal_poker_round(11)
identify_poker_hand(test1_suits, test1_ranks)
#  1. The inputs and outputs are as described in the question.

#  2. Not all cards are valid as there are duplicates.

#  3. Most of the names are correct, but when 10, "Jack", "Queen", "King", "Ace" show up, the code gives
#  "High Card" instead of "Straight".

#  4. It doesn't. If there are more than 10 hands to be dealt,the 11th players only get 2 cards and players
#  after the 11th get not hands, which shows as NA in the outputs.

#  There are 2 problems

#  The first one: The hand name of 10, "Jack", "Queen", "King", "Ace" is not "Straight" but "High Card". 
#  Bugs of identify_poker_hand(); I change rank "Ace" to integer 1, so when the code checks for a straight,
#  13 - 1 is not equal to 4. We can fix it by add some conditions for this arithmetic.
identify_poker_hand <- function(suits, ranks) {
  # Check for a flush
  is_flush <- length(unique(suits)) == 1
  
  # Check for a straight         
  if ('Ace' %in% ranks){
    if (all(c(10, 'Jack', 'Queen', 'King') %in% ranks)){    # all() returns true if the given logical
      # vector has all of the values true
      ranks[ranks == 'Ace'] <- 9                            # Set "Ace" as 9 so the difference the maximal
      # and the minimal equals 4.
    } else {
      ranks[ranks == 'Ace'] <- 1
    }
  }
  if ('Jack' %in% ranks){
    ranks[ranks == 'Jack'] <- 11
  }
  if ('Queen' %in% ranks){
    ranks[ranks == 'Queen'] <- 12
  }
  if ('King' %in% ranks){
    ranks[ranks == 'King'] <- 13
  }
  ranks <- as.numeric(ranks)    
  is_straight <- length(unique(ranks)) == 5 && (max(ranks) - min(ranks)) == 4
  
  # Check for pairs, three of a kind, and four of a kind
  rank_counts <- table(ranks)
  has_pair <- any(rank_counts == 2)
  has_three_of_a_kind <- any(rank_counts == 3)
  has_four_of_a_kind <- any(rank_counts == 4)
  
  # Determine the hand name
  if (is_flush && is_straight) {
    if (14 %in% ranks) {
      return("Royal Flush")
    } else {
      return("Straight Flush")
    }
  } else if (has_four_of_a_kind) {
    return("Four of a Kind")
  } else if (has_three_of_a_kind && has_pair) {
    return("Full House")
  } else if (is_flush) {
    return("Flush")
  } else if (is_straight) {
    return("Straight")
  } else if (has_three_of_a_kind) {
    return("Three of a Kind")
  } else if (sum(rank_counts == 2) == 2) {
    return("Two Pair")
  } else if (has_pair) {
    return("One Pair")
  } else {
    return("High Card")
  }
}
#  Test after the bug is fixed.
test6_suits <- c('Clubs', 'Clubs', 'Clubs', 'Clubs', 'Clubs')
test6_ranks <- c(10, 'Jack', 'Queen', 'King', 'Ace')
identify_poker_hand(test6_suits, test6_ranks)

#  The second one: There are duplicates in the deck. 
#  Bugs of deal_poker_round(); The bug is caused by the wrong usage of arguments times and each in rep(),
#  so the order is wrong to make each rank correspond to the 4 different suits. It can be fixed by adjust
#  the use of these two arguments in rep() in the step of creating a deck of cards.
deal_poker_round <- function(num_players) {
  # Create a deck of cards
  suits <- rep(c("Hearts", "Diamonds", "Clubs", "Spades"), times = 13)   # Each changed to times
  ranks <- rep(2:10, each = 4)                                           # Times changed to each
  ranks <- c(ranks, rep(c("Jack", "Queen", "King", "Ace"), each = 4))
  
  # Shuffle the deck
  deck <- data.frame(Suit = suits, Rank = ranks)
  deck <- deck[sample(nrow(deck)), ]
  
  # Deal cards to players
  player_hands <- vector("list", length = num_players)
  for (i in 1:num_players) {
    start <- (i - 1) * 5 + 1
    end <- i * 5
    player_hands[[i]] <- deck[start:end, ]
  }
  
  # Identify and display each player's hand
  for (i in 1:num_players) {
    cat("Player", i, "Hand:\n")
    print(player_hands[[i]])
    hand_name <- identify_poker_hand(player_hands[[i]]$Suit, player_hands[[i]]$Rank)
    cat("Hand Name:", hand_name, "\n\n")
  }
}
#  Test after the bug is fixed.
set.seed(7)
deal_poker_round(2)
deal_poker_round(11)