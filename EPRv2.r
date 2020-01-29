# 12/30/19 LSL descriptive updates for JPR article: Legislatures Elected by EPR: An Algorithm v3
# 7/18/19 LSL continue vote count until every voter's preferences are allotted to candidates
# 12/16/18 LSL copied code from 18-11-12-Anders-newAlgorithm.docx

# Init program: remove old data, set random seed
rm(list = ls())
set.seed(1)

setwd("/users/stevanleonard/dropbox/Documents/EPR Steve Bosworth/")
external.data.name <-
  "epr-voter-data-v2.csv" # required name of input ballot data
sink(paste("EPRv2-", external.data.name, ".txt", sep="")) # send output to file

current.time <- format(Sys.time(), "%a %b %d %X %Y")
print("EPRv2")
cat(current.time,"\n")
print("Candidates are elected by voters grading all the candidates using the values: ")
print("6=Excellent, 5=Very Good, 4=Good, 3=Acceptable, 2=Poor, 1=Reject.")
print("The count is complete by the end of Stage 4.")

# NOTE: In addition to defining external.data.name above, these paramaters must be set 
#       to define the operation of the algorithm
cat("Parameters defined for this run of the algorithm\n")
cat("Ballot data file:", external.data.name, "\n")
positions <- 7 # number of open positions to fill with candidates
cat("Positions to fill with candidates:", positions, "\n")
limit.percent <-
  20 # each candidate must transfer votes they receive beyond the limit
cat("Percentage of votes after which a candidate must transfer votes they receive:", limit.percent, "%\n")
# end of user defined parameters

# Initialize general values and read in voter data
grades <-
  c("Reject", "Poor", "Acceptable", "Good", "Very good", "Excellent")
min.affirmed.evaluation <-
  3 # The value of Acceptable is the minimum value a voter can give to help elect a candidate
Acceptable <- 3 # minimum grade to count
voter.mat <-
  as.matrix(read.csv(
    external.data.name,
    strip.white = TRUE,
    header = FALSE
  ))
voters <- nrow(voter.mat)
candidates <- ncol(voter.mat)
for (k in 1:voters) {
  for (l in 1:candidates)
    if ((is.na(voter.mat[k,l]))) {
      cat("Error in input ballot data: unequal length at row", k, "\n")
      stop("Program is terminating")
  }
  if ((min(voter.mat[k, ]) < 1) || (max(voter.mat[k, ] > 6))) {
    cat("Error in input ballot data: Value less than 1 or greater than 6 in row", k, "\n")
    stop("Program is terminating")
  }
}

vote.limit <- round(voters * limit.percent / 100)
if (vote.limit < round(voters / positions)) {
  vote.limit <- round(voters / positions)
  print(
    paste(
      "* * * NOTE: vote.limit set to ",
      vote.limit,
      "to meet minimum set by voters/positions",
      voters,
      "/",
      positions
    )
  )
}

# Initialize candidate and voter matrices
voter.mat.orig <- voter.mat # save: voter.mat is modified in Stage 1
names <- toupper(letters) # candidate names are A, B,..Z
candidate.names <- names[1:candidates]
voter.names <- paste("Voter", 1:voters, sep = " ")
voter.candidate <- 1:voters * NA
voter.round <- 1:voters * NA
voter.transfer <- 1:voters * NA
rownames(voter.mat) <-
  voter.names # init row heading in first column
colnames(voter.mat) <- candidate.names
candidate.proxy.default <- 1:candidates * NA

# Initialize candidate computation matrices
candidate.vec1 <-
  replicate(candidates, 0) # count of selectors by candidate for current round
candidate.evals.vec1 <-
  replicate(candidates, 0) # sum of selectors by candidate for current round
council <- names[1:candidates] # list of winners so far
council.numbers <-
  1:candidates # list of candidates (winners) exclusively yet provisionally receiving 
               # a number of votes (affirmed evaluations) during the current round
weights.stage1 <-
  replicate(candidates, 0) # affirmed evaluations by candidate
selectors <- 6 # list of grades (affirmed evaluations) currently selecting on
selector.add <- 6 # current grade selecting on
winners_so_far <- 0 # count of winning candidates

# Stage 1: Each round calculates winners across all candidates by counting the 
# most highly graded candidate by the largest number of voters
Stage <- 1
print("Begin Stage 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
cat("Perform an initial count of all the affirmed evaluations that must be ")
cat("provisionally yet exclusively counted for each of the candidates. ")
cat("This count is determined in multiple rounds. When more than one candidate ")
cat("receives the same grade from the same voter, the candidate who has ")
cat("the largest number of votes as a result of retaining that grade exclusively ")
cat("is selected. This is justified by the assumption that the candidate who has the ")
cat("largest number of same grades or higher is probably the one most qualified for office.\n")
# define a function for the loop that accumulates votes
more_voters <- TRUE
this_round <- 0
while (more_voters) {
  this_round <- this_round + 1 # i corresponds to the round
  print("*******************************************************")
  print(paste("Stage 1, Round ", this_round, sep = ""))
  print("*******************************************************")
  print(paste("Ballots remaining to be counted (", nrow(voter.mat), "):\n", sep="") )
  print(voter.mat)
  print(paste("Selector: ", selectors, " (", grades[selectors], ")", sep =
                ""))
  candidate.evals.vec1[1:candidates] <- 0 # init all to 0
  candidate.vec1[1:candidates] <- 0 # init all to 0
  for (j in 1:candidates) {
    candidate.vec1[j] <- sum(voter.mat[, j] == selectors)
    candidate.evals.vec1[j] <-
      sum(voter.mat[, j][voter.mat[, j] == selectors])
  }
  print("Count of affirmed evaluations at selector level: ")
  print(rbind(council, candidate.vec1))
  if (sum(candidate.vec1) == 0) {
    cat("No grades for selector", selectors, "(", grades[selectors], ").\n")
  }
  selection <-
    candidate.vec1 == max(candidate.vec1)
  if ((sum(selection) > 0) && (sum(candidate.vec1) > 0)) {
    winner.names <- candidate.names[which(selection)]
    # In case of tie in count of selectors (affirmed evaluations), 
    # first add grades using their ordinal values (Excellent=6..Acceptable=3) using candidate.evals.vec1
    # If a tie exists with ordinal values, use sample() to randomly select a winner
    topWinnersByEvals <-
      which(max(candidate.evals.vec1[selection]) == candidate.evals.vec1)
    # in case of tie in both count of selectors and sum of oridinal values, choose a winner randomly from top ordinal winners
    # sample() returns random choice from equal winners
    winner <-
      sample(winner.names[which(winner.names == names[topWinnersByEvals])], size = 1)
    winner_ties <-
      winner.names[which(winner.names == names[topWinnersByEvals])]
    if (length(winner_ties) > 1) {
      cat("Tied winners: ",
          winner_ties,
          "\n",
          file = "",
          sep = " ")
      cat(
        "Sum of each candidate's ordinal evaluations, counted numerically. Used first to break ties before one of the candidates with the same number of affirmed evaluations is chosen randomly.\n"
      )
      print(candidate.evals.vec1)
      #print("Selection: ")
      #print(selection)
    }
    winner.number <- which(winner == candidate.names)
    if (weights.stage1[winner.number] == 0)
    {
      winners_so_far <- winners_so_far + 1
      #council.numbers[winners_so_far] <- winner.number
    }
    weights.stage1[winner.number] <-
      weights.stage1[winner.number] + candidate.vec1[winner.number]
    #voter.ind<-which(voter.names %in% rownames(voter.mat[voter.mat[,winner.number] %in% selectors,]))
    voter.ind <-
      which(voter.names %in% rownames(voter.mat)[voter.mat[, winner.number] == selectors])
    voter.candidate[voter.ind] <- winner
    voter.round[voter.ind] <- this_round
    voter.mat <-
      voter.mat[!voter.mat[, winner.number] == selectors, ]
    print(
      paste(
        "The winner of round ",
        this_round,
        " is: ",
        winner,
        ", with ",
        candidate.vec1[winner.number],
        " affirmed evaluations.",
        sep = ""
      )
    )
    print(paste("Winner number (the candidate who received the largest number of affirmed evaluations in the current round): ", winner.number))
    print(
      paste(
        "Provisional running totals of votes (affirmed evaluations) received by each candidate after round ",
        this_round,
        ":",
        sep = ""
      )
    )
    print(rbind(council, weights.stage1))
    print("Votes thus far: ")
    print(cbind(voter.names, voter.candidate, voter.round))
  }
  # init for next round and check if all voters processed
  if (nrow(voter.mat) == 0)
    more_voters <- FALSE
  else {
    keep_selector <- FALSE
    # look for this selector in remaining ballots and keep if found
    for (k in 1:dim(voter.mat)[1]) {
      if (selector.add == max(voter.mat[k, ])) {
        keep_selector <- TRUE
      }
    }
    if (keep_selector == FALSE)
    {
      # look for a selector in valid range, quit if not found
      valid_selector <- FALSE
      for (k in 1:dim(voter.mat)[1]) {
        if (max(voter.mat[k, ]) >= min.affirmed.evaluation)
        {
          valid_selector <- TRUE
          break
        }
      }
      if (valid_selector)
      {
        selector.add <- selector.add - 1
        selectors <-
          selector.add # don't keep a list of selectors, only current
      }
      else
        more_voters <- FALSE
    }
  }
}

if (more_voters) {
  print(paste("Ballots remaining to be counted (", nrow(voter.mat), "):\n", sep="") )
  print(voter.mat)
  notremoved <- NULL
  for (k in 1:dim(voter.mat)[1]) {
    if (max(voter.mat[k,]) > (min.affirmed.evaluation - 1)) {
      newchoice <- which(voter.mat[k,] == max(voter.mat[k,]))
      weights.stage1[newchoice] <- weights.stage1[newchoice] + 1
      print(
        paste(
          "The weight of ",
          rownames(voter.mat)[k],
          " goes to ",
          names(newchoice),
          ". ",
          "This selection is based on the voter's evaluation which is above min.affirmed.evaluation.",
          sep = ""
        )
      )
      voter.ind <- which(voter.names %in% rownames(voter.mat)[k])
      voter.candidate[voter.ind] <- names(newchoice)
      voter.round[voter.ind] <-
        "Final above min.affirmed.evaluation"
    } else
      notremoved <- c(notremoved, k)
    
    voter.mat <- voter.mat[notremoved,]
    print("Ballots with no vote counted:")
    print(voter.mat)
  }
  
  for (n in 1:dim(voter.mat)[1]) {
    voter.ind <- which(voter.names %in% rownames(voter.mat)[n])
    voter.candidate[voter.ind] <- "None"
    voter.round[voter.ind] <- "Ballot not countable"
    print(paste("Uncountable ballot for voter: ",
      voter.ind))
  }
}
for (m in 1:length(council)) {
  print(paste(council[m], " has the following supporters: "))
  print(voter.names[voter.candidate %in% council[m]])
}
print("End Stage 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
print("Vote apportionment after Stage 1: ")
print(cbind(voter.names, voter.candidate, voter.round))
print("Affirmed evaluations by candidate")
print(rbind(candidate.names[council.numbers], weights.stage1))

print("Begin Stage 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
Stage <- 2
# apply a percentabe limit, such as 20%, by candidate and transfer to a candidate 
# with a remaining highest grade from the same voter. Voters are selected randomly.
weights.stage2 <- weights.stage1
print(
  paste(
    "To eliminate the possibility that any winner could receive enough affirmed evaluations ",
    "(weighted votes) to dictate to the council, this election limits the number of votes a ",
    "super-popular candidate can retain to ",
    limit.percent,
    "%, or ",
    vote.limit,
    " of the ",
    voters,
    " total voters.",
    sep = ""
  )
)
cat("During Stage 2, if some of the extra votes initially held by a super-popular candidate ")
cat("cannot be automatically transferred, the remaining votes that must be transferred ")
cat("become proxy votes that are transferred publicly during the last step in Stage 4 ")
cat(
  "to any of the other eligible winners judged by the relevant super-popular winner to be most fit for office.\n"
)
cat("Note: A future version of the algorithm will make the candidate your vote went to in Stage 1 ")
cat("the owner of your proxy vote, if required.\n")
# repeat while super-popular candidates remain, sorted by candidate with most votes to transfer
# Note: A future version of the program will select the next super-popular candidate by lot as the order of
#   candidates does not change the result
proxy.candidate <- NULL
xfer.loop <- 0
while ((max.weight <- max(weights.stage2)) > vote.limit) {
  # find the candidate with the highest weight (largest number of affirmed evaluations)
  # If more than one candidate with the same weight, select randomly
  xfer.loop <- xfer.loop + 1
  print("**********************")
  cat("Stage 2 transfer loop ", xfer.loop, "\n")
  print("**********************")
  transfer.candidates <- NULL
  for (i in 1:candidates) {
    if (weights.stage2[i] == max.weight)
      transfer.candidates <- c(transfer.candidates, i)
  }
  cat("Candidates currently holding more than ",
      vote.limit,
      "votes: ",
      names[transfer.candidates],
      "\n")
  if (length(transfer.candidates) > 1)
    transfer.candidate <- sample(transfer.candidates, size = 1)
  else
    transfer.candidate <- transfer.candidates
  transfer.count <- max.weight - vote.limit
  transfer.name <- names[transfer.candidate]
  cat("Candidate with most votes: ",
      transfer.name,
      "(",
      max.weight,
      ")\n")
  print(paste(
    "Transfer ",
    transfer.count,
    " votes from candidate ",
    transfer.name,
    sep = ""
  ))
  # process transfers for transfer.candidate until complete
  # note this might end with proxy votes for some voters if not enough voters
  # marked other candidates or if the candidates they marked have too many votes
  # Create the set of ballots from this candidate that have a remaining highest candidate marked
  voters.to.transfer <- NULL
  voter.candidate.to.transfer <- NULL
  # select voters that match candidate to transfer and have not been marked for transfer
  for (i in 1:voters) {
    if (is.na(voter.candidate[i]))
      next()
    if ((voter.candidate[i] == transfer.name) &&
        (is.na(voter.transfer[i])))
      voters.to.transfer <- c(voters.to.transfer, i)
  }
  print("Select from the following voters' ballots that have a remaining highest grade for an eligible candidate:")
  print(voters.to.transfer)
  # select only those voters who voted for this candidate
  voter.mat.stage2 <-
    voter.mat.orig[voters.to.transfer,] # use only voters for this candidate
  if (length(voters.to.transfer) == 1) {
    # if only one voter force a multi-dim array by adding and then removing an empty row
    voter.mat.stage2 <- rbind(voter.mat.stage2, NA)
    voter.mat.stage2 <- na.omit(voter.mat.stage2)    
  }
  # set grade for this candidate to 0 to remove from selection
  voter.mat.stage2[, transfer.candidate] <- 0
  # pick the remaining highest candidate for each voter
  voter.count.stage2 <- length(voters.to.transfer)
  candidates.stage2 <-
    replicate(candidates, 0) # count of candidates
  # sum the count of candidates that have the highest grade across voters
  voters.stage2 <- NULL
  voters.next.highest.graded <- NULL
  print("Next highest graded candidate by voter:")
  for (i in 1:voter.count.stage2) {
    selector.max <- max(voter.mat.stage2[i, ]) # highest selector
    if (selector.max >= Acceptable) {
      candidates.equal.stage2 <- NULL
      for (k in 1:candidates) {
        if (voter.mat.stage2[i, k] == selector.max)
          candidates.equal.stage2 <- c(candidates.equal.stage2, k)
      }
      if (length(candidates.equal.stage2) > 1)
        # select one of multiple candidates with same grade
        candidate.stage2 <-
          sample(candidates.equal.stage2, size = 1)
      else
        candidate.stage2 <- candidates.equal.stage2
      voter.candidate.to.transfer <-
        c(voter.candidate.to.transfer, candidate.stage2) # list of highest graded remaining candidates
      voters.next.highest.graded <-
        c(voters.next.highest.graded, voters.to.transfer[i]) # list of corresponding voters
      print(paste("voter ",
                  voters.to.transfer[i],
                  " candidate ",
                  names[candidate.stage2],
                  sep =
                    ""))
      if (length(candidates.equal.stage2) > 1) {
        cat("   candidate ties: ", names[candidates.equal.stage2], "\n")
      }
      candidates.stage2[candidate.stage2] <-
        candidates.stage2[candidate.stage2] + 1
    }
  } # end of loop that builds list of highest graded remaining candidates and corresponding voters
    # for super-popular candidates
  candidates.to.ignore <- which(weights.stage2 >= vote.limit)
  candidates.stage2[candidates.to.ignore] <- 0
  cat("Candidates and their current affirmed evaluations that may receive Stage 2 transfers from the voters listed above: \n")
  print(rbind(council, candidates.stage2))
  proxy.required <-
    FALSE # must transfer proxy votes if not enough graded votes to transfer
  
  # loop until all votes from this super-popular candidate are transferred or determined to be proxy votes
  while (transfer.count > 0) {
    # make sure there are votes to transfer
    if (max(candidates.stage2) == 0) {
      proxy.required <- TRUE
      break
    }
    # select up to transfer.count votes to transfer
    candidate.to.transfer.to <-
      which(candidates.stage2 == max(candidates.stage2))
    if (length(candidate.to.transfer.to) > 1) {
      cat("multiple candidates with max: ",
          candidate.to.transfer.to,
          "\n")
      candidate.to.transfer.to <-
        sample(candidate.to.transfer.to, size = 1)
    }
    xfer.count <-
      min(candidates.stage2[candidate.to.transfer.to], transfer.count)
    if ((weights.stage2[candidate.to.transfer.to] + xfer.count) > vote.limit) {
      # transfer limited to reaching vote limit
      xfer.count <-
        vote.limit - weights.stage2[candidate.to.transfer.to]
      cat("* * * Transfer to candidate ",
          names[candidate.to.transfer.to],
          "limited to ",
          xfer.count,
          " votes\n")
    }
    cat(
      "Candidate",
      names[candidate.to.transfer.to],
      "automatically selected by lot to receive up to",
      xfer.count,
      "votes from",
      transfer.name,
      "\n"
    )
    # update the current votes to show which voters had their vote transferred and to whom
    voters.to.transfer.candidate <-
      which(voter.candidate.to.transfer == candidate.to.transfer.to)
    voters.transferred <-
      sort(sample(voters.next.highest.graded[voters.to.transfer.candidate], size = xfer.count))
    cat(
      "Voters' ballots selected automatically by lot to transfer: ",
      voters.transferred,
      "\n"
    )
    voter.candidate[voters.transferred] <-
      names[candidate.to.transfer.to]
    voter.transfer[voters.transferred] <-
      paste("From ", transfer.name, " to ", names[candidate.to.transfer.to], sep =
              "")
    weights.stage2[transfer.candidate] <-
      weights.stage2[transfer.candidate] - xfer.count
    weights.stage2[candidate.to.transfer.to] <-
      weights.stage2[candidate.to.transfer.to] + xfer.count
    transfer.count <- transfer.count - xfer.count
    candidates.stage2[candidate.to.transfer.to] <-
      0 # clear: no longer open for transfer
    cat("Affirmed evaluations by voter for candidate for loop", xfer.loop, "of Stage 2\n")
    print(rbind(candidate.names[council.numbers], weights.stage2))
  }
  if (proxy.required) {
    cat(
      "* * * Proxy votes (",
      transfer.count,
      ") required for transfer from candidate ",
      transfer.name,
      "\n"
    )
    voters.to.transfer <- NULL
    # select voters that match candidate to transfer and have not been marked for transfer
    for (i in 1:voters) {
      if (is.na(voter.candidate[i]))
        next()
      if ((voter.candidate[i] == transfer.name) &&
          (is.na(voter.transfer[i])))
        voters.to.transfer <- c(voters.to.transfer, i)
    }
    cat("Voters available to transfer ", voters.to.transfer, "\n")
    xfer.proxy.count <-
      min(length(voters.to.transfer), transfer.count)
    voters.transferred <-
      sample(voters.to.transfer, size = xfer.proxy.count)
    cat(
      "Transfer ",
      xfer.proxy.count,
      " proxy votes for candidate ",
      transfer.name,
      " to these voters:\n"
    )
    print(voters.transferred)
    if (xfer.proxy.count < transfer.count) {
      # in Stage 2, this should never happen as each candidate can assign any number
      # of their voters as proxy votes. See Stage 4 for implementation of proxy votes
      stop("* * * ERROR: candidate does not have enough voters to handle proxy votes")
      # save the default proxy votes for this candidate that could not be transferred
      candidate.proxy.default[transfer.candidate] <-
        transfer.count - xfer.proxy.count
    }
    # update voter array
    voter.transfer[voters.transferred] <-
      paste("Proxy vote Stage 2 from ", transfer.name, sep = "")
    # proxy votes end the transfer phase for this candidate
    # update weights to reflect proxy votes that will occur later
    weights.stage2[transfer.candidate] <-
      weights.stage2[transfer.candidate] - transfer.count
    print("Affirmed evaluations by candidate")
    print(rbind(candidate.names[council.numbers], weights.stage2))
    transfer.count <- 0 # done with this candidate
  }
}
print("End Stage 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
print("Vote apportionment after Stage 2: ")
print(cbind(voter.names, voter.candidate, voter.round, voter.transfer))
print("Affirmed evaluations by candidate")
print(rbind(candidate.names[council.numbers], weights.stage2))
print("Begin Stage 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
Stage <- 3
weights.stage3 <- weights.stage2
candidate.count <- length(which(weights.stage3 != 0))
cat("Step 1: Determine the elected candidates, who are the candidates that received the largest number ")
cat("of affirmed evaluations by the end of Stage 2.\n")
cat(
  "Note: A future version of the algorithm will sum each candidate's ordinal evaluations, counted numerically, and will use this value first to break ties before one of the candidates with the same number of affirmed evaluations is chosen randomly.\n"
)
cat(
  "Step 2: Transfer votes from unelected candidates to those selected as winning candidates as follows:\n"
)
cat(
  "Start with the unelected candidate currently holding the largest number of affirmed evaluations, resolving any ties randomly. ")
cat(
  "Transfer all votes that can be transferred automatically by the algorithm to any of the eligible winners. ")
cat("These are ballots that identify an eligible winner with a grade of at least Acceptable. ")
cat("Ballots that connot be transferred automatically become proxy votes of that unelected candidate and are handled in Stage 4.\n")
cat("Note: A future version of the algorithm will make the candidate your vote went to in Stage 1 ")
cat("the owner of your proxy vote, if required.\n")
# NOTE: As the results will be the same, a future version of the algorithm will require the order of these transfers to be determined by lot.
if (candidate.count < positions) {
  # Contingency 1: fewer candidates than positions to fill
  print(
    paste(
      "Contingency 1: Fewer candates with votes",
      candidate.count,
      " than positions to fill",
      positions
    )
  )
  cat("The number of winners that received affirmed evaluations by this stage in the count ")
  cat("is fewer than the number of candidates to be elected. ")
  cat(
    "In this case, these fewer candidates are elected, and they will ensure that each voter ")
cat("is as fully represented as possible in the legislature "
  )
  cat("by the candidate they gave their highest grade. ")
  cat(
    "However, if a city or state wishes to require the target number of candidates to be elected "
  )
  cat(
    "even when the target number has not been elected by the above steps, the algorithm can be modified to achieve this result. "
  )
  cat(
    "At the same time, note that such a modification could also have the undesirable effect of partially wasting some citizens votes qualitatively.\n"
  )
  cat("The algorithm can be modified as follows:\n")
  cat(
    "Starting with the winning candidate who has the largest number of affirmed evaluations, automatically transfer one ballot chosen by lot to one of the candidates "
  )
  cat(
    "who has not yet received any votes. Do the same with such a ballot currently held by the winner with the second highest number of affirmed evaluations, "
  )
  cat(
    "then the third, etc. If necessary, repeat this action of one at a time transfers until each of the target number of winners has received at least one vote.\n"
  )
  stop("* * * ERROR: This contingency is not yet implemented.\n")
}
# select the elected candidates
candidates.sorted <-
  sort(weights.stage3[which(weights.stage3 != 0)],
       decreasing = TRUE) # sort the candidates by larget weight (affirmed evaluations)
candidates.sorted.index <-
  order(weights.stage3[which(weights.stage3 != 0)],
        decreasing = TRUE) # compute the sorted candidates' index (position) within the original weights
cat("\nStage 3, Step 1: Select the",
    positions,
    "candidates to be elected.\n")
print("Candidates' affirmed evaluations in descending order and corresponding candidate as received from Stage 2")
print (rbind(names[candidates.sorted.index], candidates.sorted))
if (candidate.count > positions) {
  # determine winning candidates
  if (candidates.sorted[positions] == candidates.sorted[positions + 1]) {
    # decide randomly which of the candidates with equal votes at lowest position to select as winner
    candidates.to.narrow <- candidates.sorted[positions:candidates]
    candidate.lowest <-
      sample(which(candidates.to.narrow == max(candidates.to.narrow)), size =
               1)
    cat(
      "From tie of lowest candidates,",
      "the algorithm randomly selected candidate",
      names[positions + candidate.lowest - 1],
      "\n"
    )
    # must relate this selection back to candidate in index, which points to position in original weights
    # for example, if candidate.lowest is 2, then the candidate that corresponds
    #   to the index for second candidate relative to positions
    candidate.lowest.index <-
      candidates.sorted.index[positions + candidate.lowest - 1]
    # swap the two index positions (they might be the same)
    candidate.index.orig <-
      candidates.sorted.index[positions + candidate.lowest - 1]
    candidates.sorted.index[positions + candidate.lowest - 1] =
      candidates.sorted.index[positions]
    candidates.sorted.index[positions] =
      candidate.index.orig
  }
}
print("Affirmed evaluations for winning candidates after Step 1 of Stage 3")
print (rbind(names[candidates.sorted.index[1:positions]], candidates.sorted[1:positions]))

cat("\nStage 3, Step 2: Transfer votes from unelected candidates to elected candidates.\n")
cat("Start with the unelected candidate with the largest number of votes to transfer. ")
cat("For ballots that have a remaining highest grade of at least Acceptable for an eligible candidate, ")
cat("transfer these votes to eligible winners.\n")
if (candidate.count > positions) {
  # transfer votes from highest weighted unelected candidates until all their votes are transferred
  # repeat until all unelected candidates' votes are transferred
  proxy.candidate <- NULL
  weights.stage3.transfer <-
    weights.stage3 # use this to track the remaining unelected candidates
  # zero weights of elected candidates to do transfer of votes from unelected candidates
  weights.stage3.transfer[candidates.sorted.index[1:positions]] <- 0
  xfer.loop <- 0
  while ((max.weight <- max(weights.stage3.transfer)) > 0) {
    # find the candidate with most votes to transfer, ties broken by random selection
    # Note: A future version of the program will select the next super-popular candidate by lot as the order of
    #   candidates does not change the result
    xfer.loop <- xfer.loop + 1
    print("**********************")
    cat("Stage 3, Step 2 transfer loop ", xfer.loop, "\n")
    print("**********************")
    transfer.candidates <- NULL
    for (i in 1:candidates) {
      if (weights.stage3.transfer[i] == max.weight)
        transfer.candidates <- c(transfer.candidates, i)
    }
    if (length(transfer.candidates) > 1)
      transfer.candidate <- sample(transfer.candidates, size = 1)
    else
      transfer.candidate <- transfer.candidates
    transfer.count <- max.weight
    transfer.name <- names[transfer.candidate]
    print(
      paste(
        "Transfer ",
        transfer.count,
        " votes (remaining highest affirmed evaluations) from unelected candidate ",
        transfer.name,
        sep = ""
      )
    )
    # process transfers for transfer.candidate until the number required are transferred
    # note this automated transfer might end with proxy votes from some voters if not enough voters
    #   marked other eligible candidates
    # Create the set of ballots from this candidate that have a remaining highest grade of at least Acceptable
    # for an eligible candidate
    voters.to.transfer <- NULL
    voter.candidate.to.transfer <- NULL
    # select voters that match candidate to transfer and have not been marked for transfer
    for (i in 1:voters) {
      if (is.na(voter.candidate[i]))
        next()
      if ((voter.candidate[i] == transfer.name) &&
          (is.na(voter.transfer[i])))
        voters.to.transfer <- c(voters.to.transfer, i)
    }
    cat("Voters' ballots to select from:", voters.to.transfer, "\n")
    # select only those voters who voted for this candidate
    voter.mat.stage3 <-
      voter.mat.orig[voters.to.transfer,] # use only voters for this candidate
    if (length(voters.to.transfer) == 1) {
      # if only one voter force a multi-dim array by adding and then removing an empty row
      voter.mat.stage3 <- rbind(voter.mat.stage3, NA)
      voter.mat.stage3 <- na.omit(voter.mat.stage3)    
    }
    # set grade for unelected candidates to 0 to remove from selection
    voter.count.stage3 <- length(voters.to.transfer)
    for (i in 1:voter.count.stage3)
      for (k in 1:candidates)
        if (! (k %in% candidates.sorted.index[1:positions]))
          voter.mat.stage3[i, k] <- 0
    # for each voter, pick the remaining candidate with highest grade
    candidates.stage3 <-
      replicate(candidates, 0) # count of candidates
    # sum the count of candidates that have the highest grade across voters
    voters.stage3 <- NULL
    voters.next.highest.graded <- NULL
    print("Remaining highest graded candidate by voter:")
    for (i in 1:voter.count.stage3) {
      selector.max <- max(voter.mat.stage3[i, ]) # highest selector
      if (selector.max >= Acceptable) {
        candidates.equal.stage3 <- NULL
        for (k in 1:candidates) {
          if (voter.mat.stage3[i, k] == selector.max)
            candidates.equal.stage3 <- c(candidates.equal.stage3, k)
        }
        if (length(candidates.equal.stage3) > 1)
          # select one of multiple candidates with same grade
          candidate.stage3 <-
            sample(candidates.equal.stage3, size = 1)
        else
          candidate.stage3 <- candidates.equal.stage3
        voter.candidate.to.transfer <-
          c(voter.candidate.to.transfer, candidate.stage3) # list of highest graded remaining candidates
        voters.next.highest.graded <-
          c(voters.next.highest.graded, voters.to.transfer[i]) # list of corresponding voters 
        print(paste(
          "voter ",
          voters.to.transfer[i],
          " candidate ",
          names[candidate.stage3],
          sep =
            ""
        ))
        if (length(candidates.equal.stage3) > 1) {
          cat("   candidate ties: ", names[candidates.equal.stage3], "\n")
        }
        candidates.stage3[candidate.stage3] <-
          candidates.stage3[candidate.stage3] + 1
      }
    } # end for loop to transfer candidate votes (weights) from unelected candidates
    candidates.to.ignore <- which(weights.stage3 >= vote.limit)
    if (exists("candidates.to.ignore"))
      candidates.stage3[candidates.to.ignore] <- 0
    cat("Candidates eligible to receive Stage 3 transfers from candidate", transfer.name, "\n")
    print(rbind(council, candidates.stage3))
    proxy.required <-
      FALSE # must transfer proxy votes if not enough graded votes were found to automatically transfer
    # loop until all votes from this unelected candidate are transferred or assigned to be proxy votes
    while (transfer.count > 0) {
      # make sure there are votes to transfer
      if (max(candidates.stage3) == 0) {
        proxy.required <- TRUE
        break
      }
      # select up to transfer.count votes to transfer
      candidate.to.transfer.to <-
        which(candidates.stage3 == max(candidates.stage3))
      if (length(candidate.to.transfer.to) > 1) {
        cat("multiple candidates with max: ",
            candidate.to.transfer.to,
            "\n")
        candidate.to.transfer.to <-
          sample(candidate.to.transfer.to, size = 1)
      }
      xfer.count <-
        min(candidates.stage3[candidate.to.transfer.to], transfer.count)
      if ((weights.stage3[candidate.to.transfer.to] + xfer.count) > vote.limit) {
        # transfer limited to reaching vote limit
        xfer.count <-
          vote.limit - weights.stage3[candidate.to.transfer.to]
        cat("* * * Transfer to candidate ",
            names[candidate.to.transfer.to],
            "limited to ",
            xfer.count,
            " votes\n")
      }
      cat(
        "Transfer ",
        xfer.count,
        " votes from candidate ",
        transfer.name,
        "to ",
        names[candidate.to.transfer.to],
        "\n"
      )
      # update the current votes to show which voters had their vote transferred and to whom
      voters.to.transfer.candidate <-
        which(voter.candidate.to.transfer == candidate.to.transfer.to)
      voters.transferred <-
        sort(sample(voters.next.highest.graded[voters.to.transfer.candidate], size = xfer.count))
      cat("Voters' ballots to transfer: ", voters.transferred, "\n")
      voter.candidate[voters.transferred] <-
        names[candidate.to.transfer.to]
      voter.transfer[voters.transferred] <-
        paste("From ", transfer.name, " to ", names[candidate.to.transfer.to], sep =
                "")
      weights.stage3[transfer.candidate] <-
        weights.stage3[transfer.candidate] - xfer.count
      # also update transfer weights used to track unelected candidates
      weights.stage3.transfer[transfer.candidate] <-
        weights.stage3.transfer[transfer.candidate] - xfer.count
      weights.stage3[candidate.to.transfer.to] <-
        weights.stage3[candidate.to.transfer.to] + xfer.count
      transfer.count <- transfer.count - xfer.count
      candidates.stage3[candidate.to.transfer.to] <-
        0 # clear: no longer open for transfer
      print("Remaining weights held by unelected candidates:")
      print(rbind(council, weights.stage3.transfer))
      print("Affirmed evaluations by candidate")
      print(rbind(council, weights.stage3))
    }
    if (proxy.required) {
      cat(
        "* * * No more automatic transfer of votes available from candidate ", transfer.name, ". Therefore, ",
        transfer.count,
        " proxy vote(s) must be transferred by candidate ",
        transfer.name,
        " in Stage 4.\n", sep=""
      )
      voters.to.transfer <- NULL
      # select voters that match candidate to transfer and have not been marked for transfer
      for (i in 1:voters) {
        if (is.na(voter.candidate[i]))
          next()
        if ((voter.candidate[i] == transfer.name) &&
            (is.na(voter.transfer[i])))
          voters.to.transfer <- c(voters.to.transfer, i)
      }
      cat("Voters' ballots available to transfer:",
          voters.to.transfer,
          "\n")
      xfer.proxy.count <-
        min(length(voters.to.transfer), transfer.count)
      voters.transferred <-
        sample(voters.to.transfer, size = xfer.proxy.count)
      cat(
        "Transfer ",
        xfer.proxy.count,
        "proxy vote(s) from candidate ",
        transfer.name,
        "from these voters' ballots:",
        sort(voters.transferred),
        "\n"
      )
      if (xfer.proxy.count < transfer.count) {
        # in Stage 3, this should never happen as each candidate can assign any number
        # of their voters as proxy votes.
        stop("* * * ERROR: candidate does not have enough voters to handle proxy votes")
        # save the proxy votes for this candidate that could not be transferred
        candidate.proxy.default[transfer.candidate] <-
          transfer.count - xfer.proxy.count
        #print("Proxy votes that could not be transferred automatically: ", transfer.count-xfer.proxy.count)
      }
      # update voter array
      voter.transfer[voters.transferred] <-
        paste("Proxy vote Stage 3 from ", transfer.name, sep = "")
      # proxy votes end the transfer phase for this candidate
      # update weights to reflect proxy votes that will occur later
      weights.stage3[transfer.candidate] <-
        weights.stage3[transfer.candidate] - transfer.count
      # also update transfer weights used to track unelected candidates
      weights.stage3.transfer[transfer.candidate] <-
        weights.stage3.transfer[transfer.candidate] - transfer.count
      print("Affirmed evaluations by candidate")
      print(rbind(council, weights.stage3))
      print("Remaining weights held by unelected candidates:")
      print(rbind(council, weights.stage3.transfer))
      transfer.count <- 0 # done with this candidate
    }
  }
}
print("End Stage 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
print("Vote apportionment after Stage 3: ")
print(cbind(voter.names, voter.candidate, voter.round, voter.transfer))
print("Affirmed evaluations for winning candidates")
print(rbind(candidate.names[council.numbers], weights.stage3))
cat("\n")
print("Begin Stage 4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
cat("Stage 4, though not carried out by the algorithm, identifies the proxy votes ")
cat("determined in Stages 2 and 3 that must be publicly transferred.\n")
cat("Finalize the weighted votes of each winner as follows:\n")
cat("The candidate with the largest number of affirmed evaluations (ties resolved by lot) ")
cat("publicly transfers their proxy votes to any of the eligible winners they find most suitable for office. "
)
cat("Continue the same transfer process with the candidate with the next most affirmed evaluations ")
cat("until all proxy votes are transferred.\n")
# accumulate the voters with proxy votes to be transferred
voter.names.stage4 <- NULL
voter.candidate.stage4 <- NULL
voter.transfer.stage4 <- NULL
for (i in 1:voters) {
  if (is.na(voter.transfer[i]))
    next()
  first.char <- substr(voter.transfer[i], 1, 1)
  if (first.char == "P") {
    voter.names.stage4 <- c(voter.names.stage4, voter.names[i])
    voter.transfer.stage4 <-
      c(voter.transfer.stage4, voter.transfer[i])
  }
}
if (is.null(voter.names.stage4)) {
  print("No proxy votes to transfer")
} else {
  cat("Voter ballots with proxy votes to transfer:\n")
  print(cbind(voter.names.stage4, voter.transfer.stage4))
}
print("End Stage 4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
cat("\n")
print("******** End of EPR output **********")
