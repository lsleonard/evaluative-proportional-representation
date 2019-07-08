# 7/7/19 LSL continue vote count until every voter's preferences are allotted to candidates
# 12/16/18 LSL copied code from 18-11-12-Anders-newAlgorithm.docx

# Init program: remove old data, set random seed
rm(list = ls())
set.seed(1)

# NOTE: These paramaters must be set to define the operation of the algorithm
cat("Parameters defined for this run of the algorithm\n")
external.data.name <-
  "epr-voter-data-v2.csv" # name of input ballot data
cat("Ballot data file:", external.data.name, "\n")
positions <- 7 # number of open positions to fill with candidates
cat("Positions to fill with candidates:", positions, "\n")
limit.percent <-
  20 # each candidate must transfer votes they receive beyond the limit
cat("Percentage of votes after which a candidate must transfer votes they receive:", limit.percent, "%\n")
setwd("/users/stevanleonard/dropbox/Documents/EPR Steve Bosworth/")
# end of user defined parameters

sink(paste("EPRv2-", external.data.name, ".txt", sep="")) # send output to file
today <- Sys.Date()
format(today, format = "%B %d %Y")
print("EPRv2")
print(today)
print("Candidates are elected by voters grading all the candidates using the values: ")
print("6=Excellent, 5=Very Good, 4=Good, 3=Acceptable, 2=Poor, 1=Reject.")
print("The count is complete by the end of Stage 4.")

# Initialize general values and read in voter data
grades <-
  c("Reject", "Poor", "Acceptable", "Good", "Very good", "Excellent")
min.affirmed.evaluation <-
  3 # This value of Acceptable is the minimum value a voter can give a candidate that represents an affirmed evaluation
Acceptable <- 3 # minimum grade to count
voter.mat <-
  as.matrix(read.csv(
    external.data.name,
    strip.white = TRUE,
    header = FALSE
  ))
voters <- nrow(voter.mat)
vote.limit <- round(voters * limit.percent / 100)
candidates <- ncol(voter.mat)
if (vote.limit < round(voters / candidates)) {
  vote.limit <- round(voters / candidates)
  print(
    paste(
      "* * * NOTE: vote.limit set to ",
      vote.limit,
      "to meet meet limit set by voters/candidates",
      voters,
      "/",
      candidates
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
  1:candidates # list of winner index numbers so far
weights.stage1 <-
  replicate(candidates, 0) # affirmed evaluations by candidate
selectors <- 6 # list of grades currently selecting on
selector.add <- 6 # current grade selecting on
winners_so_far <- 0 # count of winning candidates

# Stage 1: Calculate winners across all candidates by processing most valued for each voter
Stage <- 1
print("Begin Stage 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
cat("Perform an initial count of affirmed evaluations for all candidates. ")
cat("This count is determined in multiple rounds by giving the candidate ")
cat("with the most votes at the current grade that number of votes.\n")
# define a function for the loop that accumulates votes
more_voters <- TRUE
this_round <- 0
while (more_voters) {
  this_round <- this_round + 1 # i corresponds to the round
  print("*******************************************************")
  print(paste("ROUND: ", this_round, sep = ""))
  print("*******************************************************")
  print("Ballots remaining:")
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
    # In case of tie in count of selectors this selects top by sum of evals
    topWinnersByEvals <-
      which(max(candidate.evals.vec1[selection]) == candidate.evals.vec1)
    # in case of tie in both count of selectors and sum of evals, choose by lot
    # sample returns random choice from equal winners
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
        "Sum of each candidate's ordinal evaluations if counted numerically. Used first to break ties before one of the candidates with the same score is chosen randomly.\n"
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
    print(paste("Winner number: ", winner.number))
    print(
      paste(
        "Weighted votes by candidate (affirmed evaluations) after round ",
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
  print("Remaining ballots:")
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
# apply a percentabe limit by candidate and transfer to next highest candidate of voter
weights.stage2 <- weights.stage1
print(
  paste(
    "Limit of ",
    vote.limit,
    " votes per candidate based on ",
    limit.percent,
    "% of voters (",
    voters,
    ")",
    sep = ""
  )
)
cat("During Stage 2, if some of the extra votes initially held by an overly popular candidate ")
cat("cannot be automatically transferred, the remaining votes that must be transferred ")
cat("become proxy votes that must be transferred publicly during the last step in Stage 4 ")
cat(
  "to any of the other winners judged by the relevant overly popular winner to be most fit for office.\n"
)
# repeat while overweight candidates
proxy.candidate <- NULL
xfer.loop <- 0
while ((max.weight <- max(weights.stage2)) > vote.limit) {
  # find the candidate with the highest weight (random if more than one)
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
  # Create the set of ballots from this candidate that have a next highest candidate
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
  print("Select from the following voters' ballots:")
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
  # pick the highest remaining candidate for each voter
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
        c(voter.candidate.to.transfer, candidate.stage2) # list of next highest graded candidates
      voters.next.highest.graded <-
        c(voters.next.highest.graded, voters.to.transfer[i]) # list of next highest graded voters
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
  } # end for to build candidate weights for overweight candidate
  candidates.to.ignore <- which(weights.stage2 >= vote.limit)
  candidates.stage2[candidates.to.ignore] <- 0
  cat("Candidates and their affirmed evaluations that may receive Stage 2 transfers: \n")
  print(rbind(council, candidates.stage2))
  proxy.required <-
    FALSE # must transfer proxy votes if not enough graded votes to transfer
  
  # loop until all votes from this overweight candidate are transferred or assigned proxy votes
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
    cat("Affirmed evaluations by candidate for loop", xfer.loop, "\n")
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
      # of their voters as proxy votes. See Stage 3 for implementation of proxy votes
      print("* * * ERROR: candidate does not have enough voters to handle proxy votes")
      stop()
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
cat("Step 1: Determine the elected candidates.\n")
cat(
  "Step 2: Transfer votes from unelected candidates to those selected as winning candidates as follows:\n"
)
cat(
  "Starting with the unelected candidate currently holding the largest number of affirmed evaluations, "
)
cat(
  "transfer all their votes that can be transferred automatically by the algorithm to any of the winners. "
)
cat("If necessary, resolve any ties by lot.\n")
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
  cat("is fewer than the number of candidates to be elected.")
  cat(
    "The winners selected ensure that each voter is as fully represented as possible in the legislature "
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
    "then the third, etc. If necessary, repeat this action of one at a time transfers until each of the target number of winner has received at least one vote.\n"
  )
  cat("\n***This contingency is not yet implemented.***\n")
  stop()
}
# select the elected candidates
candidates.sorted <-
  sort(weights.stage3[which(weights.stage3 != 0)],
       decreasing = TRUE)
candidates.sorted.index <-
  order(weights.stage3[which(weights.stage3 != 0)],
        decreasing = TRUE)
cat("\nStage 3, Step 1: Select the",
    positions,
    "candidates to be elected.\n")
print("Candidates' affirmed evaluations in descending order and corresponding candidate")
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
    # must relate this selection back to candidate in index
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
print("Winning candidates and weights")
print (rbind(names[candidates.sorted.index[1:positions]], candidates.sorted[1:positions]))

cat("\nStage 3, Step 2: Transfer votes from unelected candidates to elected candidates\n")
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
    # find the candidate with the highest weight (random if more than one)
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
        " votes (highest affirmed evaluations) from unselected candidate ",
        transfer.name,
        sep = ""
      )
    )
    # process transfers for transfer.candidate until all are transferred
    # note this might end with proxy votes for some voters if not enough voters
    #   marked other candidates or if the candidates they marked have too many votes
    # Create the set of ballots from this candidate that have a next highest candidate
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
    # pick the highest remaining candidate for each voter
    candidates.stage3 <-
      replicate(candidates, 0) # count of candidates
    # sum the count of candidates that have the highest grade across voters
    voters.stage3 <- NULL
    voters.next.highest.graded <- NULL
    print("Next highest graded candidate by voter:")
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
          c(voter.candidate.to.transfer, candidate.stage3) # list of next highest graded candidates
        voters.next.highest.graded <-
          c(voters.next.highest.graded, voters.to.transfer[i]) # list of next highest graded voters
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
    } # end for to transfer candidate weights from unelected candidates
    candidates.to.ignore <- which(weights.stage3 >= vote.limit)
    if (exists("candidates.to.ignore"))
      candidates.stage3[candidates.to.ignore] <- 0
    cat("Candidates eligible to receive Stage 3 transfers:\n")
    print(rbind(council, candidates.stage3))
    proxy.required <-
      FALSE # must transfer proxy votes if not enough graded votes to transfer
    # loop until all votes from this unelected candidate are transferred or assigned proxy votes
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
        "* * * No more automatic transfer of votes available. Therefore,",
        transfer.count,
        " proxy vote(s) required for transfer from candidate ",
        transfer.name,
        "in Stage 4\n"
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
        " proxy vote(s) from candidate ",
        transfer.name,
        " from these voters' ballots:",
        sort(voters.transferred),
        "\n"
      )
      if (xfer.proxy.count < transfer.count) {
        # in Stage 3, this should never happen as each candidate can assign any number
        # of their voters as proxy votes.
        print("* * * ERROR: candidate does not have enough voters to handle proxy votes")
        stop()
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
cat("Finalize the weighted votes of each winner as follows:\n")
cat("Starting with the unelected candidate with the largest number of proxy votes to transfer, ")
cat(
  "all the proxy votes held by each unelected candidate must be publicly transferred to any of the winners. "
)
cat("Resolve any ties by lot.\n")
cat("Finally, starting with the winner with most proxy votes to transfer, ")
cat("publicly transfer these votes to any of the other winners. Continue with the remaining ")
cat(
  "elected candidate with the most proxy votes to transfer until all proxy votes are transferred.\n"
)
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
