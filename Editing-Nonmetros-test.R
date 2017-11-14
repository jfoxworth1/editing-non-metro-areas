given.states <- counties$state
given.states <- unique(given.states)
given.states <- str_replace(given.states, " .Nonmetropolitan Portion.", "")
given.states <- given.states[-c(1:3)]
#states.names %in% given.states
given <- list()

for(i in 1:length(given.states)){
  given[i] <- list(counties$county[str_detect(counties$state, given.states[i])])
}
names(given) <- given.states

tmp.states <- states[givenstates]

for(i in 1:length(given.states)){
  #print(paste0("number in given ", given.states[i], ":"))
  #print(length(tmp[[i]]))
  #print(paste0("number in list ", names(states)[i], ":"))
  #print(length(states[[i]][[2]][1,]))
  print(paste("Difference in ", given.states[i], " is :"))
  print(length(tmp.states[[i]][[2]][1,]) - length(given[[i]]))
}
