
getWinner = function(matchup) {
  while(matchup[1] * matchup[2] != 0){
    matchup  = matchup - oneRound(min(3,matchup[1]), min(2, matchup[2])) # eliminate casualities until someone is dead
  } 
  return(matchup[1] - matchup[2]) 
}



oneRound = function(off, def){
  outcome = c(0,0)
  offdice = sort(sample(1:6,off,T), T)
  defdice = sort(sample(1:6,def,T), T)
  for (k in 1:min(off, def)){
    if(offdice[k] > defdice[k]){
      outcome[2] = outcome[2] + 1 # defense casualties
    } else {
      outcome[1] = outcome[1] + 1 # offense casualties
    }
  }
  return(outcome)
}


getWinnerDist = function(off, def, rounds){
  x = rep(NA_integer_, rounds)
  for(i in 1:rounds){
    x[i] = getWinner(c(off,def))
  }
  return(x)
}




battles = array(NA_integer_, c(20, 20))
for(offense in 1:20){
  for(defense in 1:20){
    battles[offense, defense] = mean(getWinnerDist(offense, defense, 3000))
  }
}


write.csv(x = battles, file = file.choose())

