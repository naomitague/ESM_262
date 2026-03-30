
fish_summary = function(fish) {

  fish = as.factor(fish)
  summary_fish = summary(fish)

  max = names(which.max(summary_fish))
  rare = names(which.min(summary_fish))
  nfish = length(fish)

  return(list(mostcommon=max, rarest=rare, nfish=nfish)) }
