#' Describe diversity based on a list of species 
#' 
#' Compute a species diversity index
#' @param species list of species (names, or code) 
#' @return list with the following items
#' \describe{
#' \item{num}{ Number of distinct species}
#' \item{simpson}{Value of simpson diversity index}
#' \item{dominant}{Name of the most frequently occuring species}
#' }
#' @examples
#' computediversity(c("butterfly","butterfly","mosquito","butterfly","ladybug","ladybug")))
#' @references
#' http://www.tiem.utk.edu/~gross/bioed/bealsmodules/simpsonDI.html

summarize_diversity = function(species, index="A", showplot=FALSE) {

species = as.factor(species)

# decide which index calculation to use
if (index=="B") {
  tmp = (summary(species)/sum(summary(species))) ** 2
  diversity = sum(tmp)
  }
else {
  n  = as.numeric(summary(species))   # summary() extracts counts from factor
  N  = sum(summary(species))   # total number of individuals
  diversity <- sum(n * (n - 1)) / (N * (N - 1))
}

sm = as.data.frame(summary(species))
colnames(sm)="frequency"
sm$fish = rownames(sm)

if (showplot) 
  p=ggplot(sm,aes(x=fish,y=frequency, fill=fish))+geom_col()
else
  p=NULL
 

# number of species
nspecies = length(summary(species))

# which is the most frequent
tmp = which.max(summary(species))
dominant = names(summary(species)[tmp])

# output from function
# use 1-diversity for the second index to make it more intuitive (higher is more diverse)
# this is sometimes called simpson index
return(list(num=nspecies, simpson=(1-diversity), dominant=dominant, plt=p))
}


