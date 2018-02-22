## association rule mining

library(arules)

g = read.csv('groceries.csv', header=F)

## draw sample of 20
g = g[,1:8]
idx = sample(1:nrow(g),20)
g = g[idx,]

## summary of titanic data
summary(g)

## Apply apriori algorithm to extract rules
## min support = .1
## min conf = .8
rules.all = apriori(g)

## Check to see the rules
inspect(rules.all)

## rules with rhs containing "Survived" only
rules = apriori(g, control=list(verbose=F), parameter=list(minlen=2, supp=.005, conf=.8))

## Keep 3 decimal places
quality(rules) = round(quality(rules), digits=3)

## Order rules by lift
rules.sorted = sort(rules, by='lift')

## View the rules.sorted
inspect(rules.sorted)

## inspect first 2 rules
inspect(rules.sorted[1:2])

## Remove redundant rules
subset.matrix = is.subset(rules.sorted, rules.sorted, sparse = F)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant = colSums(subset.matrix, na.rm=T)>=1

## which rules are redundant
which(redundant)

## remove redundant rules
rules.pruned = rules.sorted[!redundant]

## inspecting pruned rules
inspect(rules.pruned)
inspect(rules.pruned[1])


## plot the rules
library(arulesViz)
plot(rules.pruned)
plot(rules.pruned, method = 'grouped')
plot(rules.pruned, method ='graph')
plot(rules.pruned, method = 'graph', control = list(type='items'))
plot(rules.pruned, method = 'paracoord')
