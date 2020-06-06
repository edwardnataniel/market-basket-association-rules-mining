install.packages("arules")
install.packages("arulesViz")
install.packages("vcd")

library(arules)
library(arulesViz)

basket <- read.transactions("basket.dat", format = "basket", sep = " ", rm.duplicates = TRUE)

# apriori
rules <- apriori(basket, parameter = list(supp = 0.1, conf = 0.8))
inspect(rules)

plot(rules, control = list(jitter = 2))

# sorting rules
rules_high_lift <- head(sort(rules, by = "lift"), 20)
inspect(rules_high_lift)

# imbalanced ratio = (supA - supB) / (supA + supB - supAUB)
supA = support(lhs(rules), basket, type = c("absolute"), control = NULL)
supB = support(rhs(rules), basket, type = c("absolute"), control = NULL)
supAUB = support(items(rules), basket, type = c("absolute"), control = NULL)
IR = abs(supA - supB) / (supA + supB - supAUB)

# kulczinski measure = 1/2 * (P(B|A) + P(A|B)) = 1/2 * ((sup AUB / sup A) + (sup AUB / sup B))
kulc = 0.5 * ((supAUB / supA) + (supAUB / supB))

# adding the IR, kulc
quality(rules) <- cbind(quality(rules), IR)
quality(rules) <- cbind(quality(rules), kulc)

rules_high_lift <- head(sort(rules, by = "kulc", decreasing = FALSE), 100)
inspect(rules_high_lift)
