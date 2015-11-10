library(kernlab)
data(spam)
head(spam)

# a very simple algorithm of distinguishing spam
plot(density(spam$your[spam$type == "nonspam"]), col="blue", main="", xlab="Frequency of 'your'")
lines(density(spam$your[spam$type == "spam"]), col="red")
abline(v=0.5,col="black")

prediction <- ifelse(spam$your>0.5,"spam","nonspam")
table(prediction, spam$type)/length(spam$type)


# in & out sample errors
# in sample versus out of sample errors
data(spam)
set.seed(333)
smallSpam = spam[sample(dim(spam)[1], size=10),]
View(smallSpam)
spamLabel = (smallSpam$type == "spam")*1+1
plot(smallSpam$capitalAve, col = spamLabel)

# prediction rule 1
rule1 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 2.7] <- "spam"
  prediction[x < 2.4] <- "nonspam"
  prediction[x >= 2.4 & x <= 2.45] <- "spam"
  prediction[x > 2.45 & x <= 2.7] <- "nonspam"
  return(prediction)
}
table(rule1(smallSpam$capitalAve), smallSpam$type)


# prediction rule 2
rule2 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 2.8] <- "spam"
  prediction[x <= 2.8] <- "nonspam"
  return(prediction)
}
table(rule2(smallSpam$capitalAve), smallSpam$type)

# apply rule1, rule2 to complete spam data
table(rule1(spam$capitalAve), spam$type)
sum(rule1(spam$capitalAve) == spam$type)

table(rule2(spam$capitalAve), spam$type)
sum(rule2(spam$capitalAve) == spam$type)

