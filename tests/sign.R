
 library(fuzzyRankTests)

 x <- c(-1.2, -0.7, -0.3, 0.1, 0.2, 0.3, 0.4, 0.9, 0.9, 1.0, 1.0,
     1.1, 1.5, 1.7, 1.9, 3.5, 5.1)

 fuzzy.sign.test(x, alternative = "greater")

 x <- c(-1.2, -0.7, 0.0, 0.0, 0.0, 0.3, 0.4, 0.9, 0.9, 1.0, 1.0,
     1.1, 1.5, 1.7, 1.9, 3.5, 5.1)

 fuzzy.sign.test(x, alternative = "greater")

 fuzzy.sign.test(- x, alternative = "less")

 fuzzy.sign.test(x, alternative = "two.sided")

 x2 <- c(-1.2, -0.7, 0.0, 0.0, 0.0, 0.3, 0.4, 0.9, 0.9, 1.0)

 fuzzy.sign.test(x2, alternative = "two.sided")

 fuzzy.sign.test(- x2, alternative = "two.sided")

 x <- c(-3.5, -2.3, -1.2, -0.7, 0.0, 0.0, 0.0, 0.0, 0.4, 0.9, 0.9, 1.0, 1.0,
     1.1, 1.9, 3.5, 5.1)

 fuzzy.sign.test(x, alternative = "two.sided")

 x <- c(-4.1, -3.5, -2.3, -1.2, -0.7, 0.0, 0.0, 0.0, 0.0, 0.4, 0.9, 0.9, 1.0,
     1.1, 1.9, 3.5)

 fuzzy.sign.test(x, alternative = "two.sided")

 x <- x[- length(x)]

 fuzzy.sign.test(x, alternative = "two.sided")

 x <- seq(-2, 2)

 fuzzy.sign.test(x, alternative = "two.sided")

 x <- x[x != 0]

 fuzzy.sign.test(x, alternative = "two.sided")

 ##### now check with alpha #####

 fuzzy.sign.test(x, alternative = "two.sided", alpha = 0.75)

 x <- c(-1.2, -0.7, 0.0, 0.0, 0.0, 0.3, 0.4, 0.9, 0.9, 1.0, 1.0,
     1.1, 1.5, 1.7, 1.9, 3.5, 5.1)

 fuzzy.sign.test(x, alternative = "greater")

 fuzzy.sign.test(x, alternative = "greater", alpha = 0.10)

 fuzzy.sign.test(x, alternative = "greater", alpha = 0.05)

 fuzzy.sign.test(x, alternative = "greater", alpha = 0.01)

 fuzzy.sign.test(x, alternative = "greater", alpha = 0.001)

 fuzzy.sign.test(x, alternative = "greater", alpha = 0.0001)

