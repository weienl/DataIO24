m1 <- read.csv("../Data/January.csv")
m2 <- read.csv("../Data/February.csv")
m3 <- read.csv("../Data/March.csv")
m4 <- read.csv("../Data/April.csv")
m5 <- read.csv("../Data/May.csv")
m6 <- read.csv("../Data/June.csv")
m7 <- read.csv("../Data/July.csv")
m8 <- read.csv("../Data/August.csv")
m9 <- read.csv("../Data/September.csv")
m10 <- read.csv("../Data/October.csv")
m11 <- read.csv("../Data/November.csv")
m12 <- read.csv("../Data/December.csv")

dat.list <- list(m1, m2, m3, m4,
                 m5, m6, m7, m8,
                 m9, m10, m11, m12)



df <- rbind(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)

nrows <- sapply(dat.list, function(x) nrow(x))

month <- unlist(sapply(c(1:12), function(i) rep(i, nrows[i])))

df$month <- month

rm(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)
rm(dat.list)
rm(month)
rm(nrows)



save.image("../Data/ebike.RData")
