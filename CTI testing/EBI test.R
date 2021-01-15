library(ggplot2)
#testing CTI with EBI data


#values for 2003 are means, not real data

data <- read.csv("testing CTI.csv", check.names = F, header = F)
data <- data.frame(t(data[-1]))
colnames(df2) <- data[, 1]
colnames(data) <- c("year", "CTI", "temp")
rownames(data) <- c()

test1 <- ccf(data$CTI, data$temp, ylab = "cross-correlation", plot = F)
#CTI[t+k] and temp[t]



fake <- read.csv("fakeTS.csv", check.names = F)
testf <- ccf(fake$effect, fake$cause, ylab = "cross-correlation")
plot(testf[testf$lag[testf$lag>=0],])

ggplot(data = fake) +
  geom_line(aes(y = cause, x = year)) +
  geom_line(aes(y = effect, x = year))