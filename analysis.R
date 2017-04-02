library(ggplot2)
library(data.table)
library(plyr)

d <- fread("cardstrings.csv", data.table=FALSE, header=F)
colnames(d) <- c("CardString", "CardCount", "NumSolutions")

dcardcounts <- aggregate(CardString ~ CardCount, d, length)
colnames(dcardcounts) <- c("CardCount", "Freq")

#d$UniqCards <- sapply(d$CardCount, function(c) dcardcounts[dcardcounts$CardCount==c,"Freq"])

#uniqCounts <- as.data.frame(table(d$NumSolutions))

#ggplot(d) + geom_histogram(aes(x=NumSolutions)) + scale_x_log10() + facet_grid(CardCount ~ .)

dpct <- ddply(d,.(CardCount),summarise,
              prop=as.numeric(prop.table(table(NumSolutions))),
              NumSolutions=as.numeric(names(table(NumSolutions))))
ggplot(dpct) + geom_line(aes(x=NumSolutions, y=prop)) +
  facet_grid(CardCount ~ .) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_log10()
