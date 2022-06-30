#----graph_distribution----#
rm(list = ls())
load('cleaning_ppa.RData')
library(ggplot2)

d <- na.omit(l)
d$ae_positive
####--ae_positive--####
p <- ggplot(data = d[which(d$condition %in% c("Intervention", "Waitlist")),], mapping = aes(x = condition, y = ae_positive, fill = condition))
p <- p + theme(legend.position = "top")
p <- p + geom_violin()
p <- p + geom_boxplot(width=0.1, fill="white")
p <- p + geom_jitter(mapping = aes(fill = condition), shape = 16, position = position_jitter(0.2), size = 0.5)
p
ggsave(file='distribution/ae_positive.png', w=5, h=5)
####--ae_negative--####
p <- ggplot(data = d[which(d$condition %in% c("Intervention", "Waitlist")),], mapping = aes(x = condition, y = ae_negative, fill = condition))
p <- p + theme(legend.position = "top")
p <- p + geom_violin()
p <- p + geom_boxplot(width=0.1, fill="white")
p <- p + geom_jitter(mapping = aes(fill = condition), shape = 16, position = position_jitter(0.2), size = 0.5)
p
ggsave(file='distribution/ae_negative.png', w=5, h=5)
####--ddf-####
p <- ggplot(data = d[which(d$condition %in% c("Intervention", "Waitlist")),], mapping = aes(x = time, y = ddf, fill = condition))
p <- p + theme(legend.position = "top")
p <- p + geom_violin()
p <- p + geom_jitter(mapping = aes(fill = condition), shape = 16, position = position_jitter(0.2), size = 0.5)
p
ggsave(file='distribution/drinking_frequency.png', w=5, h=5)
####--ddf-####
p <- ggplot(data = d[which(d$condition %in% c("Intervention", "Waitlist")),], mapping = aes(x = time, y = ddf, fill = condition))
p <- p + theme(legend.position = "top")
p <- p + geom_violin()
p <- p + geom_jitter(mapping = aes(fill = condition), shape = 16, position = position_jitter(0.2), size = 0.5)
p
ggsave(file='distribution/drinking_frequency.png', w=5, h=5)
####--most_drink-####
p <- ggplot(data = d[which(d$condition %in% c("Intervention", "Waitlist")),], mapping = aes(x = time, y = most_drink, fill = condition))
p <- p + theme(legend.position = "top")
p <- p + geom_violin()
p <- p + geom_jitter(mapping = aes(fill = condition), shape = 16, position = position_jitter(0.2), size = 0.5)
p
ggsave(file='distribution/most_drink.png', w=5, h=5)
####--consq-####
p <- ggplot(data = d[which(d$condition %in% c("Intervention", "Waitlist")),], mapping = aes(x = time, y = consq, fill = condition))
p <- p + theme(legend.position = "top")
p <- p + geom_violin()
p <- p + geom_jitter(mapping = aes(fill = condition), shape = 16, position = position_jitter(0.2), size = 0.5)
p
ggsave(file='distribution/consq.png', w=5, h=5)
