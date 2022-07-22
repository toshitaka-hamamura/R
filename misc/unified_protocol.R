#----OASIS/ODSIS self-evaluation----#
rm(list = ls())
d <- data.frame()
d <- rbind(c("2020-09-08", 9, 5),
           c("2020-09-15", 1, 3),
           c("2020-09-23", 1, 4),
           c("2020-09-29", 3, 4),
           c("2020-10-06", 6, 4),
           c("2020-10-13", 0, 3),
           c("2020-10-20", 5, 6),
           c("2020-10-27", 11, 12),
           d
           )
colnames(d) <- c("date", "OASIS", "ODSIS")
d$date <- as.Date(d$date)
d[,c("OASIS", "ODSIS")] <- sapply(d[,c("OASIS", "ODSIS")], as.numeric)

plot(d$date, d$OASIS,
     xlab = "Date", ylab = "Score")
lines(d$date, d$OASIS, col="blue")
lines(d$date, d$ODSIS, col="red", type = "b")
legend(d$date[nrow(d)-2], 9 , legend = c("OASIS", "ODSIS"), col = c("blue", "red"), lty = 2:3)

