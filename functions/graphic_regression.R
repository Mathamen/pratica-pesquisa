#plot(data_novo$number, data_novo$Authentic, type = "p", col = "blue", pch = 16,
 #    main = "Time Series by Entries", xlab = "Entry Number", ylab = "Analytic Value")

#trend_line <- lm(data_novo$Authentic ~ data_novo$number)  # Regressão linear
#abline(trend_line, col = "darkgreen", lwd = 2)  # linha de tendência

#years_to_mark <- c(1985, 1989, 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022)

#for (year in years_to_mark) {
 # year_indices <- which(data_novo$date == year)
  #if (length(year_indices) > 0) {
   # first_number <- data_novo$number[year_indices[1]]
    #abline(v = first_number, col = "red", lty = 2)
#  }
#}




