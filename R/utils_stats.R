mean_sem <-
  function (x, y) {
    paste(round(mean(as.numeric(x), na.rm = TRUE), y), '±', round(sd(as.numeric(x), na.rm =
                                                                       TRUE) / sqrt(length(x)), y))
  }