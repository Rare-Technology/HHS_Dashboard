compute_summary_line <- 
  function (x, y) {
    paste(round(mean(as.numeric(x), na.rm = TRUE), y), '±', round(sd(as.numeric(x), na.rm =
                                                                       TRUE) / sqrt(length(x)), y))
  }

mean_sem <-
  function (x, y) {
    paste(round(mean(as.numeric(x), na.rm = TRUE), y), '±', round(sd(as.numeric(x), na.rm =
                                                                       TRUE) / sqrt(length(x)), y))
  }



## TODO: this comes from original app and needs re-writing
proportion <- function(question, grouping, rounding, type)
{

  Q_length <- as.vector(tapply(question, grouping, length))
  Q_length[is.na(Q_length)] <- 0
  #calculate responses values
  Q_count <- tapply(question, list(grouping, question), length)
  Q_count[is.na(Q_count)] <- 0
  #calculate proportions and bind table
  summary_bind <-
    data.frame(N = Q_length, round(Q_count / Q_length, rounding) * 100)

  if(type == "0"){
    result <- NA
  }

  if(type == "1"){

    result <-
      rbind(summary_bind, "Mean ± SE" = c(sum(summary_bind[[1]], 1),
                                          mean_sem(summary_bind[[2]], 1)
      )
      )
  }

  if(type == "2"){

    result <-
      rbind(summary_bind, "Mean ± SE" = c(
        sum(summary_bind[[1]], 1),
        mean_sem(summary_bind[[2]], 1),
        mean_sem(summary_bind[[3]], 1)
      ))
  }

  if(type == "3"){

    result <-
      rbind(summary_bind,
            "Mean ± SE" = c(
              sum(summary_bind[[1]], 1),
              mean_sem(summary_bind[[2]], 1),
              mean_sem(summary_bind[[3]], 1),
              mean_sem(summary_bind[[4]], 1)
            ))
  }

  if(type == "4"){

    result <-
      rbind(summary_bind,
            "Mean ± SE" = c(
              sum(summary_bind[[1]], 1),
              mean_sem(summary_bind[[2]], 1),
              mean_sem(summary_bind[[3]], 1),
              mean_sem(summary_bind[[4]], 1),
              mean_sem(summary_bind[[5]], 1)
            ))
  }

  if(type == "5"){

    result <-
      rbind(
        summary_bind,
        "Mean ± SE" = c(
          sum(summary_bind[[1]]),
          mean_sem(summary_bind[[2]], 1),
          mean_sem(summary_bind[[3]], 1),
          mean_sem(summary_bind[[4]], 1),
          mean_sem(summary_bind[[5]], 1),
          mean_sem(summary_bind[[6]], 1)
        )
      )
  }

  if(type == "6"){

    result <-
      rbind(
        summary_bind,
        "Mean ± SE" = c(
          sum(summary_bind[[1]]),
          mean_sem(summary_bind[[2]], 1),
          mean_sem(summary_bind[[3]], 1),
          mean_sem(summary_bind[[4]], 1),
          mean_sem(summary_bind[[5]], 1),
          mean_sem(summary_bind[[6]], 1),
          mean_sem(summary_bind[[7]], 1)
        )
      )
  }

  if(type == "7"){

    result <-
      rbind(
        summary_bind,
        "Mean ± SE" = c(
          sum(summary_bind[[1]]),
          mean_sem(summary_bind[[2]], 1),
          mean_sem(summary_bind[[3]], 1),
          mean_sem(summary_bind[[4]], 1),
          mean_sem(summary_bind[[5]], 1),
          mean_sem(summary_bind[[6]], 1),
          mean_sem(summary_bind[[7]], 1),
          mean_sem(summary_bind[[8]], 1)
        )
      )
  }

  if(type == "8"){

    result <-
      rbind(
        summary_bind,
        "Mean ± SE" = c(
          sum(summary_bind[[1]]),
          mean_sem(summary_bind[[2]], 1),
          mean_sem(summary_bind[[3]], 1),
          mean_sem(summary_bind[[4]], 1),
          mean_sem(summary_bind[[5]], 1),
          mean_sem(summary_bind[[6]], 1),
          mean_sem(summary_bind[[7]], 1),
          mean_sem(summary_bind[[8]], 1),
          mean_sem(summary_bind[[9]], 1)
        )
      )
  }

  tibble::rownames_to_column(result, "MA name")

}

## TODO: this comes from original app and needs re-writing
proportion_Q14 <- function(submissionid, question, grouping, rounding, type)
{
  Q_length_N <- as.vector(tapply(submissionid, grouping, length.unique))
  Q_length <- as.vector(tapply(question, grouping, length))
  Q_length[is.na(Q_length)] <- 0
  Q_count <- tapply(question, list(grouping, question), length)
  Q_count[is.na(Q_count)] <- 0
  summary_bind <- data.frame(N = Q_length_N, 
                             round(Q_count / Q_length, rounding) * 100)
  if(type == "5"){
    result <-
      rbind(
        summary_bind,
        "Mean ± SE" = c(
          sum(summary_bind[[1]]),
          mean_sem(summary_bind[[2]], 1),
          mean_sem(summary_bind[[3]], 1),
          mean_sem(summary_bind[[4]], 1),
          mean_sem(summary_bind[[5]], 1),
          mean_sem(summary_bind[[6]], 1)
        )
      )
  }
  
  if(type == "6"){
    result <-
      rbind(
        summary_bind,
        "Mean ± SE" = c(
          sum(summary_bind[[1]]),
          mean_sem(summary_bind[[2]], 1),
          mean_sem(summary_bind[[3]], 1),
          mean_sem(summary_bind[[4]], 1),
          mean_sem(summary_bind[[5]], 1),
          mean_sem(summary_bind[[6]], 1),
          mean_sem(summary_bind[[7]], 1)
        )
      )
  }
  
  if(type == "7"){
    result <-
      rbind(
        summary_bind,
        "Mean ± SE" = c(
          sum(summary_bind[[1]]),
          mean_sem(summary_bind[[2]], 1),
          mean_sem(summary_bind[[3]], 1),
          mean_sem(summary_bind[[4]], 1),
          mean_sem(summary_bind[[5]], 1),
          mean_sem(summary_bind[[6]], 1),
          mean_sem(summary_bind[[7]], 1),
          mean_sem(summary_bind[[8]], 1)
        )
      )
  }
  
  if(type == "8"){
    result <-
      rbind(
        summary_bind,
        "Mean ± SE" = c(
          sum(summary_bind[[1]]),
          mean_sem(summary_bind[[2]], 1),
          mean_sem(summary_bind[[3]], 1),
          mean_sem(summary_bind[[4]], 1),
          mean_sem(summary_bind[[5]], 1),
          mean_sem(summary_bind[[6]], 1),
          mean_sem(summary_bind[[7]], 1),
          mean_sem(summary_bind[[8]], 1),
          mean_sem(summary_bind[[9]], 1)
        )
      )
  }
  tibble::rownames_to_column( result, "MA name")
}

length.unique <- function(x) { length(unique(x)) }