
#Function to calculate mean ± SE with round 0,1,2,3
mean_sem <-
  function (x, y) {
    paste(round(mean(as.numeric(x), na.rm = TRUE), y), '±', round(sd(as.numeric(x), na.rm =
                                                                       TRUE) / sqrt(length(x)), y))
  }

#length unique
length.unique <- function(x) { length(unique(x)) }

## Proportion
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
  rownames_to_column( result, "MA name")
}

# use point symbols from base R graphics as icons
pchIcons <- function(pch = 0:14,
                     width = 30,
                     height = 30,
                     ...) {
  n <- length(pch)
  files <- character(n)
  # create a sequence of png images
  for (i in seq_len(n)) {
    f <- tempfile(fileext = ".png")
    png(f,
        width = width,
        height = height,
        bg = "transparent")
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5,
           .5,
           pch = pch[i],
           cex = min(width, height) / 8,
           ...)
    dev.off()
    files[i] <- f
  }
  files
}


## Function for summary tables #####
summarySE <-
  function(data = NULL,
           measurevar,
           groupvars = NULL,
           na.rm = FALSE,
           conf.interval = .95,
           .drop = TRUE) {
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm = FALSE) {
      if (na.rm)
        sum(!is.na(x))
      else
        length(x)
    }
    # This does the summary. For each group's data frame, return a vector with N, mean, and sd
    datac <- plyr::ddply(
      data,
      groupvars,
      .drop = .drop,
      .fun = function(xx, col) {
        c(
          N    = length2(xx[[col]], na.rm = na.rm),
          mean = mean   (xx[[col]], na.rm = na.rm),
          SD   = sd     (xx[[col]], na.rm = na.rm)
        )
      },
      measurevar
    ) %>% plyr::rename(datac, c("mean" = measurevar)) # Rename the "mean" column
    datac$SE <-
      datac$SD / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval / 2 + .5, datac$N - 1)
    datac$CI <- datac$SE * ciMult
    
    return(datac)
  }


## Remove mean and convert to numeric for plotting 
    data_4plot <- function (.data_summary) {
      .data_summary %>% 
        dplyr::filter (`MA name` !=  "Mean ± SE") %>%
         dplyr::mutate(`Proportion (%)` = as.numeric(`Proportion (%)`)) 
    }
    
    