tr <- function(state, txt) {
  if (state$language == "English") {
    txt
  } else {
    out <- script %>% 
      dplyr::filter(English == txt) %>% 
      dplyr::pull(state$language)
    if (length(out) == 0) { # `txt` does not exist in the script
      txt
    } else if (is.na(out)) { # `txt` exists in the script but the translation is blank.
      # Could not do "if (length(out) == 0 | is.na(out))" because if it is the case that
      # length(out) == 0, then `out` is character(0) and does not work with is.na in an 'if'
      txt
    } else {
      out
    }
  }
}

tr_v <- Vectorize(tr)