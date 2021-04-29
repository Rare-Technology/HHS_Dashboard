collapse_wrapper <- function(internal_html, open, collapseid, title) {
  
  
  div(class = "panel-group",
      div(class = "panel-heading",
          h3(
            class = "panel-title",
            tags$a(
              `data-toggle` = "collapse",
              `aria-expanded` = ifelse(open, "true", "false"),
              href = paste0("#", collapseid),
              paste(title),
              icon("chevron-right")
            )
          )),
      div(
        id = collapseid,
        class = glue::glue("panel-collapse collapse {ifelse(open, 'in', '')}"),
        internal_html
      ))
}

inline_select <- function(id, label, choices, selected){
  div(class = 'select-holder',
      div(class ="select-label", label),
      selectInput(id, "", choices = choices, selected = selected
      )
  )
}