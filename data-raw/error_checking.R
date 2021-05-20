
#************************************************
# Code used to compare column types ----
#************************************************

# I used this code to confirm types for the spec. I created
# the initial spec based on BRA and created ALL_hhs based on
# code below.

cols_spec <- spec_hhs$cols %>% purrr::map(function(x) unname(class(x)[1]))
cols_spec <- data.frame(t(as_tibble(cols_spec)))
cols_spec$col_names <- row.names(cols_spec)
names(cols_spec) <- c("col_types_spec", "col_names")
cols_spec$col_types_spec <- gsub("collector_", '', cols_spec$col_types)
row.names(cols_spec) <- NULL
cols_spec <- cols_spec[,c("col_names", "col_types_spec")]


dt_spec <- map_chr(ALL_hhs, function(x) class(x)[1])
dt_spec <- data.frame(dt_spec)
dt_spec$col_names <- row.names(dt_spec)
names(dt_spec) <- c("col_types_dt", "col_names")
row.names(dt_spec) <- NULL
dt_spec <- dt_spec[,c("col_names", "col_types_dt")]

spec_compare <- full_join(dt_spec, cols_spec, by = "col_names")

filter(spec_compare, is.na(col_types_spec)) # manually added

#   1             level3_name       factor           <NA>
#   2               ma_status       factor           <NA>
#   3          reserve_status       factor           <NA>
#   4        35a_ma_gear_nets       factor           <NA>
#   5    35b_ma_gear_dynamite       factor           <NA>
#   6     35d_ma_gear_harpoon       factor           <NA>
#   7       35e_ma_gear_trawl       factor           <NA>
#   8    35f_ma_gear_mosquito       factor           <NA>
#   9      35g_ma_gear_poison       factor           <NA>
#   10    35h_ma_gear_gillnet       factor           <NA>
#   11     35i_ma_gear_gamboa       factor           <NA>
#   12     35j_ma_gear_quinia       factor           <NA>
#   13      35k_ma_gear_other       factor           <NA>
#   14            level4_name       factor           <NA>
#   15             username_2       factor           <NA>
#   16      35e_ma_gear_traps       factor           <NA>
#   17   35f_ma_gear_longline       factor           <NA>
#   18      35g_ma_gear_other       factor           <NA>
#   19 35k_ma_gear_other_desc       factor           <NA>

filter(
  spec_compare,
  col_types_dt != col_types_spec,
  !(col_types_dt == "factor" & col_types_spec == "character"),
  !(col_types_dt == "integer" & col_types_spec == "double"),
  !(col_types_dt == "numeric" & col_types_spec == "double"),
  !(col_types_dt == "POSIXct" & col_types_spec == "datetime")
)
