##hh_surveys #####
#Load Brazil from data world
BRA_hhs <- data.table::fread("https://query.data.world/s/rkr2loid2uqt63l2zjuqofpc2wkxes", 
                             stringsAsFactors=TRUE, encoding = "UTF-8") %>% subset(ma_name !='')

#Load IDN data from data world
IDN_hhs <- data.table::fread("https://query.data.world/s/x7rxtrqu7apiwyx7vu7gigx34w276e",
                             stringsAsFactors = TRUE, encoding = "UTF-8") %>% subset(ma_name !='')
#Load HND data from data world
HND_hhs <- data.table::fread("https://query.data.world/s/r5wsp73r66p7yqvohq4dhjnfi7wohj",
                             stringsAsFactors = TRUE, encoding = "UTF-8") %>% subset(ma_name !='')
#Load PHL data from data world
PHL_hhs <- data.table::fread("https://query.data.world/s/bmzps4cygrenel36t2kbj6rbndrejr",
                             stringsAsFactors = TRUE, encoding = "UTF-8") %>% subset(ma_name !='')
#Load MOZ data from data world
MOZ_hhs <- data.table::fread("https://query.data.world/s/3wu6qfkst77upiqryvvyymkkwa5bst",
                             stringsAsFactors = TRUE, encoding = "UTF-8") %>% subset(ma_name !='') 
#Load FSM data from data world
FSM_hhs <- data.table::fread("https://query.data.world/s/4jdlxkngaawjug5vxvcyxpqrccjbyf",
                             stringsAsFactors = TRUE, encoding = "UTF-8") %>% subset(ma_name !='')

PLW_hhs <- data.table::fread("https://query.data.world/s/r577gevshvjb74ilkdia5ugl43desr",
                            stringsAsFactors = TRUE, encoding = "UTF-8") %>% subset(ma_name !='')
PLW_hhs$level2_name <- PLW_hhs$level1_name
PLW_hhs$ma_name <- PLW_hhs$level1_name

#Combine all countries
ALL_hhs <- data.table::rbindlist(list(IDN_hhs, 
                                      PHL_hhs, 
                                      HND_hhs, 
                                      BRA_hhs, 
                                      MOZ_hhs, 
                                      FSM_hhs, 
                                      PLW_hhs), 
                                 use.names=TRUE, 
                                 fill = TRUE, 
                                 idcol = NULL)

#rename and remove duplicates that where submitted on different times
hhs <- ALL_hhs %>%
        select(-c(updatedat, endformtimestamp)) %>%
           unique() %>% 
              left_join (ALL_hhs[,c("submissionid", "updatedat", "endformtimestamp")] %>%
                                   group_by (submissionid) %>%
                                      summarise (updatedat = max(as.Date(updatedat))),
                         by = "submissionid")

#write_csv(hhs, "hhs.csv")

#hhs_Q07 people for all countries ####
q07 <- data.table::fread("https://query.data.world/s/kw5t42zthtskhvq6fbntufdpexbdql",
                                 stringsAsFactors = TRUE)

#hhs_Q14 responsibilities ####
#q14 <- data.table::fread("https://query.data.world/s/nmqgi7mtbzu65sszvm5abs67vqcnek",
 #                         stringsAsFactors = TRUE)
IDN_hhs_Q14 <- data.table::fread("https://query.data.world/s/bwn7y6xnmeefrsndef4irtxq3iwchq",
                                 stringsAsFactors = TRUE)
PHL_hhs_Q14 <- data.table::fread("https://query.data.world/s/g7bo2jmfzbpoibilz77l5ggwqzwxjg",
                                  stringsAsFactors = TRUE)
HND_hhs_Q14 <- data.table::fread("https://query.data.world/s/edjccr3vwnnr3jgl6suk6hfqqw77rn",
                                  stringsAsFactors = TRUE)
BRA_hhs_Q14 <- data.table::fread("https://query.data.world/s/5o37qji6r6542sbxa2o4by3pitk2hh",
                                  stringsAsFactors = TRUE)
MOZ_hhs_Q14 <- data.table::fread("https://query.data.world/s/e6efeftjpmv5jpx5x56rokw6qgxqct",
                                  stringsAsFactors = TRUE) 
FSM_hhs_Q14 <- data.table::fread("https://query.data.world/s/zosmnctknxod3i5ygyo2hlkmh6ubxm",
                                  stringsAsFactors = TRUE)
PLW_hhs_Q14 <- data.table::fread("https://query.data.world/s/woewvhn5o4s4bh5m2bcb7nb2wjb344",
                                 stringsAsFactors = TRUE)
PLW_hhs_Q14$level2_name <- PLW_hhs_Q14$level1_name
PLW_hhs_Q14$ma_name <- PLW_hhs_Q14$level1_name

q14 <- dplyr::tibble(data.table::rbindlist(list(IDN_hhs_Q14, 
                                                HND_hhs_Q14, 
                                                PHL_hhs_Q14, 
                                                MOZ_hhs_Q14, 
                                                BRA_hhs_Q14,
                                                FSM_hhs_Q14,
                                                PLW_hhs_Q14), 
                                           use.names = TRUE))    

### hhs_Q15 activities for all countries ####
q15 <- data.table::fread("https://query.data.world/s/7evrcpuh3r3rnxqqneihmagbitunhi",
                          stringsAsFactors = TRUE)

### hhs_Q44 #####
#Indonesia
IDN_hhs_Q44 <- data.table::fread("https://query.data.world/s/6zxawinuzovshonppsshree44inpgj",
                                 stringsAsFactors = TRUE)[,-c("level3_name", "level4_name")]
#Honduras
HND_hhs_Q44 <- data.table::fread("https://query.data.world/s/seajzrwf5kbdn6fooamk6wqpzahzwh",
                                 stringsAsFactors = TRUE)[,-c("level3_name", "level4_name")]
#Philippines
PHL_hhs_Q44 <- data.table::fread("https://query.data.world/s/rsbgtlrdk5q67nvgcy3fvwhhxbyaxb",
                                 stringsAsFactors = TRUE)[,-c("level3_name", "level4_name")]
#Mozambique
MOZ_hhs_Q44 <- data.table::fread( "https://query.data.world/s/apqj5fcarad544bt3nax2rni52euym",
                                 stringsAsFactors = TRUE)[,-c("level3_name", "level4_name")] 
#Brazil
BRA_hhs_Q44 <- data.table::fread("https://query.data.world/s/pyjvy2negkytei3thn6x4ca6yrnmg6",
                                 stringsAsFactors = TRUE)[,-c("level3_name", "level4_name")]
#FSM
FSM_hhs_Q44 <- data.table::fread("https://query.data.world/s/llwakyuv4nmgy4xcynquwjfmailsse",
                                 stringsAsFactors = TRUE)#[,-c("level3_name", "level4_name")]
#FSM
PLW_hhs_Q44 <- data.table::fread("https://query.data.world/s/llwakyuv4nmgy4xcynquwjfmailsse",
                                 stringsAsFactors = TRUE)[,-c("level3_name", "level4_name")]


q44 <- dplyr::tibble(data.table::rbindlist(list(IDN_hhs_Q44, 
                                                HND_hhs_Q44, 
                                                PHL_hhs_Q44, 
                                                MOZ_hhs_Q44, 
                                                BRA_hhs_Q44,
                                                FSM_hhs_Q44,
                                                PLW_hhs_Q44), 
                                           use.names = TRUE))

### hhs_qQ5 #####
#Indonesia
IDN_hhs_Q45 <- data.table::fread(#"data-raw/hh_leadership_idn.csv",
                                "https://query.data.world/s/mukyw6tqkokgh53b5asjkrobikwmsh",
                                 stringsAsFactors = TRUE)
#Philippines
PHL_hhs_Q45 <- data.table::fread(#"data-raw/hh_leadership_phl.csv", 
                                "https://query.data.world/s/igcsgxctlcd73yyrtdw7nmar2hmnwo",
                                 stringsAsFactors = TRUE)
#Honduras
HND_hhs_Q45 <- data.table::fread(#"data-raw/hh_leadership_hnd.csv",
                                "https://query.data.world/s/t553cxqfnfvuuv7sfqqfk4lakbmax7",
                                 stringsAsFactors = TRUE)
#Mozambique
MOZ_hhs_Q45 <- data.table::fread(#"data-raw/hh_leadership_moz.csv", 
                                "https://query.data.world/s/ki5xc2vyoh2kfj6yky6llssbzcjnfi",
                                 stringsAsFactors = TRUE) 
#Brazil
BRA_hhs_Q45 <- data.table::fread(#"data-raw/hh_leadership_bra.csv",
                                "https://query.data.world/s/qnmhegzd6xeniopglyj4x32njzfmdw",
                                 stringsAsFactors = TRUE)
#FSM
FSM_hhs_Q45 <- data.table::fread(#"data-raw/hh_leadership_fsm.csv", 
                                "https://query.data.world/s/fiedfeqol3iph5uiosdjhwqz6yqh4o",
                                 stringsAsFactors = TRUE)


q45 <- dplyr::tibble(data.table::rbindlist(list(IDN_hhs_Q45, 
                                                HND_hhs_Q45, 
                                                PHL_hhs_Q45, 
                                                MOZ_hhs_Q45, 
                                                BRA_hhs_Q45,
                                                FSM_hhs_Q45
                                                ), use.names = TRUE))

### hhs_Q48 enforcement ####
q48 <- data.table::fread("https://query.data.world/s/ce5gcgn3mfzck4azwq7jd47rvvkbz4",
                         stringsAsFactors = TRUE)

### hhs_Q48 enforcement ####
q69 <- data.table::fread("https://query.data.world/s/ag4porxxcq7ji5x2m6lmhkhmivog6h",
                         stringsAsFactors = TRUE)


##Load HHS quusestions
hhs_questions <- data.table::fread("data-raw/hhs_questions.csv", stringsAsFactors = TRUE)

#************************************************
# Create initial table ----
#************************************************
source("R/table-summary.R")
initial_table <- hhs %>% 
  dplyr::filter(country == "IDN", level1_name == "South East Sulawesi") %>% 
          droplevels() %>% 
              table_summary(hhs_question = "Household Survey Summary")


#************************************************
# Write out final data ----
#************************************************

readr::write_rds(tibble(hhs), "data/hhs.rds")
readr::write_rds(tibble(hhs_questions), "data/hhs_questions.rds")
readr::write_rds(tibble(q07), "data/hhs_q07.rds") 
readr::write_rds(tibble(q14), "data/hhs_q14.rds")
readr::write_rds(tibble(q15), "data/hhs_q15.rds") 
readr::write_rds(tibble(q44), "data/hhs_q44.rds")
readr::write_rds(tibble(q45), "data/hhs_q45.rds")
readr::write_rds(tibble(q48), "data/hhs_q48.rds") 
readr::write_rds(tibble(q69), "data/hhs_q69.rds")
