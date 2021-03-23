

#### ~ HHS Dashboard Shiny Server ~ ####

shinyServer(function(input, output, session) {
   ### Create nested selection based on inputs

   # filter the data first and then use the filtered data
   cur_hhs <- reactive({filter(hhs, country == input$Country)})
   ## Region/Province
   output$Subnational_Government <- renderUI ({
      level1vals <- get_levelnames(cur_hhs(), "level1_name")
      selectInput(
         inputId = "Subnational_Government",
         label = strong("Subnational Government"),
         choices = level1vals,
         selected = level1vals,
         multiple = TRUE,
         selectize = FALSE
      ) #, options = list(`actions-box` = TRUE))
   })
   
   ## District/Administration
   output$Local_Government <- renderUI({
      level2vals <- get_levelnames(cur_hhs(), "level2_name", 
                                   subnational = input$Subnational_Government)
      selectInput(
         inputId = 'Local_Government',
         label = strong("Local Government"),
         #selected = "No Fishing Zone",
         choices = level2vals,
         selected = level2vals,
         multiple = TRUE,
         selectize = FALSE
      ) #, options = list(`actions-box` = TRUE))
   })
   
   ## Municipality/Subdistrict
   output$Managed_Access <- renderUI({
      mavals <- get_levelnames(cur_hhs(), "ma_name", 
                               subnational = input$Subnational_Government,
                               localgov = input$Local_Government)
      selectInput(
         inputId = 'Managed_Access',
         label = strong("Managed Access Area"),
         #selected = "No Fishing Zone",
         choices = mavals,
         selected = mavals,
         multiple = TRUE,
         selectize = FALSE
      ) #, options = list(`actions-box` = TRUE))
   })
   
   
   ## Select HHS Section
   output$hhs_section <- renderUI({
      radioButtons(
         inputId = 'hhs_section',
         label = strong("SELECT SURVEY SECTION"),
         choices =  unique(hhs_questions$section),
         selected =  "Basic Information",
         inline = FALSE
      )
   })
   
   # Select HHS Question
   output$hhs_question <- renderUI({
      selectInput(
         inputId = 'hhs_question',
         label = strong("SELECT SURVEY QUESTION"),
         choices =  droplevels(subset(
            hhs_questions, section %in% input$hhs_section
         ))$question,
         multiple = FALSE,
         selectize = TRUE
      )
   })
   
   #Select country to select dataset
   #Selected Data
   selectedData <- reactive ({
      req(
         input$Country,
         input$Subnational_Government,
         input$Local_Government,
         input$Managed_Access
      )
       hhs %>%
         dplyr::filter(
            country == input$Country,
            level1_name %in% input$Subnational_Government,
            level2_name %in% input$Local_Government,
            ma_name %in% input$Managed_Access
         ) %>% 
         droplevels()

   })
   
   #Selected Data
   selectedData_Q07 <- reactive ({
      hhs_q07 %>%
         dplyr::filter(
            country == input$Country,
            level1_name %in% input$Subnational_Government,
            level2_name %in% input$Local_Government,
            ma_name %in% input$Managed_Access
         ) %>% 
         droplevels()
   })
   
   #Selected Data
   selectedData_Q14 <- reactive ({
      hhs_q14 %>%
         dplyr::filter(
            country == input$Country,
            level1_name %in% input$Subnational_Government,
            level2_name %in% input$Local_Government,
            ma_name %in% input$Managed_Access
         ) %>% 
         droplevels()
   })
   
   #Selected Data
   selectedData_Q15 <- reactive ({
      hhs_q15 %>%
         dplyr::filter(
            country == input$Country,
            level1_name %in% input$Subnational_Government,
            level2_name %in% input$Local_Government,
            ma_name %in% input$Managed_Access
         ) %>% 
         droplevels()
   })
   
   #Selected Data
   selectedData_Q44 <- reactive ({
      hhs_q44 %>%
         dplyr::filter(
            country == input$Country,
            level1_name %in% input$Subnational_Government,
            level2_name %in% input$Local_Government,
            ma_name %in% input$Managed_Access
         ) %>% 
         droplevels()
   })
   
   #Selected Data
   selectedData_Q45 <- reactive ({
       hhs_q45 %>%
         dplyr::filter(
            country == input$Country,
            level1_name %in% input$Subnational_Government,
            level2_name %in% input$Local_Government,
            ma_name %in% input$Managed_Access
         )%>% 
         droplevels()
   })
   
   selectedData_Q48 <- reactive ({
      hhs_q48 %>%
         dplyr::filter(
            country == input$Country,
            level1_name %in% input$Subnational_Government,
            level2_name %in% input$Local_Government,
            ma_name %in% input$Managed_Access
         )%>% 
         droplevels()
   })
   
   
   selectedData_Q69 <- reactive ({
      hhs_q69 %>%
         dplyr::filter(
            country == input$Country,
            level1_name %in% input$Subnational_Government,
            level2_name %in% input$Local_Government,
            ma_name %in% input$Managed_Access
         )%>% 
         droplevels()
   })
   ## ------------------------ HHS SUMMARY ------------------------------------------- #####
   
   output$table_summary <- renderTable({
      
      print("in table")
      req(nrow(selectedData()) > 0 && !input$hhs_question == "")
      table_summary(selectedData(), input$hhs_question)
   }, 
      striped = TRUE, hover = TRUE, bordered = FALSE, spacing = 'xs',
      align = c('?', 'l', 'l', 'c', 'c', 'c', 'c', 'c', 'c'), 
      na ='', 
      rownames = FALSE)
 
   
   ## --------------------------HHS QUESTIONS -----------------------------------####
   # suppress warnings for plots
   
       
   output$plot_hhs_question <- renderPlotly({
      req(!input$hhs_question %in% 
             c("", "Household Survey Summary"))
       #plot
      plot_hhs_question()
   })
   
   plot_hhs_question <- reactive ({
      print("in plot")
      
      ### Q8. What is the religion of the head of household?   ######
       if (input$hhs_question == "8. What is the religion of the head of household?") {
         
         hhs_Q8 <- selectedData()[,c("ma_name", "8_religion")] %>%
            filter(`8_religion` != "") %>%
            droplevels()
         
         Q8_summary <- proportion(hhs_Q8[[2]],
                                   hhs_Q8[[1]],
                                   3, type = length(unique(hhs_Q8[[2]])))
         Q8_summary_long <-
            Q8_summary %>% pivot_longer(
               cols = c(3:ncol(Q8_summary)),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q8 <- data_4plot(Q8_summary_long)
         Q8$key <- str_replace_all(Q8$key, "[.]", " ")
         
         plot_Q8 <-
            ggplot(Q8, aes(`MA name`, `Proportion (%)`, N = N)) +
            geom_bar(aes(fill = key),
                     position = position_stack(reverse = TRUE),
                     stat = "identity",
                     alpha = 0.8 ) +
            theme_rare + 
            scale_fill_brewer(palette = "Blues", direction = -1) +
            ggtitle("Religion of the head of the Household"
            ) +
            scale_y_continuous(limits = c(0, 105),
                               breaks = seq(0, 100, 20)) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")+
            theme(legend.position = "right") +
            guides(fill = guide_legend(reverse = TRUE)) 
         
         ggplotly(plot_Q8, height = 750)
         
      }
      
      ### Q9. Do you identify yourself as a member of a specific region or sub-national geographic area? #####
      else if (input$hhs_question == "9. Do you identify yourself as a member of a specific region or sub-national geographic area?") {
         
         hhs_Q9 <- selectedData()[,c("ma_name", "9_region_member")] %>%
            filter(`9_region_member` != "") %>%
            droplevels()
         
         Q9_summary <- proportion(hhs_Q9[[2]],
                                  hhs_Q9[[1]],
                                  3, type = length(unique(hhs_Q9[[2]])))
         colnames(Q9_summary) <- c("MA name", "N", "No", "Yes")
         
         Q9_summary_yes <- Q9_summary[c("MA name", "N", "Yes")]
         colnames(Q9_summary_yes) <- c("MA name", "N", "Proportion (%)")
         Q9 <- data_4plot(Q9_summary_yes)
         
         plot_Q9 <-
            ggplot(Q9, aes(`MA name`, `Proportion (%)`, N = N)) +
            geom_col(fill = "#005BBB", alpha = 0.8) +
            theme_rare + 
            ggtitle("Proportion of households that identify themselves \nas member of a specific region"
            ) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")+
            theme(legend.position = "right") 
            
         ggplotly(plot_Q9, height = 750)
         
      }
      
      
      
    
      ### Q10 Proportion of hh that believe that it is important that the region is managed and protected? #####
      else if (input$hhs_question == "10. Is it important to you that the region is managed and protected?") {

         hhs_Q10 <- selectedData()[ ,c("ma_name", "10_mpa_important")] %>%
            filter(`10_mpa_important` != "") %>%
            rbind(c(NA,1), c(NA,0), c(NA,-1))
         #proportion
         Q10_summary <-
            proportion(hhs_Q10$`10_mpa_important`, 
                       hhs_Q10$ma_name, 
                       3, 
                       type= 3)
         
         colnames(Q10_summary) <- c("MA name", "N", "Neutral", "No", "Yes")
         
         Q10_summary_long <-
            Q10_summary %>% pivot_longer(
               cols = c("Neutral", "No", "Yes"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         
         Q10 <- data_4plot (Q10_summary_long)
         
         #plot 
         plot_Q10 <-
            ggplot(Q10, aes(`MA name`, `Proportion (%)`, N = N)) +
            facet_wrap (~ key) + 
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +  
            ggtitle(
               "Proportion of households that believe is important \nthat the region is managed and protected",
               subtitle = "A") +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
      ggplotly(plot_Q10, height = 700)
         
      }
      
      
      ### Q11 "Household income coming from all activities" ####
      else if (input$hhs_question == "11. Sources of household income this year and estimated contribution to overall household income") {
         
         hhs_Q11 <- selectedData()[, c("ma_name",
                                       "11a_income_farming",
                                       "11b_income_harvesting",
                                       "11c_income_fishing_artisanal",
                                       "11d_income_fishing_industrial",
                                       '11e_income_buying_trading',
                                       '11f_income_processing',
                                       '11g_income_aquaculture',
                                       '11h_income_extraction',
                                       "11i_income_tourism",
                                       "11k_income_other"
                                    )] %>%
                      filter(ma_name != "")
         
         income_source <- hhs_Q11
           
         Q11_length <-
            as.vector(tapply(
               income_source$ma_name,
               income_source$ma_name,
               length
            ))
         
         income_source[is.na(income_source)] <- 0
         income_source <- data.frame(income_source)
         income_summary <-
            aggregate(. ~ ma_name,
                      FUN = mean,
                      na.rm = TRUE,
                      data = income_source)
         
         HH_avg_income <- data.frame (
            ma_name = income_summary$ma_name,
            N = Q11_length,
            Farming = round(
               income_summary$X11a_income_farming / rowSums(income_summary[, c(2:11)], 
                                                            na.rm =  TRUE) * 100,
               1
            ),
            Harvesting = round(
               income_summary$X11b_income_harvesting / rowSums(income_summary[, c(2:11)], 
                                                               na.rm = TRUE) * 100,
               1
            ),
            Artisinal_Fishing = round(
               income_summary$X11c_income_fishing_artisanal / rowSums(income_summary[, c(2:11)], 
                                                                      na.rm = TRUE) * 100,
               1
            ),
            Industrial_Fishing = round(
               income_summary$X11d_income_fishing_industrial / rowSums(income_summary[, c(2:11)], 
                                                                       na.rm = TRUE) * 100,
               1
            ),
            Buying_Trading = round(
               income_summary$X11e_income_buying_trading / rowSums(income_summary[, c(2:11)], 
                                                                   na.rm = TRUE) * 100,
               1
            ),
            Processing_Fish = round(
               income_summary$X11f_income_processing / rowSums(income_summary[, c(2:11)], 
                                                               na.rm = TRUE) * 100,
               1
            ),
            Aquaculture = round(
               income_summary$X11g_income_aquaculture / rowSums(income_summary[, c(2:11)], 
                                                                na.rm = TRUE) * 100,
               1
            ),
            Extraction = round(
               income_summary$X11h_income_extraction / rowSums(income_summary[, c(2:11)], 
                                                               na.rm = TRUE) * 100,
               1
            ),
            Tourism = round(
               income_summary$X11i_income_tourism / rowSums(income_summary[, c(2:11)], 
                                                            na.rm = TRUE) * 100,
               1
            ),
            Other = round(
               income_summary$X11k_income_other / rowSums(income_summary[, c(2:11)], 
                                                          na.rm = TRUE) * 100,
               1
            )
         )
         
         HH_avg_income_mean <- rbind(
            HH_avg_income,
            "Mean ± SE" = c(
               NA,
               sum(HH_avg_income$N),
               mean_sem(HH_avg_income$Farming, 1),
               mean_sem(HH_avg_income$Harvesting, 1),
               mean_sem(HH_avg_income$Artisinal_Fishing, 1),
               mean_sem(HH_avg_income$Industrial_Fishing, 1),
               mean_sem(HH_avg_income$Buying_Trading, 1),
               mean_sem(HH_avg_income$Processing_Fish, 1),
               mean_sem(HH_avg_income$Aquaculture, 1),
               mean_sem(HH_avg_income$Extraction, 1),
               mean_sem(HH_avg_income$Tourism, 1),
               mean_sem(HH_avg_income$Other, 1)
            )
         )
         
         Q11_summary_long <-
            HH_avg_income_mean %>% pivot_longer(
               cols = c(
                  "Farming",
                  "Harvesting",
                  "Artisinal_Fishing",
                  "Industrial_Fishing",
                  "Buying_Trading",
                  "Processing_Fish",
                  "Aquaculture",
                  "Extraction",
                  "Tourism",
                  "Other"
               ),
               names_to = "source",
               values_to = "Proportion (%)"
            )
         Q11_summary_long$source <-
            factor(
               Q11_summary_long$source,
               levels = c(
                  "Farming",
                  "Harvesting",
                  "Artisinal_Fishing",
                  "Industrial_Fishing",
                  "Buying_Trading",
                  "Processing_Fish",
                  "Aquaculture",
                  "Extraction",
                  "Tourism",
                  "Other"
               )
            )
         colnames(Q11_summary_long) <-
            c("MA name", "N", "Source", "Proportion (%)")
         
        Q11 <- Q11_summary_long %>% 
                filter(`MA name` != "") %>%
                    dplyr::mutate(`Proportion (%)` = as.numeric(`Proportion (%)`)) 
           
         #Plot
         plot_Q11 <-
            ggplot(Q11, aes(
               x = `MA name`,
               y = `Proportion (%)`,
               fill = Source,
               N = N
            )) +
            theme_rare + theme(legend.position = "right",
                               legend.text = element_text(size = 10)) +
            geom_bar(
               position = position_stack(reverse = TRUE),
               stat = "identity",
               alpha = 0.8
            ) +
            scale_fill_brewer(palette = "Spectral", direction = -1) +
            guides(fill = guide_legend(reverse = FALSE)) +
            ggtitle("Household source inconme and \nproportional income contribution") +
            xlab (NULL) + ylab ("Proportion (%)") +
            coord_flip(clip = "on")
         
         ggplotly(plot_Q11, height = 750)
      }
      
      
      
      ### Q12 Number of members of the household regularly go fishing #####
      else if (input$hhs_question == "12. How many members of the household regularly go fishing?") {
         
         hhs_Q12 <- selectedData()[,c("ma_name", "12a_fishing_men", 
                                      "12b_fishing_women", "12c_fishing_children")] %>%
            filter(`12a_fishing_men` < 10 & `12a_fishing_men` != "") %>%
            filter(`12b_fishing_women` < 10 & `12b_fishing_women` != "") %>%
            filter(`12c_fishing_children` < 10 & `12c_fishing_children` != "")
         
         Q12_summary <- hhs_Q12 %>%
            group_by(ma_name) %>%
            summarise(
               "N" = n(),
               "fisher men" = round(mean(`12a_fishing_men`, na.rm =
                                            TRUE), 1),
               "fisher women" = round(mean(`12b_fishing_women`, na.rm =
                                              TRUE), 1),
               "fisher children" = round(mean(`12c_fishing_children`, na.rm =
                                                 TRUE), 1)
            )
         
         Q12_summary <- rbind(Q12_summary,
                              c(
                                 NA,
                                 sum(Q12_summary$N),
                                 mean_sem(Q12_summary$`fisher men`, 1),
                                 mean_sem(Q12_summary$`fisher women`, 1),
                                 mean_sem(Q12_summary$`fisher children`, 1)
                              ))
        
         #plot
         Q12_summary_long <-
            Q12_summary %>% pivot_longer(
               cols = c("fisher men", "fisher women", "fisher children"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q12_summary_long$key <-
            factor(
               Q12_summary_long$key,
               levels = c("fisher men", "fisher women", "fisher children")
            )
         
         colnames(Q12_summary_long) <- c("MA name", "N", "Fishers", "Proportion (%)")
         
         Q12 <- data_4plot(Q12_summary_long)
         
         colnames(Q12) <- c("MA name", "N", "Fishers", "Average")
         
         plot_Q12 <-
            ggplot(Q12, aes(`MA name`, `Average`, N = N)) +
            facet_wrap( ~ Fishers) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            ggtitle("Average number of household members \nthat go fishing regularly") +
            scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
            labs (x= NULL, y = "\nNumber of people") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q12, height = 750)
         
         
      }
      
      ### Q13 How many members of the household regularly go fishing? (customize for community fishing activities) #####
      else if (input$hhs_question == "13. How many members of the household regularly participate in post-processing activities to prepare the fish for sale?") {
         
         hhs_Q13 <- selectedData()[,c("ma_name", 
                                     "13a_processing_men",
                                     "13b_processing_women",
                                     "13c_processing_children")]
         
         Q13_summary <- hhs_Q13 %>%
            group_by(ma_name) %>%
            summarise(
               "N" = n(),
               "men" = round(mean(`13a_processing_men`, na.rm =
                                            TRUE), 1),
               "women" = round(mean(`13b_processing_women`, na.rm =
                                              TRUE), 1),
               "children" = round(mean(`13c_processing_children`, na.rm =
                                                 TRUE), 1)
            )
         
         Q13_summary <- rbind(Q13_summary,
                              c(
                                 NA,
                                 sum(Q13_summary$N),
                                 mean_sem(Q13_summary$`men`, 1),
                                 mean_sem(Q13_summary$`women`, 1),
                                 mean_sem(Q13_summary$`children`, 1)
                              ))
         #plot
         Q13_summary_long <-
            Q13_summary %>% pivot_longer(
               cols = c("men", "women", "children"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q13_summary_long$key <-
            factor(
               Q13_summary_long$key,
               levels = c("men", "women", "children")
            )
         
         colnames(Q13_summary_long) <- c("MA name", "N", "Processors", "Proportion (%)")
         
         Q13 <- data_4plot(Q13_summary_long)
         
         colnames(Q13) <- c("MA name", "N", "Processors", "Average")
         
         plot_Q13 <-
            ggplot(Q13, aes(`MA name`, `Average`, N = N)) +
            facet_wrap( ~ Processors) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            ggtitle("Average number of household members \nthat participate in post-procesing activities") +
            scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
            labs (x= NULL, y = "\nNumber of people") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q13, height = 750)
         
      }
      
      ### Q14 What activities are the responsibility of the women in the household in a typical week? ####
      else if (input$hhs_question == "14. What activities are the responsibility of the women in the household in a typical week?") {
         
         hhs_Q14 <- selectedData_Q14()[,c("submissionid", "ma_name", "14_responsibility")] 
         
         length.unique <- function(x) { length(unique(x)) }
         
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
            
         Q14_summary <- proportion_Q14(hhs_Q14[[1]], 
                                       hhs_Q14[[3]],
                                       hhs_Q14[[2]],
                                   rounding = 3, 
                                   type = length(unique(hhs_Q14[[3]])))
          
         Q14_summary_long <-
            Q14_summary %>% pivot_longer(
               cols = c(3:ncol(Q14_summary)),
               names_to = "Activity",
               values_to = "Proportion (%)"
            )
         
         Q14 <- data_4plot(Q14_summary_long)
         Q14$Activity <- str_replace_all(Q14$Activity, "[.]", " ")
         
       
         plot_Q14 <-
            ggplot(Q14, aes(`MA name`, `Proportion (%)`, N = N)) +
            geom_bar(aes(fill = Activity),
               position = position_stack(reverse = TRUE),
               stat = "identity",
               alpha = 0.8 ) +
            theme_rare +
            scale_fill_brewer(palette = "Blues", direction = -1)+
            ggtitle(
               "Proportion of activities that are the responsibility \nof the women in the household in a typical week"
            ) +
            scale_y_continuous(limits = c(0, 105),
                               breaks = seq(0, 100, 25)) +
            
            xlab (NULL) + ylab ("Proportion (%)") + coord_flip(clip = "on") +
            theme(legend.position = "right") +
            guides(fill = guide_legend(reverse = TRUE)) 
         ggplotly(plot_Q14, height = 750)
      }
         
      ### Q16 Consider the main fisher in the household. Does this person fish? #####
      else if (input$hhs_question == "16. Consider the main fisher in the household. Does this person fish?") {
         
         hhs_Q16 <- selectedData()[,c("ma_name", "16_hh_main_fisher")] %>%
            filter(`16_hh_main_fisher` != "") %>%
               droplevels()
         
         Q16_summary <- proportion(hhs_Q16[[2]],
                                   hhs_Q16[[1]],
                                   3, type = length(unique(hhs_Q16[[2]])))
         Q16_summary_long <-
            Q16_summary %>% pivot_longer(
               cols = c(3:ncol(Q16_summary)),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q16 <- data_4plot(Q16_summary_long)
         Q16$key <- str_replace_all(Q16$key, "[.]", " ")
         
         plot_Q16 <-
            ggplot(Q16, aes(`MA name`, `Proportion (%)`, N = N)) +
            geom_bar(aes(fill = key),
                     position = position_stack(reverse = TRUE),
                     stat = "identity",
                     alpha = 0.8 ) +
            theme_rare + 
            scale_fill_brewer(palette = "Blues", direction = -1) +
            ggtitle("Proportion of main fishers in the household \nthat fish as member of a group"
            ) +
            scale_y_continuous(limits = c(0, 105),
                               breaks = seq(0, 100, 20)) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")+
            theme(legend.position = "right") +
            guides(fill = guide_legend(reverse = TRUE)) 
         
         ggplotly(plot_Q16, height = 750)
         
      }
      
      ### Q17 How often did the main fisher go fishing during lean season or less profitable fishing season? #####
      else if (input$hhs_question == "17. How often did the main fisher go fishing during lean season or less profitable fishing season?") {
         
         hhs_Q17 <- selectedData()[,c("ma_name", "17_fishing_low_profit")] %>%
            filter(`17_fishing_low_profit` != "") %>%
               droplevels()
         
         hhs_Q17$`17_fishing_low_profit` <- 
            recode_factor(hhs_Q17$`17_fishing_low_profit`,
                           "7 per week" = "Everyday",
                           "5-6 per week" = "Five or six times per week",
                           "3-4 per week" = "Three or four times per week",
                           "1-2 per week" = "One or two times per week",
                           "More than 1-2 times per week" = "More than few times per week")
         
         Q17_summary <- proportion(hhs_Q17[[2]],
                                   hhs_Q17[[1]],
                                   3, type = length(unique(hhs_Q17[[2]])))
            Q17_summary_long <-
               Q17_summary %>% pivot_longer(
                  cols = c(3:ncol(Q17_summary)),
                  names_to = "Frequency",
                  values_to = "Proportion (%)")
            
            Q17_summary_long$Frequency <- str_replace_all(Q17_summary_long$Frequency, "[.]", " ")
            
            Q17_summary_long$Frequency <- factor(Q17_summary_long$Frequency,
                                          levels = c("Once or never",
                                                     "A few times",
                                                     "A few times per month",
                                                     "One or two times per week",
                                                     "More than few times per week",
                                                     "Three or four times per week",
                                                     "Five or six times per week",
                                                     "Everyday"))
            Q17 <- data_4plot(Q17_summary_long)
  
            plot_Q17 <-
               ggplot(Q17, aes(`MA name`, `Proportion (%)`, N = N)) +
               geom_bar(aes(fill = Frequency),
                        position = position_stack(reverse = TRUE),
                        stat = "identity",
                        alpha = 0.8 ) +
               theme_rare + 
               scale_fill_brewer(palette = "Blues", direction = -1) +
               ggtitle("Frequency that the main fisher go fishing \nduring less profitable fishing season"
               ) +
               scale_y_continuous(limits = c(0, 105),
                                  breaks = seq(0, 100, 25)) +
               xlab (NULL) + 
               ylab ("Proportion (%)") + 
               coord_flip(clip = "on") +
               theme(legend.position = "right") +
               guides(fill = guide_legend(reverse = TRUE)) 
            
            ggplotly(plot_Q17, height = 750)
      }
      
      ### Q18 How often did the main fisher go fishing in the more profitable fishing season? #####
      else if (input$hhs_question == "18. How often did the main fisher go fishing in the more profitable fishing season?") {
         
         hhs_Q18 <- selectedData()[,c("ma_name", "18_fishing_high_profit")] %>%
            filter(`18_fishing_high_profit` != "") %>%
               droplevels()
         
         hhs_Q18$`18_fishing_high_profit` <- 
            recode_factor(hhs_Q18$`18_fishing_high_profit`,
                          "7 per week" = "Everyday",
                          "7 times per week" = "Everyday",
                          "6 times per week" = "Five or six times per week",
                          "5-6 per week" = "Five or six times per week",
                          "3-4 per week" = "Three or four times per week",
                          "3-4 times per week" = "Three or four times per week",
                          "1-2 per week" = "One or two times per week",
                          "More than 1-2 times per week" = "More than few times per week")
         
         Q18_summary <- proportion(hhs_Q18[[2]],
                                   hhs_Q18[[1]],
                                   3, type = length(unique(hhs_Q18[[2]])))
         Q18_summary_long <-
            Q18_summary %>% pivot_longer(
               cols = c(3:ncol(Q18_summary)),
               names_to = "Frequency",
               values_to = "Proportion (%)")
         
         Q18_summary_long$Frequency <- str_replace_all(Q18_summary_long$Frequency, "[.]", " ")
         
         Q18_summary_long$Frequency <- factor(Q18_summary_long$Frequency,
                                              levels = c("Once or never",
                                                         "A few times",
                                                         "A few times per month",
                                                         "One or two times per week",
                                                         "More than few times per week",
                                                         "Three or four times per week",
                                                         "Five or six times per week",
                                                         "Everyday"))
         Q18 <- data_4plot(Q18_summary_long)
         
         plot_Q18 <-
            ggplot(Q18, aes(`MA name`, `Proportion (%)`, N=N)) +
            geom_bar(aes(fill = Frequency),
                     position = position_stack(reverse = TRUE),
                     stat = "identity",
                     alpha = 0.8 ) +
            theme_rare + 
            scale_fill_brewer(palette = "Blues", direction = -1) +
            ggtitle("Frequency that the main fisher go fishing \nduring the high profitable fishing season"
            ) +
            scale_y_continuous(limits = c(0, 105),
                               breaks = seq(0, 100, 25)) +
            xlab (NULL) + 
            ylab ("Proportion (%)") + 
            coord_flip(clip = "on") +
            theme(legend.position = "right") +
            guides(fill = guide_legend(reverse = TRUE)) 
         
         ggplotly(plot_Q18, height = 750)
      }
      
      
      
      ### Q19 Proportion of fishers who perceived that their catch remained stable or increased over the past 2 years  #####
      else if (input$hhs_question == "19. Compared to 2 years ago, the current fish catch has...") {
         #subset
         hhs_Q19 <- selectedData()[,c("ma_name", "19_current_fish_catch")] %>%
            filter(`19_current_fish_catch` != "") %>%
               droplevels()
         #proportion
         Q19_summary <- proportion(hhs_Q19[[2]], 
                                   hhs_Q19[[1]], 
                                   3, type = "5")
         Q19 <- Q19_summary %>% filter (`MA name` != "Mean ± SE")
         
         #Combine categories in 3 choices
         Q19$Declined <- as.numeric(Q19$Declined.a.lot) + as.numeric(Q19$Declined.slightly)
         Q19$Improved <- as.numeric(Q19$Improved.slightly) + as.numeric(Q19$Improved.heavily)
         
         #Format table for Rmarkdown report
         Q19_summary <-
            rbind(Q19[, c("MA name",
                          "N",
                          "Declined",
                          "Stayed.the.same",
                          "Improved")],
                  `Mean ± SE` = c('',
                     sum(as.numeric(Q19$N)),
                     mean_sem(Q19$Declined, 1),
                     mean_sem(Q19$Stayed.the.same, 1),
                     mean_sem(Q19$Improved, 1)
                  ))
         colnames(Q19_summary) <-
            c("MA name",
              "N",
              "Declined",
              "Stayed the same",
              "Improved")
          
         #plot
         Q19_summary_long <-
            Q19_summary %>% pivot_longer(
               cols = c("Declined", "Stayed the same", "Improved"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q19_summary_long$key <-
            factor(
               Q19_summary_long$key,
               levels = c("Declined", "Stayed the same", "Improved")
            )
         
         Q19 <- data_4plot(Q19_summary_long)
         
         plot_Q19 <-
            ggplot(Q19, aes(`MA name`, `Proportion (%)`, N = N)) +
            facet_wrap( ~ key) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            ggtitle("Proportion of fishers who perceived that their catch \nremained stable or increased over the past 2 years"
            ) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            xlab (NULL) + ylab ("Proportion (%)") + coord_flip(clip = "on")
         ggplotly(plot_Q19, height = 750)
      }
      
      
      
      ### Q22 Proportion of fishers who perceived that their catch will remain stable or increase over the next 5 years #####
      else if (input$hhs_question == "22. In the next 5 years, how do you think the fish catch will be compared to today?") {
         #subset
           
         hhs_Q22 <-selectedData()[,c("ma_name", "22_catch_5yrs")] %>%
            filter(`22_catch_5yrs` != '') %>%
               rbind(c(NA,"Declines a lot"), c(NA,"Declines slightly"), 
                     c(NA,"Stays the same"), c(NA,"Improves slightly"),
                     c(NA,"Improves heavily")) %>%
                  droplevels()
         #proportion
         Q22_summary <-
            proportion(hhs_Q22[[2]], 
                       hhs_Q22[[1]], 
                       3,5)
         #remove summary row
         Q22_summary_bind <- Q22_summary %>%
                              filter(`MA name` != "Mean ± SE") %>%
                                 droplevels()
         #Combine categories
         Q22_summary_bind$Declines <-
            as.numeric(Q22_summary_bind$Declines.a.lot) + 
            as.numeric(Q22_summary_bind$Declines.slightly)
         
         Q22_summary_bind$Improves <-
            as.numeric(Q22_summary_bind$Improves.slightly) + 
            as.numeric(Q22_summary_bind$Improves.heavily)
         
         #Format table for Rmarkdown report
         Q22_summary <-
            rbind(
               Q22_summary_bind[, c("MA name",
                                       "N",
                                       "Declines",
                                       "Stays.the.same",
                                       "Improves")],
                  `Mean ± SE` = c('',
                     sum(as.numeric(Q22_summary_bind$N)),
                     mean_sem(Q22_summary_bind$Declines, 1),
                     mean_sem(Q22_summary_bind$Stays.the.same, 1),
                     mean_sem(Q22_summary_bind$Improves), 1)
               )
         
         Q22_summary <- Q22_summary %>% filter (`MA name` != "")
         #rename colums
         colnames(Q22_summary) <-
            c("MA name",
              "N",
              "Decline (%)",
              "Stay the same (%)",
              "Improve (%)")
                     
         #pivot table
         Q22_summary_long <-
            Q22_summary %>% pivot_longer(
               cols = c("Decline (%)", "Stay the same (%)", "Improve (%)"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q22_summary_long$key <-
            factor(
               Q22_summary_long$key,
               levels = c("Decline (%)", "Stay the same (%)", "Improve (%)")
            )
         
         Q22 <- data_4plot (Q22_summary_long) 
         
         #Plot
         plot_Q22 <-
            ggplot(Q22, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of fishers who perceived that their catch \nwill remain stable or increase over the next 5 years"
            ) +
            xlab (NULL) + ylab ("\n\nProportion (%)") + coord_flip(clip = "on")
         ggplotly(plot_Q22, height = 750)
      }
      
      
      
      
      ### Q23 "Proportion of fishers who are confident that their jobs are secure" ####
      
      else if (input$hhs_question == "23. Do you believe that the job of a fisher is secure in the future?") {
          
         ## Exclude NAs from answers
         hhs_Q23 <- selectedData()[,c("ma_name", "23_job_secure")] %>%
                     filter(`23_job_secure` %in% c(0,1)) %>%
                      rbind(c(NA,0), c(NA,1)) %>%
                        droplevels()
         #proportion
         Q23_summary <- proportion(hhs_Q23[[2]], 
                           hhs_Q23[[1]], rounding = 3, type = 2)[, -3]
         #rename columns
         colnames(Q23_summary) <- c("MA name", "N", "Proportion (%)")
         ## add total and mean
         Q23 <- data_4plot(Q23_summary)
          #Plot
         plot_Q23 <-
            ggplot(Q23, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of fishers who are confident \nthat their jobs are secure") +
            xlab (NULL) + ylab ("Proportion (%)") + 
               coord_flip(clip ="on")
         
         ggplotly(plot_Q23, height = 750)
         
      }
      
      
      ### Q24 "Major assets purchased in the previous 12 months" ####
      
      else if (input$hhs_question == "24. Please indicate the number of items that are owned in the previous 12 months by the household") {
         #subset data
         hhs_Q24 <- selectedData()[, c(
               "24a_item_radio_no",
               "24b_item_tv_no",
               "24c_item_satellite_no",
               "24d_item_phone_no",
               "24e_item_washing_maching_no",
               "24f_item_generator_no",
               "24g_item_fridge_no",
               "24h_item_motorboat_no",
               "24i_item_outboard_no",
               "24j_item_inboard_no",
               "24k_item_sailboat_no",
               "24l_item_bicycle_no",
               "24m_item_motorcycle_no",
               "24n_item_car_no",
               "24o_item_internet_no",
               "24p_item_other_no"
            )]
         #replace NA with 0s
         hhs_Q24[is.na(hhs_Q24)] <- 0
         hhs_Q24_01_items <- hhs_Q24
         
         #Convert matrix to 0 and 1 (ignore wanrning)
         hhs_Q24_01_items[hhs_Q24_01_items > 0] <- 1
         
         hhs_Q24_01 <- data.frame(hhs_Q24_01_items, 
                                  ma_name = selectedData()$ma_name)
         
         #Estimate Proportion of community members with each asset
         prop <- function(x) {
            round(sum(x) / length(x) * 100, 1)
         }
         
         assets_prop <-
            aggregate(
               cbind(
                  hhs_Q24_01[, 1],
                  hhs_Q24_01[, 2],
                  hhs_Q24_01[, 3],
                  hhs_Q24_01[, 4],
                  hhs_Q24_01[, 5],
                  hhs_Q24_01[, 6],
                  hhs_Q24_01[, 7],
                  hhs_Q24_01[, 8],
                  hhs_Q24_01[, 9],
                  hhs_Q24_01[, 10],
                  hhs_Q24_01[, 11],
                  hhs_Q24_01[, 12],
                  hhs_Q24_01[, 13],
                  hhs_Q24_01[, 14],
                  hhs_Q24_01[, 15],
                  hhs_Q24_01[, 16]
               ) ~ ma_name,
               data = hhs_Q24_01,
               FUN = prop
            )
         
         N_assets <- tapply(hhs_Q24_01[, 1], hhs_Q24_01[, 17], length)
         
         #Rename columns
         colnames(assets_prop) <-
            c(
               "MA name",
               "Radio",
               "TV",
               "Satellite",
               "Phone",
               "Washing_Machine",
               "Generator",
               "Fridge",
               "Motorboat",
               "Outboard",
               "Inboard",
               "Sailboat",
               "Bicycle",
               "Motorcycle",
               "Car",
               "Internet",
               "Other"
            )
         ## Assest by MA
         assets_prop_totals <-
            rbind(
               assets_prop,
               c(
                  NA,
                  mean_sem(assets_prop[[2]], 1),
                  mean_sem(assets_prop[[3]], 1),
                  mean_sem(assets_prop[[4]], 1),
                  mean_sem(assets_prop[[5]], 1),
                  mean_sem(assets_prop[[6]], 1),
                  mean_sem(assets_prop[[7]], 1),
                  mean_sem(assets_prop[[8]], 1),
                  mean_sem(assets_prop[[9]], 1),
                  mean_sem(assets_prop[[10]], 1),
                  mean_sem(assets_prop[[11]], 1),
                  mean_sem(assets_prop[[12]], 1),
                  mean_sem(assets_prop[[13]], 1),
                  mean_sem(assets_prop[[14]], 1),
                  mean_sem(assets_prop[[15]], 1),
                  mean_sem(assets_prop[[16]], 1),
                  mean_sem(assets_prop[[17]], 1)
               )
            )
         ## Total number of assets
         hhs_Q24$assets_no <- rowSums(hhs_Q24_01[, 1:16])
         hhs_Q24$ma_name <- selectedData()$ma_name
         
         ## mean number of assets per hhs in each MA
         Q24_summary_bind <-
            data.frame(cbind(N = N_assets, Assets_number = round(
               tapply(hhs_Q24$assets_no, hhs_Q24$ma_name, mean), 2
            )))
         
         Q24_summary <-
            rbind(Q24_summary_bind, "Mean ± SE" = c(
               sum(Q24_summary_bind$N),
               mean_sem(Q24_summary_bind$Assets_number, 2)
            ))
         Q24_summary <- rownames_to_column(Q24_summary, "MA name")
         colnames(Q24_summary) <- c("MA name", "N", "Proportion (%)")
         Q24 <- data_4plot(Q24_summary)
         colnames(Q24) <- c("MA name", "N", "Average")
            
         #Plot
         plot_Q24 <-
            ggplot(Q24, aes(`MA name`, `Average`, N = N)) +
            theme_rare + geom_col(alpha = 0.8, fill = "#005BBB") +
            ggtitle("Average number of major assets \npurchased in the previous 12 months") +
            xlab (NULL) + ylab ("Average number of assets") +
            coord_flip(clip = "on")
         ggplotly(plot_Q24, height = 750)
         
      }
      
      ### Q25 Proportion of households with active accounts in financial institutions #####
      else if (input$hhs_question == "25. Does any household member have an account at a financial institution or keep money in any of the following ways? If yes, please specify.") {
         ### Replace NAs with zeroes
         hhs_Q25 <- selectedData()[, c(
            "ma_name",
            "25a_financial_bank",
            "25b_financial_micro",
            "25c_financial_ngo",
            "25d_financial_lender",
            "25e_financial_insurance",
            "25f_financial_other"
         )] %>%
            filter(ma_name != "")
         #replace NA with 0s
         hhs_Q25[is.na(hhs_Q25)] <- 0
         
         #Summary of financials accounts
         hhs_Q25$X25_financial_account <-
            as.numeric(hhs_Q25$`25a_financial_bank`) +
            as.numeric(hhs_Q25$`25b_financial_micro`) +
            as.numeric(hhs_Q25$`25c_financial_ngo`) +
            as.numeric(hhs_Q25$`25d_financial_lender`) +
            as.numeric(hhs_Q25$`25e_financial_insurance`) +
            as.numeric(hhs_Q25$`25f_financial_other`) - 1 #to set at zero
         
         #Convert to 0 and 1
         hhs_Q25$X25_financial_account_0_1 <-
            ifelse(hhs_Q25$X25_financial_account > 0, 1, 0)
         
         hhs_Q25 <- hhs_Q25[,c("ma_name", "X25_financial_account_0_1")] %>%
                           rbind(c(NA,0), c(NA,1))
         
         #proportion
         Q25_summary <-
            proportion(question = hhs_Q25[[2]],
                       grouping = hhs_Q25[[1]],
                       rounding = 3,
                       type = 2)[, -3]
         #rename columns
         colnames(Q25_summary) <- c("MA name", "N", "Proportion (%)")
         #summary
         Q25 <- data_4plot(Q25_summary)
         
         #plot
         plot_Q25 <-
            ggplot(Q25, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            ggtitle("Proportion of households with \nactive accounts in financial institutions") +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q25, height = 750)
      }
      
      ### Q26 "Proportion of househols with enough income to save" ####
      
      else if (input$hhs_question == "26. Does your income provide enough to save?") {
         hhs_Q26 <- selectedData()[,c("ma_name", "26_fishing_income_save")] %>%
            filter(`26_fishing_income_save` != "") 
         
         #Convert to 0 and 1
         hhs_Q26$X26_fishing_income_save <-
            ifelse(hhs_Q26$`26_fishing_income_save` > 0, 1, 0)
         
         hhs_Q26 <- rbind(hhs_Q26, c(NA,0), c(NA,1))
         
         #proportion
         Q26_summary <-
            proportion(hhs_Q26[[2]],
                       hhs_Q26[[1]],
                       3,2)[, -3]
         #rename
         colnames(Q26_summary) <- c("MA name", "N", "Proportion (%)")
         #summary
         Q26 <- data_4plot(Q26_summary)
          
         #Plot
         plot_Q26 <-
            ggplot(Q26, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(alpha = 0.8, fill = "#005BBB") +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of househols with enough income to save") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q26, height = 750)
         
      }
      
      
      
      ### Q27 Proportion of households with access to emergency funds #####
      else if (input$hhs_question == "27. Does any household member have access to one of the following sources of funds (enough to replace fishing gear, or primary livelihood gear, boat repairs)? If yes, please specify.") {
         #subset
            hhs_Q27 <-
               selectedData()[, c(
               "ma_name",
               "27a_emergency_personal",
               "27b_emergency_family",
               "27c_emergency_friend",
               "27d_emergency_entrepreneur",
               "27e_emergency_savings_club",
               "27f_emergency_lender",
               "27g_emergency_commercial_ind",
               "27h_emergency_commercial_group",
               "27i_emergency_microfinance_ind",
               "27j_emergency_microfinance_group",
               "27k_emergency_ngo_ind",
               "27l_emergency_ngo_group",
               "27m_emergency_insurance",
               "27n_emergency_other"
            )] %>%
            filter(ma_name != '')
         #sum in obe variable
         hhs_Q27$`27n_emergency_other` <- as.numeric(as.character(hhs_Q27$`27n_emergency_other`))
         hhs_Q27[is.na(hhs_Q27)] <- 0
         hhs_Q27$X27_emergency_fund <-
            hhs_Q27$`27a_emergency_personal` +
            hhs_Q27$`27b_emergency_family` +
            hhs_Q27$`27c_emergency_friend` +
            hhs_Q27$`27d_emergency_entrepreneur` +
            hhs_Q27$`27e_emergency_savings_club` +
            hhs_Q27$`27f_emergency_lender` +
            hhs_Q27$`27g_emergency_commercial_ind` +
            hhs_Q27$`27h_emergency_commercial_group` +
            hhs_Q27$`27i_emergency_microfinance_ind` +
            hhs_Q27$`27j_emergency_microfinance_group` +
            hhs_Q27$`27k_emergency_ngo_ind` +
            hhs_Q27$`27l_emergency_ngo_group` +
            hhs_Q27$`27m_emergency_insurance` +
            hhs_Q27$`27n_emergency_other`
         
         #Convert to 0 and 1
         hhs_Q27$X27_emergency_fund_0_1 <-
            ifelse(hhs_Q27$X27_emergency_fund > 0, 1, 0) 
         ##addboth responses always
         
         hhs_Q27 <- rbind(hhs_Q27, c(NA,0), c(NA,1))
         
         #proportion
         Q27_summary <-
            proportion(hhs_Q27$X27_emergency_fund_0_1, 
                       hhs_Q27$ma_name, 3, 2)[, -3]
         #col rename
         colnames(Q27_summary) <- c("MA name", "N", "Proportion (%)")
         #summary
         Q27 <- data_4plot(Q27_summary)
           
         #plot
         plot_Q27 <-
            ggplot(Q27, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            ggtitle("Proportion of households with access to emergency funds") +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
        
          ggplotly(plot_Q27, height = 750)
         
      }
      
      
      ### Q28 "Proportion of household that take out loans from fish buyers or traders" ####
      else if (input$hhs_question == "28. Does your household take out loans from fish buyers or traders?") {
         hhs_Q28 <- selectedData()[,c("ma_name", "28_buyer_loans")] %>%
            filter(`28_buyer_loans` != "") %>%
               rbind(c(NA,0), c(NA,1))
         #recode to 0 and 1
         hhs_Q28$`28_buyer_loans` <-
            ifelse(hhs_Q28$`28_buyer_loans` > 0, 1, 0)
         #proportion
         Q28_summary <-
            proportion(hhs_Q28[[2]],
                       hhs_Q28[[1]], 
                       3, 2)[, -3] #drop the No
         #rename
         colnames(Q28_summary) <-
            c("MA name", "N", "Proportion (%)") #keep the yes
         #summary
         Q28 <- data_4plot(Q28_summary)
         
         #Plot
         plot_Q28 <-
            ggplot(Q28, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(alpha = 0.8, fill = "#005BBB") +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of household that \ntake out loans from fish buyers or traders") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q28, height = 750)
         
      }
      
      
      ### Q29 "Proportion of community members that have sufficient income to cover their family's needs" ####
       else if (input$hhs_question == "29. To cover family needs your household income is…") {
         #subset
        
         hhs_Q29 <- selectedData()[,c("ma_name", "29_family_income")] %>%
                        filter(`29_family_income` %in% c("Sufficient",
                                                         "Insufficient",
                                                         "Tight")) %>%
                           rbind(c(NA, "Sufficient"), 
                                 c(NA, "Insufficient"),
                                 c(NA, "Tight")) %>%
                            droplevels()
         #proportion
         Q29_summary <-
            proportion(hhs_Q29[[2]], hhs_Q29[[1]], 3, 3)
         #rename column
         colnames(Q29_summary) <-
            c("MA name",
              "N",
              "Insufficient (%)",
              "Sufficient (%)",
              "Tight (%)")
        
          #plot set up
         Q29_longer <-
            Q29_summary %>% pivot_longer (
               cols = c("Insufficient (%)", "Sufficient (%)", "Tight (%)"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q29_longer$key <-
            factor(Q29_longer$key,
                   levels = c("Insufficient (%)", "Tight (%)", "Sufficient (%)"))
         
         Q29 <- data_4plot(Q29_longer)

         #Plot
         plot_Q29 <-
            ggplot(Q29, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(alpha = 0.8, fill = "#005BBB") + 
            facet_wrap( ~ key) +
            ggtitle(
               "Proportion of community members that have \nsufficient income to cover their family's needs"
            ) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            xlab (NULL) + ylab ("Proportion (%)") +
            coord_flip(clip = "on")
         
         ggplotly(plot_Q29, height = 750)
         
      }
      
      
      ### Q30a Proportion of community members who trust in the national government to make decisions that benefit small scale fishing communities ####
      
      else if (input$hhs_question == "30a. Please rate your agreement with the following statement: Local decision-makers/ local authorities can be trusted to make decisions that benefit the community over their own interests") {
         #subset
         hhs_Q30a <- selectedData()[,c("ma_name", "30_trust_local_decision")] %>%
            filter(`30_trust_local_decision` %in% c(1:5)) %>%
               rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
                  droplevels()
            
         #propoortion
         Q30a_summary <-
            proportion(hhs_Q30a$`30_trust_local_decision`,
                       hhs_Q30a$ma_name,
                       3, type=5)
         
         colnames(Q30a_summary) <- c("MA name", 
                                     "N", 
                                     "Strongly disagree", 
                                     "Disagree",
                                     "Neither agree nor disagree",
                                     "Agree",
                                     "Strongly agree")
         
         Q30a <- Q30a_summary %>%
                     filter(`MA name` != "Mean ± SE")
         #combine answer
         Q30a$`Proportion (%)` <- as.numeric(Q30a$Agree) + as.numeric(Q30a$`Strongly agree`)
         
         Q30a <- data_4plot(Q30a)
         #Plot
         plot_Q30a <-
            ggplot(Q30a, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members who trust in the local goverment \nto make decisions that benefit the community over their own interests"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
        
         ggplotly(plot_Q30a, height = 750)
         
      }
      
      ### Q30b "Proportion of community members who trust in their fellow community members" ####
      
      else if (input$hhs_question == "30b. Please rate your agreement with the following statement: Regional decision-makers/ regional authorities can be trusted to make decisions that benefit the community over their own interests") {
         #subset
         hhs_Q30b <- selectedData()[,c("ma_name", "30_trust_regional_decision")] %>%
            filter(`30_trust_regional_decision` %in% c(1:5)) %>%
               rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
                  droplevels()
            
         #proportion
         Q30b_summary <-
            proportion (hhs_Q30b[[2]],
                        hhs_Q30b[[1]],
                        3, type = 5)
         colnames(Q30b_summary) <- c("MA name", 
                                     "N", 
                                     "Strongly disagree", 
                                     "Disagree",
                                     "Neither agree nor disagree",
                                     "Agree",
                                     "Strongly agree")
         Q30b <- Q30b_summary %>%
            filter(`MA name` != "Mean ± SE")
         
         #sum agree and strongly agree
         Q30b$`Political Trust` <- as.numeric(Q30b$Agree) + as.numeric(Q30b$`Strongly agree`)
         Q30b_summary <- Q30b[, c("MA name", "N", "Political Trust")]
         
         #rename columns for shiny app
         colnames(Q30b_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q30b <- data_4plot(Q30b_summary)
         
         #Plot
         plot_Q30b <-
            ggplot(Q30b, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members who trust in \nregional goverment to make decisions that benefit \nthe community over their own interests"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q30b, height = 750)
         
      }
      
      ### Q30c "Proportion of community members who trust in local decision-makers to make decisions that benefit the community over their personal interests" ####
      
      else if (input$hhs_question == "30c. Please rate your agreement with the following statement: Generally speaking, most people in my community can be trusted") {
         #subset
         hhs_Q30c <- selectedData()[,c("ma_name", "30_trust_community")] %>%
            filter(`30_trust_community` %in% c(1:5)) %>%
               rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
                  droplevels()
         
         #propoortion
         Q30c_summary<-
            proportion(hhs_Q30c[[2]], 
                       hhs_Q30c[[1]], 
                       3, type = 5)
         colnames(Q30c_summary) <- c("MA name", 
                                     "N", 
                                     "Strongly disagree", 
                                     "Disagree",
                                     "Neither agree nor disagree",
                                     "Agree",
                                     "Strongly agree")
         
         Q30c <- Q30c_summary %>%
            filter(`MA name` != "Mean ± SE")
         
         #sum agree and strongly agree
         Q30c$`Social Cohesion` <- as.numeric(Q30c$Agree) + as.numeric(Q30c$`Strongly agree`)
         Q30c_summary <- Q30c[, c("MA name", "N", "Social Cohesion")]
         
         #rename columns for shiny app
         colnames(Q30c_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q30c <- data_4plot(Q30c_summary)
         
         #Plot
         plot_Q30c <-
            ggplot(Q30c, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of community members who trust in \ntheir fellow community members"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         ggplotly(plot_Q30c, height = 750)
         
      }
      
      ### Q30d "Proportion of community members who ho believe you have to be alert to someone taking advantage of you#####
      
      else if (input$hhs_question == "30d. Please rate your agreement with the following statement: In this village, you have to be alert to someone taking advantage of you") {
         ### Exclude NA
         hhs_Q30d <- selectedData()[,c("ma_name", "30_trust_village_alert")] %>%
            filter(`30_trust_village_alert` %in% c(1:5)) %>%
               rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
                  droplevels()
         #proportion
         Q30d_summary <-
            proportion(hhs_Q30d[[2]],
                       hhs_Q30d[[1]],
                       3, type= 5)
         
         colnames(Q30d_summary) <- c("MA name", 
                                     "N", 
                                     "Strongly disagree", 
                                     "Disagree",
                                     "Neither agree nor disagree",
                                     "Agree",
                                     "Strongly agree")
         Q30d <- Q30d_summary %>%
            filter(`MA name` != "Mean ± SE")
         
         #sum agree and strongly agree
         Q30d$`Social Trust` <- as.numeric(Q30d$Agree) + as.numeric(Q30d$`Strongly agree`)
         Q30d_summary <- Q30d[, c("MA name", "N", "Social Trust")]
         
         #rename columns for shiny app
         colnames(Q30d_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q30d <- data_4plot(Q30d_summary)
         
         #Plot
         plot_Q30d <-
            ggplot(Q30d, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members who believe that \nyou have that to be alert to someone taking advantage of you"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q30d, height = 750)
      }
      
      
      ### Q31 "Proportion of community members who believe that the community has the ability to manage the fishery effectively to maximize food and profits" ####
      
      else if (input$hhs_question == "31. How much do you agree or disagree with the following statement? “My community has the ability to manage my fishery effectively and to maximize food and profits?”") {
         #subset
         hhs_Q31 <- selectedData()[,c("ma_name", "31_my_community_ability")] %>%
            filter(`31_my_community_ability` != "") %>%
            filter(`31_my_community_ability` != "No dependance") %>%
               rbind(c(NA, "Agree"), 
                     c(NA, "Neither"), 
                     c(NA, "Strongly agree"),
                     c(NA, "Disagree"),
                     c(NA, "Strongly disagree")) %>%
               droplevels()
         #proportion
         Q31_summary <-
            proportion(hhs_Q31[[2]],
                       hhs_Q31[[1]],
                       3, type=5)
         
         Q31 <- Q31_summary %>%
            filter(`MA name` != "Mean ± SE")
         
         colnames(Q31) <- c("MA name", "N", 
                                    "Agree", 
                                    "Disagree",
                                    "Neither",
                                    "Strongly agree",
                                    "Strongly disagree")
         
         #combine agree answer
         Q31$`Collective Efficacy` <- as.numeric(Q31$Agree) + as.numeric(Q31$`Strongly agree`)
         
         #summary
         Q31_summary <- Q31[, c("MA name", "N", "Collective Efficacy")]
         
         colnames(Q31_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q31 <- data_4plot(Q31_summary)
            
         #Plot
         plot_Q31 <-
            ggplot(Q31, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members who believe that the community \nhas the ability to manage the fishery effectively to maximize food and profits"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         ggplotly(plot_Q31, height = 750)
         
      }
      
      
      ### Q32 Proportion of community that believes they benefit equally from fishery as other households ####
      
      else if (input$hhs_question == "32. Do you believe you benefit equally from the fishery as other members of the community?") {
         hhs_Q32 <- selectedData()[,c("ma_name", "32_fishery_benefit_equal")] %>%
            filter(`32_fishery_benefit_equal` %in% c(0,1)) %>%
               rbind(c(NA,0), c(NA,1)) %>%
                  droplevels()
         
         #proportion
         Q32_summary <-
            proportion(hhs_Q32$`32_fishery_benefit_equal`,
                       hhs_Q32$ma_name,
                       3, type = 2)[, -3]
         #summary for table
         colnames(Q32_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q32 <- data_4plot(Q32_summary)
         
         #Plot
         plot_Q32 <-
            ggplot(Q32, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community that believes \nthey benefit equally from fishery as other households"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q32, height = 750)
      }
      
      ### Q33 Proportion of HH familiar with the managed access and reserve area management approach? ####
      
      else if (input$hhs_question == "33. Are you familiar with the managed access and reserve area management approach?") {
         #subset
         hhs_Q33 <- selectedData()[,c("ma_name", "33_ma_familiar")] %>%
            filter(`33_ma_familiar` %in% c(0,1)) %>%
               rbind(c(NA,0), c(NA,1)) %>%
                  droplevels()
         
          #proportion
         Q33_summary <-
            proportion(hhs_Q33[[2]], 
                       hhs_Q33[[1]], 
                       3, type = 2)[, -3]
         #rename
         colnames(Q33_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q33 <- data_4plot (Q33_summary)
         
         #Plot
         plot_Q33 <-
            ggplot(Q33, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community familiar with the \nmanaged access and reserve area management approach"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
               coord_flip(clip = "on")
           
         ggplotly(plot_Q33, height = 750)
      }
      
      ### Q34 Proportion of community members who are aware of gear restrictions ####
      else if (input$hhs_question == "34. Are there restrictions on the type of gear that may be used in the fisheries management/managed access area?") {
         
         hhs_Q34 <- selectedData()[,c("ma_name", "34_gear_restrictions")] %>%
                        filter(`34_gear_restrictions` %in% c(0,1,-1)) %>%
                           rbind(c(NA,0), c(NA,1)) %>%
                            droplevels()
         #No MA as 0 answer
         hhs_Q34$`34_gear_restrictions`[hhs_Q34$`34_gear_restrictions` == -1] <- 0
          
         Q34_summary <-
               proportion(hhs_Q34[[2]], 
                          hhs_Q34[[1]], 
                          3,2)[,-3]
         #rename
         colnames(Q34_summary) <-
            c("MA name", "N", "Proportion (%)")
         
         Q34 <- data_4plot (Q34_summary)
         
         #Plot
         plot_Q34 <-
            ggplot(Q34, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 125),
                               breaks = seq(0, 100, 25)) +
            ggtitle("Proportion of community members who are aware \nof gear restrictions in the managed access area") +
            xlab (NULL) + ylab ("Proportion (%)") + coord_flip(clip = "on")
         
         ggplotly(plot_Q34, height = 700)
         
      }
         
      ### Q36 Proportion of community members familar with fish size restricions ####
      else if (input$hhs_question == "36. Are there restrictions on the allowed fish size in the fisheries management/managed access area?") {
         
         hhs_Q36 <- selectedData()[,c("ma_name", "36_fish_size_restriction")] %>%
                        filter(`36_fish_size_restriction` != "") %>%
                           rbind(c(NA,0), c(NA,1)) %>%
                              droplevels()
         #No MA as 0 answer
         hhs_Q36$`36_fish_size_restriction`[hhs_Q36$`36_fish_size_restriction` == -1] <- 0
         
         #proportion
         Q36_summary <-
            proportion(hhs_Q36[[2]],
                       hhs_Q36[[1]],
                       3,2)[,-3]
         #rename
         colnames(Q36_summary) <-
            c("MA name", "N", "Proportion (%)")
         
         Q36 <- data_4plot (Q36_summary)

         #Plot
         plot_Q36 <-
            ggplot(Q36, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + 
            geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 125),
                               breaks = seq(0, 100, 25)) +
            ggtitle("Proportion of community members who are aware \nof fish size restrictions in the managed access area") +
            xlab (NULL) + 
            ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q36, height = 700)
         
      }
         
      ### Q37 Proportion of community members familar with specific minimum fish size restricions in MA ####
      else if (input$hhs_question == "37. Please consider a type of fish that you catch frequently. For this type of fish, what are the size restrictions?") {
         
         hhs_Q37min <- selectedData()[,c("ma_name", "37_ability_min_size")] %>%
                              rbind(c(NA,0), c(NA,1), c(NA,-1))
         hhs_Q37min$`37_ability_min_size`[hhs_Q37min$`37_ability_min_size` == -1] <- 0
         hhs_Q37min$`37_ability_min_size`[hhs_Q37min$`37_ability_min_size` == ''] <- 0
         
         
         #proportion
         Q37_summary_min <-
            proportion(hhs_Q37min[[2]],
                       hhs_Q37min[[1]],
                       3,2)[,-3]
         #rename
         colnames(Q37_summary_min) <-
            c("MA name", "N", "Min size restrictions")
         
         
         ### Q37 Proportion of community members familar with specific max fish size restricions in MA
         hhs_Q37max <- selectedData()[,c("ma_name", "37_ability_max_size")] %>%
                              rbind(c(NA,0), c(NA,1), c(NA,-1))
         hhs_Q37max$`37_ability_max_size`[hhs_Q37max$`37_ability_max_size` == -1] <- 0
         hhs_Q37max$`37_ability_max_size`[hhs_Q37max$`37_ability_max_size` == ''] <- 0
         
         #proportion
         Q37_summary_max <-
            proportion(hhs_Q37max$`37_ability_max_size`,
                       hhs_Q37max$ma_name,
                       3,2)[, -3]
         #rename
         colnames(Q37_summary_max) <-
            c("MA name", "N", "Max size restrictions")
         
         Q37_summary <- left_join (Q37_summary_min, Q37_summary_max[,-2], by = "MA name")
          
         Q37_summary_long <-
            Q37_summary %>% pivot_longer(
               cols = c(
                  "Min size restrictions",
                  "Max size restrictions",
               ),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         
         Q37 <- data_4plot(Q37_summary_long)
         
         plot_Q37 <-
            ggplot(Q37, aes(`MA name`, `Proportion (%)`, N = N)) +
            facet_wrap(~ key, 
                       ncol = 2) +
                       #labeller = label_wrap_gen(25)) +
            theme_rare + 
            geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 125),
                               breaks = seq(0, 100, 25)) +
            ggtitle("Proportion of community members who know specific \nfish size restrictions in the managed access area") +
            xlab (NULL) + ylab ("\nProportion (%)") + coord_flip(clip = "on")
         
       ggplotly(plot_Q37, height = 700)
         
       }

         
      ### Q38 Proportion of commmunity members that are aware that fishing is not allowed in the reserve ####
      else if (input$hhs_question == "38. Is fishing allowed in the reserve area?") {
     
       hhs_Q38 <- selectedData()[,c("ma_name", "38_reserve_fishing_allowed")] %>%
                     filter(`38_reserve_fishing_allowed` != "" &
                           `38_reserve_fishing_allowed` != -1) %>%
                        rbind(c(NA,0), c(NA,1)) %>%
                           droplevels()
         #proportion
         Q38_summary <-
            proportion(hhs_Q38[[2]],
                       hhs_Q38[[1]],
                       3,2)[,-4]
         #rename
         colnames(Q38_summary) <-
            c("MA name", "N", "Proportion (%)")
         #data for plot
         Q38 <- data_4plot (Q38_summary)
        
         #Plot
         plot_Q38 <-
            ggplot(Q38, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 125),
                               breaks = seq(0, 100, 25)) +
            ggtitle("Proportion of community members who are aware \nthat fishing is not allowed in the reserve area") +
            xlab (NULL) + ylab ("Proportion (%)") + coord_flip(clip = "on")
         
         ggplotly(plot_Q38, height = 700)
      }
      
      ### Q39 Proportion of fisher aware of the boundaries of the fisheries management/managed access area? ####
      else if (input$hhs_question == "39. Would you agree with the statement that most fishers in your community are aware of the boundaries of the fisheries management/managed access area?") {
         
         hhs_Q39 <- selectedData()[,c("ma_name", "39_ma_boundaries_aware")] %>%
            subset(`39_ma_boundaries_aware` %in% c("Agree", "Neither", 
                                                   "Disagree",
                                                   "Strongly agree",
                                                   "Strongly disagree")) %>%
            rbind(c(NA, "Agree"), 
                  c(NA, "Neither"), 
                  c(NA, "Disagree"), 
                  c(NA, "Strongly agree"), 
                  c(NA, "Strongly disagree")) %>%
            droplevels()
         
         #proportion
         Q39_summary <-
            proportion(hhs_Q39[[2]],
                       hhs_Q39[[1]],
                       3,5)
         #rename
         colnames(Q39_summary) <-
            c(
               "MA name",
               "N",
               "Agree",
               "Disagree",
               "Neither",
               "Strongly agree",
               "Strongly disagree"
            )
         
         #pivot table
         Q39_summary_long <-
            Q39_summary %>% pivot_longer(
               cols = c(
                  "Strongly disagree",
                  "Disagree",
                  "Neither",
                  "Agree",
                  "Strongly agree"
               ),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q39_summary_long$key <-
            factor(
               Q39_summary_long$key,
               levels = c(
                  "Strongly disagree",
                  "Disagree",
                  "Neither",
                  "Agree",
                  "Strongly agree"
               )
            )
         
         Q39 <- data_4plot (Q39_summary_long)
         
         #Plot
         plot_Q39 <-
            ggplot(Q39, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap(
               ~ key,
               labeller = label_wrap_gen(20),
               ncol = 5
            ) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of fishers aware of the boundaries \nof the fisheries management/managed access area"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q39, height = 750)
         
      }
      
      ### Q40 Proportion of fishermen in the community that are aware of the boundaries of the reserve area? ####
      else if (input$hhs_question == "40. Out of ten fishers in your community, how many would you guess know where the reserve boundary is?") {
         
         if (input$Country == "MOZ") {
            
            hhs_Q40_moz <- selectedData()[,c("ma_name", "40_reserve_boundaries_aware")] %>%
                              filter (`40_reserve_boundaries_aware` != "" &
                                      `40_reserve_boundaries_aware` != "No reserve") %>%
                                          droplevels()
          Q40_summary_moz <-
            proportion(
               question = hhs_Q40_moz[[2]],
               grouping = hhs_Q40_moz[[1]],
               rounding = 3, 
               type = 5
            )
         colnames(Q40_summary_moz) <-
            c(
               "MA name",
               "N",
               "Agree",
               "Disagree",
               "Neither",
               "Strongly agree",
               "Strongly disagree"
            )
         
         #pivot table
         Q40_summary_moz_long <-
            Q40_summary_moz %>% pivot_longer(
               cols = c(
                  "Strongly disagree",
                  "Disagree",
                  "Neither",
                  "Agree",
                  "Strongly agree"
               ),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q40_summary_moz_long$key <-
            factor(
               Q40_summary_moz_long$key,
               levels = c(
                  "Strongly disagree",
                  "Disagree",
                  "Neither",
                  "Agree",
                  "Strongly agree"
               )
            )
         
         Q40_moz <- data_4plot(Q40_summary_moz_long) 
         
         #Plot
         plot_Q40_moz <-
            ggplot(Q40_moz, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, #scale = input$x_axis,
                        labeller = label_wrap_gen(20), ncol = 5) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of community members that think that \nmost fishers are aware of the boundaries of the reserve area"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q40_moz, height = 750)
         
         }
      
      
      ### all countries except Moz
         else if (input$Country != "MOZ") {
            
         hhs_Q40 <- selectedData()[,c("ma_name", "40_reserve_boundaries_aware")] %>% 
                        filter(`40_reserve_boundaries_aware` %in% c(0:10)) %>%
                           droplevels()
         
         Q40_length <-
            tapply(hhs_Q40[[2]],
                   hhs_Q40[[1]],
                   length)
         Q40_length <- as.vector(Q40_length)
         Q40_mean <-
            data.frame(avg = tapply(as.numeric(
               as.character(hhs_Q40$`40_reserve_boundaries_aware`)
            ),
            hhs_Q40$ma_name, mean) / 10)
         Q40_summary_bind <-
            cbind(N = Q40_length, round(Q40_mean, 3) * 100)
         Q40_summary <-
            rbind(Q40_summary_bind, "Mean ± SE" = c(
               sum(Q40_summary_bind$N),
               mean_sem(Q40_summary_bind$avg, 1)
            ))
         Q40_summary <- rownames_to_column(Q40_summary, "MA name")
         colnames(Q40_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q40 <- data_4plot (Q40_summary)
         
         #Plot
         plot_Q40 <-
            ggplot(Q40, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members that think that most fishers \nare aware of the boundaries of the reserve area"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q40, height = 750)
         
         }
      }
      
      ### Q41 Who is allowed to fish in the fisheries management/managed access area? ####
      
      else if (input$hhs_question == "41. Who is allowed to fish in the fisheries management/managed access area?") {
         
         hhs_Q41 <- selectedData()[,c("ma_name", "41_ma_fishers_allowed")] %>%
                        filter(`41_ma_fishers_allowed` %in% 
                                  c("Community only",
                                    "Don't know",
                                    "No managed access",
                                    "No restrictions",
                                    "With authorization",
                                    "Without authorization")) %>%
                              rbind(c(NA, "Community only"),
                                    c(NA, "Don't know"),
                                    c(NA, "No managed access"),
                                    c(NA, "No restrictions"),
                                    c(NA, "With authorization"),
                                    c(NA, "Without authorization")) %>%
                              droplevels()
                        
         Q41_summary <- proportion (hhs_Q41[[2]],
                                    hhs_Q41[[1]],
                                    3,6)
         
         colnames(Q41_summary) <- c("MA name", "N", 
                                    "Community only",
                                    "Don't know",
                                    "No managed access",
                                    "No restrictions",
                                    "With authorization",
                                    "Without authorization")
         #pivot table
         Q41_summary_long <-
            Q41_summary %>% pivot_longer(
               cols = c(
                  "Community only",
                  "Don't know",
                  "No managed access",
                  "No restrictions",
                  "With authorization",
                  "Without authorization"
               ),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q41_summary_long$key <-
            factor(
               Q41_summary_long$key,
               levels = c(
                  "Community only",
                  "With authorization",
                  "Without authorization",
                  "No restrictions",
                  "Don't know",
                  "No managed access"
                  )
            )
         
         Q41 <- data_4plot(Q41_summary_long)
         
         #Plot
         plot_Q41 <-
            ggplot(Q41, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            theme(strip.text.x = element_text(margin = margin(0.25,0,0.25,0, "cm")))+
            facet_wrap(
               ~ key,
               labeller = label_wrap_gen(10),
               ncol = 6 ) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 30)) +
            ggtitle(
               "Proportion of households that are aware of who is allowed \nto fish in the fisheries management/managed access area?") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q41, height = 750)
         
      }
      
      
      ### Q42b Do you know how fishing with restricted gear affect the fishery? #####
      
      else if (input$hhs_question == "42. Do you know how fishing with restricted gear affect the fishery?") {
         
         hhs_Q42 <- selectedData()[,c("ma_name", "42b_problem_restricted_gear")] %>%
                        filter(`42b_problem_restricted_gear` %in% c(0,1)) %>%
                           rbind(c(NA,0), c(NA,1))
            
         Q42b_summary <- proportion( hhs_Q42[[2]],
                                     hhs_Q42[[1]],
                                     3,2)[,-3]
                                     
         colnames(Q42b_summary) <-  c("MA name", "N", "Proportion (%)")
         
         Q42b <- data_4plot (Q42b_summary)
         
            #Plot
         plot_Q42 <-
            ggplot(Q42b, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members that \nknow how fishing with restricted gear affect the fishery"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
               coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q42, height = 750)
         
      }
      
      ### Q42c Do you know how fishing undersize fish affect the fishery? #####
      else if (input$hhs_question == "42. Do you know how fishing undersize fish affect the fishery?") {
         
         hhs_Q42 <- selectedData()[,c("ma_name", "42c_problem_undersize")] %>%
                           filter (`42c_problem_undersize` %in% c(0,1)) %>%
                              rbind(c(NA,0), c(NA,1))
                                 
         Q42c_summary <- proportion(hhs_Q42[[2]],
                                    hhs_Q42[[1]],
                                    3,2)[,-3]
         
         colnames(Q42c_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q42c <- data_4plot(Q42c_summary)
         
         #Plot
         plot_Q42c <-
            ggplot(Q42c, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members that \nknow how fishing undersize fish affect the fishery"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q42c, height = 750)
         
      }
      
      ### Q42d Do you know how fishing inside the reserve affect the fishery? #####
      else if (input$hhs_question == "42. Do you know how fishing inside the reserve affect the fishery?") {
         
         hhs_Q42 <- selectedData()[,c("ma_name", "42d_problem_inside_reserve")] %>%
                        filter(`42d_problem_inside_reserve` %in% c(0,1)) %>%
                           rbind(c(NA,0), c(NA,1))
            
         Q42d_summary <- proportion(hhs_Q42[[2]],
                                    hhs_Q42[[1]],
                                    3,2)[,-3]
        
         colnames(Q42d_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q42d <- data_4plot(Q42d_summary)
         
         #Plot
         plot_Q42d <-
            ggplot(Q42d, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            #facet_wrap(~key, scale = input$x_axis, labeller = label_wrap_gen(20), ncol=5)+
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members that \nknow how fishing inside the reserve affect the fishery"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q42d, height = 750)
         
      }
      
      ### Q42e Do you know how unauthorized fishers fishing inside the managed access area affect the fishery? ####
      
      else if (input$hhs_question == "42. Do you know how unauthorized fishers fishing inside the managed access area affect the fishery?") {
         
         hhs_Q42 <- selectedData()[,c("ma_name","42e_problem_unauthorized")] %>%
                        filter(`42e_problem_unauthorized` %in% c(0,1))
             
         Q42e_summary <- proportion(hhs_Q42[[2]],
                                    hhs_Q42[[1]],
                                    3,2)[,-3]
         
         colnames(Q42e_summary) <- c("MA name", "N", "Proportion (%)")

         Q42e <- data_4plot (Q42e_summary)
         
         #Plot
         plot_Q42e <-
            ggplot(Q42e, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members that know how \nunauthorized fishers fishing inside the managed access area affect the fishery?"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q42e, height = 750)
         
      }
      
      
      ### Q43 Do you think there is any benefit to regulating fishing via a managed access area and a reserve area? ####
      else if (input$hhs_question == "43. Do you think there is any benefit to regulating fishing via a managed access area and a reserve area?") {
         
         hhs_Q43 <- selectedData()[,c("ma_name", "43_ma_benefits")] %>%
                        filter(`43_ma_benefits` %in% c(0,1)) %>%
                           rbind(c(NA,0), c(NA,1)) %>%
                              droplevels()
         
         Q43_summary <- proportion(hhs_Q43[[2]],
                                   hhs_Q43[[1]],
                                   3,2)[,-3]
         
         colnames(Q43_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q43 <- data_4plot(Q43_summary)
         
         #Plot
         plot_Q43 <-
            ggplot(Q43, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "\nProportion of community members that think there is benefit \nto regulating fishing via a managed access area and a reserve area"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly (plot_Q43, height = 750)
         
      }
      
      
      ### Q44a Proportion of community members who attend management body meetings regularly #####
      else if (input$hhs_question == "44a. Have you or anyone in the household attended a fisheries management body meeting in the last month?") {
         
         hhs_Q44 <- selectedData_Q44()[,c("ma_name", "44_meeting_attendance")] %>%
                        filter(`44_meeting_attendance` != '') %>%
                           filter (`44_meeting_attendance` != "na") %>%
                              droplevels()
         
         Q44_summary <- proportion(hhs_Q44[[2]],
                                   hhs_Q44[[1]],
                                   3, length(unique(hhs_Q44[[2]])))
         
         #pivot table
         Q44_summary_long <-
            Q44_summary %>% pivot_longer(
               cols = names(Q44_summary[-c(1:2)]),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         
         Q44_summary_long$key <- str_replace (Q44_summary_long$key, "[.]", " ")
         Q44 <- data_4plot(Q44_summary_long)
         
         #Plot
         plot_Q44 <-
            ggplot(Q44, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, ncol = 6) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of community members who \nattend management body meetings regularly"
            ) +
            xlab (NULL) + ylab (NULL) + 
               coord_flip(clip ="on")
         
         ggplotly(plot_Q44, height = 750)
      }
      
      
      ### Q44b Proportion of fishers who attend meetings regulary ########
      else if (input$hhs_question == "44b. Have a fisher in the household attended a fisheries management body meeting in the last month?") {
         
         hhs_fishers <- selectedData()[,c("submissionid","ma_name", 
                                          "12a_fishing_men", "12b_fishing_women",
                                          "12c_fishing_children")] %>%
                              filter(`12a_fishing_men` > 0 |
                                     `12b_fishing_women` > 0 | 
                                     `12c_fishing_children` > 0) %>%
                                 droplevels()
        hhs_Q44f <-
            left_join(
               hhs_fishers[, c("submissionid")],
               selectedData_Q44()[,c("submissionid",
                                     "ma_name", 
                                     "44_meeting_attendance")],
               by = "submissionid") %>%
           filter(`44_meeting_attendance` != "") %>% 
            filter(`44_meeting_attendance` != "na") %>%
               droplevels()
         
        # replace no respond and no management with NO
        #hhs_Q44f$`44_meeting_attendance`[hhs_Q44f$`44_meeting_attendance` == ""] <- "No"
        #hhs_Q44f$`44_meeting_attendance`[hhs_Q44f$`44_meeting_attendance` == "No management"] <- "No"
              
        Q44f_summary <- proportion(hhs_Q44f[[3]],
                                   hhs_Q44f[[2]],
                                   3,
                                   length(unique(hhs_Q44f[[3]])))
         
         Q44f_summary_long <-
            Q44f_summary %>% pivot_longer(
               cols = names(Q44f_summary[-c(1:2)]),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q44f_summary_long$key <- str_replace (Q44f_summary_long$key, "[.]", " ")
         Q44f <- data_4plot(Q44f_summary_long)
         
         #Plot
         plot_Q44f <-
            ggplot(Q44f, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, scale = input$x_axis, ncol = 6) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle("Proportion of fishers who \nattend management body meetings regularly") +
            xlab (NULL) + ylab (NULL) + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q44f, height = 750)
         
      }
      
      ### Q45 Proportion of women in community that hold leadership positions in the management body #####
      
      else if (input$hhs_question == "45. Do you or anyone in the household hold a leadership position in the fisheries management body?") {
         
         hhs_Q45 <- selectedData_Q45()[,c("ma_name", "45_leadership_position")] %>%
            filter(`45_leadership_position`!= "") %>%
               filter(`45_leadership_position`!= "na") %>%
                  droplevels()
          
         #hhs_Q45$`45_leadership_position`[hhs_Q45$`45_leadership_position` == "No management"] <- "No"
         #hhs_Q45$`45_leadership_position`[hhs_Q45$`45_leadership_position` == ""] <- "No"
         #hhs_Q45$`45_leadership_position`[hhs_Q45$`45_leadership_position` == "na"] <- "No"
         
         Q45_summary <- proportion(hhs_Q45[[2]],
                                    hhs_Q45[[1]],
                                    3,
                                    length(unique(hhs_Q45[[2]])))
         
         #colnames(Q45_summary) <- c("MA name", "N", "No",  "Not sure", "Yes female", "Yes male")
         #rownames to columns
         Q45_summary_long <-
            Q45_summary %>% pivot_longer(
               cols = names(Q45_summary[-c(1:2)]),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q45_summary_long$key <- str_replace (Q45_summary_long$key, "[.]", " ")
         Q45 <- data_4plot(Q45_summary_long)
         
         #Plot
         plot_Q45 <-
            ggplot(Q45, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, ncol=5) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of community members that \nhold leadership positions in the management body"
            ) +
            xlab (NULL) + ylab (NULL) + 
               coord_flip(clip ="on")
         
         ggplotly(plot_Q45, height = 750)
      }
      
      
      
      ### Q46 Proportion of community members who feel that the fisheries management body makes decisions that benefit the fishery and community #####
      else if (input$hhs_question == "46. Do you agree that the fisheries management body represents your interests to the fishery?") {
         ##Clean data
          hhs_Q46 <- selectedData()[,c("ma_name","46_represent_interests")] %>%
                        filter (`46_represent_interests` != "") %>%
                           filter (`46_represent_interests` != "na") %>%
                                droplevels()
          #recode no managemnt answers 
         #hhs_Q46$`46_represent_interests`[hhs_Q46$`46_represent_interests` == ""] <- "Disagree"
         #hhs_Q46$`46_represent_interests` <- recode_factor(hhs_Q46$`46_represent_interests`,
         #                                                   "No management" = "Disagree",
         #                                                   "na" = "Disagree")
      
         Q46_summary <- proportion (hhs_Q46[[2]],
                                    hhs_Q46[[1]],
                                    3, 
                                    type = length(unique(hhs_Q46[[2]])))
         #pivot table
         Q46_summary_long <-
            Q46_summary %>% pivot_longer(
               cols = names(Q46_summary)[-c(1:2)],
               names_to = "key",
               values_to = "Proportion (%)"
            )
        
          #Q46_summary_long$key <- factor(Q46_summary_long$key,
          #         levels = c("Disagree", "Neither", "Agree"))
         Q46_summary_long$key <- str_replace (Q46_summary_long$key, "[.]", " ")
         Q46 <- data_4plot(Q46_summary_long)
          
         #Plot
         plot_Q46 <-
            ggplot(Q46, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, ncol = 4) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of community members who agree that the \nfisheries management body represents their interests to the fishery"
            ) +
            xlab (NULL) + ylab (NULL) + 
               coord_flip(clip ="on")
         
         ggplotly(plot_Q46, height = 750)
      }
      
      
      
      ### Q47 Proportion of community members who feel that their contributions to the fishery are recognized #####
      else if (input$hhs_question == "47. Do you agree that the fisheries management body represents your contributions to the fishery?") {
         
         hhs_Q47 <- selectedData()[,c("ma_name", "47_represent_contributions")] %>%
                           rbind(tibble("ma_name" = c(NA,NA,NA),
                                        "47_represent_contributions"= c("Agree", 
                                                                        "Neither", 
                                                                        "Disagree"))) %>%
            filter(`47_represent_contributions` %in% c("Agree",  "Neither",  "Disagree")) %>%
                        droplevels()
         #proportion
         Q47_summary <- proportion(hhs_Q47[[2]],
                                   hhs_Q47[[1]],
                                   3,3)
         #pivot table
         Q47_summary_long <-
            Q47_summary %>% pivot_longer(
               cols = c("Neither", "Disagree", "Agree"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q47_summary_long$key <-
            factor(Q47_summary_long$key,
                   levels = c("Neither", "Disagree", "Agree"))
         
         Q47 <- data_4plot(Q47_summary_long)
         
         #Plot
         plot_Q47 <-
            ggplot(Q47, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, scale = input$x_axis, ncol = 4) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of community members who feel that \ntheir contributions to the fishery are recognized"
            ) +
            xlab (NULL) + ylab (NULL) + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q47, height = 750)
      }
      
      
      
      ### Q51a_c Frequency of observed use of unapproved gear, fishing in reserve, and unpermitted fishing in MA ####
      
      else if (input$hhs_question == "51a. Out of 10 fishers in your community waters, how many would you guess use gear that is not permitted?" |
               input$hhs_question == "51b. Out of 10 fishers in your community waters, how many would you guess fished in the reserve in the last month?" |
               input$hhs_question == "51c. Out of 10 fishers in the managed access area, how many would you guess do not have permission to fish there?") {
        
          ### Q51a Frequency of observed used of unapproved gear
         hhs_Q51a <- selectedData()[,c("ma_name", "51a_fishers_gear_not_permitted")] %>%
                        filter(`51a_fishers_gear_not_permitted` %in% c(0:10)) %>% 
                           droplevels()
         ## Summary 
         Q51a_length <-
            tapply(hhs_Q51a$`51a_fishers_gear_not_permitted`,
                   hhs_Q51a$ma_name,
                   length)
         Q51a_length <- as.vector(Q51a_length)
         Q51a_mean <-
            as.data.frame(
               tapply(
                  hhs_Q51a$`51a_fishers_gear_not_permitted`,
                  hhs_Q51a$ma_name,
                  mean
               ) / 10
            )
         Q51a_summary_bind <-
            cbind(N = Q51a_length, round(Q51a_mean, 3) * 100)
         colnames(Q51a_summary_bind) <- c("N", "Unapproved gear")
         Q51a_summary <- rbind(Q51a_summary_bind,
                               "Mean ± SE" = c(
                                  sum(Q51a_summary_bind$N),
                                  mean_sem(Q51a_summary_bind[[2]], 1)
                               ))
         Q51a_summary <- rownames_to_column(Q51a_summary, "MA name")
         
         ### Q51b Frequency of observed fishing in reserve ####
         hhs_Q51b <- selectedData()[,c("ma_name", "51b_fishers_reserves")] %>%
                        filter(`51b_fishers_reserves` %in% c(0:10)) %>%
                           droplevels()
         Q51b_length <-
            tapply(hhs_Q51b$`51b_fishers_reserves`,
                   hhs_Q51b$ma_name,
                   length)
         Q51b_length <- as.vector(Q51b_length)
         Q51b_mean <-
            data.frame(avg = tapply(
               hhs_Q51b$`51b_fishers_reserves`,
               hhs_Q51b$ma_name,
               mean
            ) / 10)
         Q51b_summary_bind <-
            cbind(N = Q51b_length, round(Q51b_mean, 3) * 100)
         Q51b_summary <-
            rbind(Q51b_summary_bind, "Mean ± SE" = c(
               sum(Q51b_summary_bind$N),
               mean_sem(Q51b_summary_bind$avg, 1)
            ))
         colnames(Q51b_summary) <- c("N", "Fishing in reserve")
         Q51b_summary <- rownames_to_column(Q51b_summary, "MA name")
         
         ### Q51c Frequency of observed unpermitted fishing in MA ####
         hhs_Q51c <- selectedData()[,c("ma_name", "51c_fishers_ma_area")] %>%
                        filter(`51c_fishers_ma_area` %in% c(0:10)) %>%
                           droplevels()
          Q51c_length <-
            tapply(hhs_Q51c$`51c_fishers_ma_area`,
                   hhs_Q51c$ma_name,
                   length)
         Q51c_length <- as.vector(Q51c_length)
         Q51c_mean <-
            data.frame(avg = tapply(
               hhs_Q51c$`51c_fishers_ma_area`,
               hhs_Q51c$ma_name,
               mean
            ) / 10)
         Q51c_summary_bind <-
            cbind(N = Q51c_length, round(Q51c_mean, 3) * 100)
         colnames(Q51c_summary_bind) <-
            c("N", "Fishing without permission")
         Q51c_summary <-
            rbind(Q51c_summary_bind, "Mean ± SE" = c(
               sum(Q51c_summary_bind$N),
               mean_sem(Q51c_summary_bind[[2]], 1)
            ))
         Q51c_summary <- rownames_to_column(Q51c_summary, "MA name")
         
         ### Combine Q51a, Q51b and Q51c ####
         Q51a_c_summary <-
            plyr::join_all(
               list(Q51a_summary, Q51b_summary, Q51c_summary),
               by = "MA name",
               type = "left"
            )
         
         #pivot table
         Q51a_c_summary_long <-
            as.data.frame(
               Q51a_c_summary %>% pivot_longer(
                  cols = c(
                     "Unapproved gear",
                     "Fishing in reserve",
                     "Fishing without permission"
                  ),
                  names_to = "key",
                  values_to = "Proportion (%)"
               )
            )
         #Fix Ns
         Q51a_c_summary_long$N <-
            as.data.frame(
               pivot_longer(
                  Q51a_c_summary,
                  cols = c("N", "N", "N"),
                  names_repair = "unique",
                  names_to = "No",
                  values_to = "N"
               )
            )$N
         
         Q51a_c <- data_4plot(Q51a_c_summary_long)
         
         #Plot
         plot_Q51a_c <-
            ggplot(Q51a_c, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        scale = input$x_axis,
                        labeller = label_wrap_gen(25)) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Perceived frequency of observing others violating regulations ") +
            xlab (NULL) + ylab ("Proportion (%)") + coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q51a_c, height = 750)
         
      }
      
      
      ### Q51d_e Perceived frequency of getting caught for violating regulations ####
      else if (input$hhs_question == "51d. Out of 10 fishers in your community waters who violate the regulations in the managed access area, how many would you guess get caught?" |
               input$hhs_question == "51e. Out of 10 fishers who fished in the reserve, how many would you guess get caught?") {
         
         hhs_Q51d <- selectedData()[,c("ma_name", "51d_fishers_violate_fish_size")] %>%
                        filter(`51d_fishers_violate_fish_size` %in% c(0:10)) %>%
                           droplevels()
         Q51d_length <-
            tapply(hhs_Q51d$`51d_fishers_violate_fish_size`,
                   hhs_Q51d$ma_name,
                   length)
         Q51d_length <- as.vector(Q51d_length)
         Q51d_mean <-
            data.frame(
               freq = tapply(
                  hhs_Q51d$`51d_fishers_violate_fish_size`,
                  hhs_Q51d$ma_name,
                  mean
               ) / 10
            )
         ### Proportions
         Q51d_summary_bind <-
            cbind(N = Q51d_length, round(Q51d_mean, 3) * 100)
         Q51d_summary <-
            rbind(Q51d_summary_bind, "Mean ± SE" = c(
               sum(Q51d_summary_bind$N, na.rm = TRUE),
               mean_sem(Q51d_summary_bind$freq, 1)
            ))
         colnames(Q51d_summary) <- c("N", "Fish Size Violations (%)")
         Q51d_summary <- rownames_to_column(Q51d_summary, "MA name")
         
         hhs_Q51e <- selectedData()[,c("ma_name", "51e_fishers_caught")] %>%
                        filter (`51e_fishers_caught` != "") %>%
                           droplevels()
            
         Q51e_length <-
            tapply(hhs_Q51e$`51e_fishers_caught`,
                   hhs_Q51e$ma_name,
                   length)
         Q51e_length <- as.vector(Q51e_length)
         Q51e_mean <-
            data.frame(freq = tapply(
               hhs_Q51e$`51e_fishers_caught`,
               hhs_Q51e$ma_name,
               mean
            ) / 10)
         Q51e_summary_bind <-
            cbind(N = Q51e_length, round(Q51e_mean, 3) * 100)
         Q51e_summary <-
            rbind(Q51e_summary_bind, "Mean ± SE" = c(
               sum(Q51e_summary_bind$N, na.rm = TRUE),
               mean_sem(Q51e_summary_bind$freq, 1)
            ))
         ##rownames to column
         Q51e_summary <- rownames_to_column(Q51e_summary, "MA name")
         colnames(Q51e_summary) <-
            c("MA name", "N", "Seasonal Closures Violations (%)")
         
         ### Combine Q51d and Q51e
         Q51d_51e_summary <-
            plyr::join_all(list(Q51d_summary, Q51e_summary),
                           by = "MA name",
                           type = "right")
         #pivot table
         Q51d_e_summary_long <-
            as.data.frame(
               Q51d_51e_summary %>% pivot_longer(
                  cols = c(
                     "Fish Size Violations (%)",
                     "Seasonal Closures Violations (%)"
                  ),
                  names_to = "key",
                  values_to = "Proportion (%)"
               )
            )
         #Fix Ns
         Q51d_e_summary_long$N <-
            as.data.frame(
               pivot_longer(
                  Q51d_e_summary_long,
                  cols = c("N", "N"),
                  names_repair = "unique",
                  names_to = "No",
                  values_to = "N"
               )
            )$N
         
         Q51d_e <- data_4plot(Q51d_e_summary_long)
         
         #Plot
         plot_Q51d_e <-
            ggplot(Q51d_e, aes(`MA name`,`Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        scale = input$x_axis,
                        labeller = label_wrap_gen(20)) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Perceived frequency of getting caught for violating regulations") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q51d_e, height = 750)
         
      }
      
      
      ### Q52 Proportion of participants who are confident they will continue to benefit from community management of the fishery for the next 5 years. #####
      else if (input$hhs_question == "52. Do you think the fisheries management/managed access and reserve approach will benefit the community over the next five years?") {
         
         hhs_Q52 <- selectedData()[,c("ma_name", "52_ma_benefit_5yrs")] %>%
                        filter(`52_ma_benefit_5yrs` %in% c("Yes", "Unsure", "No")) %>%
                           rbind (tibble(ma_name = c(NA,NA,NA),
                                         `52_ma_benefit_5yrs` = c("Yes", 
                                                                  "Unsure", 
                                                                  "No"))) %>%
                        droplevels()
            
         Q52_summary <- proportion(hhs_Q52[[2]],
                                    hhs_Q52[[1]],
                                   3,3)
         #pivot table
         Q52_summary_long <-
            Q52_summary %>% pivot_longer(
               cols = c("No", "Unsure", "Yes"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q52 <- data_4plot(Q52_summary_long)
         
         #Plot
         plot_Q52 <-
            ggplot(Q52, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, scale = input$x_axis) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of participants who are confident they will continue to \nbenefit from community management of the fishery for the next 5 years"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q52, height = 750)
      }
      
      ### Q53 Proportion of fishers that encourage others (both inside and outside their local community) to participate in sustainable/responsible activity####
      else if (input$hhs_question == "53. How often do you encourage others (both inside and outside their local community) to comply with fishing regulations?") {
         
         hhs_Q53 <- selectedData()[,c("ma_name", "53_encourage_regulations")] %>%
                        filter(`53_encourage_regulations` != "" &
                               `53_encourage_regulations` != 'No regulations') %>%
                        droplevels()
         
         Q53_summary <- proportion(hhs_Q53$`53_encourage_regulations`,
                                    hhs_Q53$ma_name,
                                    3,5)
         colnames(Q53_summary) <-
            c(
               "MA name",
               "N",
               "Never",
               "Rarely",
               "Sometimes",
               "Often",
               "Very often"
            )
         
         #pivot table
         Q53_summary_long <-
            as.data.frame(
               Q53_summary %>% pivot_longer(
                  cols = c(
                     "Never",
                     "Rarely",
                     "Sometimes",
                     "Often",
                     "Very often"
                  ),
                  names_to = "key",
                  values_to = "Proportion (%)"
               )
            )
         Q53_summary_long$key <-
            factor(
               Q53_summary_long$key,
               levels = c(
                  "Never",
                  "Rarely",
                  "Sometimes",
                  "Often",
                  "Very often"
               )
            )
         
         Q53 <-  data_4plot(Q53_summary_long) 
         #Plot
         plot_Q53 <-
            ggplot(Q53, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap(
               ~ key,
               scale = input$x_axis,
               labeller = label_wrap_gen(20),
               ncol = 5
            ) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 50)) +
            ggtitle(
               "Proportion of fishers that encourage others \nto participate in sustainable/responsible activity"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q53, height = 750)
      }
      
      
      
      ### Q54 "Proportion of households who think that food availability was good in the past year" #####
      
      else if (input$hhs_question == "54. How do you rate the last year in terms of food availability?") {
         hhs_Q54 <- selectedData()[,c("ma_name", "54_food_availability")] %>%
                        filter(`54_food_availability` != "") %>%
                           droplevels()
         
         Q54_summary_bind <- proportion (hhs_Q54$`54_food_availability` ,
                                     hhs_Q54$ma_name,
                                     3,5)
         Q54_summary_bind <-
            Q54_summary_bind[-dim(Q54_summary_bind)[1], ]
         Q54_summary_bind$Good_ <-
            as.numeric(Q54_summary_bind$Good) + as.numeric(Q54_summary_bind$Very.good)
         Q54_summary_bind$Bad <-
            as.numeric(Q54_summary_bind$Rather.bad) + as.numeric(Q54_summary_bind$Very.bad)
         Q54_summary_bind$OK <- as.numeric(Q54_summary_bind$OK)
         Q54_summary <-
            Q54_summary_bind[, c("MA name", "N", "Bad", "OK", "Good_")]
         colnames(Q54_summary) <-
            c("MA name", "N", "Bad", "Normal", "Good")
         #pivot
         Q54_summary_long <-
            Q54_summary %>% pivot_longer(
               cols = c(`Good`, `Bad`, `Normal`),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q54_summary_long$key <-
            factor(Q54_summary_long$key,
                   levels = c("Bad", "Normal", "Good"))
         
         Q54 <- data_4plot(Q54_summary_long)
         #Plot
         plot_Q54 <-
            ggplot(Q54, aes(x = `MA name`, y = `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, scale = input$x_axis, ncol = 3) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of households who think that \nfood availability was good in the past year"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q54, height = 750)
      }
      
      
      
      
      ### Q55 "Proportion of fisher households who often worry about not having enough food for everyone in the household" ####
      
      else if (input$hhs_question == "55. Consider the following statement: 'I worry about not having enough food for everyone in the household'. Was that often, sometimes or never true for you in the last 12 months?") {
         
         hhs_Q55 <- selectedData()[,c("ma_name", "55_worry_food")] %>%
                        filter(`55_worry_food` != "") %>% 
                           droplevels()
         
         Q55_summary <- 
            proportion(hhs_Q55$`55_worry_food`,
                        hhs_Q55$ma_name,
                        3,3
            )
         
         Q55_summary_long <-
            Q55_summary %>% pivot_longer(
               cols = c(`Never`, `Often`, `Sometimes`),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         
         Q55_summary_long$key <-
            factor(Q55_summary_long$key,
                   levels = c("Never", "Sometimes", "Often")
            )
         
         Q55 <- data_4plot(Q55_summary_long)
         
         #Plot
         plot_Q55 <-
            ggplot(Q55, aes(`MA name`, `Proportion (%)`, N = N)) +
            facet_wrap( ~ key) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            #
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of fisher households who often worry about \nnot having enough food for everyone in the household"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
                     coord_flip(clip = "on")
         
         ggplotly(plot_Q55, height = 750)
      }
      
      
      ### Q56 "Proportion of fisher households that had to reduce meal size due to not having enough food in the last 12 months" ####
      
      else if (input$hhs_question == "56. In the last 12 months, did any adults in the household ever reduce the size of meals because there wasn’t enough food to eat?") {
         
         hhs_Q56 <- selectedData()[c("ma_name", "56_reduce_meal_size_adult")] %>%
                        filter (`56_reduce_meal_size_adult` != "") %>%
                           droplevels()
         
         Q56_summary <- proportion (hhs_Q56$`56_reduce_meal_size_adult`,
                                      hhs_Q56$ma_name,
                                      3,2)[,-3]
         
         colnames(Q56_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q56 <- data_4plot(Q56_summary)
         
         #Plot
         plot_Q56 <-
            ggplot(Q56, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of fisher households that had to reduce meal size \ndue to not having enough food in the last 12 months"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q56, height = 750)
      }
      
      
      ### Q59 "Proportion of fisher households who are confident that they will be able to procure enough food for you and your family for the next 12 months" ####
      
      else if (input$hhs_question == "59. Are you confident that you will be able to procure enough food for you and your family for the next 12 months?") {
         
         hhs_Q59 <- selectedData()[,c("ma_name","59_food_procurement")] %>%
                        filter(`59_food_procurement` != "") %>%
                           rbind(c(NA, "Confident not"), 
                                 c(NA, "Uncertain"),
                                 c(NA, "High chance"),
                                 c(NA, "Very confident not"),
                                 c(NA, "not Certain")) %>%
                           droplevels()
        
         Q59_summary <- proportion (hhs_Q59$`59_food_procurement`,
                                     hhs_Q59$ma_name,
                                     3,5)
         colnames(Q59_summary) <- c("MA name", "N", 
                                    "Not very confident",
                                    "Not confident",
                                    "Uncertain",
                                    "Confident", 
                                    "Very confident")
         Q59_summary_long <-
            Q59_summary %>% pivot_longer(
               cols = c("Not very confident",
                        "Not confident",
                        "Uncertain",
                        "Confident", 
                        "Very confident"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         
         Q59_summary_long$key <-
            factor(Q59_summary_long$key,
                   levels = c("Not very confident",
                              "Not confident",
                              "Uncertain",
                              "Confident", 
                              "Very confident")
            )
         
         Q59 <- data_4plot(Q59_summary_long)
         
         #Plot
         plot_Q59 <-
            ggplot(Q59, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap(~key, ncol= 5)+
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of households who are confident that they will be able to \nprocure enough food for their family in the next 12 months"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
               coord_flip(clip = "on")
         
         ggplotly(plot_Q59, height = 750)
      }
      
      
      ### Q60 "Average frequency of household fish consumption" ####
      
      else if (input$hhs_question == "60. In the last 12 months, how often did your household eat fresh fish?") {
         
         hhs_Q60 <- selectedData()[,c("ma_name", "60_hh_fish_consumption")] %>%
                        filter(`60_hh_fish_consumption` != "") %>%
                           droplevels()
         
          Q60_summary <- proportion(hhs_Q60$`60_hh_fish_consumption`,
                                    hhs_Q60$ma_name,
                                    rounding = 3, type = 5)
          
          colnames(Q60_summary) <- c("MA name", "N", "Few",
                                     "Few per month", "Few per week",
                                     "More than few per week", "Rarely")
          
          Q60_summary_long <-
             Q60_summary %>% pivot_longer(
                cols = c("Few",
                         "Few per month", 
                         "Few per week",
                         "More than few per week", 
                         "Rarely"),
                names_to = "key",
                values_to = "Proportion (%)"
             )
          
          Q60_summary_long$key <-
             factor(Q60_summary_long$key,
                    levels = c("Rarely", 
                               "Few",
                               "Few per month", 
                               "Few per week",
                               "More than few per week")
             )
          
          Q60 <- data_4plot(Q60_summary_long)
          
         #plot
         plot_Q60 <-
            ggplot(Q60, aes(`MA name`, `Proportion (%)`, N = N)) +
            facet_wrap (~key, 
                        ncol=5, 
                        labeller = label_wrap_gen(15))+
            theme_rare + 
            geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 50)) +
            ggtitle("Proportion of households that consume fish \nmore than few times per week") +
            xlab (NULL) + ylab ("Proportion (%)") + 
               coord_flip(clip = "on")
         
         ggplotly(plot_Q60, height = 750)
      }
      
      
      ### Q61a Proportion of community members who feel that current fishing regulations are effective at managing the fishery and at ensuring catches remain stable ####
      else if (input$hhs_question == "61a. Please state your level of agreement with the following statement: Current fishing regulations are effective at ensuring catches remain stable or improve.") {
         
         hhs_Q61a <- selectedData()[,c("ma_name", "61a_current_regulations")] %>%
                           filter(`61a_current_regulations` %in% c(1:5)) %>%
                              rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
        
         Q61a_summary <- proportion (hhs_Q61a$`61a_current_regulations`,
                                      hhs_Q61a$ma_name,
                                      3,5)
         
         Q61a_summary_grouped <- Q61a_summary %>%
                                   filter (`MA name` != "Mean ± SE")
         #grouped
         Q61a_summary_grouped$Disagree <-  as.numeric(Q61a_summary_grouped$X1) +
                                           as.numeric(Q61a_summary_grouped$X2)
         
         Q61a_summary_grouped$Neither <-  
            as.numeric(Q61a_summary_grouped$X3) 
         
         Q61a_summary_grouped$Agree <-  as.numeric(Q61a_summary_grouped$X4) +
                                        as.numeric(Q61a_summary_grouped$X5)
         
         Q61a_summary <-
            rbind(
               Q61a_summary_grouped[,c("MA name", "N", 
                                       "Disagree",
                                       "Neither",
                                       "Agree")],
                c(NA,
                  sum(as.numeric(Q61a_summary_grouped$N)),
                  mean_sem(Q61a_summary_grouped$Disagree, 1),
                  mean_sem(Q61a_summary_grouped$Neither, 1),
                  mean_sem(Q61a_summary_grouped$Agree, 1)
               )
            )
         
         colnames(Q61a_summary) <-
            c("MA name",
              "N",
              "Disagree (%)",
              "Neither (%)",
              "Agree (%)")
         #pivot table
         Q61a_summary_long <-
            Q61a_summary %>% pivot_longer(
               cols = c(
                  "Disagree (%)",
                  "Neither (%)",
                  "Agree (%)"
               ),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q61a_summary_long$key <-
            factor(
               Q61a_summary_long$key,
               levels = c(
                  "Disagree (%)",
                  "Neither (%)",
                  "Agree (%)"
               )
            )
         
         Q61a <- data_4plot(Q61a_summary_long)
            
         #Plot
         plot_Q61a <-
            ggplot(Q61a, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        labeller = label_wrap_gen(20)) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of community members who feel that current fishing regulations \nare effective at managing the fishery and at ensuring catches remain stable"
            ) +
            xlab (NULL) + ylab ("\nProportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q61a, height = 750)
      }
      
      ### Q61b Proportion of fishers who believe that registering and recording will help to maintain or improve fish catch ####
      else if (input$hhs_question == "61b. Please state your level of agreement with the following statement: Registering and recording fish catch will help to maintain or improve fish catch") {
         
         hhs_Q61b <- selectedData()[,c("ma_name", "61b_catch_recording")] %>%
                        filter( `61b_catch_recording` %in% c(1:5)) %>%
                           rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
                                       droplevels()
      
         Q61b_summary <- proportion(hhs_Q61b$`61b_catch_recording`,
                                 hhs_Q61b$ma_name,
                                 3,5)
      
         colnames(Q61b_summary) <-
            c( "MA name",
               "N",
               "Strongly disagree",
               "Disagree",
               "Neither agree nor disagree (%)",
               "Agree",
               "Strongly agree"
            )
         
         Q61b_summary_grouped <- Q61b_summary %>%
                                    filter(`MA name` != "Mean ± SE")
         
         Q61b_summary_grouped$`Disagree (%)` <- as.numeric(Q61b_summary_grouped$Disagree) +
                                           as.numeric(Q61b_summary_grouped$`Strongly disagree`)
         
         Q61b_summary_grouped$`Neither agree nor disagree (%)` <- 
                  as.numeric(Q61b_summary_grouped$`Neither agree nor disagree`)
            
         Q61b_summary_grouped$`Agree (%)`<- as.numeric(Q61b_summary_grouped$Agree) +
                                          as.numeric(Q61b_summary_grouped$`Strongly agree`)
         
         #pivot table
         Q61b_summary_long <-
            as.data.frame(
               Q61b_summary_grouped [, c("MA name", "N", 
                                         "Disagree (%)", 
                                         "Neither agree nor disagree (%)",
                                         "Agree (%)")] %>% 
                  pivot_longer(
                     cols = c(
                        "Disagree (%)",
                        "Neither agree nor disagree (%)",
                        "Agree (%)"
                     ),
                     names_to = "key",
                     values_to = "Proportion (%)"
                  )
               )
         Q61b_summary_long$key <-
            factor(
               Q61b_summary_long$key,
               levels = c(
                  "Disagree (%)",
                  "Neither agree nor disagree (%)",
                  "Agree (%)"
               )
            )
         Q61b <- data_4plot(Q61b_summary_long)
         
         #Plot
         plot_Q61b <-
            ggplot(Q61b, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        scale = input$x_axis,
                        labeller = label_wrap_gen(25)) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "\nProportion of fishers who believe that registering and recording \nwill help to maintain or improve fish catch"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q61b, height =750)
      }
      
      
      
      ### Q61c Proportion of fishers who believe that participation in management will help to maintain or improve fish catch####
      else if (input$hhs_question == "61c. Please state your level of agreement with the following statement: Local community participation in management will help to maintain or improve fish catch") {
         
         hhs_Q61c <- selectedData()[,c("ma_name", "61c_community_participation")] %>%
                        filter (`61c_community_participation` %in% c(1:5)) %>%
                           rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
         
         Q61c_summary <- proportion (hhs_Q61c$`61c_community_participation`,
                                     hhs_Q61c$ma_name,
                                     3,5)
         
         colnames(Q61c_summary) <- c("MA name",
                                      "N",
                                      "Strongly disagree",
                                      "Disagree",
                                      "Neither agree nor disagree (%)",
                                      "Agree",
                                      "Strongly agree")
         
         Q61c_summary_grouped <- Q61c_summary %>%
                                       filter (`MA name` != "Mean ± SE")
          
         Q61c_summary_grouped$`Agree (%)` <- as.numeric(Q61c_summary_grouped$Agree) + 
                                     as.numeric(Q61c_summary_grouped$`Strongly agree`)
         
         Q61c_summary_grouped$`Neither agree nor disagree (%)` <- 
                                    as.numeric(Q61c_summary_grouped$`Neither agree nor disagree`)
         
         Q61c_summary_grouped$`Disagree (%)` <- as.numeric(Q61c_summary_grouped$Disagree) + 
                                 as.numeric(Q61c_summary_grouped$`Strongly disagree`)
          #pivot table
         Q61c_summary_long <-
            as.data.frame(
               Q61c_summary_grouped %>% 
                  pivot_longer(
                     cols = c(
                        "Disagree (%)",
                        "Neither agree nor disagree (%)",
                        "Agree (%)"
                     ),
                     names_to = "key",
                     values_to = "Proportion (%)"
                  )
               )
         Q61c_summary_long$key <-
            factor(
               Q61c_summary_long$key,
               levels = c(
                  "Disagree (%)",
                  "Neither agree nor disagree (%)",
                  "Agree (%)"
               )
            )
         Q61c <- data_4plot(Q61c_summary_long)
         #Plot
         plot_Q61c <-
            ggplot(Q61c, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        labeller = label_wrap_gen(25)) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of fishers who believe that participation in management \nwill help to maintain or improve fish catch"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q61c, height = 750)
      }
      
      
      
      ### Q61d Proportion of fishers who believe that is important to have a strong enforcement system ####
      else if (input$hhs_question == "61d. Please state your level of agreement with the following statement: It is important to have a strong enforcement system") {
        
          hhs_Q61d <- selectedData()[,c("ma_name", "61d_strong_enforcement")] %>%
                           filter (`61d_strong_enforcement` %in% c(1:5)) %>%
                              rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
          
          Q61d_summary <- proportion ( hhs_Q61d$`61d_strong_enforcement`,
                                       hhs_Q61d$ma_name,
                                       3,5)
         
           colnames(Q61d_summary) <- c("MA name",
                                      "N",
                                      "Strongly disagree",
                                      "Disagree",
                                      "Neither agree nor disagree (%)",
                                      "Agree",
                                      "Strongly agree")
          
          Q61d_summary_grouped <- Q61d_summary %>%
                                       filter(`MA name` != "Mean ± SE")
          
          Q61d_summary_grouped$`Agree (%)` <- as.numeric(Q61d_summary_grouped$Agree) + 
                                             as.numeric(Q61d_summary_grouped$`Strongly agree`)
          
          Q61d_summary_grouped$`Neither agree nor disagree (%)` <- 
             as.numeric(Q61d_summary_grouped$`Neither agree nor disagree`)
          
          Q61d_summary_grouped$`Disagree (%)` <- as.numeric(Q61d_summary_grouped$Disagree) + 
             as.numeric(Q61d_summary_grouped$`Strongly disagree`)
          
         #pivot table
         Q61d_summary_long <-
            as.data.frame(
               Q61d_summary_grouped[, c("MA name", "N",
                               "Disagree (%)",
                               "Neither agree nor disagree (%)",
                               "Agree (%)" )] %>% 
                  pivot_longer(
                     cols = c(
                        "Disagree (%)",
                        "Neither agree nor disagree (%)",
                        "Agree (%)"
                     ),
                     names_to = "key",
                     values_to = "Proportion (%)"
                  )
               )
         Q61d_summary_long$key <-
            factor(
               Q61d_summary_long$key,
               levels = c(
                  "Disagree (%)",
                  "Neither agree nor disagree (%)",
                  "Agree (%)"
               )
            )
         Q61d <- data_4plot(Q61d_summary_long)
         
         #Plot
         plot_Q61d <-
            ggplot(Q61d, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        scale = input$x_axis,
                        labeller = label_wrap_gen(25)) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of fishers who believe that \nit is important to have a strong enforcement system"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q61d, height = 750)
      }
      
      
      
      ### Q61e Proportion of fishers who believe that access rights have been distributed fairly ####
      else if (input$hhs_question == "61e. Please state your level of agreement with the following statement: Access rights to the managed access area have been distributed fairly to fishers") {
         
         hhs_Q61f <- selectedData()[,c("ma_name", "61f_rights_distribution_fair")] %>%
                           filter(`61f_rights_distribution_fair` %in% c(1:5)) %>%
                              rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
          
         Q61f_summary <- proportion ( hhs_Q61f$`61f_rights_distribution_fair`,
                                      hhs_Q61f$ma_name,
                                      3,5)
         
         colnames(Q61f_summary) <-
            c( "MA name",
               "N",
               "Strongly disagree",
               "Disagree",
               "Neither agree nor disagree",
               "Agree",
               "Strongly agree"
            )
         
         Q61f_summary_grouped <- Q61f_summary %>% 
                                          filter (`MA name` != "Mean ± SE")
            
         Q61f_summary_grouped$`Agree (%)` <- as.numeric(Q61f_summary_grouped$Agree) + 
            as.numeric(Q61f_summary_grouped$`Strongly agree`)
         
         Q61f_summary_grouped$`Neither agree nor disagree (%)` <- 
            as.numeric(Q61f_summary_grouped$`Neither agree nor disagree`)
         
         Q61f_summary_grouped$`Disagree (%)` <- as.numeric(Q61f_summary_grouped$Disagree) + 
            as.numeric(Q61f_summary_grouped$`Strongly disagree`)
         
         Q61f_summary <- Q61f_summary_grouped[,c("MA name", "N", 
                                                 "Disagree (%)",
                                                 "Neither agree nor disagree (%)",
                                                 "Agree (%)" )]
          #pivot table
         Q61f_summary_long <-
            as.data.frame(
               Q61f_summary_grouped[, c("MA name", "N",
                                        "Disagree (%)",
                                        "Neither agree nor disagree (%)",
                                        "Agree (%)" )] %>% 
                  pivot_longer(
                     cols = c(
                        "Disagree (%)",
                        "Neither agree nor disagree (%)",
                        "Agree (%)"
                     ),
                     names_to = "key",
                     values_to = "Proportion (%)"
                  )
            )
         Q61f_summary_long$key <-
            factor(
               Q61f_summary_long$key,
               levels = c(
                  "Disagree (%)",
                  "Neither agree nor disagree (%)",
                  "Agree (%)"
               )
            )
         
         Q61f <- data_4plot(Q61f_summary_long)
         
         #Plot
         plot_Q61f <-
            ggplot(Q61f, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        scale = input$x_axis,
                        labeller = label_wrap_gen(20)) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of fishers who believe that \naccess rights have been distributed fairly"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q61f, height = 750)
      }
      
      
      ### Q61f Willing to change my fishing behavior ####
      else if (input$hhs_question == "61f. Please state your level of agreement with the following statement: I am willing to change my fishing behavior") {
        
          hhs_Q61g <- selectedData()[,c("ma_name","61g_fishing_change_behavior")] %>%
                           filter( `61g_fishing_change_behavior` %in% c(1:5)) %>%
                              rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
         
         Q61g_summary <- proportion (hhs_Q61g$`61g_fishing_change_behavior`,
                                      hhs_Q61g$ma_name,
                                      3,5 )
         
         colnames(Q61g_summary) <-
            c( "MA name",
               "N",
               "Strongly disagree",
               "Disagree",
               "Neither agree nor disagree",
               "Agree",
               "Strongly agree"
            )
         
         Q61g_summary_grouped <- Q61g_summary %>% 
            filter (`MA name` != "Mean ± SE")
         
         Q61g_summary_grouped$`Agree (%)` <- as.numeric(Q61g_summary_grouped$Agree) + 
            as.numeric(Q61g_summary_grouped$`Strongly agree`)
         
         Q61g_summary_grouped$`Neither agree nor disagree (%)` <- 
            as.numeric(Q61g_summary_grouped$`Neither agree nor disagree`)
         
         Q61g_summary_grouped$`Disagree (%)` <- as.numeric(Q61g_summary_grouped$Disagree) + 
            as.numeric(Q61g_summary_grouped$`Strongly disagree`)
         
         #pivot table
         Q61g_summary_long <-
            as.data.frame(
               Q61g_summary_grouped[, c("MA name", "N",
                                        "Disagree (%)",
                                        "Neither agree nor disagree (%)",
                                        "Agree (%)" )] %>% 
                  pivot_longer(
                     cols = c(
                        "Disagree (%)",
                        "Neither agree nor disagree (%)",
                        "Agree (%)"
                     ),
                     names_to = "key",
                     values_to = "Proportion (%)"
                  )
            )
         Q61g_summary_long$key <-
            factor(
               Q61g_summary_long$key,
               levels = c(
                  "Disagree (%)",
                  "Neither agree nor disagree (%)",
                  "Agree (%)"
               )
            )
         
         Q61g <- data_4plot(Q61g_summary_long)
         
         #Plot
         plot_Q61g <-
            ggplot(Q61g, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        scale = input$x_axis,
                        labeller = label_wrap_gen(20)) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members who are \nwilling to change their individual fishing behavior"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q61g, height = 750)
      }
      
      
      ### Q61g Through my individual fishing behavior, I can make a meaningful contribution to the sustainability of the fish catch. #####
      else if (input$hhs_question == "61g. Please state your level of agreement with the following statement: Through my individual fishing behavior, I can make a meaningful contribution to the sustainability of the fishery") {
         
         hhs_Q61h <- selectedData()[,c("ma_name","61h_individual_behavior")] %>%
                           filter (`61h_individual_behavior` %in% c(1:5)) %>%
                              rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
            
         Q61h_summary <- proportion(hhs_Q61h$`61h_individual_behavior`,
                                     hhs_Q61h$ma_name,
                                     rounding = 3,
                                     type = 5)
                                      
          colnames(Q61h_summary) <-
            c( "MA name",
               "N",
               "Strongly disagree",
               "Disagree",
               "Neither agree nor disagree",
               "Agree",
               "Strongly agree"
            )
         
         Q61h_summary_grouped <- Q61h_summary %>% 
            filter (`MA name` != "Mean ± SE")
         
         Q61h_summary_grouped$`Agree (%)` <- as.numeric(Q61h_summary_grouped$Agree) + 
            as.numeric(Q61h_summary_grouped$`Strongly agree`)
         
         Q61h_summary_grouped$`Neither agree nor disagree (%)` <- 
            as.numeric(Q61h_summary_grouped$`Neither agree nor disagree`)
         
         Q61h_summary_grouped$`Disagree (%)` <- as.numeric(Q61h_summary_grouped$Disagree) + 
            as.numeric(Q61h_summary_grouped$`Strongly disagree`)
         
         #pivot table
         Q61h_summary_long <-
            as.data.frame(
               Q61h_summary_grouped[, c("MA name", "N",
                                        "Disagree (%)",
                                        "Neither agree nor disagree (%)",
                                        "Agree (%)" )] %>% 
                  pivot_longer(
                     cols = c(
                        "Disagree (%)",
                        "Neither agree nor disagree (%)",
                        "Agree (%)"
                     ),
                     names_to = "key",
                     values_to = "Proportion (%)"
                  )
            )
         Q61h_summary_long$key <-
            factor(
               Q61h_summary_long$key,
               levels = c(
                  "Disagree (%)",
                  "Neither agree nor disagree (%)",
                  "Agree (%)"
               )
            )
         
         Q61h <- data_4plot(Q61h_summary_long)
         
         #Plot
         plot_Q61h <-
            ggplot(Q61h, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        scale = input$x_axis,
                        labeller = label_wrap_gen(20)) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members who feel that, \nthrough their individual fishing behavior, \ncan make a meaningful contribution to the sustainability of the fishery"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q61h, height = 750)
         
      }
      
      ### Q61h It is important for me to be able to help my neighbors in times of need ####
      else if (input$hhs_question == "61h. Please state your level of agreement with the following statement: It is important for me to be able to help my neighbors in times of need") {
         ## Exclude 0s and NA
         hhs_Q61i <- selectedData()[,c("ma_name", "61i_help_neighbors")] %>%
                        filter(`61i_help_neighbors` %in% c(1:5)) %>%
                           rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
         
         Q61i_summary <- proportion(hhs_Q61i$`61i_help_neighbors`,
                                    hhs_Q61i$ma_name,
                                    rounding = 3,
                                    type = 5)
         
         colnames(Q61i_summary) <-
            c( "MA name",
               "N",
               "Strongly disagree",
               "Disagree",
               "Neither agree nor disagree",
               "Agree",
               "Strongly agree"
            )
         
         Q61i_summary_grouped <- Q61i_summary %>% 
            filter (`MA name` != "Mean ± SE")
         
         Q61i_summary_grouped$`Agree (%)` <- as.numeric(Q61i_summary_grouped$Agree) + 
            as.numeric(Q61i_summary_grouped$`Strongly agree`)
         
         Q61i_summary_grouped$`Neither agree nor disagree (%)` <- 
            as.numeric(Q61i_summary_grouped$`Neither agree nor disagree`)
         
         Q61i_summary_grouped$`Disagree (%)` <- as.numeric(Q61i_summary_grouped$Disagree) + 
            as.numeric(Q61i_summary_grouped$`Strongly disagree`)
         
         #pivot table
         Q61i_summary_long <-
            as.data.frame(
               Q61i_summary_grouped[, c("MA name", "N",
                                        "Disagree (%)",
                                        "Neither agree nor disagree (%)",
                                        "Agree (%)" )] %>% 
                  pivot_longer(
                     cols = c(
                        "Disagree (%)",
                        "Neither agree nor disagree (%)",
                        "Agree (%)"
                     ),
                     names_to = "key",
                     values_to = "Proportion (%)"
                  )
            )
         Q61i_summary_long$key <-
            factor(
               Q61i_summary_long$key,
               levels = c(
                  "Disagree (%)",
                  "Neither agree nor disagree (%)",
                  "Agree (%)"
               )
            )
         
         Q61i <- data_4plot(Q61i_summary_long)
         
         #Plot
         plot_Q61i <-
            ggplot(Q61i, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        scale = input$x_axis,
                        labeller = label_wrap_gen(20)) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members who feel that it is important for them \nto be able to help their neighbors in times of need"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q61i, height = 750)
         
      }
      
      
      ### Q63 Some people fish in the reserve and some people do not. In the past month, have you fished in the reserve? ######
      
      else if (input$hhs_question == "63. Some people fish in the reserve and some people do not. In the past month, have you fished in the reserve?") {
         
         hhs_Q63 <- selectedData()[,c("ma_name", "63_fishing_in_reserve")] %>%
                        filter(`63_fishing_in_reserve` %in% c(0,1))
         
         Q63_summary <- proportion ( hhs_Q63$`63_fishing_in_reserve`,
                                     hhs_Q63$ma_name,
                                     3,2)
         
         Q63_summary[Q63_summary == "NaN"] <- 0
         
         colnames(Q63_summary) <- c("MA name", "N", "No", "Yes")
         
         Q63 <- Q63_summary[,c("MA name", "N", "Yes")] %>% 
                        filter(`MA name` != "Mean ± SE")
         
         colnames(Q63) <- c("MA name", "N", "Proportion (%)")
         
         Q63 <- data_4plot(Q63)
         
        #plot
         plot_Q63 <-
            ggplot(Q63, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of fishers that have fished \nin the reserve in the past month") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q63, height = 750)
      }
      
      
      ### Q64 How wrong would it be for someone to fish in the reserve? ####
      else if (input$hhs_question == "64. How wrong would it be for someone to fish in the reserve?") {
         
         hhs_Q64 <- selectedData()[,c("ma_name", "64_wrong_fishing_reserve")] %>%
                        filter (`64_wrong_fishing_reserve` != "") %>%
                           droplevels()
         
         hhs_Q64$`64_wrong_fishing_reserve` <- 
               dplyr::recode_factor(
                  hhs_Q64$`64_wrong_fishing_reserve`,
                        "1. Nada malo" = "not at all",
                        "Un poquito malo" = "slightly",
                        "3. Moderadamente malo" = "moderately",
                        "4. Muy malo" = "very wrong",
                        "5. Extremadamente malo" = "extremely wrong"
                  )
                                                                    
         Q64_summary <- proportion (hhs_Q64$`64_wrong_fishing_reserve`,
                                    hhs_Q64$ma_name,
                                    3,5)
         colnames(Q64_summary) <-
            c("MA name",
               "N",
               "Not wrong at all",
               "Slightly wrong",
               "Moderately wrong",
               "Very wrong",
               "Extremely wrong"
              )
         
         Q64_summary_long <-
            as.data.frame(
               Q64_summary %>% 
                  pivot_longer(
                     cols = c(
                        "Not wrong at all",
                        "Slightly wrong",
                        "Moderately wrong",
                        "Very wrong",
                        "Extremely wrong"
                     ),
                     names_to = "key",
                     values_to = "Proportion (%)"
                  )
               )
         
         Q64_summary_long$key <-
            factor(
               Q64_summary_long$key,
               levels = c(
                  "Not wrong at all",
                  "Slightly wrong",
                  "Moderately wrong",
                  "Very wrong",
                  "Extremely wrong"
               )
            )
         
         Q64 <- data_4plot(Q64_summary_long)
         
         #plot
         plot_Q64 <-
            ggplot(Q64, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare +
            geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap(
               ~ key,
               scale = input$x_axis,
               labeller = label_wrap_gen(25),
               ncol = 5
            ) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of community members who think is wrong to fish in the reserve") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q64, height = 750)
         
         
      }
      ### Q65 Out of ten members of your community, how many would you guess believe it is wrong to fish in the reserve? ####
      else if (input$hhs_question == "65. Out of ten members of your community, how many would you guess believe it is wrong to fish in the reserve?") {
        
          hhs_Q65 <- selectedData()[,c("ma_name", "65_no_wrong_fishing_reserve")] %>%
                        dplyr::filter(`65_no_wrong_fishing_reserve` %in% c(0:10))
          
         Q65_length <-
            tapply(hhs_Q65$`65_no_wrong_fishing_reserve`,
                   hhs_Q65$ma_name,
                   length)
         Q65_length <- as.vector(Q65_length)
         Q65_mean <-
            data.frame(avg = tapply(as.numeric(
               as.character(hhs_Q65$`65_no_wrong_fishing_reserve`)
            ),
            hhs_Q65$ma_name, mean) / 10)
         Q65_summary_bind <- cbind(N = Q65_length, round(Q65_mean, 3) * 100)
         Q65_summary <-
            rbind(Q65_summary_bind, "Mean ± SE" = c(
               sum(Q65_summary_bind$N),
               mean_sem(Q65_summary_bind$avg, 1)
            ))
         colnames(Q65_summary) <- c("N", "Proportion (%)")
         Q65_summary <- rownames_to_column(Q65_summary, "MA name")
         
         Q65 <- data_4plot(Q65_summary)
         
         #plot
         plot_Q65 <-
            ggplot(Q65, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of community members who think \nis wrong to fish in the reserve") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q65, height = 750)
      }
      
      ### Q67 Do you know where the boundary if the reserve is? #####
      
      else if (input$hhs_question == "67. Do you know where the boundary of the reserve is?") {
        
          hhs_Q67 <- selectedData()[,c("ma_name", "67_reserve_boundry")] %>%
                           filter(`67_reserve_boundry` != "")
          
          Q67_summary <- proportion( hhs_Q67$`67_reserve_boundry`,
                                     hhs_Q67$ma_name,
                                     3,2)[,-3]
          
          Q67_summary[Q67_summary == "NaN"] <- 0
          colnames(Q67_summary) <- c("MA name", "N", "Proportion (%)")
          
          Q67 <- data_4plot(Q67_summary)
         
                   #plot
         plot_Q67 <-
            ggplot(Q67, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of community members who \nare aware of the reserve boundary") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q67, height = 750)
      }
      
      
      ### Q68 "Proportion of catch used for sustenance" ####
      
      else if (input$hhs_question == "68. Based on your recollection, enter total number of kilograms of fish sold and eaten (consumed) in your household per week in the last 6 months...") {
         
         hhs_Q68 <- selectedData()[,c("ma_name", "68_fish_eaten", "68_fish_sold")] %>%
                        dplyr::filter(!is.na(`68_fish_sold`)) %>%
                              filter(`68_fish_eaten` < 800000)
         Q68_fish_eaten <-
            tapply(hhs_Q68$`68_fish_eaten`, hhs_Q68$ma_name, mean, na.rm = TRUE)
         Q68_fish_sold <-
            tapply(hhs_Q68$`68_fish_sold`, hhs_Q68$ma_name, mean, na.rm = TRUE)
         Q68_summary_bind <-
            as.data.frame(cbind(as.vector(summary(
               hhs_Q68$ma_name
            )),
            round(
               Q68_fish_sold / (Q68_fish_eaten + Q68_fish_sold), 3
            ) * 100))
         colnames(Q68_summary_bind) <- c("N", "Fish for subsistence")
         Q68_summary <-
            rbind(Q68_summary_bind, "Mean ± SE" = c(
               sum(Q68_summary_bind$N),
               mean_sem(Q68_summary_bind$`Fish for subsistence`, 1)
            ))
         
         Q68_summary <- rownames_to_column(Q68_summary_bind, "MA name")
         colnames(Q68_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q68 <- data_4plot(Q68_summary)
         
         #Plot
         plot_Q68 <-
            ggplot(Q68, aes(x = `MA name`, y = `Proportion (%)`, N = N)) +
            theme_rare + 
            geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of catch sold versus eaten in the household") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q68, height = 750)
      }
      
      ### Q70 "Household income coming from all activities" ####
      
      else if (input$hhs_question == "70a. What is your households’ average monthly income from all activities, including salaried and non-salaried labor?") {
         ### Currency conversions (USD)
         if (input$Country == "IDN") {
            conversion <- quantmod::getQuote("IDRUSD=X")$Last
         }
         else if (input$Country == "HND") {
            conversion <- quantmod::getQuote("HNLUSD=X")$Last
         }
         else if (input$Country == "PHL") {
            conversion <- quantmod::getQuote("PHPUSD=X")$Last
         }
         else if (input$Country == "MOZ") {
            conversion <- quantmod::getQuote("MZNUSD=X")$Last
         }
         else if (input$Country == "BRA") {
            conversion <- quantmod::getQuote("BRLUSD=X")$Last
         }
         
         ### Q70 What is your households' average monthly income from all activities, including salaried and non-salaried labor? ####
         
         hhs_Q70 <- selectedData() %>%
                        filter (between(
                           as.numeric(`70_hh_average_income`), 10, 1.00e+09)) %>%
                              droplevels()
         Q70_lenght <-
            tapply(hhs_Q70$`70_hh_average_income`,
                   hhs_Q70$ma_name,
                   length)
         
         #Proportion of income and Average income per MA
         income_source <-
            hhs_Q70[, c(
               "ma_name",
               "11a_income_farming",
               "11b_income_harvesting",
               "11c_income_fishing_artisanal",
               "11d_income_fishing_industrial",
               "11e_income_buying_trading",
               "11f_income_processing",
               "11g_income_aquaculture",
               "11h_income_extraction",
               "11i_income_tourism",
               "11k_income_other",
               "70_hh_average_income"
            )]
         
         income_source[is.na(income_source)] <- 0
         income_source <- data.frame(income_source)
         ## Agregate income
         income_summary <-
            aggregate(. ~ ma_name,
                      FUN = mean,
                      na.rm = TRUE,
                      data = income_source)
         
         HH_avg_income <- data.frame (
            N = Q70_lenght,
            Total_Income_USD = round(income_summary$X70_hh_average_income, 1),
            Farming = round(income_summary$X11a_income_farming, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Harvesting = round(income_summary$X11b_income_harvesting, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Artisinal_Fishing = round(income_summary$X11c_income_fishing_artisanal, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Industrial_Fishing = round(income_summary$X11d_income_fishing_industrial, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Buying_Trading = round(income_summary$X11e_income_buying_trading, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Processing_Fish = round(income_summary$X11f_income_processing, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Aquaculture = round(income_summary$X11g_income_aquaculture, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Extraction = round(income_summary$X11h_income_extraction, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Tourism = round(income_summary$X11i_income_tourism, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Other = round(income_summary$X11k_income_other, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100
         )
         
         HH_avg_income_mean <-
            rbind(
               HH_avg_income,
               "Mean ± SE" = c(
                  sum(HH_avg_income$N),
                  mean_sem(HH_avg_income$Total_Income_USD, 1),
                  mean_sem(HH_avg_income$Farming, 1),
                  mean_sem(HH_avg_income$Harvesting, 1),
                  mean_sem(HH_avg_income$Artisinal_Fishing, 1),
                  mean_sem(HH_avg_income$Industrial_Fishing, 1),
                  mean_sem(HH_avg_income$Buying_Trading, 1),
                  mean_sem(HH_avg_income$Processing_Fish, 1),
                  mean_sem(HH_avg_income$Aquaculture, 1),
                  mean_sem(HH_avg_income$Extraction, 1),
                  mean_sem(HH_avg_income$Tourism, 1),
                  mean_sem(HH_avg_income$Other, 1)
               )
            )
         
         Q70_summary <-
            rownames_to_column(HH_avg_income_mean[, c(1:2)], "MA name")
         colnames(Q70_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q70 <- data_4plot (Q70_summary)
         
         colnames(Q70) <- c("MA name", "N", "Average")
         Q70$Average <- round(Q70$Average * conversion, 1)
         
         #Plot
         plot_Q70 <-
            ggplot(Q70, aes(`MA name`, Average, N = N)) +
            theme_rare + geom_col(alpha = 0.8, fill = "#005BBB") +
            
            ggtitle("Average household income in USD ") +
            xlab (NULL) + ylab ("Average income in USD") +
            coord_flip(clip = "on")
         
         ggplotly(plot_Q70, height=750)
      }
      
      
      ### Q70 "Household income coming from all fishing related activities" ####
      
      else if (input$hhs_question == "70b. What is your households’ average monthly income from all fishing related activities?") {
         ### Currency conversions (USD)
         library(quantmod)
         if (input$Country == "IDN") {
            conversion <- getQuote("IDRUSD=X")$Last
         }
         else if (input$Country == "HND") {
            conversion <- getQuote("HNLUSD=X")$Last
         }
         else if (input$Country == "PHL") {
            conversion <- getQuote("PHPUSD=X")$Last
         }
         else if (input$Country == "MOZ") {
            conversion <- getQuote("MZNUSD=X")$Last
         }
         else if (input$Country == "BRA") {
            conversion <- getQuote("BRLUSD=X")$Last
         }
         else if (input$Country == "FSM") {
            conversion <- getQuote("USDUSD=X")$Last
         }
         
         
         hhs_Q70 <- hhs_moz %>%
            filter (between(
               as.numeric(`70_hh_average_income`), 10, 1.00e+09)) %>%
                  droplevels()
         
         Q70_lenght <-
            tapply(hhs_Q70$`70_hh_average_income`,
                   hhs_Q70$ma_name,
                   length)
         
         #Proportion of income and Average income per MA
         income_source <-
            hhs_Q70[, c(
               "ma_name",
               "11a_income_farming",
               "11b_income_harvesting",
               "11c_income_fishing_artisanal",
               "11d_income_fishing_industrial",
               "11e_income_buying_trading",
               "11f_income_processing",
               "11g_income_aquaculture",
               "11h_income_extraction",
               "11i_income_tourism",
               "11k_income_other",
               "70_hh_average_income"
            )]
         
         income_source[is.na(income_source)] <- 0
         income_source <- data.frame(income_source)
         ## Agregate income
         income_summary <-
            aggregate(. ~ ma_name,
                      FUN = mean,
                      na.rm = TRUE,
                      data = income_source)
         
         HH_avg_income <- data.frame (
            N = Q70_lenght,
            Total_Income_USD = round(income_summary$X70_hh_average_income, 1),
            Farming = round(income_summary$X11a_income_farming, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Harvesting = round(income_summary$X11b_income_harvesting, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Artisinal_Fishing = round(income_summary$X11c_income_fishing_artisanal, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Industrial_Fishing = round(income_summary$X11d_income_fishing_industrial, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Buying_Trading = round(income_summary$X11e_income_buying_trading, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Processing_Fish = round(income_summary$X11f_income_processing, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Aquaculture = round(income_summary$X11g_income_aquaculture, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Extraction = round(income_summary$X11h_income_extraction, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Tourism = round(income_summary$X11i_income_tourism, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
            Other = round(income_summary$X11k_income_other, 1) /
               rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100
         )
         
         HH_avg_income$Fishing_related <-
            HH_avg_income$Artisinal_Fishing + 
            HH_avg_income$Industrial_Fishing +
            HH_avg_income$Buying_Trading +
            HH_avg_income$Processing_Fish
         
         HH_avg_income <-
            rbind(
               HH_avg_income,
               "Mean ± SE" = c(
                  sum(HH_avg_income$N),
                  mean_sem(HH_avg_income$Total_Income_USD, 1),
                  mean_sem(HH_avg_income$Farming, 1),
                  mean_sem(HH_avg_income$Harvesting, 1),
                  mean_sem(HH_avg_income$Artisinal_Fishing, 1),
                  mean_sem(HH_avg_income$Industrial_Fishing, 1),
                  mean_sem(HH_avg_income$Buying_Trading, 1),
                  mean_sem(HH_avg_income$Processing_Fish, 1),
                  mean_sem(HH_avg_income$Aquaculture, 1),
                  mean_sem(HH_avg_income$Extraction, 1),
                  mean_sem(HH_avg_income$Tourism, 1),
                  mean_sem(HH_avg_income$Other, 1),
                  mean_sem(HH_avg_income$Fishing_related)
               )
            )
         
         Q70_summary <-
            rownames_to_column(HH_avg_income[, c(1:2, 13)], "MA name")
         colnames(Q70_summary) <-
            c("MA name", "N", "Proportion (%)", "Fishing_related")
         Q70b <-
            cbind (Q70_summary[, c(1:2)],
                   "Proportion (%)" = round(
                      as.numeric(Q70_summary$`Proportion (%)`) * as.numeric(Q70_summary$Fishing_related) * conversion /
                         100,
                      1
                   ))
         
         Q70b <- data_4plot(Q70b)
         colnames(Q70b) <- c("MA name", "N", "Average")
         
         #Plot
         plot_Q70b <-
            ggplot(Q70b, aes(`MA name`, Average, N = N)) +
            theme_rare + 
            geom_col(alpha = 0.8, fill = "#005BBB") +
            ggtitle("Average household income from fishing in USD") +
            xlab (NULL) + ylab ("Average income in USD") +
            coord_flip(clip = "on")
         
         ggplotly(plot_Q70b, height = 750)
         
      }
      
      ### Q72_73 "Mean perception of personal economic situation" ####
      
      else if (input$hhs_question == "72. How do you judge the value of your catch compared to two years ago?" |
               input$hhs_question == "73. How do you expect the value of your catch to develop in the next two years?") {
         
         hhs_Q72 <- selectedData()[,c("ma_name", "72_current_economic")] %>%
                        filter(`72_current_economic`!= "")
                              
         Q72_length <-
            tapply(hhs_Q72$`72_current_economic` ,
                   hhs_Q72$ma_name,
                   length)
         hhs_Q72$`72_current_economic_no` <-
            as.numeric(as.character(
               recode_factor(
                  hhs_Q72$`72_current_economic`,
                  "Much better" = "5",
                  "Slightly better" = "4",
                  "Neither" = "3",
                  "Slightly worse" = "2",
                  "Much worse" = "1"
               )
            ))
         Q72_summary_bind <-
            data.frame(N = Q72_length, AVG = round(
               tapply(
                  hhs_Q72$`72_current_economic_no`,
                  hhs_Q72$ma_name,
                  mean
               ),
               2
            ))
         
         Q72_summary <-
            rbind(Q72_summary_bind, "Mean ± SE" = c(
               sum(Q72_summary_bind$N),
               mean_sem(Q72_summary_bind$AVG, 2)
            ))
         
         Q72_summary_rn <- rownames_to_column(Q72_summary, "MA name")
         colnames(Q72_summary_rn) <- c("MA name", "N", "Current")
         
         ### Q73 Future economic income ####
        
         hhs_Q73 <- selectedData()[,c("ma_name", "73_future_economic")] %>%
                        filter(`73_future_economic` != "")
        
         Q73_length <-
            tapply(hhs_Q73$`73_future_economic` ,
                   hhs_Q73$ma_name,
                   length)
         hhs_Q73$`73_future_economic_no` <-
            as.numeric(as.character(
               recode_factor(
                  hhs_Q73$`73_future_economic`,
                  "Much better" = "5",
                  "Slightly better" = "4",
                  "Neither" = "3",
                  "Slightly worse" = "2",
                  "Much worse" = "1"
               )
            ))
         Q73_summary_bind <- data.frame(N = Q73_length,
                                        AVG = round(
                                           tapply(
                                              hhs_Q73$`73_future_economic_no`,
                                              hhs_Q73$ma_name,
                                              mean
                                           ),
                                           2
                                        ))
         
         Q73_summary <-
            rbind(Q73_summary_bind, "Mean ± SE" = c(
               sum(Q73_summary_bind$N),
               mean_sem(Q73_summary_bind$AVG, 2)
            ))
         Q73_summary_rn <- rownames_to_column(Q73_summary, "MA name")
         colnames(Q73_summary_rn) <- c("MA name", "N", "Future")
         
         #combine Q72 and Q73
         Q72_73_summary <- cbind(Q72_summary_rn, Q73_summary_rn[, -1])
         
         Q72_73 <-
            Q72_73_summary %>% pivot_longer (
               cols = c("Current", "Future"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         #Fix Ns
         Q72_73$N <-
            as.data.frame(
               pivot_longer(
                  Q72_73,
                  cols = c("N", "N"),
                  names_repair = "unique",
                  names_to = "No",
                  values_to = "N"
               )
            )$N
         
         Q72_73a <- data_4plot(Q72_73)
         
         colnames(Q72_73a) <- c("MA name", "N", "key", "Average")
         
         #Plot
         plot_Q72_73 <-
            ggplot(Q72_73a, aes(`MA name`, Average, N = N)) +
               theme_rare + 
               geom_col(alpha = 0.8, fill = "#005BBB") + 
               facet_wrap(~key) +
               ggtitle("Average perception of current and future personal economic situation") +
               scale_y_continuous(limits = c(0, 5.5), breaks = c(1:5)) +
               #scale_fill_distiller(palette = "Spectral")+
               xlab (NULL) + 
               ylab ("Average score (much worse = 1; worse = 2; neither = 3; better = 4; much better = 5)") +
               coord_flip(clip = "on")
         
         ggplotly(plot_Q72_73, height = 750)
         
      }
      
      ### Q77 Proportion of household able to make ends meet ####
      
      else if (input$hhs_question == "77. Thinking of your household’s total income, is your household able to make ends meet?") {
      
      hhs_Q77 <- selectedData()[,c("ma_name", "77_hh_ends_meet")] %>%
                        filter(`77_hh_ends_meet` != "") %>%
                           droplevels()
         
      Q77_summary <- proportion(hhs_Q77$`77_hh_ends_meet`,
                                   hhs_Q77$ma_name,
                                   3, type=5)
      
      colnames(Q77_summary) <- c("MA name", "N", 
                                 "Easy", "Fairly easy", 
                                 "Very easy", "With difficulty",
                                 "With great difficulty")
       Q77_summary_long <-
            as.data.frame(
               Q77_summary %>% 
                  pivot_longer(
                     cols = c(
                        "With great difficulty",
                        "With difficulty",
                        "Fairly easy",
                        "Easy",
                        "Very easy"
                       ),
                     names_to = "key",
                     values_to = "Proportion (%)"
                  )
            )
         
         Q77_summary_long$key <-
            factor(
               Q77_summary_long$key,
               levels = c(
                  "With great difficulty",
                  "With difficulty",
                  "Fairly easy",
                  "Easy",
                  "Very easy"
                )
            )
         
         Q77 <- data_4plot(Q77_summary_long)
         
          
        #Plot
        plot_Q77 <-
           ggplot(Q77, aes(`MA name`, `Proportion (%)`, N = N)) +
           theme_rare + 
           geom_col(fill = "#005BBB", alpha = 0.8) +
           facet_wrap( ~ key, ncol=5,
                       labeller = label_wrap_gen(25)) +
           scale_y_continuous(limits = c(0, 110),
                              breaks = seq(0, 100, 25)) +
           ggtitle("Proportion of household able to make ends meet") +
           xlab (NULL) + ylab ("Proportion (%)") + 
               coord_flip(clip = "on")
        
         ggplotly(plot_Q77, height = 750)
        
       }
      
      ### Q78 Proportion of sex in community who make financial decisions for the household #####
      
      else if (input$hhs_question == "78. Who makes the financial decisions in your household?") {
        
         hhs_Q78 <- selectedData()[,c("6_gender", "ma_name", "78_financial_decisions")] %>%
                           filter(`78_financial_decisions` != "") %>%
                              droplevels()
            
         hhs_Q78_f <- (subset(hhs_Q78, `6_gender` == "F"))
         hhs_Q78_m <- (subset(hhs_Q78, `6_gender` == "M"))
         
         Q78_length <-
            tapply(hhs_Q78$`78_financial_decisions`,
                   hhs_Q78$ma_name,
                   length)
         Q78_length <- as.vector(Q78_length)
         Q78_count_f <-
            as.data.frame(tapply(
               hhs_Q78_f$`78_financial_decisions`,
               list(
                  hhs_Q78_f$ma_name,
                  hhs_Q78_f$`78_financial_decisions`
               ),
               length
            ))
         Q78_count_m <-
            as.data.frame(tapply(
               hhs_Q78_m$`78_financial_decisions`,
               list(
                  hhs_Q78_m$ma_name,
                  hhs_Q78_m$`78_financial_decisions`
               ),
               length
            ))
         Q78_count_f[is.na(Q78_count_f)] <- 0
         Q78_count_m[is.na(Q78_count_m)] <- 0
         Q78_count_both <-  (Q78_count_f$Both +  Q78_count_m$Both)
         Q78_count_f_total <-
            Q78_count_f$`Female partner` + Q78_count_f$Myself + Q78_count_m$`Female partner`
         Q78_count_m_total <-
            Q78_count_m$`Male partner` + Q78_count_m$Myself + Q78_count_f$`Male partner`
         
         ### Proportions
         Q78_summary_bind <-
            data.frame(
               "MA name" = levels(hhs_Q78_f$ma_name),
               N = as.numeric(Q78_length),
               Female = round(Q78_count_f_total / Q78_length, 3) *
                  100,
               Male = round(Q78_count_m_total / Q78_length, 3) *
                  100,
               Both = round(Q78_count_both / Q78_length, 3) *
                  100
            )
         Q78_summary <-
            rbind(Q78_summary_bind,
                  c(
                     NA,
                     sum(Q78_summary_bind$N),
                     mean_sem(Q78_summary_bind$Female, 1),
                     mean_sem(Q78_summary_bind$Male, 1),
                     mean_sem(Q78_summary_bind$Both, 1)
                  ))
         colnames(Q78_summary) <-
            c("MA name", "N", "Female (%)", "Male (%)", "Both (%)")
         
         #pivot table
         Q78_summary_long <-
            as.data.frame(
               Q78_summary %>% pivot_longer(
                  cols = c("Female (%)", "Male (%)", "Both (%)"),
                  names_to = "key",
                  values_to = "Proportion (%)"
               )
            )
         
         Q78 <- data_4plot(Q78_summary_long)
         
         #Plot
         plot_Q78 <-
            ggplot(Q78, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + 
            geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        scale = input$x_axis,
                        labeller = label_wrap_gen(25)) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of community members who \nmake financial decisions for the household"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q78, height = 750)
      }
      
   })

   ### Download plot and summary tables #######
   #Download Data

   output$downloadData0 <- downloadHandler(
      filename = function() {
         paste0(
            "Q",
            substr(input$hhs_question, 1, 2),
            "_summary",
            ".csv",
            sep = ""
         )
      },
      content = function(file) {
         #readr::write_csv(plot_hhs_question(), file)
         ma_name <- last_plot()$x$layout$yaxis$categoryarray
         
         if (length(last_plot()$x$data) %in% c(2:6)) {
            var_name <- NULL
            for(i in c(1:length(last_plot()$x$data))) {
               var_name[i] <- last_plot()$x$layout$annotations[[i]]$text
                  ma_name <- cbind(ma_name,
                             as.vector(last_plot()$x$data[[i]]$x)
                              )
            }
            colnames(ma_name) <- c("MA name", var_name)
            download_summary <- data.frame(ma_name)
         readr::write_csv(download_summary, file)
         }
         
         else if (length(last_plot()$x$data) > 6) {
           var_name <- NULL
           for(i in c(1:length(last_plot()$x$data)) ) {
             var_name[i] <- last_plot()$x$data[[i]]$name
             ma_name <- cbind(ma_name,
                              as.vector(last_plot()$x$data[[i]]$x)
             )
           }
           colnames(ma_name) <- c("MA name", var_name)
           download_summary <- data.frame(ma_name)
           readr::write_csv(download_summary, file)
         }
         
         else if (length(last_plot()$x$data) == 1) {
            download_summary <- data.frame(
                                    cbind("MA name" = ma_name,
                                          "Values" = as.vector(last_plot()$x$data[[1]]$x)
                                    )
                            )
            readr::write_csv(download_summary, file)
         }
      }
   )
   #Download Figure
   output$downloadPlot0 <- downloadHandler(
      filename = function() {
         paste0(
            "plot_Q",
            substr(input$hhs_question, 1, 2), "_",
            input$Country,
            ".html",
            sep = ""
         )
      },
      content = function(file) {
   
           htmlwidgets::saveWidget(last_plot(), 
             file=file,
                selfcontained = TRUE)
         
      }
   )
   
   
   ### VIEW MAP ####
   output$map <- renderLeaflet({
      req(nrow(selectedData()) > 0)
      plot_map()
   })
   
   plot_map <- function () {
      # ----------------------------- LOAD SHAPE FILES -----------------------------------
      #Download footprintdata from dataworld
      ses_adm2 <-
         read_sf(
            "./adm/IDN/SES_adm2.geojson",
            stringsAsFactors = TRUE,
            as_tibble = FALSE
         )
      ses_ma <- read_sf("./adm/IDN/SES_ma.geojson")
      ses_r <- read_sf("./adm/IDN/SES_r.geojson")
      ##Add leafleat function
      leaflet(data = ses_adm2) %>%
         ## Setup SetView
         setView(
            lng = mean(selectedData()$lon, na.rm = T),
            lat = mean(selectedData()$lat, na.rm = T),
            zoom = 8
         ) %>%
         ### fly to bounds
         flyToBounds(
            lng1 = min(selectedData()$lon, na.rm = TRUE),
            lat1 = min(selectedData()$lat, na.rm = TRUE),
            lng2 = max(selectedData()$lon, na.rm = TRUE),
            lat2 = max(selectedData()$lat, na.rm = TRUE),
            options = list(
               padding = c(100, 100),
               maxZomm = 11,
               animate = FALSE,
               duration = 0.5
            )
         ) %>%
         
         ## Add Basemap
         addProviderTiles(input$basemap,
                          options = providerTileOptions(maxZoom = 10, attributionControl = FALSE))  %>%
         
         ## Add Proposed Managed Access areas
         addPolygons(
            data = ses_ma,
            group = 'Managed Access Area',
            color = "red",
            weight = 0.5,
            smoothFactor = 1,
            opacity = 0.5,
            fillColor = "red",
            fillOpacity = 0.05,
            label = paste("Managed Access", ses_ma$Name),
            labelOptions = labelOptions(textsize = "12px"),
            highlightOptions = highlightOptions(
               color = "red",
               weight = 2,
               bringToFront = TRUE
            )
         ) %>%
         
         ## Add Reserves
         addPolygons(
            data = ses_r,
            group = 'Reserve',
            color = "black",
            weight = 0.3,
            smoothFactor = 1,
            opacity = 0.5,
            popup = paste("Reserve", ses_r$MPA_Name),
            fillColor = "lightblue",
            fillOpacity = 0.02,
            label = paste("Reserve", ses_r$MPA_Name),
            labelOptions = labelOptions(textsize = "12px"),
            highlightOptions = highlightOptions(
               color = "darkblue",
               weight = 2,
               bringToFront = TRUE
            )
         ) %>%
         
         ## Add Local governemnt unit (district)
         addPolygons(
            data = ses_adm2,
            group = 'LGU',
            color = "black",
            weight = 0.3,
            smoothFactor = 1,
            opacity = 0.5,
            popup = paste("District", ses_adm2$District_N),
            fillColor = "white",
            fillOpacity = 0.02,
            label = paste("District", ses_adm2$District_N),
            labelOptions = labelOptions(textsize = "12px"),
            highlightOptions = highlightOptions(
               color = "darkorange",
               weight = 2,
               bringToFront = TRUE
            )
         ) %>%
         
         ## Add survey locations as markers
         addCircleMarkers(
            data = selectedData(),
            group = 'Community/Village',
            lng = unique(selectedData()$lon),
            lat = unique(selectedData()$lat),
            radius = 4,
            #~runif(100, 1, 1),
            color = "black",
            fillOpacity = 0.01,
            weight = 0.5,
            fillColor = "blue",
            popup = paste0(
               "<strong> MA name: </strong>",
               selectedData()$ma_name,
               "<br><strong>Community/Village: </strong>",
               selectedData()$`3_community`,
               "<br>__________________",
               "<br><strong>Households interviewed: </strong>",
               tapply(
                  selectedData()$`3_community`,
                  selectedData()$`3_community`,
                  length
               ),
               "<br><strong>Fisher households: </strong>",
               round(
                  tapply(
                     subset(selectedData(), `12a_fishing_men` > 0)$`3_community`,
                     subset(selectedData(), `12a_fishing_men` >
                               0)$`3_community`,
                     length
                  ) /
                     as.vector(
                        tapply(
                           selectedData()$`3_community`,
                           selectedData()$`3_community`,
                           length
                        )
                     ) * 100,
                  0
               ),
               "%",
               "<br><strong> Females interviewed: </strong>",
               data.frame(round(
                  tapply(
                     selectedData()$`6_gender`,
                     list(
                        selectedData()[, -1]$`3_community`,
                        selectedData()$`6_gender`
                     ),
                     length
                  ) / as.vector(
                     tapply(
                        selectedData()$`3_community`,
                        selectedData()$`3_community`,
                        length
                     )
                  ) * 100,
                  0
               ))$F,
               "%",
               "<br><strong> Males interviewed: </strong>",
               data.frame(round(
                  tapply(
                     selectedData()$`6_gender`,
                     list(
                        selectedData()[, -1]$`3_community`,
                        selectedData()$`6_gender`
                     ),
                     length
                  ) / as.vector(
                     tapply(
                        selectedData()$`3_community`,
                        selectedData()$`3_community`,
                        length
                     )
                  ) * 100,
                  0
               ))$M,
               "%"
            ),
            popupOptions = popupOptions(closeButton = FALSE, textsize = '14px'),
            label =  paste("Community/Village: ", selectedData()$`3_community`),
            labelOptions = labelOptions(
               permanet = TRUE,
               textsize = '14px',
               textOnly = FALSE
            ),
            options = markerOptions(riseOnHover = TRUE, riseOffset = 500)
         ) %>%
         ## Add legend to map
         leaflet::addLegend(
            title = "LEGEND",
            colors = c('white', '', 'red', '', 'lightblue', '', 'blue'),
            labels = c(
               'LGU',
               '',
               'Managed Access Area',
               '',
               'Reserve',
               '',
               'Community/Village'
            ),
            opacity = 0.7
         ) %>%
         
         ## Add scale ba
         addScaleBar(position = "bottomleft",
                     options = scaleBarOptions(imperial = FALSE)) %>%
         ## Add miniMap
         addMiniMap(
            tiles = input$basemap,
            toggleDisplay = TRUE,
            position = "bottomright"
         ) %>%
         
         # Layers control
         addLayersControl(
            overlayGroups = c(
               "LGU",
               "Managed Access Area",
               "Reserve",
               "Community/Village"
            ),
            options = layersControlOptions(collapsed = TRUE)
         )
      
      ## Add graticule and add measure and add mouse coordinates
      #addGraticule(interval = 1) %>%  addMeasure() %>% leafem::addMouseCoordinates() %>%
      
   }
   
   
   #Download Figure
   output$downloadMap <- downloadHandler(
      filename = function() {
         paste0("Map",
                input$Country,
                ".png",
                sep = "")
      },
      content = function(file) {
         file
      }
   )
   
   
   ### CREATE REPORTS #####
   #Report 1
   output$downloadReport1 <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function() {
         paste0("HHS_Report_",
                input$Country,
                "_Outcome_Impacts",
                ".docx")},
      content = function(file) {
         tempReport1 <- file.path(tempdir(), "hhs_report1.Rmd")
         file.copy("./report/hhs_report1.Rmd", tempReport1, overwrite = TRUE)
         withProgress(message = 'Generating report, please wait, this could take up to 20 seconds', {
            render(tempReport1,
                   output_file = file,
                   output_format = 'word_document')
         })
      }
   )
   
   #Report 2
   output$downloadReport2 <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function() {
        paste0("HHS_Report_",
                input$Country,
                  "_Questions",
                        ".docx")},
      content = function(file) {
         tempReport2 <- file.path(tempdir(), "hhs_report2.Rmd")
         file.copy("./report/hhs_report2.Rmd", tempReport2, overwrite = TRUE)
         withProgress(message = 'Generating report, please wait, this could take up to 20 seconds', {
            render(tempReport2,
                   output_file = file,
                   output_format = 'word_document')
         })
      }
   )
   
   #Download Raw Data
   output$downloadRawData1 <- downloadHandler(
      filename = function() {
         paste0(
            "HHS_data_",
            input$Country,
            ".csv",
            sep = ""
         )
      },
      content = function(file) {
         #filter out variables of no interest
         hh_surveys <- dplyr::select(selectedData(), 
                                            -c("level4_id",
                                             "1_interviewer",
                                             "username_2",
                                             "username",
                                             "80_full_name",
                                             "81_name_other",                 
                                             "81_telephone_other",
                                             "82_comments_interviewee",       
                                             "83_comments_interviewer",
                                             )
                                          )
         #add NA or 0 to blanks
         hh_surveys$`11a_months_farming`[is.na(hh_surveys$`11a_months_farming`)] <- 0
         hh_surveys$`11a_income_farming`[is.na(hh_surveys$`11a_income_farming`)] <- 0
         hh_surveys$`11b_months_harvesting`[is.na(hh_surveys$`11b_months_harvesting`)] <- 0
         hh_surveys$`11b_income_harvesting`[is.na(hh_surveys$`11b_income_harvesting`)] <- 0
         hh_surveys$`11c_months_fishing_artisanal`[is.na(hh_surveys$`11c_months_fishing_artisanal`)] <-0
         hh_surveys$`11c_income_fishing_artisanal`[is.na(hh_surveys$`11c_income_fishing_artisanal`)] <- 0
         hh_surveys$`11d_months_fishing_industrial`[is.na(hh_surveys$`11d_months_fishing_industrial`)] <- 0
         hh_surveys$`11d_income_fishing_industrial`[is.na(hh_surveys$`11d_income_fishing_industrial`)] <- 0
         hh_surveys$`11e_months_buying_trading`[is.na(hh_surveys$`11e_months_buying_trading`)] <- 0
         hh_surveys$`11e_income_buying_trading`[is.na(hh_surveys$`11e_income_buying_trading`)] <- 0
         hh_surveys$`11f_months_processing`[is.na(hh_surveys$`11f_months_processing`)] <- 0
         hh_surveys$`11f_income_processing`[is.na(hh_surveys$`11f_income_processing`)] <- 0
         hh_surveys$`11g_months_aquaculture`[is.na(hh_surveys$`11g_months_aquaculture`)] <- 0
         hh_surveys$`11g_income_aquaculture`[is.na(hh_surveys$`11g_income_aquaculture`)] <- 0
         hh_surveys$`11h_months_extraction`[is.na(hh_surveys$`11h_months_extraction`)] <- 0
         hh_surveys$`11h_income_extraction`[is.na(hh_surveys$`11h_income_extraction`)] <- 0
         hh_surveys$`11i_months_tourism`[is.na(hh_surveys$`11i_months_tourism`)] <- 0
         hh_surveys$`11i_income_tourism`[is.na(hh_surveys$`11i_income_tourism`)] <- 0
         hh_surveys$`11j_months_other_wage`[is.na(hh_surveys$`11j_months_other_wage`)] <- 0
         hh_surveys$`11j_income_other_wage`[is.na(hh_surveys$`11j_income_other_wage`)] <- 0
         hh_surveys$`11k_months_other`[is.na(hh_surveys$`11k_months_other`)] <- 0
         hh_surveys$`11k_income_other`[is.na(hh_surveys$`11k_income_other`)] <- 0
         
         hh_surveys[hh_surveys == ""] <- NA
         
         #Q7 multichoice to string
         hhs_q07_c <- selectedData_Q07() %>%
            group_by(submissionid) %>%
               summarise(`7_gender` = toString(`7_gender`),
                         `7_age` = toString(`7_age`),
                         `7_relationship` = toString(`7_relationship`),
                         `7_education` = toString(`7_education`)) 
        
         #Q14 multichoice to string
         hhs_q14_c <- selectedData_Q14() %>%
            group_by(submissionid) %>%
               summarise(`14_responsibility` = toString(`14_responsibility`))
         
         #Q15 multichoice to string
         hhs_q15_c <- selectedData_Q15() %>%
            group_by(submissionid) %>%
               summarise(`15_activity` = toString(`15_activity`),
                         `15_hours` = toString(`15_hours`)) 
                  
         #Q44 multichoice to string
         hhs_q44_c <- selectedData_Q44() %>%
            group_by(submissionid) %>%
               summarise(`44_meeting_attendance` = toString(`44_meeting_attendance`)) 
          
         #Q45 multichoice to string
         hhs_q45_c <- selectedData_Q45() %>%
            group_by(submissionid) %>%
               summarise(`45_leadership_position` = toString(`45_leadership_position`)) 
         
         #Q48 multichoice to string
         hhs_q48_c <- hhs_q48 %>%
            group_by(submissionid) %>%
               summarise(`48_enforcement_participation` = toString(`48_enforcement_participation`))
         
         #Q69 multichoice to string
         hhs_q69_c <- hhs_q69 %>%
            group_by(submissionid) %>%
            summarise(`69_fish_type` = toString(`69_fish_type`),
                      `69_street` = toString(`69_street`),
                      `69_customer_home` = toString(`69_customer_home`),
                      `69_market` = toString(`69_market`),
                      `69_shop` = toString(`69_shop`),
                      `69_fishing_company` = toString(`69_fishing_company`),
                      `69_restaurant`= toString(`69_restaurant`),
                      `69_own_home` = toString(`69_own_home`),
                      `69_other` = toString(`69_other`))
         
      #combine all data
         hhs_data <- plyr::join_all(list(hh_surveys, 
                                    hhs_q07_c,
                                    hhs_q14_c,
                                    hhs_q15_c,
                                    hhs_q44_c,
                                    hhs_q45_c,
                                    hhs_q48_c,
                                    hhs_q69_c),
                                    by = "submissionid",
                                    type = "left",
                                    match = "first")
         write_csv(hhs_data, file)
       }
    )
   
   #Download Raw Data
   output$downloadRawData2 <- downloadHandler(
      filename = function() {
         paste0(
            "HHS_data_",
            input$Country,
            ".csv",
            sep = ""
         )
      },
      content = function(file) {
         #filter out variables of no interest
         hh_surveys <- dplyr::select(selectedData(), 
                                     -c("level4_id",
                                        "1_interviewer",
                                        "username_2",
                                        "username",
                                        "80_full_name",
                                        "81_name_other",                 
                                        "81_telephone_other",
                                        "82_comments_interviewee",       
                                        "83_comments_interviewer",
                                     )
         )
         #add NA or 0 to blanks
         hh_surveys$`11a_months_farming`[is.na(hh_surveys$`11a_months_farming`)] <- 0
         hh_surveys$`11a_income_farming`[is.na(hh_surveys$`11a_income_farming`)] <- 0
         hh_surveys$`11b_months_harvesting`[is.na(hh_surveys$`11b_months_harvesting`)] <- 0
         hh_surveys$`11b_income_harvesting`[is.na(hh_surveys$`11b_income_harvesting`)] <- 0
         hh_surveys$`11c_months_fishing_artisanal`[is.na(hh_surveys$`11c_months_fishing_artisanal`)] <-0
         hh_surveys$`11c_income_fishing_artisanal`[is.na(hh_surveys$`11c_income_fishing_artisanal`)] <- 0
         hh_surveys$`11d_months_fishing_industrial`[is.na(hh_surveys$`11d_months_fishing_industrial`)] <- 0
         hh_surveys$`11d_income_fishing_industrial`[is.na(hh_surveys$`11d_income_fishing_industrial`)] <- 0
         hh_surveys$`11e_months_buying_trading`[is.na(hh_surveys$`11e_months_buying_trading`)] <- 0
         hh_surveys$`11e_income_buying_trading`[is.na(hh_surveys$`11e_income_buying_trading`)] <- 0
         hh_surveys$`11f_months_processing`[is.na(hh_surveys$`11f_months_processing`)] <- 0
         hh_surveys$`11f_income_processing`[is.na(hh_surveys$`11f_income_processing`)] <- 0
         hh_surveys$`11g_months_aquaculture`[is.na(hh_surveys$`11g_months_aquaculture`)] <- 0
         hh_surveys$`11g_income_aquaculture`[is.na(hh_surveys$`11g_income_aquaculture`)] <- 0
         hh_surveys$`11h_months_extraction`[is.na(hh_surveys$`11h_months_extraction`)] <- 0
         hh_surveys$`11h_income_extraction`[is.na(hh_surveys$`11h_income_extraction`)] <- 0
         hh_surveys$`11i_months_tourism`[is.na(hh_surveys$`11i_months_tourism`)] <- 0
         hh_surveys$`11i_income_tourism`[is.na(hh_surveys$`11i_income_tourism`)] <- 0
         hh_surveys$`11j_months_other_wage`[is.na(hh_surveys$`11j_months_other_wage`)] <- 0
         hh_surveys$`11j_income_other_wage`[is.na(hh_surveys$`11j_income_other_wage`)] <- 0
         hh_surveys$`11k_months_other`[is.na(hh_surveys$`11k_months_other`)] <- 0
         hh_surveys$`11k_income_other`[is.na(hh_surveys$`11k_income_other`)] <- 0
         
         hh_surveys[hh_surveys == ""] <- NA
         
         #Q7 multichoice to string
         hhs_q07_c <- selectedData_Q07() %>%
            group_by(submissionid) %>%
            summarise(`7_gender` = toString(`7_gender`),
                      `7_age` = toString(`7_age`),
                      `7_relationship` = toString(`7_relationship`),
                      `7_education` = toString(`7_education`)) 
         
         #Q14 multichoice to string
         hhs_q14_c <- selectedData_Q14() %>%
            group_by(submissionid) %>%
            summarise(`14_responsibility` = toString(`14_responsibility`))
         
         #Q15 multichoice to string
         hhs_q15_c <- selectedData_Q15() %>%
            group_by(submissionid) %>%
            summarise(`15_activity` = toString(`15_activity`),
                      `15_hours` = toString(`15_hours`)) 
         
         #Q44 multichoice to string
         hhs_q44_c <- selectedData_Q44() %>%
            group_by(submissionid) %>%
            summarise(`44_meeting_attendance` = toString(`44_meeting_attendance`)) 
         
         #Q45 multichoice to string
         hhs_q45_c <- selectedData_Q45() %>%
            group_by(submissionid) %>%
            summarise(`45_leadership_position` = toString(`45_leadership_position`)) 
         
         #Q48 multichoice to string
         hhs_q48_c <- hhs_q48 %>%
            group_by(submissionid) %>%
            summarise(`48_enforcement_participation` = toString(`48_enforcement_participation`))
         
         #Q69 multichoice to string
         hhs_q69_c <- hhs_q69 %>%
            group_by(submissionid) %>%
            summarise(`69_fish_type` = toString(`69_fish_type`),
                      `69_street` = toString(`69_street`),
                      `69_customer_home` = toString(`69_customer_home`),
                      `69_market` = toString(`69_market`),
                      `69_shop` = toString(`69_shop`),
                      `69_fishing_company` = toString(`69_fishing_company`),
                      `69_restaurant`= toString(`69_restaurant`),
                      `69_own_home` = toString(`69_own_home`),
                      `69_other` = toString(`69_other`))
         
         #combine all data
         hhs_data <- plyr::join_all(list(hh_surveys, 
                                         hhs_q07_c,
                                         hhs_q14_c,
                                         hhs_q15_c,
                                         hhs_q44_c,
                                         hhs_q45_c,
                                         hhs_q48_c,
                                         hhs_q69_c),
                                    by = "submissionid",
                                    type = "left",
                                    match = "first")
         write_csv(hhs_data, file)
      }
   )
   
   ### HTML Report
   
   #output$renderedReport <- renderUI({           
   #   includeMarkdown(knitr::knit("./report/hhs_report2.Rmd"))           
  # })
   
   
   ### OUTPUT Instructions ###
   output$report_HHS <- renderText("HOUSEHOLD SURVEY REPORT")
   output$report_instructions <-
      renderText(
         "You can generate two structuraly different household survey reports, 1) based on socio-economic outcomes and impacts, or 2) based on household survey questions. Please select the desired options within each report panel on the right."
      )
   output$report_cover <-
      renderImage({
         list(src = "./images/hhs_report_cover.jpg",
              contentType = 'image/jpg',
              width = 225)
      }, deleteFile = FALSE)
   output$Note <- renderText("PLEASE NOTE:")
   output$Notetext <-
      renderText(
         "This app may time-out if left idle too long, which will cause the screen to grey-out. To use the app again, refresh the page"
      )
   output$create_reporttitle1 <-
      renderText("REPORT BY OUTCOMES AND IMPACTS")
   output$create_reporttitle2 <-
      renderText("REPORT BY HH SURVEY QUESTIONS")
   output$generatingmap <-
      renderText("Generating map, please wait ...")
   
   
})
