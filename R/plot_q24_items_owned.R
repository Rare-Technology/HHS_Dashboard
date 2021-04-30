
prep_q24_items_owned <- function(.data){

}

plot_q24_items_owned <- function(.data, use_plotly = TRUE){
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