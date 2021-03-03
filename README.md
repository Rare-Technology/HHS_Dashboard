## RARE HHS Dashboard for Socioeconomic Data Visualization

This tool summarizes information collected through the Fish Forever Household Survey (HHS). The HHS contains defined-answer questions assessing respondents’ livelihoods, engagement with the local fishery, resilience to economic shocks, social capital, knowledge of fishing regulations, attitudes toward fisheries management, participation in fisheries management, and perceptions of current management approaches.

Here is a the metadata associated with the dataset. Each HHS question starts with a number (1 to 83). The rest of the columns are information regarding administration levels, management levels, date of survey, submission ids, etc. The data is divided in **eight data files** depending on the type of questions asked. These data files can be joined together by a unique *submissionid* for analysis.


The data files are:       
*hhs_surveys* - Most questions         
*hhs_people* - Q7     
*hhs_responsabilities* - Q14      
*hhs_activities* - Q15      
*hhs_meeting* - Q44     
*hhs_leadership* - Q45      
*hhs_enforcement* - Q48        
*hhs_customers* - Q69     

The following are the data files column names and associated **metadata**:

#### HHS_surveys metadata

**id**:  Unique numeric identifier  
**country**: Country name    
**level1_name**: Subnational goverment name (usually State or Province)    
**level2_name**: Local goverment name (usually Municipality or District)     
**level3_name**: Sublocal goverment name (usually Subdistrict)      
**level4_name**: Community or Village name        
**level4_id**: Community or Village unique ID linked to level4_name (can be used to join datasets)     
**lat**: Latitude of the Community in decimal degress      
**lon**: Longitude of the Community in decimal degrees       
**ma_name**: Managed Access Area name (proposed or established)        
**ma_area**: Managed Access Area in hectares   
**ma_status**: Managed Access Area status (Proposed or Established)       
**reserves**: Number of reserves within the the Managaed Access Area         
**reserve_status**: Reserve status (Proposed or Established)        
**username**: User name for entering data in FastField (no available in data dowloads)   
**submissionid**: Unique submission Id from FastField    
**updatedat**: Submission date as Factor in year-month-day:time format ("2019:-07-12T03:01:46")   
**startformtimestamp**: Start time of the survey     
**endformtimestamp**: End time of the survey       
**1_interviewer**: Name of interviewer   
**2_affiliation**: Affiliation of interviewer       
**3_community**: Name of community or village    
**5_hh_status**: Household status of interviewed person                        
**6_gender**: Sex of the interviewee   
**8_religion**: Religion of the household head (dropdown list)   
**8_religion_other**: Other religion not in the list above     
**9_region_member**: Region identification: 1 (Yes), 0 (No), NA (no answered)    
**9_region_name**: Region name   
**10_mpa_important**: 1 (Yes), 0 (No), -1 (Neutral), NA (no answered)     
**11a_months_farming**: 1-12 (Jan - Dec), NA (no answered)      
**11a_income_farming**: 0-100 (percent), NA (no answered)       
**11b_months_harvesting**: 1-12 (Jan - Dec), NA (no answered)       
**11b_income_harvesting**:  0-100 (percent), NA (no answered)     
**11c_months_fishing_artisanal**: 1-12 (Jan - Dec), NA (no answered)        
**11c_income_fishing_artisanal**: 0-100 (percent), NA (no answered)      
**11d_months_fishing_industrial**: 1-12 (Jan - Dec), NA (no answered)              
**11d_income_fishing_industrial**: 0-100 (percent), NA (no answered)      
**11e_months_buying_trading**: 1-12 (Jan - Dec), NA (no answered)           
**11e_income_buying_trading**: 0-100 (percent), NA (no answered)                
**11f_months_processing**: 1-12 (Jan - Dec), NA (no answered)         
**11f_income_processing**: 0-100 (percent), NA (no answered)                       
**11g_months_aquaculture**: 1-12 (Jan - Dec), NA (no answered)        
**11g_income_aquaculture**: 0-100 (percent), NA (no answered)                
**11h_months_extraction**: 1-12 (Jan - Dec), NA (no answered)       
**11h_income_extraction**: 0-100 (percent), NA (no answered)                 
**11i_months_tourism**: 1-12 (Jan - Dec), NA (no answered)        
**11i_income_tourism**: 0-100 (percent), NA (no answered)                   
**11j_months_other_wage**: 1-12 (Jan - Dec), NA (no answered)         
**11j_income_other_wage**: 0-100 (percent), NA (no answered)                 
**11k_other_source**: Names of other source of income not in the list         
**11k_months_other**: 1-12 (Jan - Dec), NA (no answered)                    
**11k_income_other**: 0-100 (percent), NA (no answered)       
**12a_fishing_men**: Integer, NA (no answered)        
**12b_fishing_women**: Integer, NA (no answered)        
**12c_fishing_children**: Integer, NA (no answered)              
**13a_processing_men**: Integer, NA (no answered)                 
**13b_processing_women**: Integer, NA (no answered)                 
**13c_processing_children**: Integer, NA (no answered)      
**14_responsibilities_other**: Factor, names of other responsibilities                                
**16_hh_main_fisher**: Factor, 6 levels, blanks (no answered)       
**16_hh_other_fisher**: Name of other option in question 16         
**17_fishing_low_profit**: Factor, 5 levels (first version of HHS had different wording than the last version)    
**18_fishing_high_profit**: Factor, 5 levels (first version of HHS had different wording than the last version)     
 [63] "19_current_fish_catch"            
 "20a_gear_hand"                   
 "20b_gear_stationary_net"          
 "20c_gear_mobile_net"             
 "20d_gear_stationary_line"         
 "20e_gear_mobile_line"            
 "20f_gear_explosives"              
 "20g_gear_other"                  
 "21_boat_owner_status"             
 "22_catch_5yrs"                   
 "23_job_secure"                    
 "24a_item_radio_no"               
 "24a_item_radio_value"             
 "24b_item_tv_no"                  
 "24b_item_tv_value"                
 "24c_item_satellite_no"           
 "24c_item_satellite_value"         
 "24d_item_phone_no"               
 "24d_item_phone_value"             
 "24e_item_washing_maching_no"     
 "24e_item_washing_machine_value"   
 "24f_item_generator_no"           
 "24f_item_generator_value"         
 "24g_item_fridge_no"              
 "24g_item_fridge_value"            
 "24h_item_motorboat_no"           
 "24h_item_motorboat_value"         
 "24i_item_outboard_no"            
 "24i_item_outboard_value"          
 "24j_item_inboard_no"             
 "24j_item_inboard_value"           
 "24k_item_sailboat_no"            
 "24k_item_sailboat_value"          
 "24l_item_bicycle_no"             
 "24l_item_bicycle_value"           
 "24m_item_motorcycle_no"          
 "24m_item_motorcycle_value"        
 "24n_item_car_no"                 
 "24n_item_car_value"               
 "24o_item_internet_no"            
 "24o_item_internet_value"          
 "24p_item_other_no"               
 "24p_item_other_specify"           
 "24p_item_other_value"            
 "25a_financial_bank"               
 "25b_financial_micro"             
 "25c_financial_ngo"                
 "25d_financial_lender"            
 "25e_financial_insurance"          
 "25f_financial_other"             
 "26_fishing_income_save"           
 "27a_emergency_personal"          
 "27b_emergency_family"             
 "27c_emergency_friend"            
 "27d_emergency_entrepreneur"       
 "27e_emergency_savings_club"      
 "27f_emergency_lender"             
 "27g_emergency_commercial_ind"    
 "27h_emergency_commercial_group"   
 "27i_emergency_microfinance_ind"  
 "27j_emergency_microfinance_group" 
 "27k_emergency_ngo_ind"           
 "27l_emergency_ngo_group"          
 "27m_emergency_insurance"         
 "27n_emergency_other"              
 "28_buyer_loans"                  
 "28_loan_purpose"                  
 "29_family_income"                
 "30_trust_local_decision"          
 "30_trust_regional_decision"      
 "30_trust_community"               
 "30_trust_village_alert"          
 "30_trust_religious_leaders"       
 "30_trust_ngo"                    
 "30_trust_fishers_community"       
 "30_trust_fishers_other"          
 "30_trust_community_neighbors"     
 "31_my_community_ability"         
 "32_fishery_benefit_equal"         
 "33_ma_familiar"                  
 "34_gear_restrictions"             
 "35a_ma_gear_nets"                
 "35b_ma_gear_dynamite"             
 "35c_ma_gear_hookline"            
 "35d_ma_gear_harpoon"              
 "35e_ma_gear_trawl"               
 "35f_ma_gear_mosquito"             
 "35g_ma_gear_poison"              
 "35h_ma_gear_gillnet"              
 "35i_ma_gear_gamboa"              
 "35j_ma_gear_quinia"               
 "35k_ma_gear_other"               
 "36_fish_size_restriction"         
 "37_fish_catch"                   
 "37_ability_min_size"              
 "37_ability_max_size"             
 "38_reserve_fishing_allowed"       
 "39_ma_boundaries_aware"          
 "40_reserve_boundaries_aware"      
 "41_ma_fishers_allowed"           
 "42a_problem_regulation"           
 "42b_problem_restricted_gear"     
 "42c_problem_undersize"            
 "42d_problem_inside_reserve"      
 "42e_problem_unauthorized"         
 "43_ma_benefits"                  
 "43_ma_benefits_opinion"           
 "46_represent_interests"          
 "47_represent_contributions"       
 "49_enforcement_responsible"      
 "50_ma_punishment"                 
 "51a_fishers_gear_not_permitted"  
 "51b_fishers_reserves"             
 "51c_fishers_ma_area"             
 "51d_fishers_violate_fish_size"    
 "51e_fishers_caught"              
 "52_ma_benefit_5yrs"               
 "53_encourage_regulations"        
 "54_food_availability"             
 "55_worry_food"                   
 "56_reduce_meal_size_adult"        
 "57_reduce_meal_size_frequency"   
 "58_reduce_meal_size_child"        
 "59_food_procurement"             
 "60_hh_fish_consumption"           
 "61a_current_regulations"         
 "61b_catch_recording"              
 "61c_community_participation"     
 "61d_strong_enforcement"           
 "61e_violations_decrease_profit"  
 "61f_rights_distribution_fair"     
 "61g_fishing_change_behavior"     
 "61h_individual_behavior"          
 "61i_help_neighbors"              
 "62_reserve_compliance"            
 "63_fishing_in_reserve"           
 "63_times_fishing_reserve"         
 "64_wrong_fishing_reserve"        
 "65_no_wrong_fishing_reserve"      
 "66_response_fishing_reserve"     
 "66_reaction_fishing_reserve"      
 "67_reserve_boundry"              
 "68_fish_eaten"                    
 "68_fish_sold"                    
 "70_hh_average_income"             
 "71_post_hours_man"               
 "71_post_income_man"               
 "71_post_hours_woman"             
 "71_post_income_woman"             
 "72_current_economic"             
 "73_future_economic"               
 "74_housing_costs"                
 "75_luxury_goods"                  
 "76_fishing_costs"                
 "77_hh_ends_meet"                  
 "78_financial_decisions"          
 "79_allow_reinterview"             
 "79_interview_years"              
 "80_full_name"                     
 "81_name_other"                   
 "81_telephone_other"               
 "82_comments_interviewee"         
 "83_comments_interviewer"          
 "username_2"                       
 "35e_ma_gear_traps"               
 "35f_ma_gear_longline"             
 "35g_ma_gear_other"               
 "35a_ma_gear_gillnet"              
 "35b_ma_gear_loop"                
 "35d_ma_gear_matapi"               
 "35e_ma_gear_longline"            
 "35f_ma_gear_handline"             
 "35g_ma_gear_rag"                 
 "35h_ma_gear_trawl"                
 "35i_ma_gear_mesh"                
 "35j_ma_gear_poison"               
 "35k_ma_gear_net"                 
 "35l_ma_gear_cover"                
 "35m_ma_gear_corral"              
 "35n_ma_gear_bacamento"            
 "35o_ma_gear_other"               
 "ma_gear_other_desc"               
 "35k_ma_gear_other_desc"     
=======
**11b_months_harvesting**: 1-12 (Jan - Dec), NA (no answered)               
"11b_income_harvesting"           
 [33] "11c_months_fishing_artisanal"     "11c_income_fishing_artisanal"    
 [35] "11d_months_fishing_industrial"    "11d_income_fishing_industrial"   
 [37] "11e_months_buying_trading"        "11e_income_buying_trading"       
 [39] "11f_months_processing"            "11f_income_processing"           
 [41] "11g_months_aquaculture"           "11g_income_aquaculture"          
 [43] "11h_months_extraction"            "11h_income_extraction"           
 [45] "11i_months_tourism"               "11i_income_tourism"              
 [47] "11j_months_other_wage"            "11j_income_other_wage"           
 [49] "11k_other_source"                 "11k_months_other"                
 [51] "11k_income_other"                 "12a_fishing_men"                 
 [53] "12b_fishing_women"                "12c_fishing_children"            
 [55] "13a_processing_men"               "13b_processing_women"            
 [57] "13c_processing_children"          "14_responsibilities_other"       
 [59] "16_hh_main_fisher"                "16_hh_other_fisher"              
 [61] "17_fishing_low_profit"            "18_fishing_high_profit"          
 [63] "19_current_fish_catch"            "20a_gear_hand"                   
 [65] "20b_gear_stationary_net"          "20c_gear_mobile_net"             
 [67] "20d_gear_stationary_line"         "20e_gear_mobile_line"            
 [69] "20f_gear_explosives"              "20g_gear_other"                  
 [71] "21_boat_owner_status"             "22_catch_5yrs"                   
 [73] "23_job_secure"                    "24a_item_radio_no"               
 [75] "24a_item_radio_value"             "24b_item_tv_no"                  
 [77] "24b_item_tv_value"                "24c_item_satellite_no"           
 [79] "24c_item_satellite_value"         "24d_item_phone_no"               
 [81] "24d_item_phone_value"             "24e_item_washing_maching_no"     
 [83] "24e_item_washing_machine_value"   "24f_item_generator_no"           
 [85] "24f_item_generator_value"         "24g_item_fridge_no"              
 [87] "24g_item_fridge_value"            "24h_item_motorboat_no"           
 [89] "24h_item_motorboat_value"         "24i_item_outboard_no"            
 [91] "24i_item_outboard_value"          "24j_item_inboard_no"             
 [93] "24j_item_inboard_value"           "24k_item_sailboat_no"            
 [95] "24k_item_sailboat_value"          "24l_item_bicycle_no"             
 [97] "24l_item_bicycle_value"           "24m_item_motorcycle_no"          
 [99] "24m_item_motorcycle_value"        "24n_item_car_no"                 
[101] "24n_item_car_value"               "24o_item_internet_no"            
[103] "24o_item_internet_value"          "24p_item_other_no"               
[105] "24p_item_other_specify"           "24p_item_other_value"            
[107] "25a_financial_bank"               "25b_financial_micro"             
[109] "25c_financial_ngo"                "25d_financial_lender"            
[111] "25e_financial_insurance"          "25f_financial_other"             
[113] "26_fishing_income_save"           "27a_emergency_personal"          
[115] "27b_emergency_family"             "27c_emergency_friend"            
[117] "27d_emergency_entrepreneur"       "27e_emergency_savings_club"      
[119] "27f_emergency_lender"             "27g_emergency_commercial_ind"    
[121] "27h_emergency_commercial_group"   "27i_emergency_microfinance_ind"  
[123] "27j_emergency_microfinance_group" "27k_emergency_ngo_ind"           
[125] "27l_emergency_ngo_group"          "27m_emergency_insurance"         
[127] "27n_emergency_other"              "28_buyer_loans"                  
[129] "28_loan_purpose"                  "29_family_income"                
[131] "30_trust_local_decision"          "30_trust_regional_decision"      
[133] "30_trust_community"               "30_trust_village_alert"          
[135] "30_trust_religious_leaders"       "30_trust_ngo"                    
[137] "30_trust_fishers_community"       "30_trust_fishers_other"          
[139] "30_trust_community_neighbors"     "31_my_community_ability"         
[141] "32_fishery_benefit_equal"         "33_ma_familiar"                  
[143] "34_gear_restrictions"             "35a_ma_gear_nets"                
[145] "35b_ma_gear_dynamite"             "35c_ma_gear_hookline"            
[147] "35d_ma_gear_harpoon"              "35e_ma_gear_trawl"               
[149] "35f_ma_gear_mosquito"             "35g_ma_gear_poison"              
[151] "35h_ma_gear_gillnet"              "35i_ma_gear_gamboa"              
[153] "35j_ma_gear_quinia"               "35k_ma_gear_other"               
[155] "36_fish_size_restriction"         "37_fish_catch"                   
[157] "37_ability_min_size"              "37_ability_max_size"             
[159] "38_reserve_fishing_allowed"       "39_ma_boundaries_aware"          
[161] "40_reserve_boundaries_aware"      "41_ma_fishers_allowed"           
[163] "42a_problem_regulation"           "42b_problem_restricted_gear"     
[165] "42c_problem_undersize"            "42d_problem_inside_reserve"      
[167] "42e_problem_unauthorized"         "43_ma_benefits"                  
[169] "43_ma_benefits_opinion"           "46_represent_interests"          
[171] "47_represent_contributions"       "49_enforcement_responsible"      
[173] "50_ma_punishment"                 "51a_fishers_gear_not_permitted"  
[175] "51b_fishers_reserves"             "51c_fishers_ma_area"             
[177] "51d_fishers_violate_fish_size"    "51e_fishers_caught"              
[179] "52_ma_benefit_5yrs"               "53_encourage_regulations"        
[181] "54_food_availability"             "55_worry_food"                   
[183] "56_reduce_meal_size_adult"        "57_reduce_meal_size_frequency"   
[185] "58_reduce_meal_size_child"        "59_food_procurement"             
[187] "60_hh_fish_consumption"           "61a_current_regulations"         
[189] "61b_catch_recording"              "61c_community_participation"     
[191] "61d_strong_enforcement"           "61e_violations_decrease_profit"  
[193] "61f_rights_distribution_fair"     "61g_fishing_change_behavior"     
[195] "61h_individual_behavior"          "61i_help_neighbors"              
[197] "62_reserve_compliance"            "63_fishing_in_reserve"           
[199] "63_times_fishing_reserve"         "64_wrong_fishing_reserve"        
[201] "65_no_wrong_fishing_reserve"      "66_response_fishing_reserve"     
[203] "66_reaction_fishing_reserve"      "67_reserve_boundry"              
[205] "68_fish_eaten"                    "68_fish_sold"                    
[207] "70_hh_average_income"             "71_post_hours_man"               
[209] "71_post_income_man"               "71_post_hours_woman"             
[211] "71_post_income_woman"             "72_current_economic"             
[213] "73_future_economic"               "74_housing_costs"                
[215] "75_luxury_goods"                  "76_fishing_costs"                
[217] "77_hh_ends_meet"                  "78_financial_decisions"          
[219] "79_allow_reinterview"             "79_interview_years"              
[221] "80_full_name"                     "81_name_other"                   
[223] "81_telephone_other"               "82_comments_interviewee"         
[225] "83_comments_interviewer"          "level4_name"                     
[227] "username_2"                       "35e_ma_gear_traps"               
[229] "35f_ma_gear_longline"             "35g_ma_gear_other"               
[231] "35a_ma_gear_gillnet"              "35b_ma_gear_loop"                
[233] "35d_ma_gear_matapi"               "35e_ma_gear_longline"            
[235] "35f_ma_gear_handline"             "35g_ma_gear_rag"                 
[237] "35h_ma_gear_trawl"                "35i_ma_gear_mesh"                
[239] "35j_ma_gear_poison"               "35k_ma_gear_net"                 
[241] "35l_ma_gear_cover"                "35m_ma_gear_corral"              
[243] "35n_ma_gear_bacamento"            "35o_ma_gear_other"               
[245] "ma_gear_other_desc"               "35k_ma_gear_other_desc"     
>>>>>>> a557229bb7cded45e452009b6aa21801bdc6632c

