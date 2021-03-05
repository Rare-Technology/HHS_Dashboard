## RARE HHS Dashboard for Socioeconomic Data Visualization

This tool summarizes information collected through the Fish Forever Household Survey (HHS). The HHS contains defined-answer questions assessing respondents’ livelihoods, engagement with the local fishery, resilience to economic shocks, social capital, knowledge of fishing regulations, attitudes toward fisheries management, participation in fisheries management, and perceptions of current management approaches.

Here is a the metadata associated with the dataset. Please refer to the [HHS Guidance document](https://portal.rare.org/wp-content/uploads/2021/01/HHS-Instrument_updated_011221.pdf) for the Survey questions and answers. Each HHS question starts with a number (1 to 83).  The rest of the columns are information regarding administration levels, management levels, date of survey, submission ids, etc. The data is divided in **eight data files** depending on the type of questions asked. These data files can be joined together by a unique *submissionid* for analysis.


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

### HHS_surveys metadata

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
**9_region_member**: Region identification: 1 (Yes), 0 (No), NA (not answered)    
**9_region_name**: Region name   
**10_mpa_important**: 1 (Yes), 0 (No), -1 (Neutral), NA (not answered)     
**11a_months_farming**: 1-12 (Jan - Dec), NA (not answered)      
**11a_income_farming**: 0-100 (percent), NA (not answered)       
**11b_months_harvesting**: 1-12 (Jan - Dec), NA (not answered)       
**11b_income_harvesting**:  0-100 (percent), NA (not answered)     
**11c_months_fishing_artisanal**: 1-12 (Jan - Dec), NA (not answered)        
**11c_income_fishing_artisanal**: 0-100 (percent), NA (not answered)      
**11d_months_fishing_industrial**: 1-12 (Jan - Dec), NA (not answered)              
**11d_income_fishing_industrial**: 0-100 (percent), NA (not answered)      
**11e_months_buying_trading**: 1-12 (Jan - Dec), NA (not answered)           
**11e_income_buying_trading**: 0-100 (percent), NA (not answered)                
**11f_months_processing**: 1-12 (Jan - Dec), NA (not answered)         
**11f_income_processing**: 0-100 (percent), NA (not answered)                       
**11g_months_aquaculture**: 1-12 (Jan - Dec), NA (not answered)        
**11g_income_aquaculture**: 0-100 (percent), NA (not answered)                
**11h_months_extraction**: 1-12 (Jan - Dec), NA (not answered)       
**11h_income_extraction**: 0-100 (percent), NA (not answered)                 
**11i_months_tourism**: 1-12 (Jan - Dec), NA (not answered)        
**11i_income_tourism**: 0-100 (percent), NA (not answered)                   
**11j_months_other_wage**: 1-12 (Jan - Dec), NA (not answered)         
**11j_income_other_wage**: 0-100 (percent), NA (not answered)                 
**11k_other_source**: Names of other source of income not in the list         
**11k_months_other**: 1-12 (Jan - Dec), NA (not answered)                    
**11k_income_other**: 0-100 (percent), NA (not answered)       
**12a_fishing_men**: Integer, NA (not answered)        
**12b_fishing_women**: Integer, NA (not answered)        
**12c_fishing_children**: Integer, NA (not answered)              
**13a_processing_men**: Integer, NA (not answered)                 
**13b_processing_women**: Integer, NA (not answered)                 
**13c_processing_children**: Integer, NA (not answered)      
**14_responsibilities_other**: Factor, names of other responsibilities                                
**16_hh_main_fisher**: Factor, 6 levels, blanks (not answered)       
**16_hh_other_fisher**: Name of other option in question 16         
**17_fishing_low_profit**: Factor, 9 levels (first HHS version had different options than the last version)    
**18_fishing_high_profit**: Factor, 12 levels (first HHS version had different options than the last version)     
**19_current_fish_catch**: Factor, 5 levels, blanks (not answered)      
**20a_gear_hand**: 1 (Yes), 0 (No), NA (not answered)                     
**20b_gear_stationary_net**: 1 (Yes), 0 (No), NA (not answered)        
**20c_gear_mobile_net**: 1 (Yes), 0 (No), NA (not answered)           
**20d_gear_stationary_line**: 1 (Yes), 0 (No), NA (not answered)                 
**20e_gear_mobile_line**: 1 (Yes), 0 (No), NA (not answered)              
**20f_gear_explosives**: 1 (Yes), 0 (No), NA (not answered)                
**20g_gear_other**: 1 (Yes), 0 (No), NA (not answered)              
**21_boat_owner_status**: Factor, 5 levels, blanks (not answered)      
**22_catch_5yrs**: Factor, 5 levels, blanks (not answered)     
**23_job_secure**: 1 (Yes), 0 (No), NA (not answered)    
**20c_gear_mobile_net**: 1 (Yes), 0 (No), NA (not answered)      
**20d_gear_stationary_line**: 1 (Yes), 0 (No), NA (not answered)            
**20e_gear_mobile_line**: 1 (Yes), 0 (No), NA (not answered)        
**20f_gear_explosives**: 1 (Yes), 0 (No), NA (not answered)               
**20g_gear_other**: 1 (Yes), 0 (No), NA (not answered)        
**21_boat_owner_status**: Factor, 5 levels, NA or blanks (not answered)      
**22_catch_5yrs**: Factor, 5 levels, NA or blanks (not answered)       
**23_job_secure**: 1 (Yes), 0 (No), NA (not answered)              
**24a_item_radio_no**: Integer, NA (not answered)          
**24a_item_radio_value**: 1 (Yes), 0 (No), NA (not answered)       
**24b_item_tv_no**: Integer, NA (not answered)         
**24b_item_tv_value**: 1 (Yes), 0 (No), NA (not answered)        
**24c_item_satellite_no**: Integer, NA (not answered)        
**24c_item_satellite_value**: 1 (Yes), 0 (No), NA (not answered)           
**24d_item_phone_no**: Integer, NA (not answered)        
**24d_item_phone_value**: 1 (Yes), 0 (No), NA (not answered)            
**24e_item_washing_maching_no**:  Integer, NA (not answered)         
**24e_item_washing_machine_value**: 1 (Yes), 0 (No), NA (not answered)         
**24f_item_generator_no**: Integer, NA (not answered)          
**24f_item_generator_value**: 1 (Yes), 0 (No), NA (not answered)         
**24g_item_fridge_no**: Integer, NA (not answered)           
**24g_item_fridge_value**: 1 (Yes), 0 (No), NA (not answered)        
**24h_item_motorboat_no**: Integer, NA (not answered)            
**24h_item_motorboat_value**: 1 (Yes), 0 (No), NA (not answered)         
**24i_item_outboard_no**: Integer, NA (not answered)             
**24i_item_outboard_value**: 1 (Yes), 0 (No), NA (not answered)            
**24j_item_inboard_no**: Integer, NA (not answered)          
**24j_item_inboard_value**: 1 (Yes), 0 (No), NA (not answered)     
**24k_item_sailboat_no**: Integer, NA (not answered)       
**24k_item_sailboat_value**: 1 (Yes), 0 (No), NA (not answered)        
**24l_item_bicycle_no**: Integer, NA (not answered)          
**24l_item_bicycle_value**: 1 (Yes), 0 (No), NA (not answered)               
**24m_item_motorcycle_no**: Integer, NA (not answered)           
**24m_item_motorcycle_value**: 1 (Yes), 0 (No), NA (not answered)              
**24n_item_car_no**: Integer, NA (not answered)            
**24n_item_car_value**: 1 (Yes), 0 (No), NA (not answered)             
**24o_item_internet_no**: Integer, NA (not answered)           
**24o_item_internet_value**: 1 (Yes), 0 (No), NA (not answered)          
**24p_item_other_no**:  Integer, NA (not answered)         
**24p_item_other_specify**: Name of asset not in the list               
**24p_item_other_value**: 1 (Yes), 0 (No), NA (not answered)                 
**25a_financial_bank**: 1 (Yes), 0 (No), NA (not answered)                            
**25b_financial_micro**: 1 (Yes), 0 (No), NA (not answered)          
**25c_financial_ngo**: 1 (Yes), 0 (No), NA (not answered)          
**25d_financial_lender**: 1 (Yes), 0 (No), NA (not answered)          
**25e_financial_insurance**: 1 (Yes), 0 (No), NA (not answered)          
**25f_financial_other**: 1 (Yes), 0 (No), NA (not answered)          
**26_fishing_income_save**: 1 (Yes), 0 (No), NA (not answered)          
**27a_emergency_personal**: 1 (Yes), 0 (No), NA (not answered)          
**27b_emergency_family**: 1 (Yes), 0 (No), NA (not answered)          
**27c_emergency_friend**: 1 (Yes), 0 (No), NA (not answered)          
**27d_emergency_entrepreneur**: 1 (Yes), 0 (No), NA (not answered)          
**27e_emergency_savings_club**: 1 (Yes), 0 (No), NA (not answered)          
**27f_emergency_lender**: 1 (Yes), 0 (No), NA (not answered)          
**27g_emergency_commercial_ind**: 1 (Yes), 0 (No), NA (not answered)          
**27h_emergency_commercial_group**: 1 (Yes), 0 (No), NA (not answered)          
**27i_emergency_microfinance_ind**: 1 (Yes), 0 (No), NA (not answered)          
**27j_emergency_microfinance_group**: 1 (Yes), 0 (No), NA (not answered)          
**27k_emergency_ngo_ind**: 1 (Yes), 0 (No), NA (not answered)          
**27l_emergency_ngo_group**: 1 (Yes), 0 (No), NA (not answered)          
**27m_emergency_insurance**: 1 (Yes), 0 (No), NA (not answered)          
**27n_emergency_other**: Names of others type of emergency funds                    
**28_buyer_loans**: 1 (Yes), 0 (No), NA (not answered)                   
**28_loan_purpose**: Open answer, it should be chategorized           
**29_family_income**: Factor, 3 levels            
**30_trust_local_decision**: 1 (strongly disagree), 2 (disagree), 3 (neither), 4 (agree), 5 (strongly agree), 6 (i dont know), NA (not answered)              
**30_trust_regional_decision**: 1 (strongly disagree), 2 (disagree), 3 (neither), 4 (agree), 5 (strongly agree), 6 (i dont know), NA (not answered)              
**30_trust_community**: 1 (strongly disagree), 2 (disagree), 3 (neither), 4 (agree), 5 (strongly agree), 6 (i dont know), NA (not answered)              
**30_trust_village_alert**: 1 (strongly disagree), 2 (disagree), 3 (neither), 4 (agree), 5 (strongly agree), 6 (i dont know), NA (not answered)              
**30_trust_religious_leaders**: 1 (strongly disagree), 2 (disagree), 3 (neither), 4 (agree), 5 (strongly agree), 6 (i dont know), NA (not answered)              
**30_trust_ngo**: 1 (strongly disagree), 2 (disagree), 3 (neither), 4 (agree), 5 (strongly agree), 6 (i dont know), NA (not answered)              
**30_trust_fishers_community**: 1 (strongly disagree), 2 (disagree), 3 (neither), 4 (agree), 5 (strongly agree), 6 (i dont know), NA (not answered)              
**30_trust_fishers_other**: 1 (strongly disagree), 2 (disagree), 3 (neither), 4 (agree), 5 (strongly agree), 6 (i dont know), NA (not answered)              
**30_trust_community_neighbors**: 1 (strongly disagree), 2 (disagree), 3 (neither), 4 (agree), 5 (strongly agree), 6 (i dont know), NA (not answered)              
**31_my_community_ability**: Factor, 6 levels, blanks (not answered)           
**32_fishery_benefit_equal**: 1 (Yes), 0 (No), -1 (no depend on or benefit from fishery), NA (not answered)        
**33_ma_familiar**: 1 (Yes), 0 (No), -1 (skipped household), NA (not answered)           
**34_gear_restrictions**1 (Yes), 0 (No), -1 (community does not have a managed area), NA (not answered)        
**35a_ma_gear_nets**: Factor, 3 levels, NA (not answered), na (not answered)                 
**35b_ma_gear_dynamite**: Factor, 3 levels, NA (not answered), na (not answered)          
**35c_ma_gear_hookline**: Factor, 3 levels, NA (not answered), na (not answered)          
**35d_ma_gear_harpoon**: Factor, 3 levels, NA (not answered), na (not answered)         
**35e_ma_gear_trawl**: Factor, 3 levels, NA (not answered), na (not answered)         
**35f_ma_gear_mosquito**: Factor, 3 levels, NA (not answered), na (not answered)                     
**35g_ma_gear_poison**: Factor, 3 levels, NA (not answered), na (not answered)          
**35h_ma_gear_gillnet**: Factor, 3 levels, NA (not answered), na (not answered)         
**35i_ma_gear_gamboa**:Factor, 3 levels, NA (not answered), na (not answered)                 
**35j_ma_gear_quinia**: Factor, 3 levels, NA (not answered), na (not answered)          
**35k_ma_gear_other**: Factor, 3 levels, NA (not answered), na (not answered)           
**36_fish_size_restriction**: 1 (Yes), 0 (No), -1 (Community does not have a managed area), NA (not answered)      
**37_fish_catch**: Factor, many levels (open answer)                    
**37_ability_min_size**: 1 (Yes), 0 (No), NA (not answered)              
**37_ability_max_size**: 1 (Yes), 0 (No), NA (not answered)            
**38_reserve_fishing_allowed**: 1 (Yes), 0 (No), -1 (Community does not have a reserve area), NA (not answered)    
**39_ma_boundaries_aware**: Factor, 6 levels, na (not answered)         
**40_reserve_boundaries_aware**: Integer 0-10 in last HHS verison, but as factor (5 levels) for Mozambique        
**41_ma_fishers_allowed**: Factor, 9 levels, na (not answered)            
**42a_problem_regulation**: 1 (Yes), 0 (No), NA (not answered)   
**42b_problem_restricted_gear**: 1 (Yes), 0 (No), NA (not answered), -1 (not answered, 1st hhs had NA as default)  
**42c_problem_undersize**: 1 (Yes), 0 (No), NA (not answered), -1 (not answered)            
**42d_problem_inside_reserve**: 1 (Yes), 0 (No), NA (not answered), -1 (not answered)               
**42e_problem_unauthorized**: 1 (Yes), 0 (No), NA (not answered), -1 (not answered)             
**43_ma_benefits**: 1 (Yes), 0 (No), NA (not answered), -1 (not answered)             
**43_ma_benefits_opinion**: Factor, many levels, open answer          
**46_represent_interests**: Factor, 4 levels, na (not answered)             
**47_represent_contributions**: Factor, 4 levels, na (not answered)         
**49_enforcement_responsible**: Factor, 5 levels (multiple choice allowed), na OR blanks (not answered)         
**50_ma_punishment**: Factor, 6 levels, na (not answered)           
**51a_fishers_gear_not_permitted**: Integer (0-10): NA (not answered), values >10 are entry errors     
**51b_fishers_reserves**: Integer (0-10): NA (not answered), values >10 are entry errors     
**51c_fishers_ma_area**: Integer (0-10): NA (not answered), values >10 are entry errors          
**51d_fishers_violate_fish_size**: Integer (0-10): NA (not answered), values >10 are entry errors        
**51e_fishers_caught**: Integer (0-10): NA (not answered), values >10 are entry errors             
**52_ma_benefit_5yrs**: Factor, 4 levels, na OR blanks (not answered)         
**53_encourage_regulations**: Factor, 6 levels, blanks (not answered)             
**54_food_availability**: Factor, 5 levels, blanks (not answered)             
**55_worry_food**: Factor, 3 levels, blanks (not answered)            
**56_reduce_meal_size_adult**: 1 (Yes), 0 (No), NA (not answered)         
**57_reduce_meal_size_frequency**: Factor, 3 levels, blanks (not answered)          
**58_reduce_meal_size_child**: 1 (Yes), 0 (No), NA (not answered)       
**59_food_procurement**             
**60_hh_fish_consumption**           
**61a_current_regulations**         
**61b_catch_recording**              
**61c_community_participation**     
**61d_strong_enforcement**           
**61e_violations_decrease_profit**  
**61f_rights_distribution_fair**     
**61g_fishing_change_behavior**     
**61h_individual_behavior**          
**61i_help_neighbors**              
**62_reserve_compliance**            
**63_fishing_in_reserve**           
**63_times_fishing_reserve**         
**64_wrong_fishing_reserve**        
**65_no_wrong_fishing_reserve**      
**66_response_fishing_reserve**     
**66_reaction_fishing_reserve**      
**67_reserve_boundry**              
**68_fish_eaten**                    
**68_fish_sold**                    
**70_hh_average_income**             
**71_post_hours_man**               
**71_post_income_man**               
**71_post_hours_woman**             
**71_post_income_woman**             
**72_current_economic**             
**73_future_economic**               
**74_housing_costs**                
**75_luxury_goods**                  
**76_fishing_costs**                
**77_hh_ends_meet**                  
**78_financial_decisions**          
**79_allow_reinterview**             
**79_interview_years**              
**80_full_name**                     
**81_name_other**                   
**81_telephone_other**               
**82_comments_interviewee**         
**83_comments_interviewer**          