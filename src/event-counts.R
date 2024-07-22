campaign.events_intentions <- deaths_unfiltered %>% group_by(intentionality) %>%
  summarize(
    count= n()
  ) 

deaths_inc <- 
  deaths_unfiltered %>% filter(intentionality=="Incidental")

campaign.events_more <- deaths_unfiltered %>% group_by(protest_campaign, event_title, year) %>%
  summarize(
    n_unconfirmed = sum(unconfirmed =="TRUE",  na.rm = TRUE),
    n_collateral = sum(intentionality == "Collateral", na.rm = TRUE),
    n_nonconflict = sum(str_detect(intentionality, "Nonconflict"), na.rm = TRUE),
    n_state_perp_hi = sum(state_perpetrator=="Yes", na.rm = TRUE),
    n_state_victim_hi = sum(state_responsibility=="State victim", na.rm = TRUE),
    n_state_separate_hi = sum(str_detect(state_responsibility, "Separate from state"), na.rm = TRUE),
    n_unfiltered = n()
  ) 