## Variables to compare:
# svd: "state victim" deaths
# gvd: deaths with government employees as victims
# pvd: "political victim" deaths, per state_responsibility

svd <- 
  def %>% 
  filter(str_detect(state_responsibility, "State victim")) %>% 
  select(event_title, dec_affiliation, perp_affiliation, state_perpetrator, state_responsibility)

gvd <-
  def %>% 
  filter(str_detect(dec_affiliation, "Government")|str_detect(dec_affiliation, "Security Force")) %>% 
  select(event_title, dec_affiliation, perp_affiliation, state_perpetrator, state_responsibility)

pvd <- 
  def %>% 
  filter(str_detect(state_responsibility, "Political")) %>% 
  select(event_title, dec_affiliation, perp_affiliation, state_perpetrator, state_responsibility, pol_assassination)
