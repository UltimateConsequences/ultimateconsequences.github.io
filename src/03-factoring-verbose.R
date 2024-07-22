library(janitor) # for tabyl

source(here::here("src","data-cleaning.R"), local = knitr::knit_global())
#
## Factoring: Presidential administration
#
# (this code is performed to create global variables by assign_presidency_levels())
# 
# president_levels <- c(
#   "Hernán Siles Zuazo", "Víctor Paz Estenssoro", "Jaime Paz Zamora", 
#   "Gonzalo Sanchez de Lozada (1st)", "Hugo Banzer (2nd)", "Jorge Quiroga",
#   "Gonzalo Sanchez de Lozada (2nd)", "Carlos Diego Mesa Gisbert", 
#   "Eduardo Rodríguez", "Evo Morales", "Interim military government", 
#   "Jeanine Áñez", "Luis Arce") # All presidencies, in chronological order
# president_initials <- c(
#   "HSZ", "VPE", "JPZ", 
#   "GSL", "HB", "JQ",
#   "GSL", "CM", 
#   "ER", "EM", "Mil", 
#   "JA", "LA")
# de <- de %>% mutate(pres_admin=factor(pres_admin, levels=president_levels))
de <- assign_presidency_levels(de)

de %>% tabyl(pres_admin)  %>% # Table to confirm level are assigned in order
  adorn_totals("row") %>%
  adorn_pct_formatting()

## Factoring: State perpetrator (a quasilogical variable)
de <- assign_state_perpetrator_levels(de, simplify=TRUE)
# 
# de$state_perpetrator <- str_to_title(de$state_perpetrator)
# de$state_perpetrator <- fct_explicit_na(de$state_perpetrator, na_level = "Unknown")
# de$state_perpetrator <- fct_collapse(de$state_perpetrator, 
#                                      Yes = c("Yes", "Likely Yes", "Presumed Yes"), 
#                                      Indirect = c("Indirect"),
#                                      No = c("No", "Likely No"),
#                                      Mutiny = c("In Mutiny"),
#                                      Unknown  = c("Unknown", "Disputed", "Suspected") )
# de$state_perpetrator <- fct_relevel(de$state_perpetrator, 
#                                     c("Unknown", "No", "Indirect", "Mutiny", "Yes")) # default ordering

de %>% tabyl(state_perpetrator) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

## Factoring: State responsibility (a categorical variable)
de.imported %>% tabyl(state_responsibility)  %>%
  adorn_totals("row") %>%
  adorn_pct_formatting() # Look at it before

de <- assign_state_responsibility_levels(de)
# assign_state_perpetrator_levels accomplishhes all of the following…
# 
# de <- mutate(de, sr_text = state_responsibility) %>% # add a new column with original text in "state_responsibility"
#       relocate(sr_text, .after=state_responsibility)
#   
# de <- de %>% mutate(state_responsibility = case_when(    # overwrite the state responsibility for unintentional cases
#   intentionality == "Incidental" ~ "Incidental",
#   intentionality == "Conflict Accident" ~ "Accidental",
#   TRUE ~ state_responsibility))
# 
# de$state_responsibility <- fct_explicit_na(de$state_responsibility, na_level = "Unknown")
# de$state_responsibility <- fct_collapse(de$state_responsibility, 
#                                         Perpetrator = c("State perpetrator", "State likely perpetrator", 
#                                                         "State perpetrator, State victim refusing orders", 
#                                                         "State perpetrator, State victim in mutiny",
#                                                         "State indirect perpetrator"),
#                                         Involved = c("State involved", "Political victim", 
#                                                      "Political victim / political perpetrator",
#                                                      "Political victim / unknown perpetrator",
#                                                      "Possibly state involved"),
#                                         Victim = c("State victim",  
#                                                    "State victim, State perpetrator in mutiny"), 
#                                         Separate = c("Separate from state"),
#                                         Unintentional = c("Incidental", "Accidental"),
#                                         Unknown  = c("Unknown", "Unclear", "Disputed") )
# 
# sr_levels <- c("Perpetrator", "Victim", "Involved", "Separate", "Unintentional", "Unknown")
# de$state_responsibility <- fct_relevel(de$state_responsibility, sr_levels)

state_resp.colors <-  c(
  Perpetrator = "forestgreen", 
  Victim = "#cd6600",                  # "darkorange3",
  Involved = "#90ee90",                # "lightgreen",         
  Separate = "#eeb422",                # "goldenrod2",
  Unintentional = "darkgray",
  Unknown = "lightgray")

de %>% tabyl(state_responsibility)  %>%
  adorn_totals("row") %>%
  adorn_pct_formatting() # Look at it after

# Age: new integer variable for the age of the deceased
de <- de %>% mutate(age = round(dec_age, digits = 0)) # must occur before filtering

## Factoring: Protest Domain (a categorical variable)

# de <- assign_protest_domain_levels(de)
# Capitalization handled into comma-separated title case
# Factored into 20 domains (including the composite "Rural land, Partisan politics")
# Color set: protest_domain.colors
de <- assign_protest_domain_levels(de)
protest_domain.colors <- assign_protest_domain.colors()
tabyl(de, protest_domain)

# Factoring: location_precision
#
# location_precision_levels <<- c(
#   "address", "poi_small", "intersection", 
#   "block", "poi_large", "road", "community", 
#   "town", "rural_zone", 
#   "municipality", "province", 
#   "region", "department")
de <- assign_location_precision_levels(de)
de %>% tabyl(location_precision) %>% adorn_pct_formatting()
