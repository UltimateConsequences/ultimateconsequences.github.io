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

## Factoring: State perpetrator (a quasilogical variable)
de <- assign_state_responsibility_levels(de, simplify=TRUE)

## Factoring: State perpetrator (a categorical variable)
de <- assign_state_perpetrator_levels(de, simplify=TRUE)

# sr_levels <- c("Perpetrator", "Victim", "Involved", "Separate", "Unintentional", "Unknown")

state_resp.colors <-  c(
  Perpetrator = "forestgreen", 
  Victim = "#cd6600",                  # "darkorange3",
  Involved = "#90ee90",                # "lightgreen",         
  Separate = "#eeb422",                # "goldenrod2",
  Unintentional = "darkgray",
  Unknown = "lightgray")

# Age: new integer variable for the age of the deceased
de <- de %>% mutate(age = round(dec_age, digits = 0)) # must occur before filtering

## Factoring: Protest Domain (a categorical variable)
# 
# Capitalization handled into comma-separated title case
# Factored into 20 domains (including the composite "Rural land, Partisan politics")
# Color set: protest_domain.colors
de <- assign_protest_domain_levels(de)

# protest_domain.colors <- assign_protest_domain.colors()
# tabyl(de, protest_domain)

# Factoring: location_precision
#
# location_precision_levels <<- c(
#   "address", "poi_small", "intersection", 
#   "block", "poi_large", "road", "community", 
#   "town", "rural_zone", 
#   "municipality", "province", 
#   "region", "department")
de <- assign_location_precision_levels(de)