# clean_pt_variables()
# This function cleans up variable names from the presidents table

clean_pt_variables <- function(pt){
  stopifnot(is.tbl(pt))
  pt <- rename_with(pt, ~ sub(" \\(.*", "", .x))
  pt <- rename_with(pt, ~ tolower(gsub(" ", "_", .x, fixed = TRUE)))
  pt <- rename_with(pt, ~ gsub("frequency_of_", "", .x, fixed = TRUE))   
  pt <- rename_with(pt, ~ gsub("-", "_", .x, fixed = TRUE))     
  pt <- rename_with(pt, ~ gsub("/", "_", .x, fixed = TRUE))     
  pt <- rename_with(pt, ~ gsub("?", "", .x, fixed = TRUE))   
  
  return(pt)
}

assign_presidency_levels <- function(dataframe){
  president_levels <<- c(
    "Hernán Siles Zuazo", "Víctor Paz Estenssoro", "Jaime Paz Zamora", 
    "Gonzalo Sanchez de Lozada (1st)", "Hugo Banzer (2nd)", "Jorge Quiroga",
    "Gonzalo Sanchez de Lozada (2nd)", "Carlos Diego Mesa Gisbert", 
    "Eduardo Rodríguez", "Evo Morales", "Interim military government", 
    "Jeanine Áñez", "Luis Arce")
  president_initials <<- c(
    "HSZ", "VPE", "JPZ", 
    "GSL", "HB", "JQ",
    "GSL", "CM", 
    "ER", "EM", "Mil", 
    "JA", "LA")
  
  # This complicated construction ensures that no errors will be generated if there is no pres_admin column
  if (!("pres_admin" %in% colnames(dataframe))) return(dataframe)
  
  dataframe <- dataframe %>% mutate(pres_admin = factor(pres_admin, levels=president_levels))
  return(dataframe)
}

assign_state_perpetrator_levels <- function(dataframe, simplify=FALSE){
  de <- dataframe
  de$state_perpetrator <- str_to_title(de$state_perpetrator)
  de$state_perpetrator <- fct_explicit_na(de$state_perpetrator, na_level = "Unknown")
  if (simplify){
    de$state_perpetrator <- fct_collapse(de$state_perpetrator, 
                                         Yes = c("Yes", "Likely Yes", "Presumed Yes"), 
                                         Indirect = c("Indirect"),
                                         No = c("No", "Likely No"),
                                         Mutiny = c("In Mutiny"),
                                         Unknown  = c("Unknown", "Disputed", "Suspected") ) %>% suppressWarnings()
    de$state_perpetrator <- fct_relevel(de$state_perpetrator, 
                                        c("Unknown", "No", "Indirect", "Mutiny", "Yes")) # default ordering
  }
  return(de)
}

assign_state_responsibility_levels <- function(dataframe, simplify=FALSE){
  de <- dataframe
  de <- mutate(de, sr_text = state_responsibility) %>% # add a new column with original text in "state_responsibility"
    relocate(sr_text, .after=state_responsibility)
  
  de <- de %>% mutate(state_responsibility = case_when(    # overwrite the state responsibility for unintentional cases
    intentionality == "Incidental" ~ "Incidental",
    intentionality == "Conflict Accident" ~ "Accidental",
    TRUE ~ state_responsibility))
  
  de$state_responsibility <- fct_explicit_na(de$state_responsibility, na_level = "Unknown")
  if (simplify){
    de$state_responsibility <- fct_collapse(de$state_responsibility, 
                                            Perpetrator = c("State perpetrator", "State likely perpetrator", 
                                                            "State perpetrator, State victim refusing orders", 
                                                            "State perpetrator, State victim in mutiny",
                                                            "State indirect perpetrator"),
                                            Involved = c("State involved", "Political victim", 
                                                         "Political victim / political perpetrator",
                                                         "Political victim / unknown perpetrator",
                                                         "Possibly state involved"),
                                            Victim = c("State victim",  
                                                       "State victim, State perpetrator in mutiny"), 
                                            Separate = c("Separate from state"),
                                            Unintentional = c("Incidental", "Accidental"),
                                            Unknown  = c("Unknown", "Unclear", "Disputed") )
    
    sr_levels <<- c("Perpetrator", "Victim", "Involved", "Separate", "Unintentional", "Unknown")
    de$state_responsibility <- fct_relevel(de$state_responsibility, sr_levels)
  }
  return(de)
}  

library(incase)
 
estimated_date_string <- function(year, month, day){
  date_string <- in_case(
    (is.na(year)) ~ NA,
    (is.na(day) & is.na(month) & !is.na(year)) ~ str_glue("{year}-06-30"),
    (is.na(day) & !is.na(month) & !is.na(year)) ~ str_glue("{year}-{month}-15"),
    TRUE ~ paste(year, month, day, sep = "-")
  ) 
  
  return(date_string)
}

displayed_date_string <- function(year, month, day){
  # Unfortunate work around to the simultaneous evaluation done by in_case()
  # when using the vectors month.abb, month.name
  month_name <- ""
  month_abb <- ""
  if (!is.na(month)) {
    month_name <- month.name[month]
    month_abb <- month.abb[month]
  }
  
  date_string <- in_case(
    (is.na(year)) ~ "Date Unknown",
    ((is.na(day) & is.na(month) & !is.na(year))) ~ str_glue("{year}"),
    (is.na(day) & !is.na(month) & !is.na(year)) ~ str_glue("{month_name} {year}"),
    TRUE ~ str_glue("{day} {month_abb} {year}")
  ) 
  
  return(date_string)
}

combine_dates <- function(dataframe, incl_laterdate=FALSE, date_at_front=FALSE,
                          unknown_date_string = NA){
  dataframe <- dataframe %>% rowwise() %>%
                 mutate(date_text = estimated_date_string(year, month, day)) %>%
                 mutate(date = as.Date(ymd(date_text))) %>%
                 mutate(date_text = displayed_date_string(year, month, day))
  
  if(incl_laterdate & ("later_day" %in% colnames(dataframe))){
    dataframe <- mutate(dataframe,
                        laterdate = (paste(later_year, later_month, later_day, sep="-") %>% 
                                       ymd() %>% as.Date()))
  }
  if(date_at_front){
    dataframe <- dataframe %>% relocate(event_title, date) %>% 
                               relocate(year, month, day, .after = last_col())
  }
  
  dataframe
}

assign_location_precision_levels <- function(dataframe){
  location_precision_levels <<- c(
    "address", "poi_small", "intersection", 
    "block", "poi_large", "road", "community", 
    "town", "rural_zone", 
    "municipality", "province", 
    "region", "department")
  
  # This complicated construction ensures that no errors will be generated if there is no location_precision column
  if (!("location_precision" %in% colnames(dataframe))) return(dataframe)
  
  dataframe <- dataframe %>% mutate(location_precision = factor(location_precision, levels=location_precision_levels))
  return(dataframe)
}

string_to_listcase <- function(string) {
  string %>% str_replace(",", ".") %>% 
    str_to_sentence() %>% 
    str_replace("\\.", ",")
  }

assign_protest_domain_levels <- function(dataframe, na.level = "Unknown"){
  # This complicated construction ensures that no errors will be generated if there is no location_precision column
  if (!("protest_domain" %in% colnames(dataframe))) return(dataframe)
  
  # factor protest_domain
  protest_domain.grouped <<- c( 
    "Gas wars",                         # Economic
    "Economic policies",
    "Labor",
    "Education",
    "Mining",
    "Coca",                             # Rural
    "Peasant",
    "Rural land",
    "Rural land, Partisan politics",
    "Ethno-ecological",
    "Drug trade",                       # Criminalized
    "Contraband",
    "Local development",                # Local
    "Municipal governance",
    "Partisan politics",                # (solo)
    "Disabled",                         # (solo)
    "Guerrilla",                        # Armed actors
    "Paramilitary",
    "Urban land",
    "Unknown")                       # (solo)
  
  if(is.character(dataframe$protest_domain))
  {
    dataframe <- dataframe %>% 
                mutate(protest_domain = string_to_listcase(protest_domain))
  }
  dataframe$protest_domain <- fct_explicit_na(dataframe$protest_domain, na_level = na.level)
  dataframe$protest_domain <- fct_relevel(dataframe$protest_domain, protest_domain.grouped)
  return(dataframe)
}

# This function produces a single name string
combine_names <- function(dataframe, unknown_string = "?"){
  dataframe <- dataframe %>% 
    mutate(name = str_c(str_replace_na(dec_firstname, replacement = unknown_string),
                        str_replace_na(dec_surnames, replacement = unknown_string),
                        sep=" "))
  dataframe
}
