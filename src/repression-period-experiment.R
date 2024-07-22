

presidents.calm=str_c("Carlos Diego Mesa Gisbert", "Eduardo Rodríguez", "Evo Morales", sep = "|")
presidents.righti=str_c("Interim military government", "Jeanine Áñez", sep = "|")

def.periods <- def %>% mutate(period = case_when(
  year < 2000 ~ "A. Oct 1982–1999",
  (year>=2000)&(year<2003) ~ "B. 2000–2003",
  (year == 2003) & (str_detect(string = pres_admin, pattern="Gonzalo Sanchez de Lozada \\(2nd\\)"))~ "B. 2000–2003",
  str_detect(string = pres_admin, pattern=presidents.calm) ~ "C. 2004–Nov 2019",
  str_detect(string = pres_admin, pattern=presidents.righti) ~ "D. Nov 2019–Nov 2020",
  (pres_admin=="Luis Arce") ~ "E. Nov 2020-present",
  TRUE ~ ""))

def.periods %>% select(event_title, year, period) -> defpcheck # simplify to make sure it worked

repression.periods.categorized <- def.periods %>%
  n_categorized_by(period, complete=TRUE) %>% 
  select(period, n, n_state_perp) %>%
  mutate(perc_state_perp = percent(n_state_perp / n, digits = 0) )

repression.periods.categorized <- 
  repression.periods.categorized %>% mutate(days=c(
           -as.numeric(difftime("1982-10-10", "2000-01-01", units = "days")),
           -as.numeric(difftime("2000-01-01", "2003-10-19", units = "days")),
           -as.numeric(difftime("2003-10-19", "2019-11-10", units = "days")),
           -as.numeric(difftime("2019-11-10", "2020-11-08", units = "days")),
           -as.numeric(difftime("2020-11-08", "2022-11-16", units = "days"))), .after=period)

repression.periods.categorized <- 
  repression.periods.categorized %>% 
  mutate(years=days/365, .after=days) %>% 
  mutate(n_peryear=n/years, .after=n)  %>% 
  mutate(sp_peryear=n_state_perp/years, .after=n_state_perp)
      

repression.periods.categorized


