devtools::install_github("liamgilbey/ggwaffle")
install.packages("hrbrthemes")
devtools::install_github("hrbrmstr/waffle")

library(tidyverse)
library(ggwaffle)
library(ggthemes)
library(hrbrthemes)
library(waffle)
library(dplyr)

storms %>%
  filter(year >= 2010) %>%
  dplyr::count(year, status) -> storms_df

ggplot(storms_df, aes(fill = status, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "Faceted Waffle Bar Chart",
    subtitle = "{dplyr} storms data",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))

de_tendomains <- de %>% 
  filter(!is.na(protest_domain)) %>%
  mutate(protest_domain = fct_lump(
    protest_domain,
    n=9,
    other_level="Other domain"
  )) 

det <- de_tendomains # let's call it det for short

det %>%
  filter(!is.na(year)) %>%
  dplyr::count(year, protest_domain) -> deaths_df

ggplot(deaths_df, aes(fill = protest_domain, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~year, ncol = 5, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))

###

state_resp.colors <-  c(
  Perpetrator = "forestgreen", 
  Victim = "firebrick4",
  Involved = "lightgreen",
  Separate = "goldenrod2",
  Unintentional = "darkgray",
  Unknown = "lightgray")

det %>%
  #  filter(year >= 1990) %>%
  filter(!is.na(year)) %>%
  dplyr::count(year, state_responsibility) -> deaths_df

ggplot(deaths_df, aes(fill = state_responsibility, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~year, ncol = 5, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  scale_fill_manual(values = state_resp.colors) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))

det %>%
  #  filter(year >= 1990) %>%
  filter(!is.na(year)) %>%
  dplyr::count(pres_admin, state_responsibility) -> deaths_df

ggplot(deaths_df, aes(fill = state_responsibility, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~pres_admin, nrow = 2, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  scale_fill_manual(values = state_resp.colors) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))

det %>%
  #  filter(year >= 1990) %>%
  # filter(!is.na(year)) %>%
  dplyr::count(protest_domain, state_responsibility) -> deaths_df

ggplot(deaths_df, aes(fill = state_responsibility, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~protest_domain, nrow = 2, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  scale_fill_manual(values = state_resp.colors) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))

### 

three_states <- sample(state.name, 3)

data.frame(
  states = factor(rep(three_states, 3), levels = three_states),
  vals = c(10, 20, 30, 6, 14, 40, 30, 20, 10),
  col = rep(c("blue", "black", "red"), 3),
  fct = c(rep("Thing 1", 3), rep("Thing 2", 3), rep("Thing 3", 3))
) -> xdf

xdf

xdf %>%
  count(states, wt = vals) %>%
  ggplot(aes(fill = states, values = n)) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = NULL, colour = NULL) +
  theme_ipsum_rc(grid="") -> waf

waf +
  geom_waffle(
    n_rows = 20, size = 0.33, colour = "white", flip = TRUE
  )

###

iris$Species <- as.character(iris$Species)
waffle_data <- waffle_iron(iris, aes_d(group = Species))

ggplot(waffle_data, aes(x, y, fill = group)) + 
  geom_waffle() + 
  coord_equal() + 
  scale_fill_waffle() + 
  theme_waffle()

###

ggplot(de.test, aes(factor(pres_admin, president_levels), fill = fct_rev(death_category))) +
  
de.test %>% count(pres_admin, death_category) -> de.test.counts

ggplot(de.test.counts, aes(fill = fct_rev(death_category), values=n )) +
  geom_waffle() +
  facet_wrap(~factor(pres_admin, president_levels), nrow = 1, strip.position = "bottom") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  coord_equal() + 
  scale_fill_waffle() + 
  theme_waffle()

###

de.test %>% filter(pres_admin=="Evo Morales") -> de.test.evo


ggplot(data=waffle_iron(de.test.evo, aes_d(group=death_category)), aes(x,y,fill=group))+
         geom_waffle() +
         coord_equal() +
  theme_waffle()

ggplot(data=waffle_iron(de.test.evo, aes_d(group=death_category)), aes(x,y,fill=group))+
  geom_waffle() +
  coord_equal()


waffle_iron(de.test.evo, aes_d(group=death_category))

###

data.frame(
  parts = factor(rep(month.abb[1:3], 3), levels=month.abb[1:3]),
  vals = c(10, 20, 30, 6, 14, 40, 30, 20, 10),
  col = rep(c("blue", "black", "red"), 3),
  fct = c(rep("Thing 1", 3),
          rep("Thing 2", 3),
          rep("Thing 3", 3))
) -> xdf

xdf %>%
  count(parts, wt = vals) %>%
  ggplot(aes(fill = parts, values = n)) +
  geom_waffle(n_rows = 20, size = 0.33, colour = "white", flip = TRUE) +
  scale_fill_manual(
    name = NULL,
    values = c("#a40000", "#c68958", "#ae6056"),
    labels = c("Fruit", "Sammich", "Pizza")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") 


parts <- c(`Un-breached\nUS Population` = (318 - 11 - 79), `Premera` = 11, `Anthem` = 79)            
waffle(
  parts, rows = 8, size = 1, 
  colors = c("#969696", "#1879bf", "#009bda"), legend_pos = "bottom"
)
