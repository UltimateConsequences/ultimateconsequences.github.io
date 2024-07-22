library(plyr)
library(dplyr)

# plot
p <- ggplot(dat, aes(x = x, y = y)) + geom_bin2d() 

p <- ggplot(def, aes(x=year, y=protest_domain, fill=protest_domain, alpha=0.1)) + 
     geom_tile(color="white", size=0.3) + 
     scale_y_discrete(limits = rev) 


# working copy!

def <- de.confirmed # reset filtering

def$protest_domain <- factor(def$protest_domain)

defd <- def %>% filter(!is.na(protest_domain) & !is.na(year))

p <- ggplot(def, aes(x=year, y=department, fill=department, alpha=0.01)) + 
  geom_tile(color="white", size=.5) +
  stat_bin2d(geom = "text", aes(label = ..count..), breaks=seq(1913.5,2022.5,1)) + 
  scale_x_continuous(breaks=1982:2022, limits=c(1981.5,2022.5)) +
  scale_y_discrete(limits = rev) 

p <- ggplot(defd, aes(x=year, y=fct_infreq(protest_domain), fill=fct_infreq(protest_domain))) + 
  annotate("rect", xmin = 1981.5,xmax = 1985.5,ymin = 0, ymax = Inf,
           fill="lightblue", 
           alpha = .15)+
  annotate("text", x = 1983.5, y = 1, label = "bold(UDP)",
           parse = TRUE) +
  annotate("rect", xmin = 1985.5,xmax = 2005.5,ymin = 0, ymax = Inf,
              fill="darkgreen", 
              alpha = .15)+
  annotate("text", x = 1995.5, y = 1, label = "bold(Neoliberal)",
           parse = TRUE) +
  annotate("rect", xmin = 2005.5,xmax = 2019,ymin = 0, ymax = Inf,
             fill="navyblue", 
             alpha = .15) +
  annotate("text", x = 2012, y = 1, label = "bold(MAS-IPSP)",
           parse = TRUE) +
  annotate("rect", xmin = 2019,xmax = 2020,ymin = 0, ymax = Inf,
           fill="red", 
           alpha = .15)+
  annotate("text", x = 2019.5, y = 1, label = "bold(Interim)",
           parse = TRUE) +
  annotate("rect", xmin = 2020,xmax = 2022.5,ymin = 0, ymax = Inf,
           fill="navyblue", 
           alpha = .15) +
  # geom_rect(aes(xmin = 2018.5,xmax = 2020.5,ymin = 0, ymax = Inf),
  #           fill="orange", 
  #           alpha = .2) +
  # geom_rect(aes(xmin = 2020.5,xmax = 2022.5,ymin = 0, ymax = Inf),
  #           fill="navyblue", 
  #           alpha = .2) +
  geom_tile(color="white", size=.5, alpha=.15) +
  stat_bin2d(geom = "text", aes(label = ..count..), breaks=seq(1981.5,2022.5,1)) + 
  scale_x_continuous(breaks=1982:2022, limits=c(1981.5,2022.5)) +
  scale_y_discrete(limits = rev) 

p <- p +
  theme(
#    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major.y = element_line(color = 1, size = 0.1,
                                      linetype = 1),
    panel.grid.major.x = element_line(color = 1, size = 0.02,
                                      linetype = 1),
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

p <- p + labs(
  x = "Year",
  y = "Protest Domain (sorted by number of deaths)",
  fill = "Protest Domain",
  title = "Deaths in Bolivian political conflict by protest domain and year, 1982-2022",
  subtitle = "Compiled from the Ultimate Consequences database",
  caption = waiver(),
  tag = waiver(),
  alt = waiver(),
  alt_insight = waiver()
) + 
  scale_alpha_continuous(guide="none")

p

tab <- tabyl(defd, protest_domain)
r2<-ddply(defd, .(protest_domain), summarize, mean=round(mean(year)))
freqtable <- left_join(tab, r2, "protest_domain")
freqtable <- arrange(freqtable, desc(n))
freqtable <- select(freqtable, c(1,2,4))
flextable(freqtable) %>% colformat_num(j=3, big.mark = "")