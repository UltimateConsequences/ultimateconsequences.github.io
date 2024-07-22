library(tmaptools)
source(here::here("src","colors-material-design.R"), local = knitr::knit_global())

bolivia.bb <- bb (x="Bolivia")
bolivia.center.long <- unname((bolivia.bb$xmin + bolivia.bb$xmax)/2)
bolivia.center.lat <- unname((bolivia.bb$ymin + bolivia.bb$ymax)/2)



de.located <- de.located %>% filter(!is.na(lat))

de.located <- de.located %>% mutate(label_text=paste("<h3>", event_title, "</h3><br>", 
                                                      name_line, "<br>",
                                                      "<b>Affiliation:</b> ", dec_affiliation, "<br>", 
                                                      sr_text, " | ", intentionality, "<br>", 
                                                      "<b>Event:</b> ", event_title, "<br>", 
                                                      "<b>Cause of death:</b> ", cause_death, "<br>",
                                                      incident_line, "<br>",
                                                      "<b>Location:</b> ", location, sep=""))

protest_domain.colors <- assign_protest_domain.colors()
pal <- colorFactor(protest_domain.colors, domain = protest_domain.grouped)
pal2 <- function(domain){
  protest_domain.colors[[domain]]
}

de.located.overlaps <- de.located %>% 
                         group_by(lat, lon) %>% 
                         mutate(duplicated = n() > 1) %>%
                         ungroup()

# de.located <- de.located.overlaps %>% filter(duplicated==TRUE)

m <- leaflet(de.located) %>% addProviderTiles(providers$CartoDB.Voyager,
                                              options = tileOptions(maxZoom = 16)) %>% 
  setView(bolivia.center.long, bolivia.center.lat, zoom = 7) %>% 
  addCircleMarkers(group = "Separated",
                   ~lon, ~lat, label = ~event_title, 
                   popup = ~label_text,
                   weight = 3, radius=6, 
                   color=~lapply(de.located$protest_domain, pal2), stroke = TRUE, fillOpacity = 0.25
                   ) %>%
  addCircleMarkers(group = "Clustered",
                   clusterOptions = markerClusterOptions(maxClusterRadius = 20,
                                                         spiderfyDistanceMultiplier = 1.7,
                                                         singleMarkerMode = TRUE),
                   ~lon, ~lat, label = ~event_title, 
                   popup = ~label_text,
                   weight = 3, radius=6, 
             color=~lapply(de.located$protest_domain, pal2), stroke = TRUE, fillOpacity = 0.3) %>%
  addLayersControl(baseGroups = c("Clustered", "Separated"),
                   options = layersControlOptions(collapsed = FALSE)
                   ) %>%
  addLegend(         position = "bottomright",
                     colors = protest_domain.colors,
                     labels = protest_domain.grouped, 
                     opacity = 1,
                     title = "Protest Domain"
                   )  
m
