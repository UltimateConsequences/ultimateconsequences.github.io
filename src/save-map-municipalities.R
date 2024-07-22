municipalities_list_gb2014 <- all_municipalities %>% 
                              arrange(department, municipality) %>% 
                              select(-count)

write_csv(municipalities_list_gb2014, "municipalities_list_gb2014.csv")