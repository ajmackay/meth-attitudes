load("objects/all-objects.RData")

librarian::shelf(packages)



# MA Modelling ------------------------------------------------------------






# Group comparisons -------------------------------------------------------


#### Demographics ####
dems.df %>% 
  group_by(ma.ingest) %>% 
  summarise(n = n(),
            mean.age = mean(age, na.rm = TRUE))


summ.df %>% 
  filter(ma.ingest) %>%
  summarise(sds = mean(sds.total))




