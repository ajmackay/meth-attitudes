load("objects/all-objects.RData")

librarian::shelf(packages)


# Group comparisons -------------------------------------------------------
n.ma.id <- summ.df %>% 
  filter(!ma.ingest,
         dems.full,
         audit.full,
         # sds.full,
         trait.full,
         dd.full,
         # dui.strat.full,
         # duid.strat.full,
         # dui.att.full,
         duid.att.full
  ) %>% pull(id)

ma.id <- summ.df %>% 
  filter(ma.ingest,
         dems.full,
         audit.full,
         # sds.full,
         trait.full,
         dd.full,
         # dui.strat.full,
         # duid.strat.full,
         # dui.att.full,
         duid.att.full
  ) %>% pull(id)

#### Demographics ####
dems.df %>% 
  group_by(ma.ingest) %>% 
  summarise(n = n(),
            mean.age = mean(age, na.rm = TRUE))


table1(~ )


