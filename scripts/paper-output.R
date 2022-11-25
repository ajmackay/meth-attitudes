if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

load("objects/all-objects.RData")

source("scripts/output-prep.R")


# Tring this officer thing ------------------------------------------------
output.doc <- read_docx()


#### Demographics ####
ma.all.df <- summ.df %>% 
  filter(id %in% ma.id) %>% 
  mutate(sex = factor(sex, levels = c("Male", "Female")),
         audit.risky = replace_na(audit.risky, FALSE)) 

dems.summ.tbl <- ma.all.df %>% 
  select(sex, age, education, employment.status, psychiatric.diagnosis) %>% 
  mutate(psychiatric.diagnosis = !is.na(psychiatric.diagnosis),
         
         # Ordering factor variables
         education = factor(education, c("Did not finish High School",
                                         "Did not finish University",
                                         "Highschool/Technical Degree",
                                         "University Degree")),
         employment.status = fct_collapse(employment.status,
                                          Unemployed = c("Unemployed looking for work",
                                                         "Unemployed not looking for work")),
         
         employment.status = factor(employment.status, c("Student",
                                                         "Homemaker",
                                                         "Employed part time",
                                                         "Employed full time",
                                                         "Unemployed"))) %>% 
  
  tbl_summary(label = c(sex ~ "Sex",
                        age ~ "Age",
                        education ~ "Education",
                        employment.status ~ "Employment Status",
                        psychiatric.diagnosis ~ "Any Psychiatric Diagnosis"),
              statistic = all_continuous() ~ c("{mean} ({sd}) [{min}-{max}]")) %>% 
  bold_labels()

if(FALSE){
  dems.summ.tbl %>% as_flex_table() %>% 
    flextable::save_as_docx(path = "output/dems-table.docx")
}

#### Substance use characteristics ####
substance.summ.prep <- ma.all.df %>% 
  left_join(
    druguse.df %>% 
      filter(id %in% ma.id) %>% 
      mutate(other.drug = if_else((cocaine != "Never" | cannabis != "Never" | club.drugs != "Never" | 
                                     hallucinogens != "Never" | inhalants != "Never" | heroin != "Never" |
                                     sedatives != "Never" | new.psychoactive != "Never"), "Yes", "No"))
  ) %>% 
  left_join(
    ma.df %>% 
      filter(id %in% ma.id) %>% 
      select(id, ma.recent.use, ma.use.ways)
  ) %>% 
  select(audit.risky, other.drug, ma.use.peak, severity.dependence = ma.type, ma.recent.use, ma.use.age, ma.use.peak, ma.use.ways) %>% 
  
  mutate(audit.risky = if_else(audit.risky == "TRUE", "At risk", "Not at risk"),
         ma.use.peak = factor(ma.use.peak, levels = c("1 to 2 times per month",
                                                      "Weekly",
                                                      "Daily")),
         ma.use.ways = factor(ma.use.ways, levels = c("Snorting", "Oral", "Smoking", "Injection")))


substance.summ.tbl <- substance.summ.prep %>% 
  relocate(ma.use.age, audit.risky, ma.use.peak, ma.recent.use, ma.use.ways,
           other.drug, severity.dependence) %>% 
  tbl_summary(label = c(audit.risky ~ "Alcohol Use Disorder",
                        severity.dependence ~ "Substance Dependence Severity",
                        ma.recent.use ~ "Last Meth Use",
                        ma.use.age ~ "Age of First Use",
                        ma.use.peak ~ "Frequency of Use at Peak",
                        ma.use.ways ~ "Mode of Use",
                        other.drug ~ "Other Illicit Drug Use"),
              
              
              statistic = list(all_continuous() ~ c("{mean} ({sd}) [{min}-{max}]")
              )
  ) %>% bold_labels() %>% 
  as_flex_table()

if(FALSE){
  substance.summ.tbl %>% 
    flextable::save_as_docx(path = "output/substance-summ-table.docx")
}



#### Best Subsets selection ####
# plt.subset.selection +
#   plot.theme

p.subset <- p.subset.comparison +
  labs(x = blank) +
  theme_light()+
  theme(panel.background = blank,
        panel.grid.major.y = blank,
        panel.grid.minor = blank,
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = blank,
        
        strip.background = element_rect(fill = "white",
                                        color = "black"),
        strip.text = element_text(colour = "black", face = "bold",
                                  size = 10))

# Save plot
if(FALSE){
ggsave(p.subset,
       width = 10,
       height = 10,
       units = "cm",
       filename = "output/tmp/subset.png")


# camcorder::gg_record(dir = "output/tmp",
#                      device = "png",
#                      width = 8,
#                      height = 5,
#                      units = "in")
# }



#### Regression Output ####
final.model %>% tbl_regression()

final.model %>% tidy() %>% 
  mutate(p.value = if_else(p.value < .001, "<.001", as.character(round(p.value, 3))),
         across(where(is.numeric), ~round(.x, 2)),
         CI = confint(final.model)) 
write_csv("output/regression output/basic-regress.csv")

ma.final %>% 
  select(dd.total, trait.total, sds.total, audit.total) %>% 
  lm.effect.size(dv = "dd.total", iv = c("trait.total", "sds.total", "audit.total")) %>% 
  mutate(across(where(is.numeric), ~round(.x, 3))) %>% 
  write_csv("output/regression output/cohens-f.csv")


best.poss %>% 
  mutate(across(where(is.numeric), ~round(.x, 2))) %>% 
  write_csv("output/regression output/best-models.csv")


#### DDDI Subscales ####
##### Errorbar #####
plt.error.subscale <- dd.subset.summ %>% 
  ggplot(aes(x = dd.subscale, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  plot.theme +
  theme(
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_line()
  )

##### Multivariate Regression #####
format.p <- function(p){
  if_else(p < .001, "<.001", as.character(round(p, 3)))
}

lm.mv %>% 
  tidy() %>% 
  mutate(p.value = format.p(p.value),
         across(where(is.numeric), ~round(.x, 2))) %>% left_join(
           lm.effect.size(ma.final, dv = "dd.ad.total", iv = c("audit.total", "sds.total", "trait.total")) %>% 
             mutate(response = "dd.ad.total"), by = c("term" = "Variable", "response")
         )


# Output ------------------------------------------------------------------
output.doc <- read_docx()


output.doc <- body_add_flextable(output.doc,
                                 substance.summ.tbl)

output.doc <- body_add_gg(output.doc, p.subset,
                          height = 5,
                          width = 8)


if(FALSE){
  print(output.doc, target = "output/tables-plots.docx")
  
}
  