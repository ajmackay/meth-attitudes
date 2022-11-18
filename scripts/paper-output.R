if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/output-prep.R")

load("objects/all-objects.RData")



#### Demographics ####
ma.all.df <- summ.df %>% 
  filter(id %in% ma.id) %>% 
  mutate(sex = factor(sex, levels = c("Male", "Female")))

dems.summ <- ma.all.df %>% 
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
  dems.summ %>% as_flex_table() %>% 
    flextable::save_as_docx(path = "output/dems-table.docx")
}

#### Substance use characteristics ####
substance.summ <- ma.all.df %>% 
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
         ma.use.ways = factor(ma.use.ways, levels = c("Snorting", "Oral", "Smoking", "Injection"))) %>% 
  tbl_summary(label = c(audit.risky ~ "Audit Use Disorder",
                        other.drug ~ "Other Illicit Drug Use",
                        severity.dependence ~ "Substance Dependence",
                        ma.recent.use ~ "Last time using",
                        ma.use.age ~ "Age of first use",
                        ma.use.peak ~ "Frequency of use at peak",
                        ma.use.ways ~ "Mode of use"),
              statistic = list(all_continuous() ~ c("{mean} ({sd}) [{min}-{max}]")
              )
  ) %>% bold_labels()

if(FALSE){
  substance.summ %>% as_flex_table() %>% 
    flextable::save_as_docx(path = "output/substance-summ-table.docx")
}

#### Best Subsets selection ####
# plt.subset.selection +
#   plot.theme

plt.subset.comparison +
  plot.theme +
  theme(panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line())

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
         ) %>% 
