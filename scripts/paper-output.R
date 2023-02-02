if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}



# If data is updated or significant changes made to script then source the scripts below
# Otherwise just load all objects
if(FALSE){
  source("scripts/functions.R")
  source("scripts/data-processing.R")
  source("scripts/analyses.R") # TODO: stop script without stopping program
  # source("scripts/output-prep.R")
}

load("objects/all-objects.RData")


# Prep --------------------------------------------------------------------
## Font for flextables
fontname <- "Calibri"

blank <- element_blank()

plot.theme <- theme_classic() +
  theme(
    title = element_text(size = 12)
    # axis.text.x = element_text()
    # legend.title = element_text("none")
  )

theme.blank <- theme(
  axis.text.x = blank,
  axis.text.y = blank,
  axis.line = blank,
  axis.ticks = blank
)

theme.hist <-
  theme(
    axis.line = blank,
    axis.text.y = blank,
    axis.ticks.y = blank
  )

swin.red <- "#E4051F"
.red <- "#d7191c"
.blue <- "#2c7bb6"
.black <- "#1A242F"

hsize <- 4

pal.cfh <- c("#0b0b16", "#353454", "#171733", "#0a0a15", "#a9a1f1")
pal <- c("#d5d6ed", "#161639", "#b8c35d", "#332c72", "#c4c88a")


#### Demographic table prep ####
ma.all.df <- summ.df %>% 
  filter(id %in% ma.id) %>% 
  mutate(sex = factor(sex, levels = c("Male", "Female")),
         audit.risky = replace_na(audit.risky, FALSE))

# Outputs -----------------------------------------------------------------
#### Demographic Plots ####
##### Bar Plots #####
dat <- ma.all.df %>% 
  mutate(# Ordering factor variables
         education = factor(education, c("Did not finish High School",
                                         "Did not finish University",
                                         "Highschool/Technical Degree",
                                         "University Degree")),
         employment.status = fct_collapse(employment.status,
                                          Unemployed = c("Unemployed looking for work",
                                                         "Unemployed not looking for work")),
         
         employment.status = factor(employment.status, c("Student",
                                                         "Homemaker",
                                                         "Unemployed",
                                                         "Employed part time",
                                                         "Employed full time"))) %>% 
  select(id, sex, area.live, education, employment.status) %>% 
  pivot_longer(cols = -id, names_to = "variable") %>% 
  group_by(variable) %>% 
  count(value) %>% 
  mutate(p = round(n / sum(n) * 100, 2))

p.dems <- dat %>% 
  ggplot(aes(x = value, y = p, group = 1)) +
  geom_col(width = 0.6) +
  
  # Numbers for p > 10 (for formatting reasons)
  geom_text(data = filter(dat, p > 10),
            aes(label = str_c(round(p, 1), "%")), 
            size = 3.5, 
            hjust = 1,
            color = "white") +
  
  # Numbers for p <= 10 placed on the right side of chart
  geom_shadowtext(data = filter(dat, p <= 10),
            aes(label = str_c(round(p, 1), "%")),
            size = 3.5,
            hjust = -0.1,
            bg.colour = "white",
            colour = .black,
            bg.r = 0.2) +
  
  coord_flip() +
  
  facet_wrap(variable ~., scales = "free",
             labeller = as_labeller(c(
               `area.live` = "Residential Area",
               `education` = "Education",
               `employment.status` = "Employment Status",
               `sex` = "Sex"
             ))) +
  
  labs(x = blank, y = blank) +
  
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(size = 11),
    axis.text = element_text(colour = "black"),
    
    # panel.grid.major = blank,
    # panel.grid.minor = blank,
    
    axis.text.x = blank
  )

if(!TRUE){
  p.dems %>% ggsave(filename = "output/dems-plot.png",
         height = 5,
         width = 7)
}




#### Substance use characteristics ####
##### Prep #####
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

##### Table #####
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
              
              type = all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{mean} ({sd}) [{min}-{max}]")
              )
  ) %>% 
  bold_labels() %>% 
  modify_footnote(update = everything() ~ NA) %>%  # Remove footnote
  
  as_flex_table() %>% 
  fontsize(size = 10, part = "all") %>% 
  fontsize(size = 9, part = "footer") %>% 
  font(fontname = fontname)
  

if(FALSE){
  substance.summ.tbl %>% 
    flextable::save_as_docx(path = "output/substance-summ-table.docx")
}



#### Best Subsets selection ####
##### Plot #####
p.subset <- p.subset.comparison +
  theme_minimal() +
  labs(x = "Number of Predictors", 
       y = blank) +
  theme(panel.background = blank,
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = blank,
        
        axis.text.x = element_text(colour = "black"),
        
        strip.background = element_rect(fill = "white",
                                        color = "white"),
        strip.text = element_text(colour = "black",
                                  size = 11),
        
        axis.title.x = element_text(vjust = -1, colour = "black")
        
        )

# Save plot
if(!TRUE){
ggsave(p.subset,
       width = 7,
       height = 4,
       units = "in",
       filename = "output/subset-fit.png")


}

##### Best Possible Models #####
ft.best.poss 

best.poss %>% 
  mutate(across(where(is.numeric), ~round(.x, 2)),
         
         predictors = str_replace_all(predictors, "trait.total", "Trait Anger"),
         predictors = str_replace_all(predictors, "state.total", "State Anger"),
         predictors = str_replace_all(predictors, "audit.total", "Alcohol Use"),
         predictors = str_replace_all(predictors, "area.live", "Residential Area"),
         predictors = str_replace_all(predictors, "sds.total", "Methamphetamine Dependence"),
         predictors = str_replace_all(predictors, "age", "Age"),
         predictors = str_replace_all(predictors, "sex", "Sex"),
         predictors = str_replace_all(predictors, "education", "Education"),
         predictors = str_replace_all(predictors, "k6.total", "Psychological Distress")
         
  ) %>% write_csv("output/regression/2022-12-05_best-subsets-models.csv")
  flextable() %>% 
  set_header_labels(n = "N",
                    predictors = "Predictors",
                    rsquare = "R2",
                    adjr = "Adj R2",
                    aic = "AIC") %>% 
  fontsize(size = 10, part = "all") %>% 
  font(fontname = fontname)




#### Regression Output ####

# final.model %>% tbl_regression()

##### Prep #####
final.model.ci <- confint(final.model) %>% as_tibble() %>% 
    mutate(CI = str_c(round(`2.5 %`, 2), " - ", round(`97.5 %`, 2)))

# Model parameters
model.params = list(
  f.stat = round(glance(final.model)$statistic, 2),
  
  dfs = str_c("(", glance(final.model)$df, ", ", glance(final.model)$df.residual, ")"),
  
  p.value = round(glance(final.model)$p.value, 3),
  
  r2 = round(glance(final.model)$r.squared, 2),
  
  adjr = round(glance(final.model)$adj.r.squared, 2),
  
  aic = round(glance(final.model)$AIC, 2)

)


##### Final Model Table #####
ft.regression <- final.model %>% tidy() %>% 
  mutate(p.value = if_else(p.value < .001, "<.001", as.character(round(p.value, 3))),
         CI = final.model.ci$CI) %>% 
  
  left_join(final.model.effects, by = c("term" = "Variable")) %>% 
  select(term, Estimate = estimate, CI, Partial, F2, p = p.value) %>% 
  mutate(across(where(is.numeric), ~round(.x, 2)),
         Estimate = as.character(Estimate)) %>% 
  
  add_row(
    tibble(term = 
             c("", "F", "df", "p", "R2", "Adj R2", "AIC"),
           Estimate = 
             c("", model.params$f.stat, model.params$dfs, model.params$p.value,
               model.params$r2, model.params$adjr, model.params$aic
               
             ))
  ) %>% write_csv("output/regression/2022-12-04_final-model.csv")
  
  # flextable() %>% 
  # # add_footer_lines(str_c("AIC: ", round(AIC(final.model), 2))) %>% 
  # 
  # bold(part = "header") %>% 
  # 
  # fontsize(size = 10, part = "all") %>% 
  # font(fontname = fontname)




ma.final %>% 
  select(dd.total, trait.total, sds.total, audit.total) %>% 
  lm.effect.size(dv = "dd.total", iv = c("trait.total", "sds.total", "audit.total")) %>% 
  mutate(across(where(is.numeric), ~round(.x, 3))) %>% identity()
  # write_csv("output/regression output/cohens-f.csv")






#### DDDI Subscales ####
##### ANOVA #####
get_anova_table(m.anova, correction = "none") %>% 
  format.p() %>% 
  flextable()


##### Boxplot #####
##### TODO: Make prettier (violin plot?) #####
plt.subscales <- p.subscales + 
  # geom_jitter(width = .1)
  
  scale_x_discrete(labels = c("Agressive Driving", "Negative Emotional Driving", "Risky Driving")) +
  
  labs(x = "DDDI Subscale", y = "DDDI Score") +
  
  theme_minimal() +
  
  theme(
    panel.grid.major.x = blank,
    axis.text.x = element_text(colour = "black", face = "bold", size = 9),
    axis.title.x = element_text(vjust = -1, size = 11, colour = "black"),
    axis.title.y = element_text(vjust = 1, size = 11, colour = "black")
    )

if(!TRUE){
  ggsave(plt.subscales,
         width = 7,
         height = 5,
         units = "in",
         filename = "output/anova/subscales.png")
}


# Alternatives
if(FALSE){
summ.dd.subscale %>% 
  ggplot(aes(x = dd.subscale, y = mean, fill = dd.subscale)) +
  geom_bar(stat = "identity", color = "black",
           position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = .2) +
  
  scale_fill_manual(values = pal) +
  
  theme_minimal()

subscales.df %>% 
  ggplot(aes(x = dd.subscale, y = value, fill = dd.subscale)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(width = .1)
  
  


##### Multivariate Regression #####
# Needs Attention
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

#### Those excluded from analysis ####
survey.df %>% 
  filter(ma.ingest) %>% count(status)

survey.df %>% filter(ma.ingest, status != "Spam", !id %in% survey.screened$id) %>% count(ma.most.common)

survey.df %>% 
  filter(ma.ingest) %>% 
  # mutate(ma.most.common = if_else(ma.most.common, "Yes", "No")) %>% 
  
  select(consent, status, ma.most.common, finished) %>% 
tbl_summary(type = list(where(is.logical) ~ "categorical"))

survey.df %>% 
  filter(ma.ingest, ma.most.common, status != "Spam", finished) %>% glimpse

# Exporting ---------------------------------------------------------------
#### TODO: Table captions with numbers ####
export.all <- TRUE

if(export.all){

output.doc <- read_docx()

# Substance Summary Table
output.doc <- body_add_flextable(output.doc,
                                 substance.summ.tbl)

# Subset Regression Plot
output.doc <- body_add_gg(output.doc, p.subset,
                          height = 4,
                          width = 7)

# Demographic Plot
output.doc <- body_add_gg(output.doc, p.dems,
                          height = 5,
                          width = 7,
                          res = 300)

# Best Possible Subset Models
output.doc <- body_add_flextable(output.doc,
                                 ft.best.poss)

# Regression Model
output.doc <- body_add_flextable(output.doc,
                                 ft.regression)



print(output.doc, target = "output/tables-plots2.docx")

}

flextable::csv



















# Archive -----------------------------------------------------------------

#### NA: Demographic summary Table ####
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
