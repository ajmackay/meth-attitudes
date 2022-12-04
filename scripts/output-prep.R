# Setup -------------------------------------------------------------------
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

hsize <- 4








stop_quietly()
# Demographic charts ------------------------------------------------------
# sds, peak use,
p.age <- ma.final %>% 
  ggplot(aes(x = age)) +
  plot.theme +
  geom_histogram(binwidth = 1, col = "black", fill = "#2b8cbe") +
  geom_vline(data = summarise(ma.final, mean.age = mean(age)),
             aes(xintercept = mean.age), col = swin.red, linetype = "dashed", size = 1) +
  # geom_text(label = "Mean: 30.8", x = 28.5, y = 8) +
  labs(x = "",
       y = "",
       title = "Age") +
  scale_y_continuous(expand = expansion(0.01)) +
  theme(
    axis.line = blank,
    axis.text.y = blank,
    axis.ticks.y = blank
  )
  
ggsave(p.age, filename = "output/plots/01_age.png", width = 12, height = 9, units = "cm")


fill.donut <- c("#2b8cbe", "#ece7f2")

p.area <- ma.final %>% 
  count(area.live) %>% 
  mutate(p = n/sum(n) * 100) %>% 
  ggplot(aes(x = hsize, y = p, fill = area.live)) +
  plot.theme +
  theme.blank +
  theme(
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("#fee8c8", "#e34a33")) +
  # scale_fill_brewer() +
  geom_col() +
  # geom_text(aes(label = str_c(round(p, 0), "%")),
  #           position = position_stack(vjust = 0.5),
  #           col = c("black")) +
  coord_polar(theta = "y") +
  xlim(c(2, hsize + 0.5)) +
  labs(x = blank,
       y = blank,
       fill = blank,
       title = "Residential Area")

ggsave(p.area, filename = "output/plots/02_area-live_donut.png", width = 9, height = 9, units = "cm")
  
p.sex <- ma.final %>% 
  count(sex) %>% 
  mutate(p = round(n / sum(n) * 100)) %>% 
  ggplot(aes(x = hsize, y = p, fill = sex)) +
  plot.theme +
  theme.blank +
  theme(
    legend.position = "none"
  ) +
  scale_fill_manual(values = fill.donut) +
  geom_col() + 
  coord_polar(theta = "y") +
  xlim(c(2, hsize + 0.5)) +
  # geom_text(aes(label = str_c(p, "%")),
  #           position = position_stack(vjust = 0.5),
  #           col = "white") +
  labs(x = blank, y = blank, fill = blank,
       title = blank)

ggsave(p.sex, filename = "output/plots/01_sex-donut.png", width = 9, height = 9, units = "cm")
  
#### SDS ####
p.sds.hist <- ma.final %>% 
  left_join(select(summ.df, id, ma.type)) %>% 
  ggplot(aes(x = sds.total, fill = ma.type)) +
  plot.theme +
  theme(
    legend.position = "none",
    axis.text.y = blank,
    axis.line.y = blank,
    axis.ticks.y = blank,
    axis.line.x = blank
  ) +
  scale_y_continuous(expand = expansion(0.01)) +
  scale_fill_manual(values = c("#d7191c", "#2c7bb6"))+
  geom_histogram(binwidth = 1, col = "black") +
  labs(x = blank, y = blank, fill = blank,
       title = "Severity of Depdendence")


ggsave(p.sds.hist, filename = "output/plots/01_sds-hist.png", width = 12, height = 9, units = "cm")

#### Age of use ####
p.age.use <- ma.final %>% 
  left_join(select(summ.df, id, ma.use.age)) %>% 
  mutate(use.age.bracket = case_when(
    between(ma.use.age, 15, 20) ~ "15-20",
    between(ma.use.age, 21, 30) ~ "21-30",
    between(ma.use.age, 31, 39) ~ "31-39",
    ma.use.age >= 40 ~ "40+"
  )) %>% 
  count(use.age.bracket) %>% arrange(desc(n)) %>% 
  mutate(p = round(n/sum(n) * 100, 0)) %>% 
  ggplot(aes(x = hsize, y = p, fill = use.age.bracket)) +
  
  plot.theme +
  theme.hist +
  theme(
    axis.text = blank
  ) +
  
  geom_col() +
  coord_polar(theta = "y") +
  xlim(c(2, hsize + 0.5)) +
  
  geom_text(aes(label = str_c(p, "%")),
            position = position_stack(vjust = 0.5),
            col = "white") +
  
  scale_fill_brewer(palette = "Set1") +
  
  labs(x = blank, y = blank, fill = blank)

ggsave(p.age.use, filename = "output/plots/01_age-use_donut.png", height = 11, width = 11, units = "cm")

# geom_text(aes(label = str_c(round(p, 0), "%")),
#           position = position_stack(vjust = 0.5),
#           col = c("black")) +
  


# k6, STAXI, DDDI


#### k6 ####
p.k6 <- ma.final %>% 
  ggplot(aes(x = k6.total)) +
  plot.theme +
  theme.hist +
  geom_histogram(binwidth = 1, col = "black", fill = "#4575b4") +
  geom_vline(data = summarise(ma.final, mean = mean(k6.total)), 
             aes(xintercept = mean),
             linetype = "dashed",
             size = 1,
             col = swin.red) +
  scale_y_continuous(expand = expansion(0.01)) +
  labs(x = blank, y = blank,
       title = "Kessler Psychological Distress Scale")



#### Peak Use ####
p.use <- ma.final %>% 
  left_join(select(ma.df, id, ma.use.peak)) %>% 
  mutate(ma.use.peak = factor(ma.use.peak, levels = c("1 to 2 times per month", "Weekly", "Daily"))) %>% 
  count(ma.use.peak) %>% 
  mutate(p = n/sum(n)) %>% 

  ggplot(aes(x = 4, y = p, fill = ma.use.peak)) +
  plot.theme +
  theme(
    axis.line = blank,
    axis.text.y = blank,
    axis.ticks.y = blank
  ) +
  geom_col(position = position_stack()) +
  coord_flip()+
  geom_text(aes(label = str_c(round(p * 100, 0), "%")),
            position = position_stack(vjust = 0.5),
            col = "white") +

  scale_fill_manual(values = c("#74a9cf", "#2b8cbe", "#045a8d")) +
  scale_y_continuous(labels = scales::percent) +
  
  labs(x = blank, y = blank, fill = blank)

ggsave(p.use, filename = "output/plots/01_peak-use_bar.png", height = 4, width = 15, units = "cm")



#### STAXI ####
p.staxi <- ma.final %>% 
  # left_join(select(staxi.df, id, state.total)) %>% 
  # pivot_longer(cols = c(trait.total, state.total), names_to = "group") %>% 
  ggplot(aes(x = trait.total)) +
  plot.theme +
  theme(
    axis.line = blank,
    axis.text.y = blank,
    axis.ticks.y = blank,
  ) + 
  
  geom_histogram(binwidth = 1, col = "black", fill = .blue, position = "identity") +
  geom_vline(data = summarise(ma.final, mean = mean(trait.total)), aes(xintercept = mean),
             linetype = "dashed", size = 1, col = .red) +
  scale_y_continuous(expand = expansion(0.01)) +
  labs(x = blank, y = blank)
  
ggsave(p.staxi, filename = "output/plots/01_staxi_hist.png", height = 9, width = 12, units = "cm")

#### DDDI ####
p.dd <- ma.final %>% 
  ggplot(aes(x = dd.total)) +
  plot.theme +
  theme(
    axis.line = blank,
    axis.text.y = blank,
    axis.ticks.y = blank
  ) +
  
  geom_histogram(binwidth = 2, col = "black", fill = .red) +
  geom_vline(data = summarise(ma.final, mean = mean(dd.total)), aes(xintercept = mean),
             col = .blue, linetype = "dashed", size = 1) +
  scale_y_continuous(expand = expansion(0.01)) +
  labs(x = blank, y = blank)

ggsave(p.dd, filename = "output/plots/01_dddi_hist.png", height = 9, width = 12, units = "cm")


# Correlation Plots -------------------------------------------------------


p.cor.1 <- ma.final %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(cols = c(age, audit.total, sds.total, k6.total, trait.total)) %>%
  mutate(name = factor(name) %>% 
         fct_recode("Age" = "age",
                    "AUDIT-C" = "audit.total", 
                    "SDS" = "sds.total",
                    "K6" = "k6.total",
                    "Trait Anger" = "trait.total")) %>% 
  filter(name %in% c("Age", "K6")) %>% 
  ggplot(aes(x = value, y = dd.total, col = name, fill = name)) +
  
  theme_light() +
  theme(
    legend.position = "none",
    
    panel.background = element_rect("#fdf6e3"),
    panel.grid.major = blank,
    panel.grid.minor.x = blank,
    
    strip.background = blank,
    strip.text = element_text(colour = "black", size = 12)
  ) +
  scale_color_brewer(palette = "Set2") +
  
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", alpha = 0.1, col = swin.red,
              size = 0.5) +
  
  facet_wrap(~name, scales = "free") +
  labs(x = blank, y = "DDDI Score", col = "Predictor")

ggsave(p.cor.1, filename = "output/plots/01_point_age-sex.png", height = 12, width = 20, units = "cm")  


p.cor.2 <- ma.final %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(cols = c(age, audit.total, sds.total, k6.total, trait.total)) %>%
  mutate(name = factor(name) %>% 
           fct_recode("Age" = "age",
                      "AUDIT-C" = "audit.total", 
                      "SDS" = "sds.total",
                      "K6" = "k6.total",
                      "Trait Anger" = "trait.total")) %>% 
  filter(name %in% c("AUDIT-C", "SDS", "Trait Anger")) %>% 
  ggplot(aes(x = value, y = dd.total, fill = name, col = name)) +
  
  theme_light() +
  theme(
    legend.position = "none",
    
    panel.background = element_rect("#fdf6e3"),
    panel.grid.major = blank,
    panel.grid.minor.x = blank,
    
    strip.background = blank,
    strip.text = element_text(colour = "black", size = 12)
  ) +
  # scale_color_brewer(palette = "Set2") +
  
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", alpha = 0.1, col = swin.red,
              size = 0.5) +
  
  facet_wrap(~name, scales = "free") +
  labs(x = blank, y = "DDDI Score", col = "Predictor")

ggsave(p.cor.2, filename = "output/plots/01_point_audit-sds-trait.png", height = 15, width = 30, units = "cm")


#### Error Bars ####
p.error <- ma.final %>% 
  select(where(is.factor), dd.total) %>% 
  pivot_longer(cols = c(sex, education, area.live)) %>% 
  mutate(name = factor(name) %>% 
           fct_recode("Sex" = "sex",
                      "Education" = "education",
                      "Residential Area" = "area.live")) %>% 
  
  ggplot(aes(x = value, y = dd.total, fill = value)) +
  theme_light() +
  theme(
    legend.position = "none",
    
    panel.background = element_rect("#fdf6e3"),
    panel.grid.major = blank,
    panel.grid.minor.x = blank,
    
    strip.background = blank,
    strip.text = element_text(colour = "black", size = 12)
  ) +
  
  scale_fill_brewer(palette = "Paired") +
  
  geom_boxplot() + 
  coord_flip() +
  facet_wrap(~name, scales = "free", ncol = 1) +
  
  labs(x = blank, y = "DDDI Score") 

ggsave(p.error, filename = "output/plots/01_error-bar_IVs.png",
       height = 20, width = 25, units = "cm")





  pivot_longer(cols = c(age, audit.total, sds.total, k6.total, trait.total)) %>%
  mutate(name = factor(name) %>% 
           fct_recode("Age" = "age",
                      "AUDIT-C" = "audit.total", 
                      "SDS" = "sds.total",
                      "K6" = "k6.total",
                      "Trait Anger" = "trait.total")) %>% 
  filter(name %in% c("AUDIT-C", "SDS", "Trait Anger")) %>% 
  ggplot(aes(x = value, y = dd.total, fill = name, col = name)) +
  
  theme_light() +
  theme(
    legend.position = "none",
    
    panel.background = element_rect("#fdf6e3"),
    panel.grid.major = blank,
    panel.grid.minor.x = blank,
    
    strip.background = blank,
    strip.text = element_text(colour = "black", size = 12)
  ) +
  # scale_color_brewer(palette = "Set2") +
  
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", alpha = 0.1, col = swin.red,
              size = 0.5) +
  
  facet_wrap(~name, scales = "free") +
  labs(x = blank, y = "DDDI Score", col = "Predictor")

ggsave(p.cor.2, filename = "output/plots/01_point_audit-sds-trait.png", height = 15, width = 30, units = "cm")


# Model Selection ---------------------------------------------------------

#### Model Selection ####
p.model <- tibble(
  model = c(1, 2, 3, 4, 5, 6, 7, 8),
  # AIC = AIC(m1, m2, m3, m4, m5, m6, m7, m8)$AIC,
  BIC = BIC(m1, m2, m3, m4, m5, m6, m7, m8)$BIC,
  AdjR2 = c(m1.summ$adj.r.squared, m2.summ$adj.r.squared, m3.summ$adj.r.squared, m4.summ$adj.r.squared, m5.summ$adj.r.squared, m6.summ$adj.r.squared,
            m7.summ$adj.r.squared, m8.summ$adj.r.squared)
) %>% 
  pivot_longer(cols = c(BIC, AdjR2), names_to = "Test") %>% 
  ggplot(aes(x = model, y = value)) +
  theme_light() +
  theme(
    
  ) +
  
  geom_point(size = 2) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 8, 1)) +
  # stat_summary()
  facet_wrap(~Test, scales = "free", nrow = 2) +
  labs(x = "Model", y = blank) 

ggsave(p.model, filename = "output/plots/01_model-selection_point.png", height = 15, width = 18, units = "cm")

#### Final Model ####
ma.final %>% 
  ggplot(aes(x = dd.total, y = m3$fitted.values)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(breaks = seq(40, 120, 20))

# Comparison Tables -------------------------------------------------------
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}




#### Demographics ####
label(dems.df$license.status) <- "License Status"
label(dems.df$age) <- "Age"
label(dems.df$sex) <- "Sex"
dems.df$education <- factor(dems.df$education, levels = c(
                              "Did not finish high school",
                            "High School Diploma",
                            "Vocational/Technical degree or certificate",
                            "Did not finish University",
                            "Bachelor Degree",
                            "Postgraduate Degree"
                            ))
label(dems.df$education) <- "Education"
dems.df$employment.status <- factor(dems.df$employment.status, levels = c(
"Unemployed not looking for work",
"Unemployed looking for work",
"Student",
"Employed part time",
"Employed full time",
"Homemaker",
"Retired"
))
label(dems.df$employment.status) = "Employment Status"
dems.df$area.live <- factor(dems.df$area.live, levels = c(
  "Urban/Inner-city",
  "Suburban",
  "Rural"
))
label(dems.df$area.live) <- "Home Location"
label(dems.df$alcohol.ever) <- "Ever Used Alcohol?"


dems.tbl <- table1(~ license.status + age + sex + education + employment.status +
         area.live + alcohol.ever | ma.ingest, data = filter(dems.df, id %in% c(ma.id, n.ma.id)),
       overall = FALSE, extra.col = list(`P-value` = pvalue))

#### Ma Final Characteristics ####
table1(~ age + sex + education + area.live + audit.total + sds.total + dependent + k6.total + trait.total + dd.total, data = ma.final)

#### MA dems table ####
table1(~ ma.use.peak + ma.use.age, data = ma.dems)

#### MA Final distribution ####
# Demographics
p1 <- ma.dems %>% 
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 1, col = "black") +
  labs(title = "Age", x = "", y = "")

# Use proportion
p2 <- ma.dems %>% 
  ggplot(aes(x = sex)) +
  geom_bar(col = "black") +
  labs(title = "Sex", x = "") +
  coord_flip()

p3 <- ma.dems %>% 
  ggplot(aes(x = education)) +
  geom_bar(col = "black") +
  labs(title = "Education", x= "") +
  coord_flip()

p4 <- ma.dems %>% 
  ggplot(aes(x = area.live)) +
  geom_bar(col = "black") +
  labs(title = "Residential Area", x = "") +
  coord_flip()

(p1 + p2) / (p3 + p4)

#### Assessments ####

summ.prep <- summ.df %>% 
  filter(id %in% c(ma.id, n.ma.id)) %>%
  select(id, ma.ingest, k6.total, sds.total, audit.total, state.total, trait.total, dd.total, dui.att.total, dui.strat.total, duid.att.total, duid.strat.total)

label(summ.prep$k6.total) <- "K6 Total Score"
label(summ.prep$audit.total) <- "AUDIT-C Score"
label(summ.prep$state.total) <- "STAXI State Score"
label(summ.prep$trait.total) <- "STAXI Trait Score"
label(summ.prep$dd.total) <- "Dangerous Driving Score (DDDI)"
label(summ.prep$dui.att.total) <- "DUI Attitudes Score"
label(summ.prep$dui.strat.total) <- "DUI Strategies Score"
label(summ.prep$duid.att.total) <- "DUID Attitudes Score"
label(summ.prep$duid.strat.total) <- "DUID Strategies Score"


ass.tbl <- table1(~ k6.total + audit.total + state.total + trait.total + dd.total + dui.att.total + dui.strat.total + duid.att.total |
         ma.ingest, data = summ.prep,
       overall = FALSE, extra.col = list(`P-value` = pvalue))

summ.prep %>% 
  count(duid.strat.total) %>% view


# Plots -------------------------------------------------------------------


# Mean lines would be cool
ass.plot <- summ.prep %>% 
  pivot_longer(cols = -c(id, ma.ingest), names_to = "assessment") %>% 
  group_by(ma.ingest) %>% 
  # filter(assessment == "dd.total") %>% 
  ggplot(aes(x = value, fill = ma.ingest)) + 
  geom_density(alpha = 0.6) +
  # geom_vline(aes(xintercept = mean(value))) +
  facet_wrap(~assessment, scales = "free")





n#### OPTIONAL - create separate file containing output objects
if(FALSE){
output.objects <- ls()[!ls() %in% old.objects & ls() != "old.objects"]
output.objects

save(tmp, file = "objects/output-objects.RData")

}

save.image("objects/all-objects.RData")



