# Data Prep ---------------------------------------------------------------

if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

source("scripts/functions.R")

load("objects/all-objects.RData")

# Best Possible Subsets ---------------------------------------------------
# Variables to include in model
model.vars <- c(
  "age",
  "sex",
  "education",
  "area.live",
  "audit.total",
  "sds.total",
  "k6.total",
  "trait.total",
  # "state.total",
  "dd.total"
)

#### Using dummy variables for education ####
# dummy.vars <- fastDummies::dummy_cols(ma.final, select_columns = "education") %>% 
#   select(-c(id, education, dd.ne.total, dd.ad.total, dd.rd.total)) 

model <- lm(dd.total ~ ., data = select(ma.final, all_of(model.vars)))

# Best possible based on adj R2
all.poss <- ols_step_all_possible(model)

best.poss <- all.poss %>% 
  group_by(n) %>% 
  arrange(n, desc(adjr)) %>% 
  slice(1) %>% 
  select(n, predictors, rsquare, adjr, aic)

## All 4 selection criteria
# p.subset.selection <- all.poss %>% 
#   group_by(n) %>% 
#   arrange(n, desc(adjr)) %>% 
#   slice(1) %>% 
#   pivot_longer(cols = c(rsquare, adjr, aic, sbic), names_to = "measure") %>% 
#   ggplot(aes(x = n, y = value)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~measure, scales = "free")

## Adjusted R2 and AIC
p.subset.comparison <- best.poss %>% 
  pivot_longer(cols = c(adjr, aic), names_to = "measure") %>% 
  
  ggplot(aes(x = n, y = value)) +
  geom_point(size = 2) +
  geom_line() +
  
  scale_x_continuous(breaks = seq(1, 8)) +
  labs(y = element_blank(),
       x = "N") +
  
  facet_wrap(~measure, scales = "free", ncol = 2,
             labeller = as_labeller(c(
               `adjr` =  "Adjusted R2", 
               `aic` =  "AIC")))

final.model <- lm(dd.total ~ ., select(ma.final, dd.total, trait.total, sds.total, audit.total))

#### Effect Sizes ####
final.model.df <- ma.final %>% 
  select(dd.total, audit.total, sds.total, trait.total)

final.model.effects <- lm.effect.size(final.model.df, iv = c("audit.total", "sds.total", "trait.total"), 
               dv = "dd.total")


# DDDI Subsets ------------------------------------------------------------
subscales.df <- ma.final %>% 
  pivot_longer(cols = c("dd.ad.total", "dd.rd.total", "dd.ne.total"), names_to = "dd.subscale") %>% 
  select(id, dd.subscale, value)

summ.dd.subscale <- ma.final %>% 
  pivot_longer(cols = c("dd.ad.total", "dd.rd.total", "dd.ne.total"), names_to = "dd.subscale") %>% 
  group_by(dd.subscale) %>% 
  get_summary_stats(value, type = "mean_ci") %>% 
  mutate(lower = mean - ci,
         upper = mean + ci)
  # summarise(mean = mean(value),
  #           sd = sd(value),
  #           se = sd/sqrt(n()),
  #           error = qt(0.975, df = n() - 1) * se,
  #           lower = mean - error,
  #           upper = mean + error)


# Assess difference between subsets means
#### ANOVA ####
m.anova <- anova_test(data = subscales.df, dv = value, wid = id, within = dd.subscale)

# m.aov <- aov(value~factor(dd.subscale) + Error(factor(id)), data = anova.prep)

#### Post-hoc tests ####
# Pairwise comparisons
pwc <- subscales.df %>% 
  pairwise_t_test(
    value ~ dd.subscale, paired = TRUE,
    p.adjust.method = "bonferroni"
  ) %>% 
  mutate(p = scales::pvalue(p),
         p.adj = scales::pvalue(p.adj, add_p = TRUE))


# Boxplots
# Adding coordinate positions for boxplot
pwc <- pwc %>% add_xy_position(x = "dd.subscale")

# Boxplot with p value
p.subscales <- subscales.df %>% 
  mutate(dd.subscale = factor(dd.subscale, levels = c("dd.ad.total", "dd.ne.total", "dd.rd.total"))) %>% 
  
  ggboxplot(x = "dd.subscale", y = "value", width = 0.5) +
  stat_pvalue_manual(
    pwc, label = "p.adj",
    y.position = c(45, 50, 57),
    size = 3
  ) 



subscales.df %>% 
  mutate(dd.subscale = factor(dd.subscale, levels = c("dd.ad.total", "dd.ne.total", "dd.rd.total"))) %>% 
  
  ggboxplot(x = "dd.subscale", y = "value") +
  stat_pvalue_manual(
    pwc, label = "p.adj",
    y.position = c(45, 50, 57)
  ) 
  
 


ma.final %>% 
  pivot_longer(cols = c("dd.ad.total", "dd.rd.total", "dd.ne.total"), names_to = "dd.subscale") %>% 
  group_by(dd.subscale) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            se = sd/sqrt(n()),
            error = qt(0.975, df = n - 1) * se,
            lower = mean - error,
            upper = mean + error) 


#### Multivariate Regression ####
lm.mv <- lm(cbind(dd.ad.total, dd.ne.total, dd.rd.total) ~ audit.total + sds.total + trait.total, data = ma.final)

#### Plots ####

  
##### Bar Chart with error bar #####

  
dd.subset.summ %>% 
  ggplot(aes(x = dd.subscale, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  plot.theme +
  theme(
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_line()
  )

##### Error bar #####
ma.final %>%
  pivot_longer(cols = c("dd.ad.total", "dd.rd.total", "dd.ne.total"), names_to = "dd.subscale") %>%
  ggplot(aes(x = dd.subscale, y = value, fill = dd.subscale)) +
  stat_summary(geom = "errorbar") +
  scale_y_continuous(breaks = seq(10, 30, 1))


# 



# Assumptions -------------------------------------------------------------
#### Multicolinearity ####
# Correlation matrix
if(FALSE){
ma.final %>% 
  select(names(final.model.df)) %>% 
  ggpairs()
}

#### Multicolinearity ####
model <- lm(dd.total ~ ., data = final.model.df)


# Overall Multicolinearity Diagnostics
mctest::omcdiag(model)

# Individual Multicolinearity Diagnostics
mctest::imcdiag(model)

save.objects()


# Archive -----------------------------------------------------------------
# stop_quietly()
stop("Run next script again")


  
# Best subsets regression (selects based on highest R2 value)
bs <- ols_step_best_subset(model)

plot.subset.selection <- tibble(
  model = bs$n,
  adjr = bs$adjr,
  cp = bs$cp,
  aic = bs$aic,
  sbic = bs$sbic
) %>% 
  pivot_longer(cols = -model) %>% 
  ggplot(aes(x = model, y = value)) +
  geom_point() +
  geom_line() +
  
  scale_x_continuous(breaks = 1:12) +
  
  facet_wrap(name~., scales = "free")




# Regression Analysis -----------------------------------------------------
# All Subsets Regression
model.vars <- c(
  "age",
  "sex",
  "education",
  "area.live",
  "audit.total",
  "sds.total",
  "k6.total",
  "trait.total",
  "dd.total"
)

models <- leaps::regsubsets(dd.total ~ ., data = select(ma.final, all_of(model.vars)), nbest = 1, method = "exhaustive")

models.summ <- summary(models)

tibble(
  Adj.R2 = which.max(models.summ$adjr2),
  CP = which.min(models.summ$cp),
  BIC = which.min(models.summ$bic)
)

m1 <- lm(dd.total ~ ., data = select(ma.final, trait.total, dd.total))
m2 <- lm(dd.total ~ ., data = select(ma.final, trait.total, sds.total, dd.total))
m3 <- lm(dd.total ~ ., data = select(ma.final, audit.total, sds.total, trait.total, dd.total))
m4 <- lm(dd.total ~ ., data = select(ma.final, education, audit.total, sds.total, trait.total, dd.total))
m5 <- lm(dd.total ~ ., data = select(ma.final, dd.total, education, audit.total, sds.total, trait.total, k6.total))
m6 <- lm(dd.total ~., data = select(ma.final, dd.total, education, audit.total, sds.total, trait.total, k6.total, area.live))
m7 <- lm(dd.total ~., data = select(ma.final, dd.total, education, audit.total, sds.total, trait.total, k6.total, area.live, sex))
m8 <- lm(dd.total ~., select(ma.final, dd.total, education, audit.total, sds.total, trait.total, k6.total, area.live, sex, age))
m9 <- lm(dd.total ~., select(ma.final, dd.total, education, audit.total, sds.total, trait.total, k6.total, sex, age))

m1.summ <- summary(m1)
m2.summ <- summary(m2)
m3.summ <- summary(m3)
m4.summ <- summary(m4)
m5.summ <- summary(m5)
m6.summ <- summary(m6)
m7.summ <- summary(m7)
m8.summ <- summary(m8)
m9.summ <- summary(m9)


tibble(
  model = c(1, 2, 3, 4, 5, 6, 7, 8),
  AIC = AIC(m1, m2, m3, m4, m5, m6, m7, m8)$AIC,
  BIC = BIC(m1, m2, m3, m4, m5, m6, m7, m8)$BIC,
  AdjR2 = c(m1.summ$adj.r.squared, m2.summ$adj.r.squared, m3.summ$adj.r.squared, m4.summ$adj.r.squared, m5.summ$adj.r.squared, m6.summ$adj.r.squared,
            m7.summ$adj.r.squared, m8.summ$adj.r.squared)
) %>% 
  pivot_longer(cols = c(AIC, BIC, AdjR2), names_to = "Test") %>% 
  ggplot(aes(x = model, y = value)) +
  geom_point(size = 2) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 8, 1)) +
  # stat_summary()
  facet_wrap(~Test, scales = "free", nrow = 3)

# I guess it's better to go with less predictors to avoid over-fitting? Is that how it works?

#### Exploring m3 ####
confint(m3)

source("scripts/functions.R")


# DDDI Total
lm.effect.size(dat = ma.final, 
               iv = c("audit.total", "sds.total", "trait.total"),
               dv = "dd.total")

#### DDDI Subscales ####
# Negative Emotional Driving
m3.ne <- lm(data = ma.final.dd, dd.ne.total ~ audit.total + sds.total + trait.total)

# Agressive Driving
m3.ad <- lm(data = ma.final.dd, dd.ad.total ~ audit.total, sds.total, trait.total)

# Risky Driving
m3.rd <- lm(data = ma.final.dd, dd.rd.total ~ audit.total, sds.total, trait.total)

summary(m3.ne)

IVs <- c("audit.total", "sds.total", "trait.total")

lm.effect.size(ma.final.dd, iv = IVs, dv = "dd.ne.total")
lm.effect.size(ma.final.dd, iv = IVs, dv = "dd.ad.total")
lm.effect.size(ma.final.dd, iv = IVs, dv = "dd.rd.total")




#### Running first model with dummy vars ####
model.dummy <- lm(dd.total ~ ., data = dummy.vars)

bs <- ols_step_best_subset(model.dummy)

tibble(
  model = bs$n,
  adjr = bs$adjr,
  cp = bs$cp,
  aic = bs$aic,
  sbic = bs$sbic
) %>% 
  pivot_longer(cols = -model) %>% 
  ggplot(aes(x = model, y = value)) +
  geom_point() +
  geom_line() +
  
  scale_x_continuous(breaks = 1:12) +
  
  facet_wrap(name~., scales = "free")


final.model2 <- lm(dd.total ~ ., select(model.vars, dd.total, audit.total, sds.total, trait.total, `education_Did not finish High School`))
  
                 

# 
# final.model.educ <- lm(dd.total ~ ., select(dummy.vars, dd.total, audit.total, sds.total, trait.total,
#                                                        `education_Did not finish High School`,`education_Did not finish University`,
#                                                        `education_Highschool/Technical Degree`, `education_University Degree`))
# final.model.educ %>% tbl_regression()


stop("not necessary")
# Model without dummy education then put into best subsets
## UPDATE: lm() automatically converts factor into dummy





# final.model %>% gtsummary::tbl_regression()
if(FALSE){
  final.model %>% tidy(conf.int = TRUE) %>% mutate(across(where(is.numeric), ~round(.x, 2))) %>% 
    write_csv("output/regression/final-model-coef-3vars.csv")
}




# Univariate Statistics ---------------------------------------------------
uni.summ <- ma.final %>% 
  select(age, audit.total:dd.total) %>% 
  pivot_longer(cols = c(age:dd.total), names_to = "variable") %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value))

ma.final %>% 
  select(id, age, audit.total:dd.total) %>% 
  pivot_longer(cols = c(age:dd.total), names_to = "variable") %>% 
  ggplot(aes(x = value)) +
  geom_histogram(col = "black") +
  facet_wrap(~variable, scales = "free", nrow = 2) +
  geom_vline(data = uni.summ, aes(xintercept = mean), linetype = "dashed", col = "red")

#### Correlation Matrix ####
ma.final %>% 
  select(where(is.numeric), -id) %>% 
  ggpairs()

#### Multicolinearity ####
model <- lm(dd.total ~ ., data = select(ma.final, ma.vars))


# Overall Multicolinearity Diagnostics
omcdiag(model)

# Individual Multicolinearity Diagnostics
imcdiag(model)

## No evidence of multicolinearity
