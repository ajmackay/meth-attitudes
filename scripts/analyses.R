# Data Prep ---------------------------------------------------------------

if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}


load("objects/all-objects.RData")

ma.final <- ma.final %>% 
  mutate(sex = factor(sex, levels = c("Male", "Female")))

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
model <- lm(dd.total ~ sex + education + area.live +  age + audit.total + sds.total + k6.total + trait.total, data = ma.final)


# Overall Multicolinearity Diagnostics
omcdiag(model)

# Individual Multicolinearity Diagnostics
imcdiag(model)

## No evidence of multicolinearity



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


# Best Possible Subsets take 2 --------------------------------------------
## So apparently 'leaps' only works with quantitative variables (so not good for our factors...)
### The above may not be true - leaps just automatically converts the categorical variable into dummies so may not be all that bad rly...


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

#### Using dummy variables for education ####
dummy.vars <- fastDummies::dummy_cols(ma.final, select_columns = "education") %>% 
  select(-c(id, education, dd.ne.total, dd.ad.total, dd.rd.total)) 

# Model without dummy education then put into best subsets
model <- lm(dd.total ~ ., data = select(ma.final, model.vars))

# ols_step_all_possible(model)

# Best subsets regression
bs <- ols_step_best_subset(model)

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



# Final model with education dummy variable
final.model.educ <- lm(dd.total ~ ., select(dummy.vars, dd.total, audit.total, sds.total, trait.total, 
                                                       `education_Did not finish High School`,`education_Did not finish University`,
                                                       `education_Highschool/Technical Degree`, `education_University Degree`))
final.model.educ %>% tbl_regression()

final.model <- lm(dd.total ~ ., select(ma.final, dd.total, trait.total, sds.total, audit.total))

summary(final.model)


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




# No dummy
# final.model <- lm(dd.total ~ ., select(ma.final, dd.total, audit.total, sds.total, trait.total, education))

# DDDI Subsets ------------------------------------------------------------
ma.final %>% 
  pivot_longer(cols = c("dd.ad.total", "dd.rd.total", "dd.ne.total"), names_to = "dd.subscale") %>% 
  group_by(dd.subscale) %>% 
  summarise(n())

# Assess difference between subsets means
ma.final %>% 
  pivot_longer(cols = c("dd.ad.total", "dd.rd.total", "dd.ne.total"), names_to = "dd.subscale") %>% 
  group_by(dd.subscale) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            se = sd/sqrt(n())) %>% identity()
  ggplot(aes(x = dd.subscale, y = mean, fill = dd.subscale)) +
  geom_bar(stat = "identity", color = "black",
           position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean+sd), width = .2)

  ma.final %>% 
    pivot_longer(cols = c("dd.ad.total", "dd.rd.total", "dd.ne.total"), names_to = "dd.subscale") %>% 
    ggplot(aes(x = dd.subscale, y = value, fill = dd.subscale)) +
    stat_summary(geom = "errorbar")
  
                 