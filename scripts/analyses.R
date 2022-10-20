if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}


load("objects/all-objects.RData")

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
models <- regsubsets(dd.total ~ ., nvmax = 10, data = select(ma.final, -id), nbest = 1)

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


# DDDI Subsets ------------------------------------------------------------
ma.final %>% 
  pivot_longer(cols = c("dd.ad.total", "dd.rd.total", "dd.ne.total"), names_to = "dd.subscale") %>% 
  group_by(dd.subscale) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            se = sd/sqrt(n())) %>% 
  ggplot(aes(x = dd.subscale, y = mean, fill = dd.subscale)) +
  geom_bar(stat = "identity", color = "black",
           position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean+sd), width = .2)

