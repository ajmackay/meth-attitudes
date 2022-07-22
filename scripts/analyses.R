packages <- append(packages, c("GGally", "mctest", "leaps"))
librarian::shelf(packages)


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
models <- regsubsets(dd.total ~ ., nvmax = 10, data = select(ma.final, -id))

models.summ <- summary(models)

tibble(
  Adj.R2 = which.max(models.summ$adjr2),
  CP = which.min(models.summ$cp),
  BIC = which.min(models.summ$bic)
)

m3 <- lm(dd.total ~ ., data = select(ma.final, audit.total, sds.total, trait.total, dd.total))
m4 <- lm(dd.total ~ ., data = select(ma.final, education, audit.total, sds.total, trait.total, dd.total))
m5 <- lm(dd.total ~ ., data = select(ma.final, dd.total, education, audit.total, sds.total, trait.total, k6.total))
m6 <- lm(dd.total ~., data = select(ma.final, dd.total, education, audit.total, sds.total, trait.total, k6.total, area.live))

m3.summ <- summary(m3)
m4.summ <- summary(m4)
m5.summ <- summary(m5)
m6.summ <- summary(m6)


tibble(
  model = c(3, 4, 5, 6),
  AIC = AIC(m3, m4, m5, m6)$AIC,
  BIC = BIC(m3, m4, m5, m6)$BIC,
  AdjR2 = c(m3.summ$adj.r.squared, m4.summ$adj.r.squared, m5.summ$adj.r.squared, m6.summ$adj.r.squared)
)

# I guess it's better to go with less predictors to avoid over-fitting? Is that how it works?


