lm.effect.size <- function(dat, iv, dv){
  # browser()
  part.cor <- ppcor::spcor(select(dat, !!enquo(iv), !!enquo(dv)))
  
  message(str_c("Dependent Variable: ", dv))
  
  tibble(
    variable = c(iv, dv),
    Part = data.frame(part.cor$estimate) %>% pull(!!enquo(dv)),
    F2 = (Part^2) / (1 - Part^2)
  ) %>% 
    filter(variable != dv)
}


# Model comparison

tmp <- function(models){
  map(models, function(models){
    browser()
    summ <- summary(models)
    tibble(
      dv = summ$terms[[2]]
    )
  # tibble(dv = summary(models)$terms[[2]])
    
    
  })
}

tmp(models = c(m3.ad, m3.ne))



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



# Table -------------------------------------------------------------------
# https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html#example-a-column-of-p-values


