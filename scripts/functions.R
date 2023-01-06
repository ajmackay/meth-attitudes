#### Format p ####
format.p <- function(dat){
  if(!any(str_detect(class(dat), "tbl"))){
    
    as_tibble(dat) %>% 
      mutate(p = scales::pvalue(p))
  }
}

# format.p(anova.tbl)

# any(str_detect(class(anova.tbl), "data.frame"))

#### Stop Quietly ####
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

#### Margin of error calculator ####
margin.error <- function(level = 0.975, sd, n){
  # browser()
  qnorm(level) * sd / sqrt(n)
}



#### Save Objects ####
save.objects <- function() save.image(file = "objects/all-objects.RData")


#### Effect Size Calculator ####
lm.effect.size <- function(dat, iv, dv){
  # browser()
  part.cor <- ppcor::spcor(select(dat, !!enquo(iv), !!enquo(dv)))
  partial.cor <- ppcor::pcor(select(dat, !!enquo(iv), !!enquo(dv)))
  
  message(str_c("Dependent Variable: ", dv))
  
  tibble(
    Variable = c(iv, dv),
    Part = data.frame(part.cor$estimate) %>% pull(!!enquo(dv)),
    Partial = data.frame(partial.cor$estimate) %>% pull(!!enquo(dv)),
    F2 = (Part^2) / (1 - Part^2)
  ) %>% 
    filter(Variable != dv)
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







# Table -------------------------------------------------------------------
# https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html#example-a-column-of-p-values


