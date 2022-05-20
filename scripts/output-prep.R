old.objects <- load("objects/all-objects.RData")
packages <- append(packages, c("stargazer", "flextable", "xaringan", "DT"))
librarian::shelf(packages)



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
label(dems.df$education) <- "Education"
dems.df$education <- factor(dems.df$education, levels = c(
                              "Did not finish high school",
                            "High School Diploma",
                            "Vocational/Technical degree or certificate",
                            "Did not finish University",
                            "Bachelor Degree",
                            "Postgraduate Degree"
                            ))
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
dems.df$ma.ingest

#### sort out dems table ####
dems.tbl

table1(~ license.status + age + sex + education + employment.status +
         area.live + alcohol.ever | ma.ingest, data = filter(dems.df, id %in% c(ma.id, n.ma.id)),
       overall = FALSE, extra.col = list(`P-value` = pvalue)) %>% 
  as.data.frame() %>% datatable()




#### OPTIONAL - create separate file containing output objects
if(FALSE){
output.objects <- ls()[!ls() %in% old.objects & ls() != "old.objects"]
output.objects

save(tmp, file = "objects/output-objects.RData")

}

save.image("objects/all-objects.RData")



