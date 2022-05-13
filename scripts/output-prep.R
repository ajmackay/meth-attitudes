packages <- c(packages, "stargazer")
librarian::shelf(packages)

# Comparison Tables -------------------------------------------------------
#### Demographics ####
# Do like this but better and for word
table1(~ license.status + age + sex + ethnicity + education + employment.status +
         area.live + alcohol.ever | ma.ingest, data = filter(dems.df,
                                                             id %in% c(ma.id, n.ma.id)))

