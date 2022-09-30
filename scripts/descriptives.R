if(!"packages" %in% ls()){
  source("scripts/load-packages.R")
}

# source("scripts/data-processing.R")
load("objects/all-objects.RData")

ma.names <- c(
   `FALSE` = "Non-Drug Users",
   `TRUE` = "MA Users"
)

## GGplot theme set
theme_set(theme_light())


# Final Particpants -------------------------------------------------------
#### Visualising Missing ####
# Non-drug group
if(FALSE){
summ.df %>% 
  filter(!ma.ingest) %>% 
  select(!ends_with("full")) %>% vis_miss()

# Ma group
summ.df %>% 
  filter(ma.ingest) %>% 
  select(!ends_with("full")) %>% vis_miss()
}

n.ma.id <- summ.df %>% 
  filter(!ma.ingest,
         dems.full,
         audit.full,
         # sds.full,
         trait.full,
         dd.full,
         dui.strat.full,
         # duid.strat.full,
         dui.att.full,
         # duid.att.full
  ) %>% pull(id)

ma.id <- summ.df %>% 
  filter(ma.ingest,
         dems.full,
         audit.full,
         sds.full,
         k6.full,
         trait.full,
         dd.full,
         # dui.strat.full,
         # duid.strat.full,
         # dui.att.full,
         # duid.att.full
  ) %>% pull(id)

# Includes full (relevant) responses
dat <- summ.df %>% 
  filter(id %in% c(ma.id, n.ma.id)) %>% 
  select(-c(ends_with("full")))

# Response Numbers --------------------------------------------------------

#### Screening ####
# OLD
screen.sum.df <- tibble(
  total.n = nrow(survey.raw),
  total.not.spam = filter(survey.raw, status != "Spam", status != "Survey Preview") %>% nrow(),
  total.full.license = dems.df %>% 
    filter(str_detect(license.screen, "Full license")) %>% nrow(),
  total.ma = dems.df %>% 
    filter(ma.ingest) %>% nrow(),
  total.ma.sds = nrow(ma.df),
  total.not.ma = dems.df %>% 
    filter(!ma.ingest) %>% nrow(),
  total.ma.dd = filter(dd.df, id %in% ma.id, dd.full) %>% nrow(),
  recent.response = survey.raw %>% 
    filter(status != "Spam" & finished & str_detect(q126.13, "Full license")) %>% 
    summarise(max.enddate = max(enddate)) %>% pull()
) 



screen.plot <- screen.sum.df %>% 
  select(-recent.response) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(reorder(name, desc(value)), y = value)) +
  geom_col() +
  geom_text(aes(label = value),
            vjust = 1.5,
            color = "white")
  theme_minimal() +
  labs(x = "", y = "") +
  scale_x_discrete(labels = c("Total", "Not Spam", "Full License", "No Drug", "MA User", "MA User with SDS", 
                              "MA User with DDDI"))


#### Breakdown ####
# left_join(dems.df, dd.df) %>% 
#   group_by(ma.ingest) %>% 
#   count(dd.full)



# Summary -----------------------------------------------------------------
# How many MA responses are full
summ.df %>% 
    filter(ma.ingest,
           dems.full,
           audit.full,
           sds.full,
           trait.full,
           dd.full,
           dui.strat.full,
           duid.strat.full,
           dui.att.full,
           k6.full
           # duid.att.full
           ) %>% 
    count()
  
summ.df %>% 
  filter(!ma.ingest,
         dems.full,
         audit.full,
         # sds.full,
         trait.full,
         dd.full,
         duid.att.full) %>% 
  count()

vis_miss(summ.df)
# STAXI -------------------------------------------------------------------
staxi.df %>% 
    group_by(ma.ingest) %>% 
    count(trait.full)



# Demographics ------------------------------------------------------------



# DUID Instances and Attitudes  --------------------------------------------------
## Attitudes

# How many are missing one, two, three, etc.
duid.att.miss.p <- duid.att.df %>% 
  left_join(select(dd.df, id, ma.ingest, dd.full)) %>% 
  mutate(dd.full = na_if(dd.full, FALSE)) %>% # Convert FALSE to NA for missing data vis
  select(-duid.att.total) %>%
  group_by(ma.ingest) %>% 
  miss_case_summary() %>% 
  group_by(ma.ingest, n_miss) %>% 
  count() %>% 
  ggplot(aes(x = n_miss, y = n, fill = ma.ingest)) +
  geom_col() +
  geom_text(aes(label = n),
            hjust = 1.5,
            color = "black") +
  
  scale_x_continuous(breaks = seq(0, 11)) +
  ggtitle("Missing Data for DUID Attitudes") +
    labs(x = "Amount Mising", 
         subtitle = "Amount Missing represents the total number of missing responses within the DUID attitudes questionnaire per observation") +
  coord_flip() +
  facet_wrap(~ma.ingest, labeller = as_labeller(ma.names)) +
  theme(legend.position = "none")

# Visualise observations that 
duid.att.na <- duid.att.df %>% 
  select(-duid.att.total) %>% 
  filter(if_all(.cols = starts_with("duid.att"), all_vars(is.na(.x)))) %>% 
  pull(id)

duid.att.miss.vis.p <- duid.att.df %>% 
  filter(!id %in% duid.att.na,
         is.na(duid.att.total)) %>%
  select(-c(id, duid.att.total)) %>% 
  vis_miss() +
  theme(legend.position = "none") +
  ggtitle("Missing Data (Not missing all)")

duid.att.dems <- left_join(duid.att.df, dems.df)

duid.att.dems.tbl <- table1::table1(~duid.att.total + age + sex + education + employment.status + ethnicity + alcohol.ever | ma.ingest,
               data = left_join(duid.att.dems, select(dd.df, id, ma.ingest, dd.full)) %>%  
                 filter(!is.na(duid.att.total), dd.full)) %>% 
  t1kable()

# dems.tbl <- table1::table1(~age + sex + education + employment.status + ethnicity + alcohol.ever,
#                            data = filter(dems.df, ma.ingest)) %>% 
#   t1kable()


#### Only full observations ####
duid.att.mean <-
  duid.att.df %>% 
  group_by(ma.ingest) %>% 
  summarise(mean.att = mean(duid.att.total, na.rm = TRUE))

dd.mean <- 
  dd.df %>% 
  group_by(ma.ingest) %>% 
  summarise(mean.dd = mean(dd.total, na.rm = TRUE))

duid.att.n <- duid.att.df %>% 
  filter(!is.na(duid.att.total)) %>% 
  group_by(ma.ingest) %>% 
  count()

dd.duid.att.p <- duid.att.df %>% 
  filter(!is.na(duid.att.total)) %>% 
  left_join(dems.df) %>% 
  left_join(select(dd.df, id, dd.total)) %>% 
  group_by(ma.ingest) %>% 
  ggplot(aes(x = duid.att.total, y = dd.total, color = ma.ingest)) +
  geom_vline(data = duid.att.mean, aes(xintercept = mean.att), color = "black") +
  geom_hline(data = dd.mean, aes(yintercept = mean.dd), color = "black", linetype = "dashed") +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.3) +
  facet_grid(ma.ingest~., labeller = as_labeller(ma.names)) +
  
  ggtitle("DDDI Score with DUID Attitudes") +
  labs(x = "Attitudes toward Drug Driving",
       y = "Dula Dangerous Driving Score",
       subtitle = "Full black line = Mean of DUID Attitudes score.  Dotted line = Mean of DDDI score") +
  theme(legend.position = "none")
  # stat_summary()



duid.inst.att.df %>% 
  filter(!is.na(duid.att.total) &
           ma.ingest) %>% 
  ggplot(aes(x = duid.inst.ever, y = duid.att.total)) +
  geom_boxplot() +
  coord_flip()


# Strategies with attitudes to drug driving
duid.strat.df %>% left_join(select(duid.att.df, id, duid.att.total, duid.att.full)) %>% 
  group_by(duid.strat.full, duid.att.full) %>% count()

duid.strat.df %>% left_join(select(duid.att.df, id, duid.att.total)) %>% 
  ggplot(aes(x = duid.att.total, y = duid.strat.total)) +
  geom_point() +
  geom_smooth(method = "lm")

duid.att.df %>% left_join(select(dd.df, id, dd.total)) %>% 
  ggplot(aes(x = duid.att.total, y = dd.total)) +
  geom_point() +
  geom_smooth(method = "lm")


# Strategies --------------------------------------------------------------
duid.strat.df %>% 
  left_join(select(dd.df, id, dd.total, dd.full)) %>% 
  group_by(duid.strat.full, dd.full) %>% 
  count()

duid.strat.df %>% 
  left_join(select(dd.df, id, dd.total)) %>% 
  ggplot(aes(x = dd.total, y = duid.strat.total)) +
  geom_point() +
  geom_smooth(method = "lm")



# AUDIT -------------------------------------------------------------------
# Number who said yes/no to ever drinking who completed the AUDIT
audit.df %>% left_join(select(dems.df, id, ma.ingest, alcohol.ever)) %>% 
  group_by(ma.ingest, alcohol.ever) %>% 
  count(audit.full) %>%
  group_by(ma.ingest) %>% 
  mutate(total.n = sum(n))

# Missing data ------------------------------------------------------------
#### SDS ####
ma.df %>% 
  select(starts_with("sds")) %>% 
  vis_miss()

#### STAXI ####


#### DDDI ####
miss.x <- select(dems.df, id, ma.ingest) %>% left_join(dd.df) %>% 
  filter(ma.ingest == "Yes") %>% 
  vis_miss()


# DDDI --------------------------------------------------------------------
## Exploring subscales
ma.final %>% 
  left_join(select(dd.df, id, dd.ne.total, dd.ad.total, dd.rd.total)) %>% 
  select(audit.total, sds.total, trait.total, dd.ne.total, dd.ad.total, dd.rd.total) %>% 
  ggpairs()
  





# Save Image --------------------------------------------------------------
save.image(file = "objects/all-objects.RData") # Turn into a function or something you can call quickly



