library(table1)
library(gt)
library(plotly)
library(naniar)


# Response Numbers --------------------------------------------------------

#### Screening ####
screen.sum.df <- tibble(
  total.n = nrow(survey.raw),
  total.not.spam = filter(survey.raw, status != "Spam", status != "Survey Preview") %>% nrow(),
  total.full.license = dems.df %>% 
    filter(str_detect(license.screen, "Full license")) %>% nrow(),
  total.ma = dems.df %>% 
    filter(ma.ingest == "Yes") %>% nrow(),
  total.ma.sds = nrow(ma.df),
  total.not.ma = dems.df %>% 
    filter(ma.ingest == "No" | is.na(ma.ingest)) %>% nrow(),
  total.ma.dd = filter(dd.df, id %in% ma.id, keep.dd) %>% nrow(),
  recent.response = survey.raw %>% 
    filter(status != "Spam" & finished & str_detect(q126.13, "Full license")) %>% 
    summarise(max.enddate = max(enddate)) %>% pull()
) 



screen.plot <- screen.sum.df %>% 
  select(-recent.response) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(reorder(name, desc(value)), y = value)) +
  geom_col() +
  theme_minimal() +
  labs(x = "", y = "") +
  scale_x_discrete(labels = c("Total", "Not Spam", "Full License", "No Drug", "MA User", "MA User with SDS", 
                              "MA User with DDDI"))


#### Breakdown ####
left_join(dems.df, dd.df) %>% 
  group_by(ma.ingest) %>% 
  count(keep.dd)




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


save.image(file = "objects/all-objects.RData")


