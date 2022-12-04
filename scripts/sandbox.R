# Model including state anger ---------------------------------------------
model.vars2 <- c(
  "age",
  "sex",
  "education",
  "area.live",
  "audit.total",
  "sds.total",
  "k6.total",
  "trait.total",
  "state.total",
  "dd.total"
)

#### Using dummy variables for education ####
model <- lm(dd.total ~ ., data = select(ma.final, model.vars2))

# Best possible based on adj R2
all.poss <- ols_step_all_possible(model)

best.poss <- all.poss %>% 
  group_by(n) %>% 
  arrange(n, desc(adjr)) %>% 
  slice(1) %>% 
  select(n, predictors, rsquare, adjr, aic)

all.poss %>% 
  group_by(n) %>% 
  arrange(n, desc(adjr)) %>% 
  slice(1) %>% 
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




fontname <- "Times New Roman"





final.model2 <- lm(dd.total ~ ., select(ma.final, dd.total, trait.total, sds.total, audit.total)) # Automatically converts education to dummy

final.model2 %>% tbl_regression()



filez <- list.files("data/archive", full.names = TRUE)

x <- map(filez, read_csv)
x.df <- map_dfr(filez, read_csv)
x[[1]]


# Sussing out where qualtrics goes wrong ----------------------------------

x.id <- summ.df %>% filter(is.na(age)) %>% pull(id)

survey.screened %>% filter(id %in% x.id) %>% view





# Tings -------------------------------------------------------------------
summ.df %>% select(id, ma.ingest, dems.full, audit.full, trait.full, dd.full, duid.att.full) %>% 
  filter(!ma.ingest) %>% 
  replace_with_na(replace = list(dems.full = FALSE, audit.full = FALSE, trait.full = FALSE, dd.full = FALSE, duid.att.full = FALSE)) %>%
  filter(duid.att.full) %>% view
  summarise(duid.present = sum(duid.att.full, na.rm = TRUE))

  
dems.df %>% filter(id == 75)





audit.id <- audit.df %>% 
  filter(!is.na(audit.total)) %>% 
  pull(id)

audit.df %>% 
  filter(!audit.full,
         id %in% alcohol.ids)



df <- data.frame(
  x = c(1, 1, 2, 2, 1.5),
  y = c(1, 2, 1, 2, 1.5),
  text = c("banana-left", "bottom-right", "top-left", "top-right", "center")
)
ggplot(df, aes(x, y)) +
  geom_text(aes(label = text))



duid.att.df %>% 
  select(-c(id, duid.att.total)) %>% 
  filter(if_all(.cols = everything(), !is.na(.)))
  filter(across(.cols = everything(), ~!is.na(.)))
  
  
duid.att.df %>% 
  select(-c(id, duid.att.total)) %>% 
  filter(across(.cols = everything(), ~!is.na(.x)))
  
duid.att.df %>% 
  select(-c(id, duid.att.total)) %>% 
  filter(if_all(.cols = everything(), ~!is.na(.x)))



# Create column that counts the number of NAs across columns per observation
duid.att.df %>% 
  rowwise() %>% 
  mutate(count.na = sum(is.na(.))) %>% relocate(count.na)

df %>% filter_at(vars(type,company),all_vars(!is.na(.)))

a<-as.data.frame(c(2000:2005))
a$Col1<-c(1:5, NA)
a$Col2<-seq(2,12,2)

colnames(a)<-c("year","Col1","Col2")

for (i in 1:2){
  a[[paste("Var_", i, sep="")]]<-i*a[[paste("Col", i, sep="")]]
}

a %>%
  mutate(Total = select(., Var_1:Var_2) %>% rowSums(na.rm = TRUE))


# STAXI -------------------------------------------------------------------



%>% 
  map_df(., ~sum(is.na(.))) %>% view()


survey.screened %>% 
  rename_with(~str_c("state.", seq(1, 14)),
              .cols = q146.1:q146.14) %>% 
  mutate(across(starts_with("state."), ~case_when(
    .x == "Not at all" ~ 1,
    .x == "Somewhat" ~ 2,
    .x == "Moderately so" ~ 3,
    .x == "Very much so" ~ 4)),
    state.total =  %>% 
  select(starts_with("state.")) %>% 
  map_df(., ~sum(is.na(.)))

S_Ang_F_1 = as.numeric(fct_recode(as_factor(Q146_1),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_F_2 = as.numeric(fct_recode(as_factor(Q146_2),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_F_3 = as.numeric(fct_recode(as_factor(Q146_3),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_V_1 = as.numeric(fct_recode(as_factor(Q146_4),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_P_1 = as.numeric(fct_recode(as_factor(Q146_5),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_F_4 = as.numeric(fct_recode(as_factor(Q146_6),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_P_2 = as.numeric(fct_recode(as_factor(Q146_7),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_P_3 = as.numeric(fct_recode(as_factor(Q146_8),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_V_2 = as.numeric(fct_recode(as_factor(Q146_9),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_F_5 = as.numeric(fct_recode(as_factor(Q146_10),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_P_4 = as.numeric(fct_recode(as_factor(Q146_11),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_V_3 = as.numeric(fct_recode(as_factor(Q146_12),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_V_4 = as.numeric(fct_recode(as_factor(Q146_13),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_P_5 = as.numeric(fct_recode(as_factor(Q146_14),
                                  "1" = "Not at all",
                                  "2" = "Somewhat",
                                  "3" = "Moderately so",
                                  "4" = "Very much so")
),
S_Ang_F_Total = (S_Ang_F_1 + S_Ang_F_2 + S_Ang_F_3 + S_Ang_F_4 + S_Ang_F_5),
S_Ang_P_Total = (S_Ang_P_1 + S_Ang_P_2 + S_Ang_P_3 + S_Ang_P_4 + S_Ang_P_5),
S_Ang_V_Total = (S_Ang_V_1 + S_Ang_V_2 + S_Ang_V_3 + S_Ang_V_4), 
S_Ang_Total = (S_Ang_F_1 + S_Ang_F_2 + S_Ang_F_3 + S_Ang_F_4 + S_Ang_F_5 + 
                 S_Ang_P_1 + S_Ang_P_2 + S_Ang_P_3 + S_Ang_P_4 + S_Ang_P_5 +
                 S_Ang_V_1 + S_Ang_V_2 + S_Ang_V_3 + S_Ang_V_4),
## Trait Anger
T_Ang_T_1 = as.numeric(fct_recode(as_factor(Q147_1),
                                  "1" = "Almost never",
                                  "2" = "Sometimes",
                                  "2" = "Almost never,Sometimes",
                                  "3" = "Often",
                                  "4" = "Almost always")
),
T_Ang_T_2 = as.numeric(fct_recode(as_factor(Q147_2),
                                  "1" = "Almost never",
                                  "2" = "Sometimes",
                                  "2" = "Almost never,Sometimes",
                                  "3" = "Often",
                                  "3" = "Sometimes,Often",
                                  "4" = "Almost always")
),
T_Ang_T_3 = as.numeric(fct_recode(as_factor(Q147_3),
                                  "1" = "Almost never",
                                  "2" = "Sometimes",
                                  "2" = "Almost never,Sometimes",
                                  "3" = "Often",
                                  "3" = "Sometimes,Often",
                                  "3" = "Almost never,Often",
                                  "4" = "Almost always",
                                  "4" = "Sometimes,Often,Almost always")
),
T_Ang_R_1 = as.numeric(fct_recode(as_factor(Q147_4),
                                  "1" = "Almost never",
                                  "2" = "Sometimes",
                                  "2" = "Almost never,Sometimes",
                                  "3" = "Often",
                                  "3" = "Sometimes,Often",
                                  "4" = "Almost always")
),
T_Ang_R_2 = as.numeric(fct_recode(as_factor(Q147_5),
                                  "1" = "Almost never",
                                  "2" = "Sometimes",
                                  "2" = "Almost never,Sometimes",
                                  "3" = "Often",
                                  "3" = "Almost never,Sometimes,Often",
                                  "4" = "Almost always")
),
T_Ang_T_4 = as.numeric(fct_recode(as_factor(Q147_6),
                                  "1" = "Almost never",
                                  "2" = "Sometimes",
                                  "2" = "Almost never,Sometimes",
                                  "3" = "Often",
                                  "4" = "Almost always",
                                  "4" = "Often,Almost always")
),
T_Ang_1 = as.numeric(fct_recode(as_factor(Q147_7),
                                "1" = "Almost never",
                                "2" = "Sometimes",
                                "2" = "Almost never,Sometimes",
                                "3" = "Often",
                                "3" = "Sometimes,Often",
                                "4" = "Almost always",
                                "4" = "Sometimes,Often,Almost always")
),
T_Ang_R_3 = as.numeric(fct_recode(as_factor(Q147_8),
                                  "1" = "Almost never",
                                  "2" = "Sometimes",
                                  "2" = "Almost never,Sometimes",
                                  "3" = "Often",
                                  "3" = "Almost never,Often",
                                  "4" = "Almost always")
),
T_Ang_2 = as.numeric(fct_recode(as_factor(Q147_9),
                                "1" = "Almost never",
                                "2" = "Sometimes",
                                "2" = "Almost never,Sometimes",
                                "3" = "Often",
                                "4" = "Almost always",
                                "4" = "Often,Almost always")
),
T_Ang_R_4 = as.numeric(fct_recode(as_factor(Q147_10),
                                  "1" = "Almost never",
                                  "2" = "Sometimes",
                                  "2" = "Almost never,Sometimes",
                                  "3" = "Often",
                                  "3" = "Sometimes,Often",
                                  "3" = "Almost never,Sometimes,Often",
                                  "3" = "Almost never,Often",
                                  "4" = "Almost always",
                                  "4" = "Often,Almost always")
),
T_Ang_T_Total = (T_Ang_T_1 + T_Ang_T_2 + T_Ang_T_3 + T_Ang_T_4),
T_Ang_R_Total = (T_Ang_R_1 + T_Ang_R_2 + T_Ang_R_3 + T_Ang_R_4),
T_Ang_Total = (T_Ang_T_1 + T_Ang_T_2 + T_Ang_T_3 + T_Ang_T_4 +
                 T_Ang_R_1 + T_Ang_R_2 + T_Ang_R_3 + T_Ang_R_4 +
                 T_Ang_1 + T_Ang_2)
);



# Instances Driving Under Influence of Drugs (DUID)
DUID_incidence = as_factor(Q114),
DUID_recent = as_factor(Q118),
DUID_recent_drugs = fct_recode(as_factor(Q119), # Should Consolidate levels in a better way
                               "Amphetamine only" = "Amphetamine",
                               "Cocaine only" = "Cocaine",
                               "Marijuana only" = "Marijuana/Cannabis",
                               "Club drugs only" = "Club drugs (e.g ketamine, GHB)",
                               "Hallucinogens only" = "Hallucinogens/psychedelics (e.g acid, LSD, DMT)",
                               "Sedatives only" = "Sedatives/tranquilisers (e.g. Xanax, Valium)"
),
DUID_lifetime = as_factor(Q120),
DUID_lifetime_drugs = Q121, # Consolidate levels
DUID_ongoing = as_factor(ifelse(DUID_recent == "Yes" & DUID_lifetime == "Yes", "Yes", "No")),
DUID_suspended = as_factor(Q122),
DUID_suspended_drugs = Q124, # Consolidate levels
DUID_hurt = as_factor(Q123),
DUID_hurt_drugs = as_factor(Q125),
# Attitudes toward Drug Driving
DUID_attitude_friends = as.numeric(fct_recode(as_factor(Q126...75), # Reverse coded
                                              "1" = "Strongly disagree",
                                              "2" = "Disagree",
                                              "3" = "Somewhat disagree",
                                              "4" = "Neither agree nor disagree",
                                              "5" = "Somewhat agree",
                                              "6" = "Agree",
                                              "7" = "Strongly agree")
),
DUID_attitude_high = as.numeric(fct_recode(as_factor(Q128), # Reverse coded
                                           "1" = "Strongly disagree",
                                           "2" = "Disagree",
                                           "3" = "Somewhat disagree",
                                           "4" = "Neither agree nor disagree",
                                           "5" = "Somewhat agree",
                                           "6" = "Agree",
                                           "7" = "Strongly agree")
),
DUID_attitude_jail = as.numeric(fct_recode(as_factor(Q129),
                                           "1" = "Strongly agree",
                                           "2" = "Agree",
                                           "3" = "Somewhat agree",
                                           "4" = "Neither agree nor disagree",
                                           "5" = "Somewhat disagree",
                                           "6" = "Disagree",
                                           "7" = "Strongly disagree")
),
DUID_attitude_strict = as.numeric(fct_recode(as_factor(Q130),
                                             "1" = "Strongly agree",
                                             "2" = "Agree",
                                             "3" = "Somewhat agree",
                                             "4" = "Neither agree nor disagree",
                                             "5" = "Somewhat disagree",
                                             "6" = "Disagree",
                                             "7" = "Strongly disagree")
),
DUID_attitude_tough = as.numeric(fct_recode(as_factor(Q131),
                                            "1" = "Strongly agree",
                                            "2" = "Agree",
                                            "3" = "Somewhat agree",
                                            "4" = "Neither agree nor disagree",
                                            "5" = "Somewhat disagree",
                                            "6" = "Disagree",
                                            "7" = "Strongly disagree")
),
DUID_attitude_caught = as.numeric(fct_recode(as_factor(Q132), # Reverse coded
                                             "1" = "Strongly disagree",
                                             "2" = "Disagree",
                                             "3" = "Somewhat disagree",
                                             "4" = "Neither agree nor disagree",
                                             "5" = "Somewhat agree",
                                             "6" = "Agree",
                                             "7" = "Strongly agree")
),
DUID_attitude_while = as.numeric(fct_recode(as_factor(Q133), # Reverse coded
                                            "1" = "Strongly disagree",
                                            "2" = "Disagree",
                                            "3" = "Somewhat disagree",
                                            "4" = "Neither agree nor disagree",
                                            "5" = "Somewhat agree",
                                            "6" = "Agree",
                                            "7" = "Strongly agree")
),
DUID_attitude_dumb = as.numeric(fct_recode(as_factor(Q134),
                                           "1" = "Strongly agree",
                                           "2" = "Agree",
                                           "3" = "Somewhat agree",
                                           "4" = "Neither agree nor disagree",
                                           "5" = "Somewhat disagree",
                                           "6" = "Disagree",
                                           "7" = "Strongly disagree")
),
DUID_attitude_overrated = as.numeric(fct_recode(as_factor(Q135), # Reverse coded
                                                "1" = "Strongly disagree",
                                                "2" = "Disagree",
                                                "3" = "Somewhat disagree",
                                                "4" = "Neither agree nor disagree",
                                                "5" = "Somewhat agree",
                                                "6" = "Agree",
                                                "7" = "Strongly agree")
),
DUID_attitude_license = as.numeric(fct_recode(as_factor(Q136),
                                              "1" = "Strongly agree",
                                              "2" = "Agree",
                                              "3" = "Somewhat agree",
                                              "4" = "Neither agree nor disagree",
                                              "5" = "Somewhat disagree",
                                              "6" = "Disagree",
                                              "7" = "Strongly disagree")
),
DUID_attitude_Total = DUID_attitude_caught + DUID_attitude_dumb + DUID_attitude_friends + DUID_attitude_high + DUID_attitude_jail + DUID_attitude_license + DUID_attitude_overrated + DUID_attitude_strict + DUID_attitude_tough + DUID_attitude_while,
DUID_attitude_Mean = DUID_attitude_Total/10,

# Attitudes towards strategies for avoiding Drug Driving: Discrete
DUID_strategy_leave = as.numeric(fct_recode(as_factor(Q137),
                                            "1" = "Definitely yes",
                                            "2" = "Probably yes",
                                            "3" = "Might or might not",
                                            "4" = "Probably not",
                                            "5" = "Definitely not")
),
DUID_strategy_less = as.numeric(fct_recode(as_factor(Q138),
                                           "1" = "Extremely likely",
                                           "2" = "Moderately likely",
                                           "2" = "Slightly likely",
                                           "3" = "Neither likely nor unlikely",
                                           "4" = "Slightly unlikely",
                                           "4" = "Moderately unlikely",
                                           "5" = "Extremely unlikely")
),
DUID_strategy_plan_driver = as.numeric(fct_recode(as_factor(Q139),
                                                  "1" = "Extremely likely",
                                                  "2" = "Moderately likely",
                                                  "2" = "Slightly likely",
                                                  "3" = "Neither likely nor unlikely",
                                                  "4" = "Slightly unlikely",
                                                  "4" = "Moderately unlikely",
                                                  "5" = "Extremely unlikely")
),
DUID_strategy_plan_use = as.numeric(fct_recode(as_factor(Q140),
                                               "1" = "Extremely likely",
                                               "2" = "Moderately likely",
                                               "2" = "Slightly likely",
                                               "3" = "Neither likely nor unlikely",
                                               "4" = "Slightly unlikely",
                                               "4" = "Moderately unlikely",
                                               "5" = "Extremely unlikely")
),
DUID_strategy_taxi = as.numeric(fct_recode(as_factor(Q141),
                                           "1" = "Extremely likely",
                                           "2" = "Moderately likely",
                                           "2" = "Slightly likely",
                                           "3" = "Neither likely nor unlikely",
                                           "4" = "Slightly unlikely",
                                           "4" = "Moderately unlikely",
                                           "5" = "Extremely unlikely")
),
DUID_strategy_track = as.numeric(fct_recode(as_factor(Q142),
                                            "1" = "Extremely likely",
                                            "2" = "Moderately likely",
                                            "2" = "Slightly likely",
                                            "3" = "Neither likely nor unlikely",
                                            "4" = "Slightly unlikely",
                                            "4" = "Moderately unlikely",
                                            "5" = "Extremely unlikely")
),
DUID_strategy_overnight = as.numeric(fct_recode(as_factor(Q143),
                                                "1" = "Extremely likely",
                                                "2" = "Moderately likely",
                                                "2" = "Slightly likely",
                                                "3" = "Neither likely nor unlikely",
                                                "4" = "Slightly unlikely",
                                                "4" = "Moderately unlikely",
                                                "5" = "Extremely unlikely")
),
# Attitudes towards strategies for avoiding Drug Driving: Dichotomous
DUID_strategy_leave_dich = as.numeric(fct_recode(as_factor(Q137),
                                                 "agree" = "Definitely yes",
                                                 "agree" = "Probably yes",
                                                 "disagree" = "Might or might not",
                                                 "disagree" = "Probably not",
                                                 "disagree" = "Definitely not")
),
DUID_strategy_less = as.numeric(fct_recode(as_factor(Q138),
                                           "agree" = "Extremely likely",
                                           "agree" = "Moderately likely",
                                           "agree" = "Slightly likely",
                                           "disagree" = "Neither likely nor unlikely",
                                           "disagree" = "Slightly unlikely",
                                           "disagree" = "Moderately unlikely",
                                           "disagree" = "Extremely unlikely")
),
DUID_strategy_plan_driver = as.numeric(fct_recode(as_factor(Q139),
                                                  "agree" = "Extremely likely",
                                                  "agree" = "Moderately likely",
                                                  "agree" = "Slightly likely",
                                                  "disagree" = "Neither likely nor unlikely",
                                                  "disagree" = "Slightly unlikely",
                                                  "disagree" = "Moderately unlikely",
                                                  "disagree" = "Extremely unlikely")
),
DUID_strategy_plan_use = as.numeric(fct_recode(as_factor(Q140),
                                               "agree" = "Extremely likely",
                                               "agree" = "Moderately likely",
                                               "agree" = "Slightly likely",
                                               "disagree" = "Neither likely nor unlikely",
                                               "disagree" = "Slightly unlikely",
                                               "disagree" = "Moderately unlikely",
                                               "disagree" = "Extremely unlikely")
),
DUID_strategy_taxi = as.numeric(fct_recode(as_factor(Q141),
                                           "agree" = "Extremely likely",
                                           "agree" = "Moderately likely",
                                           "agree" = "Slightly likely",
                                           "disagree" = "Neither likely nor unlikely",
                                           "disagree" = "Slightly unlikely",
                                           "disagree" = "Moderately unlikely",
                                           "disagree" = "Extremely unlikely")
),
DUID_strategy_track = as.numeric(fct_recode(as_factor(Q142),
                                            "agree" = "Extremely likely",
                                            "agree" = "Moderately likely",
                                            "agree" = "Slightly likely",
                                            "disagree" = "Neither likely nor unlikely",
                                            "disagree" = "Slightly unlikely",
                                            "disagree" = "Moderately unlikely",
                                            "disagree" = "Extremely unlikely")
),
DUID_strategy_overnight = as.numeric(fct_recode(as_factor(Q143),
                                                "agree" = "Extremely likely",
                                                "agree" = "Moderately likely",
                                                "agree" = "Slightly likely",
                                                "disagree" = "Neither likely nor unlikely",
                                                "disagree" = "Slightly unlikely",
                                                "disagree" = "Moderately unlikely",
                                                "disagree" = "Extremely unlikely")
),