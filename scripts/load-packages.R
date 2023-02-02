packages <- c("janitor", 
              "table1", 
              "gt", 
              "naniar", 
              "kableExtra", 
              "GGally", 
              "mctest",
              "stargazer", 
              "flextable", 
              "xaringan", 
              "DT", 
              "patchwork",  
              "tidyverse",
              "olsrr", 
              "broom", 
              "gtsummary", 
              "officer", 
              "shadowtext", 
              "rstatix", # For ANOVA test and quick summary stats
              "ggpubr", # Publishable graphs
              "gdtools" # For font name
              )

librarian::shelf(packages)

message(crayon::green("Packages Loaded Successfully"))
