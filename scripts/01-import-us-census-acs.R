source("scripts/00-setup.R")

# package libraries 
library(here)
library(tidyverse)
library(tidycensus)
library(janitor)


# acs variable tables 
v20_subject <- load_variables(2020, "acs5/subject", cache = TRUE)
v20_profile <- load_variables(2020, "acs5/profile", cache = TRUE)
v20 <- load_variables(2020, "acs5", cache = TRUE)

# get_acs(
#   geography = "zcta",
#   survey = "acs5",
#   variables = c(
#     
#     
#   ),
#   cache_table = TRUE,
#   year = 2020,
#   state = "AZ",
#   county = "Coconino"
# )

# by zip code geography #### 
# zip code of interest 
zip_code_target_list <- c("86001", "86004", "86005", "86015", "86046")

# race and ethnicity ####
acs_race_eth <- get_acs(
  geography = "zcta",
  survey = "acs5",
  variables = c(
    white_not_hisp = "DP05_0077",
    ai_not_hisp = "DP05_0079",
    hisp_any_race = "DP05_0071",
    total = "DP05_0070"
  ),
  cache_table = TRUE,
  year = 2020,
  output = "tidy"
)

# subset to zip code of interest 
(demo_zip_race <- acs_race_eth %>%
  clean_names() %>%
  filter(geoid %in% zip_code_target_list))

demo_zip_race_tab <- demo_zip_race %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>%
  mutate(
    calc_est_white_p = estimate_white_not_hisp / estimate_total,
    calc_est_ai_p = estimate_ai_not_hisp / estimate_total,
    calc_est_hisp_p = estimate_hisp_any_race / estimate_total 
  ) %>%
  select(
    geoid,
    name,
    starts_with("calc_")
  ) %>%
  pivot_longer(
    cols = !c(geoid, name),
    names_to = "variable",
    values_to = "estimate"
  ) 

ggplot() +
  geom_col(
    data = filter(demo_zip_race_tab, geoid != "86001"), 
    mapping = aes(
      x = variable,
      y = estimate,
      group = geoid,
      fill = geoid
    ),
    position = "dodge",
    color = "#2b2d42",
    alpha = 5/5
  ) +
  geom_col(
    data = filter(demo_zip_race_tab, geoid == "86001"),
    mapping = aes(
      x = variable,
      y = estimate
    ),
    fill = "#f4cae4",
    color = "#2b2d42",
    alpha = 2/5
  ) +
  scale_fill_manual(
    values = c(
      "#b3e2cd",
      "#fdcdac",
      "#cbd5e8"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = c(
      "American Indian/Alaska Native",
      "Hispanic/Latino, of any race",
      "White-Not Hispanic/Latino"
    )
  ) +
  labs(
    title = "Race and Ethnicity by Zip Code",
    x = "Race and Ethnicity Group",
    y = "Percentage of Total Population",
    fill = "Zip Code",
    caption = "Source: US Census ACS 5-Year (2016-2020)"
  ) +
  theme_classic()

ggsave(
  filename = "plot-race-by-zip.pdf",
  device = "pdf",
  path = onedrive_path
)

# sex  #### 
acs_sex <- get_acs(
  geography = "zcta",
  survey = "acs5",
  variables = c(
    female = "DP05_0003",
    male = "DP05_0002",
    total = "DP05_0001"
  ),
  cache_table = TRUE,
  year = 2020
)

# subset to zip code of interest 
(demo_zip_sex <- acs_sex %>%
  clean_names() %>%
  filter(geoid %in% zip_code_target_list ))

demo_zip_sex_tab <- demo_zip_sex %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>%
  mutate(
    calc_est_fem_p = estimate_female / estimate_total,
    calc_est_male_p = estimate_male / estimate_total
  ) %>%
  select(
    geoid,
    name,
    starts_with("calc_")
  ) %>%
  pivot_longer(
    cols = !c(geoid, name),
    names_to = "variable",
    values_to = "estimate"
  ) 

ggplot() +
  geom_col(
    data = filter(demo_zip_sex_tab, geoid != "86001"), 
    mapping = aes(
      x = variable,
      y = estimate,
      group = geoid,
      fill = geoid
    ),
    position = "dodge",
    color = "#2b2d42",
    alpha = 5/5
  ) +
  geom_col(
    data = filter(demo_zip_sex_tab, geoid == "86001"),
    mapping = aes(
      x = variable,
      y = estimate
    ),
    fill = "#f4cae4",
    color = "#2b2d42",
    alpha = 2/5
  ) +
  scale_fill_manual(
    values = c(
      "#b3e2cd",
      "#fdcdac",
      "#cbd5e8"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = c(
      "Female",
      "Male"
    )
  ) +
  labs(
    title = "Sex by Zip Code",
    x = "Sex",
    y = "Percentage of Total Population",
    fill = "Zip Code",
    caption = "Source: US Census ACS 5-Year (2016-2020)"
  ) +
  theme_classic()

ggsave(
  filename = "plot-sex-by-zip.pdf",
  device = "pdf",
  path = onedrive_path
)

# age  #### 
acs_age <- get_acs(
  geography = "zcta",
  survey = "acs5",
  variables = c(
    median_age = "DP05_0018",
    age_under5 = "DP05_0005", # age < 5
    'age_over61' = "DP05_0023", # age >= 62
    total = "DP05_0001"
  ),
  cache_table = TRUE,
  year = 2020
)

# subset to zip code of interest 
(demo_zip_age <- acs_age %>%
  clean_names() %>%
  filter(geoid %in% zip_code_target_list ))

demo_zip_age_tab <- demo_zip_age %>%
  filter(variable != "median_age") %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>%
  mutate(
    calc_est_under5 = estimate_age_under5  / estimate_total,
    calc_est_over61 = estimate_age_over61  / estimate_total
  ) %>%
  select(
    geoid,
    name,
    starts_with("calc_")
  ) %>%
  pivot_longer(
    cols = !c(geoid, name),
    names_to = "variable",
    values_to = "estimate"
  ) 

ggplot() +
  geom_col(
    data = filter(demo_zip_age_tab, geoid != "86001"), 
    mapping = aes(
      x = variable,
      y = estimate,
      group = geoid,
      fill = geoid
    ),
    position = "dodge",
    color = "#2b2d42",
    alpha = 5/5
  ) +
  geom_col(
    data = filter(demo_zip_age_tab, geoid == "86001"),
    mapping = aes(
      x = variable,
      y = estimate
    ),
    fill = "#f4cae4",
    color = "#2b2d42",
    alpha = 2/5
  ) +
  scale_fill_manual(
    values = c(
      "#b3e2cd",
      "#fdcdac",
      "#cbd5e8"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = c(
      "62 and over",
      "Under 5"
    )
  ) +
  labs(
    title = "Age by Zip Code",
    x = "Age Group",
    y = "Percentage of Total Population",
    fill = "Zip Code",
    caption = "Source: US Census ACS 5-Year (2016-2020)"
  ) +
  theme_classic()

ggsave(
  filename = "plot-age-by-zip.pdf",
  device = "pdf",
  path = onedrive_path
)

# economic indicators  #### 
acs_economics <- get_acs(
  geography = "zcta",
  survey = "acs5",
  variables = c(
    gini_index = "B19083_001", 
    median_household_income = "DP03_0062",
    average_household_size = "DP02_0016",
    median_family_income = "DP03_0086",
    average_family_size = "DP02_0017",
    below_poverty = "S1701_C02_001", # percent of families and people whose income is below poverty in last 12 months 
    total = "DP05_0001"
  ),
  cache_table = TRUE,
  year = 2020
)

# subset to zip code of interest 
(demo_zip_economics <- acs_economics %>%
    clean_names() %>%
    filter(geoid %in% zip_code_target_list ))

# poverty ####
demo_zip_eco_pov_tab <- demo_zip_economics %>%
  filter(variable == "below_poverty" | variable == "total") %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>%
  mutate(
    calc_est_poverty = estimate_below_poverty  / estimate_total
  ) %>%
  select(
    geoid,
    name,
    starts_with("calc_")
  ) %>%
  pivot_longer(
    cols = !c(geoid, name),
    names_to = "variable",
    values_to = "estimate"
  ) 

ggplot() +
  geom_col(
    data = filter(demo_zip_eco_pov_tab, geoid != "86001"), 
    mapping = aes(
      x = variable,
      y = estimate,
      group = geoid,
      fill = geoid
    ),
    position = "dodge",
    color = "#2b2d42",
    alpha = 5/5
  ) +
  geom_col(
    data = filter(demo_zip_eco_pov_tab, geoid == "86001"),
    mapping = aes(
      x = variable,
      y = estimate
    ),
    fill = "#f4cae4",
    color = "#2b2d42",
    alpha = 2/5
  ) +
  scale_fill_manual(
    values = c(
      "#b3e2cd",
      "#fdcdac",
      "#cbd5e8"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = c(
      "Below Poverty Level"
    )
  ) +
  labs(
    title = "Below Poverty by Zip Code",
    x = "",
    y = "Percentage of Total Population",
    fill = "Zip Code",
    caption = "Source: US Census ACS 5-Year (2016-2020)"
  ) +
  theme_classic()

ggsave(
  filename = "plot-poverty-by-zip.pdf",
  device = "pdf",
  path = onedrive_path
)

# income #### 
demo_zip_income_tab <- demo_zip_economics %>%
  filter(variable == "median_household_income" | variable == "total") %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>%
  mutate(
    calc_est_income = estimate_median_household_income
  ) %>%
  select(
    geoid,
    name,
    starts_with("calc_")
  ) %>%
  pivot_longer(
    cols = !c(geoid, name),
    names_to = "variable",
    values_to = "estimate"
  ) 

ggplot() +
  geom_col(
    data = filter(demo_zip_income_tab, geoid != "86001"), 
    mapping = aes(
      x = variable,
      y = estimate,
      group = geoid,
      fill = geoid
    ),
    position = "dodge",
    color = "#2b2d42",
    alpha = 5/5
  ) +
  geom_col(
    data = filter(demo_zip_income_tab, geoid == "86001"),
    mapping = aes(
      x = variable,
      y = estimate
    ),
    fill = "#f4cae4",
    color = "#2b2d42",
    alpha = 2/5
  ) +
  scale_fill_manual(
    values = c(
      "#b3e2cd",
      "#fdcdac",
      "#cbd5e8"
    )
  ) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_discrete(
    labels = c(
      "Median Household Income"
    )
  ) +
  labs(
    title = "Median Household Income by Zip Code",
    x = "",
    y = "Percentage of Total Population",
    fill = "Zip Code",
    caption = "Source: US Census ACS 5-Year (2016-2020)"
  ) +
  theme_classic()

ggsave(
  filename = "plot-income-by-zip.pdf",
  device = "pdf",
  path = onedrive_path
)

# educational attainment for age >= 25  #### 
acs_edu <- get_acs(
  geography = "zcta",
  survey = "acs5",
  variables = c(
    high_school = "DP02_0062",
    bachelor_deg = "DP02_0065",
    total = "DP02_0059"
  ),
  cache_table = TRUE,
  year = 2020
)

# subset to zip code of interest 
(demo_zip_edu <- acs_edu %>%
    clean_names() %>%
    filter(geoid %in% zip_code_target_list ))

demo_zip_edu_tab <- demo_zip_edu %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>%
  mutate(
    calc_est_hs = estimate_high_school / estimate_total,
    calc_est_bd = estimate_bachelor_deg / estimate_total
  ) %>%
  select(
    geoid,
    name,
    starts_with("calc_")
  ) %>%
  pivot_longer(
    cols = !c(geoid, name),
    names_to = "variable",
    values_to = "estimate"
  ) 

ggplot() +
  geom_col(
    data = filter(demo_zip_edu_tab, geoid != "86001"), 
    mapping = aes(
      x = variable,
      y = estimate,
      group = geoid,
      fill = geoid
    ),
    position = "dodge",
    color = "#2b2d42",
    alpha = 5/5
  ) +
  geom_col(
    data = filter(demo_zip_edu_tab, geoid == "86001"),
    mapping = aes(
      x = variable,
      y = estimate
    ),
    fill = "#f4cae4",
    color = "#2b2d42",
    alpha = 2/5
  ) +
  scale_fill_manual(
    values = c(
      "#b3e2cd",
      "#fdcdac",
      "#cbd5e8"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = c(
      "Bachelor's Degree",
      "High School Diploma/GED"
    )
  ) +
  labs(
    title = "Educational Attainment by Zip Code",
    x = "Education",
    y = "Percentage of Total Population",
    fill = "Zip Code",
    caption = "Source: US Census ACS 5-Year (2016-2020)"
  ) +
  theme_classic()

ggsave(
  filename = "plot-edu-by-zip.pdf",
  device = "pdf",
  path = onedrive_path
)

# save to: C:\Users\rherrera\OneDrive - Coconino County\General - Epidemiology\Epidemiology Projects
onedrive_path <- str_c(
  "C:/Users/rherrera/OneDrive - Coconino County/General - Epidemiology/Epidemiology Projects/Environmental Remediation Grant - Data Request/"
)

write_csv(
  x = demo_zip_age,
  file = str_c(
    onedrive_path,
    "demo_zip_age",
    ".csv",
    sep = ""
  )
)

write_csv(
  x = demo_zip_economics,
  file = str_c(
    onedrive_path,
    "demo_zip_economics",
    ".csv",
    sep = ""
  )
)

write_csv(
  x = demo_zip_edu,
  file = str_c(
    onedrive_path,
    "demo_zip_edu",
    ".csv",
    sep = ""
  )
)

write_csv(
  x = demo_zip_race,
  file = str_c(
    onedrive_path,
    "demo_zip_race",
    ".csv",
    sep = ""
  )
)

write_csv(
  x = demo_zip_sex,
  file = str_c(
    onedrive_path,
    "demo_zip_sex",
    ".csv",
    sep = ""
  )
)

# by census tract geography #### 
# census tract of interest 
tract_list <- c(
  "001201",
  "000100",
  "000200",
  "000800",
  "001103",
  "980200",
  "000900",
  "000301",
  "000302",
  "000700",
  "001000",
  "001104"
) %>%
  paste(collapse = "|")

# race and ethnicity  #### 
acs_race_eth <- get_acs(
  geography = "tract",
  survey = "acs5",
  variables = c(
    white_not_hisp = "DP05_0077",
    ai_not_hisp = "DP05_0079",
    hisp_any_race = "DP05_0071",
    total = "DP05_0070"
  ),
  cache_table = TRUE,
  year = 2020,
  output = "tidy",
  state = "AZ",
  county = "Coconino"
)

# subset to census tract of interest 
(demo_tract_race <- acs_race_eth %>%
    clean_names() %>%
    mutate(
      tract_hit = if_else(
        condition = str_detect(
        string = geoid,
        pattern = tract_list
      ),
      true = 1,
      false = 0
      )
    ) %>%
    filter(tract_hit == 1))

demo_tract_race_tab <- demo_tract_race %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>%
  mutate(
    calc_est_white_p = estimate_white_not_hisp / estimate_total,
    calc_est_ai_p = estimate_ai_not_hisp / estimate_total,
    calc_est_hisp_p = estimate_hisp_any_race / estimate_total 
  ) %>%
  select(
    geoid,
    name,
    starts_with("calc_")
  ) %>%
  pivot_longer(
    cols = !c(geoid, name),
    names_to = "variable",
    values_to = "estimate"
  ) 

ggplot() +
  geom_col(
    data = filter(demo_tract_race_tab, geoid != "04005001201"), 
    mapping = aes(
      x = variable,
      y = estimate,
      group = name,
      fill = name
    ),
    position = "dodge",
    color = "#2b2d42",
    alpha = 5/5
  ) +
  geom_col(
    data = filter(demo_tract_race_tab, geoid == "04005001201"),
    mapping = aes(
      x = variable,
      y = estimate
    ),
    fill = "#f4cae4",
    color = "#2b2d42",
    alpha = 2/5
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = c(
      "American Indian/Alaska Native",
      "Hispanic/Latino, of any race",
      "White-Not Hispanic/Latino"
    )
  ) +
  labs(
    title = "Race and Ethnicity by Census Tract",
    x = "Race and Ethnicity Group",
    y = "Percentage of Total Population",
    fill = "Census Tract",
    caption = "Source: US Census ACS 5-Year (2016-2020)"
  ) +
  theme_classic()

ggsave(
  filename = "plot-race-by-tract.pdf",
  device = "pdf",
  path = onedrive_path
)

# sex  #### 
acs_sex <- get_acs(
  geography = "tract",
  survey = "acs5",
  variables = c(
    female = "DP05_0003",
    male = "DP05_0002",
    total = "DP05_0001"
  ),
  cache_table = TRUE,
  year = 2020,
  state = "AZ",
  county = "Coconino"
)

# subset to census tract of interest 
(demo_tract_sex <- acs_sex %>%
    clean_names() %>%
    mutate(
      tract_hit = if_else(
        condition = str_detect(
          string = geoid,
          pattern = tract_list
        ),
        true = 1,
        false = 0
      )
    ) %>%
    filter(tract_hit == 1))

demo_tract_sex_tab <- demo_tract_sex %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>%
  mutate(
    calc_est_fem_p = estimate_female / estimate_total,
    calc_est_male_p = estimate_male / estimate_total
  ) %>%
  select(
    geoid,
    name,
    starts_with("calc_")
  ) %>%
  pivot_longer(
    cols = !c(geoid, name),
    names_to = "variable",
    values_to = "estimate"
  ) 

ggplot() +
  geom_col(
    data = filter(demo_tract_sex_tab, geoid != "04005001201"), 
    mapping = aes(
      x = variable,
      y = estimate,
      group = name,
      fill = name
    ),
    position = "dodge",
    color = "#2b2d42",
    alpha = 5/5
  ) +
  geom_col(
    data = filter(demo_tract_sex_tab, geoid == "04005001201"),
    mapping = aes(
      x = variable,
      y = estimate
    ),
    fill = "#f4cae4",
    color = "#2b2d42",
    alpha = 2/5
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = c(
      "Female",
      "Male"
    )
  ) +
  labs(
    title = "Sex by Census Tract",
    x = "Sex",
    y = "Percentage of Total Population",
    fill = "Census Tract",
    caption = "Source: US Census ACS 5-Year (2016-2020)"
  ) +
  theme_classic()

ggsave(
  filename = "plot-sex-by-tract.pdf",
  device = "pdf",
  path = onedrive_path
)

# age  #### 
acs_age <- get_acs(
  geography = "tract",
  survey = "acs5",
  variables = c(
    median_age = "DP05_0018",
    age_under5 = "DP05_0005", # age < 5
    'age_over61' = "DP05_0023", # age >= 62
    total = "DP05_0001"
  ),
  cache_table = TRUE,
  year = 2020,
  state = "AZ",
  county = "Coconino"
)

# subset to census tract of interest 
(demo_tract_age <- acs_age %>%
    clean_names() %>%
    mutate(
      tract_hit = if_else(
        condition = str_detect(
          string = geoid,
          pattern = tract_list
        ),
        true = 1,
        false = 0
      )
    ) %>%
    filter(tract_hit == 1))

demo_tract_age_tab <- demo_tract_age %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>%
  mutate(
    calc_est_under5 = estimate_age_under5  / estimate_total,
    calc_est_over61 = estimate_age_over61  / estimate_total
  ) %>%
  select(
    geoid,
    name,
    starts_with("calc_")
  ) %>%
  pivot_longer(
    cols = !c(geoid, name),
    names_to = "variable",
    values_to = "estimate"
  ) 

ggplot() +
  geom_col(
    data = filter(demo_tract_age_tab, geoid != "04005001201"), 
    mapping = aes(
      x = variable,
      y = estimate,
      group = name,
      fill = name
    ),
    position = "dodge",
    color = "#2b2d42",
    alpha = 5/5
  ) +
  geom_col(
    data = filter(demo_tract_age_tab, geoid == "04005001201"),
    mapping = aes(
      x = variable,
      y = estimate
    ),
    fill = "#f4cae4",
    color = "#2b2d42",
    alpha = 2/5
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = c(
      "62 and over",
      "Under 5"
    )
  ) +
  labs(
    title = "Age by Census Tract",
    x = "Age Group",
    y = "Percentage of Total Population",
    fill = "Census Tract",
    caption = "Source: US Census ACS 5-Year (2016-2020)"
  ) +
  theme_classic()

ggsave(
  filename = "plot-age-by-tract.pdf",
  device = "pdf",
  path = onedrive_path
)

# economic indicators  #### 
acs_economics <- get_acs(
  geography = "tract",
  survey = "acs5",
  variables = c(
    gini_index = "B19083_001", 
    median_household_income = "DP03_0062",
    average_household_size = "DP02_0016",
    median_family_income = "DP03_0086",
    average_family_size = "DP02_0017",
    below_poverty = "S1701_C02_001", # percent of families and people whose income is below poverty in last 12 months 
    total = "DP05_0001"
  ),
  cache_table = TRUE,
  year = 2020,
  state = "AZ",
  county = "Coconino"
)

# subset to census tract of interest 
(demo_tract_economics <- acs_economics %>%
    clean_names() %>%
    mutate(
      tract_hit = if_else(
        condition = str_detect(
          string = geoid,
          pattern = tract_list
        ),
        true = 1,
        false = 0
      )
    ) %>%
    filter(tract_hit == 1))

# income #### 
demo_tract_eco_inc_tab <- demo_tract_economics %>%
  filter(variable == "median_household_income" | variable == "total") %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>%
  mutate(
    calc_est_income = estimate_median_household_income
  ) %>%
  select(
    geoid,
    name,
    starts_with("calc_")
  ) %>%
  pivot_longer(
    cols = !c(geoid, name),
    names_to = "variable",
    values_to = "estimate"
  ) 

ggplot() +
  geom_col(
    data = filter(demo_tract_eco_inc_tab, geoid != "04005001201"), 
    mapping = aes(
      x = variable,
      y = estimate,
      group = name,
      fill = name
    ),
    position = "dodge",
    color = "#2b2d42",
    alpha = 5/5
  ) +
  geom_col(
    data = filter(demo_tract_eco_inc_tab, geoid == "04005001201"),
    mapping = aes(
      x = variable,
      y = estimate
    ),
    fill = "#f4cae4",
    color = "#2b2d42",
    alpha = 2/5
  ) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_discrete(
    labels = c(
      "Median Household Income"
    )
  ) +
  labs(
    title = "Median Household Income by Census Tract",
    x = "",
    y = "Percentage of Total Population",
    fill = "Census Tract",
    caption = "Source: US Census ACS 5-Year (2016-2020)"
  ) +
  theme_classic()

ggsave(
  filename = "plot-income-by-tract.pdf",
  device = "pdf",
  path = onedrive_path
)

# poverty #### 
demo_tract_eco_pov_tab <- demo_tract_economics %>%
  filter(variable == "below_poverty" | variable == "total") %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>%
  mutate(
    calc_est_poverty = estimate_below_poverty  / estimate_total
  ) %>%
  select(
    geoid,
    name,
    starts_with("calc_")
  ) %>%
  pivot_longer(
    cols = !c(geoid, name),
    names_to = "variable",
    values_to = "estimate"
  ) 

ggplot() +
  geom_col(
    data = filter(demo_tract_eco_pov_tab, geoid != "04005001201"), 
    mapping = aes(
      x = variable,
      y = estimate,
      group = name,
      fill = name
    ),
    position = "dodge",
    color = "#2b2d42",
    alpha = 5/5
  ) +
  geom_col(
    data = filter(demo_tract_eco_pov_tab, geoid == "04005001201"),
    mapping = aes(
      x = variable,
      y = estimate
    ),
    fill = "#f4cae4",
    color = "#2b2d42",
    alpha = 2/5
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = c(
      "Below poverty level"
    )
  ) +
  labs(
    title = "Below Poverty Level by Census Tract",
    x = "",
    y = "Percentage of Total Population",
    fill = "Census Tract",
    caption = "Source: US Census ACS 5-Year (2016-2020)"
  ) +
  theme_classic()

ggsave(
  filename = "plot-poverty-by-tract.pdf",
  device = "pdf",
  path = onedrive_path
)

# educational attainment for age >= 25 #### 
acs_edu <- get_acs(
  geography = "tract",
  survey = "acs5",
  variables = c(
    high_school = "DP02_0062",
    bachelor_deg = "DP02_0065",
    total = "DP02_0059"
  ),
  cache_table = TRUE,
  year = 2020,
  state = "AZ",
  county = "Coconino"
)

# subset to census tract of interest 
(demo_tract_edu <- acs_edu %>%
    clean_names() %>%
    mutate(
      tract_hit = if_else(
        condition = str_detect(
          string = geoid,
          pattern = tract_list
        ),
        true = 1,
        false = 0
      )
    ) %>%
    filter(tract_hit == 1))

demo_tract_edu_tab <- demo_tract_edu %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>%
  mutate(
    calc_est_hs = estimate_high_school / estimate_total,
    calc_est_bd = estimate_bachelor_deg / estimate_total
  ) %>%
  select(
    geoid,
    name,
    starts_with("calc_")
  ) %>%
  pivot_longer(
    cols = !c(geoid, name),
    names_to = "variable",
    values_to = "estimate"
  ) 

ggplot() +
  geom_col(
    data = filter(demo_tract_edu_tab, geoid != "04005001201"), 
    mapping = aes(
      x = variable,
      y = estimate,
      group = name,
      fill = name
    ),
    position = "dodge",
    color = "#2b2d42",
    alpha = 5/5
  ) +
  geom_col(
    data = filter(demo_tract_edu_tab, geoid == "04005001201"),
    mapping = aes(
      x = variable,
      y = estimate
    ),
    fill = "#f4cae4",
    color = "#2b2d42",
    alpha = 2/5
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = c(
      "Bachelor's Degree or greater",
      "High School Diploma/GED"
    )
  ) +
  labs(
    title = "Educational Attainment by Census Tract",
    x = "Educational Attainment",
    y = "Percentage of Total Population",
    fill = "Census Tract",
    caption = "Source: US Census ACS 5-Year (2016-2020)"
  ) +
  theme_classic()

ggsave(
  filename = "plot-edu-by-tract.pdf",
  device = "pdf",
  path = onedrive_path
)

# health insurance  #### 
acs_ins <- get_acs(
  geography = "tract",
  survey = "acs5",
  variables = c(
    no_health_ins = "DP03_0099",
    total = "DP03_0095"
  ),
  cache_table = TRUE,
  year = 2020,
  state = "AZ",
  county = "Coconino"
)

# subset to census tract of interest 
(demo_tract_ins <- acs_ins %>%
    clean_names() %>%
    mutate(
      tract_hit = if_else(
        condition = str_detect(
          string = geoid,
          pattern = tract_list
        ),
        true = 1,
        false = 0
      )
    ) %>%
    filter(tract_hit == 1))

demo_tract_ins_tab <- demo_tract_ins %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>%
  mutate(
    calc_est_ins = estimate_no_health_ins / estimate_total
  ) %>%
  select(
    geoid,
    name,
    starts_with("calc_")
  ) %>%
  pivot_longer(
    cols = !c(geoid, name),
    names_to = "variable",
    values_to = "estimate"
  ) 

ggplot() +
  geom_col(
    data = filter(demo_tract_ins_tab, geoid != "04005001201"), 
    mapping = aes(
      x = variable,
      y = estimate,
      group = name,
      fill = name
    ),
    position = "dodge",
    color = "#2b2d42",
    alpha = 5/5
  ) +
  geom_col(
    data = filter(demo_tract_ins_tab, geoid == "04005001201"),
    mapping = aes(
      x = variable,
      y = estimate
    ),
    fill = "#f4cae4",
    color = "#2b2d42",
    alpha = 2/5
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = c(
      "No Health Insurance"
    )
  ) +
  labs(
    title = "Inadequate Health Insurance Coverage by Census Tract",
    x = "",
    y = "Percentage of Total Population",
    fill = "Census Tract",
    caption = "Source: US Census ACS 5-Year (2016-2020)"
  ) +
  theme_classic()

ggsave(
  filename = "plot-insurance-by-tract.pdf",
  device = "pdf",
  path = onedrive_path
)

# poverty status #### 
acs_pov <- get_acs(
  geography = "tract",
  survey = "acs5",
  variables = c(
    below_poverty = "S1701_C02_001",
    total = "S1701_C01_001"
  ),
  cache_table = TRUE,
  year = 2020,
  state = "AZ",
  county = "Coconino"
)

# subset to census tract of interest 
(demo_tract_pov <- acs_pov %>%
    clean_names() %>%
    mutate(
      tract_hit = if_else(
        condition = str_detect(
          string = geoid,
          pattern = tract_list
        ),
        true = 1,
        false = 0
      )
    ) %>%
    filter(tract_hit == 1))



write_csv(
  x = demo_tract_age,
  file = str_c(
    onedrive_path,
    "demo_tract_age",
    ".csv",
    sep = ""
  )
)


write_csv(
  x = demo_tract_economics,
  file = str_c(
    onedrive_path,
    "demo_tract_economics",
    ".csv",
    sep = ""
  )
)


write_csv(
  x = demo_tract_edu,
  file = str_c(
    onedrive_path,
    "demo_tract_edu",
    ".csv",
    sep = ""
  )
)

write_csv(
  x = demo_tract_race,
  file = str_c(
    onedrive_path,
    "demo_tract_race",
    ".csv",
    sep = ""
  )
)

write_csv(
  x = demo_tract_sex,
  file = str_c(
    onedrive_path,
    "demo_tract_sex",
    ".csv",
    sep = ""
  )
)

write_csv(
  x = demo_tract_ins,
  file = str_c(
    onedrive_path,
    "demo_tract_ins",
    ".csv",
    sep = ""
  )
)

write_csv(
  x = demo_tract_pov,
  file = str_c(
    onedrive_path,
    "demo_tract_pov",
    ".csv",
    sep = ""
  )
)



# demo_list <- as_tibble(ls()) %>%
#   filter(str_detect(value, "demo_")) %>%
#   as_vector()
# 
# map(
#   .x = demo_list,
#   write_csv(
#     x = .,
#     file = str_c(
#     "data-tidy/acs-demographics-",
#     as.character(.),
#     ".rds"
#   ))
# )
# 
# list(demo_list)
# 
# walk(
#   .x = demo_list,
#   .f = write_csv(
#     x = .x,
#     file = str_c(
#       "data-tidy/acs-demographics-",
#       as.character(.),
#       ".rds",
#       sep = ""
#     )
#   )
# )
# 
# for (i in demo_list) {
#   write_csv(
#     x = i,
#     file = str_c(
#       "data-tidy/acs-demographics-",
#       as.character(i),
#       ".rds",
#       sep = ""
#     )
#   )
# }

# bivariate #### 
demo_tract_age_tab
demo_tract_eco_pov_tab

bivar_tab <- bind_rows(
  filter(demo_tract_age_tab, variable == "calc_est_under5"),
  demo_tract_eco_pov_tab
) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate
  )
