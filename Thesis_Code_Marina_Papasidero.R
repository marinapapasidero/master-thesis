# ===========================================================
# Erasmus School of Economics

# Master Thesis - Data Science & Marketing Analytics

# Academic Year 2024/2025

# The Social Glow-Up: The Role of Digital Marketing Practices 
# in Shaping Consumer Preferences and Brand Reputation within 
# the Skincare Sector – A Choice-Based Conjoint Analysis

# Student Name: Marina Papasidero
# Student ID number: 739106

# Supervisor: Erjen van Nierop
# Second Assessor: Eoghan O’Neill 

# Final Version Date: 15th July 2025
# ===========================================================

# Use source() to avoid debugSource() errors given the size of the script!
if (sys.nframe() == 0) source("Thesis_Code_Marina_Papasidero_739106.R")

# ===========================
# --- Load Required Libraries
# ===========================
library(tidyverse)
library(mlogit)
library(ggplot2)
library(broom)
library(car)
library(bayesm)
library(data.table)
library(dplyr)
library(stringr)
library(fastDummies)
library(GGally)
library(readr)
library(tibble)
library(kableExtra)
library(interactions)
library(gtsummary)
library(tidyr)
library(gt)
library(knitr)
library(broom)
library(lmtest)
library(RColorBrewer)
library(ggpubr)
library(psych)
library(ordinal)

# ===============================
# --- Data Import and Preparation
# ===============================
# Import full raw survey dataset, skipping metadata rows
survey <- read_csv("Thesis_Survey.csv", skip = 2)
# Import CBC choice task design
cbc <- read_csv("CBC_Combinations.csv")

# Rename column headers for easier reference
colnames(survey) <- c(
  "StartDate", "EndDate", "Status", "IPAddress", "Progress",
  "DurationInSeconds", "Finished", "RecordedDate", "ResponseId",
  "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalReference",
  "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage",
  "Q1", "Q2", "Q3_1", "Q3_2", "Q3_3", "Q3_4", "Q3_5", "Q3_6", "Q4",
  "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14",
  "Q15_1", "Q15_2", "Q15_3", "Q16", "Q17", "Q18", "Q19",
  "Gender", "Age", "Nationality", "Occupation")

# Remove identifying information to ensure respondent anonymity
survey <- dplyr::select(survey, -RecipientLastName, -RecipientFirstName, -RecipientEmail, -ExternalReference)

# Number of initial participants in the survey
n_participants_initial <- nrow(survey)
print(paste("Initial number of participants:", n_participants_initial)) #576

# Filter out respondents who do not follow a skincare routine
no_count <- survey %>% filter(Q1 == "No (the Survey Ends)") %>% nrow()
cat("Respondents to exclude (answered 'No' as they do not follow a skincare routine):", no_count, "\n") #48 (to exclude)

# Retain only valid responses from skincare users
survey <- survey %>% filter(Q1 == "Yes")
cat("Valid Respondents:", nrow(survey), "\n") #528 (to retain)

# ===========================
# --- Demographic Exploration
# ===========================
# Distribution by Gender
gender_table <- survey %>%
  count(Gender) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  arrange(desc(n)) %>%
  rename("Gender" = Gender, "Count" = n, "Percentage (%)" = Percentage) %>%
  as_tibble()

# Styled table for Gender
gender_table %>%
  kbl(
    caption = "Distribution by Gender",
    booktabs = TRUE,
    align = c("l", "c")
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 11,
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, background = "#f2f2f2") %>%  
  column_spec(1, bold = FALSE) %>%                      
  add_footnote("Note. Percentages refer to the proportion of the final sample (N = 528).")

# Distribution by Age Group
age_table <- survey %>%
  count(Age) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  arrange(desc(n)) %>%
  rename("Age Group" = Age, "Count" = n, "Percentage (%)" = Percentage) %>%
  as_tibble()

# Styled table for Age Group
age_table %>%
  kbl(
    caption = "Distribution by Age Group",
    booktabs = TRUE,
    align = c("l", "c")
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 11,
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, background = "#f2f2f2") %>%   
  column_spec(1, bold = FALSE) %>%                      
  add_footnote("Note. Percentages refer to the proportion of the final sample (N = 528).")

# Distribution by Nationality
nationality_table <- survey %>%
  count(Nationality) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  arrange(desc(n)) %>%
  rename("Nationality" = Nationality, "Count" = n, "Percentage (%)" = Percentage) %>%
  as_tibble()

# Styled table for Nationality
nationality_table %>%
  kbl(
    caption = "Distribution by Nationality",
    booktabs = TRUE,
    align = c("l", "c")
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 11,
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, background = "#f2f2f2") %>%  
  column_spec(1, bold = FALSE) %>%                     
  add_footnote("Note. Percentages refer to the proportion of the final sample (N = 528).")

# Distribution by Occupation
occupation_table <- survey %>%
  count(Occupation) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  arrange(desc(n)) %>%
  rename("Occupation" = Occupation, "Count" = n, "Percentage (%)" = Percentage) %>%
  as_tibble()

# Styled table for Occupation
occupation_table %>%
  kbl(
    caption = "Distribution by Occupation",
    booktabs = TRUE,
    align = c("l", "c")
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 11,
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, background = "#f2f2f2") %>% 
  column_spec(1, bold = FALSE) %>%                     
  add_footnote("Note. Percentages refer to the proportion of the final sample (N = 528).")

# Save cleaned dataset
write_csv(survey, "survey_cleaned.csv")

# ===========================================================
# --- Q2 EXPLORATION: First Skincare Brand Discovery Channels
# ===========================================================
# --- 1. Create respondent-level dataset (q2_data)
q2_data <- survey %>%
  filter(!is.na(Q2)) %>%                      # include only valid Q2 responses
  select(ResponseId, Gender, Age, Occupation, Channel = Q2) %>%
  mutate(
    Channel    = factor(Channel),                   # convert Channel to factor
    Gender     = factor(Gender),
    Age        = factor(Age, levels = c(                     # order Age levels
      "Under 18", "18-24", "25-34", "35-44",
      "45-54", "55-64", "65 or Older"
    ), ordered = TRUE),
    Occupation = factor(Occupation))

# --- 2. Prepare data with percentages
q2_summary <- q2_data %>%
  count(Channel) %>%
  mutate(Percentage = n / sum(n) * 100)

# --- 3. Overall bar plot: Discovery channel distribution with percentages above bars
ggplot(q2_summary, aes(x = fct_infreq(Channel), y = Percentage, fill = Channel)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(round(Percentage, 1), "%")),
    hjust = -0.1,
    size = 4,
    color = "black"
  ) +
  coord_flip() +
  labs(
    title = "Where Did You First Discover a Skincare Product You Frequently Use?",
    subtitle = "Overall Distribution of First-Discovery Channels",
    x = "Discovery Channel",
    y = "Proportion of Respondents (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, margin = margin(b = 5), hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    axis.text     = element_text(color = "black"),
    axis.title.x  = element_text(margin = margin(t = 10)),
    axis.title.y  = element_text(margin = margin(r = 10)),
    plot.margin   = margin(t = 20, r = 10, b = 10, l = 10),
    panel.grid    = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2") +
  expand_limits(y = max(q2_summary$Percentage) * 1.1)


# --- 4. Plotting function: Faceted bar plot by demographic variable with percentages
plot_q2_facet <- function(data, facet_var, facet_title) {
  summary_df <- data %>%
    group_by(.data[[facet_var]], Channel) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(.data[[facet_var]]) %>%
    mutate(Percentage = round(Count / sum(Count) * 100, 1)) %>%
    ungroup()
  
  ggplot(summary_df, aes(x = fct_infreq(Channel), y = Count, fill = Channel)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_text(aes(label = paste0(Percentage, "%")),
              hjust = -0.1, size = 4, color = "black") +
    coord_flip() +
    facet_wrap(as.formula(paste("~", facet_var))) +
    labs(
      title = facet_title,
      subtitle = "Distribution of First-Discovery Channels by Group",
      x = "Discovery Channel",
      y = "Number of Respondents"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 16, margin = margin(b = 5), hjust = 0),
      plot.subtitle = element_text(size = 12, hjust = 0),
      axis.text     = element_text(color = "black"),
      axis.title.x  = element_text(margin = margin(t = 10)),
      axis.title.y  = element_text(margin = margin(r = 10)),
      plot.margin   = margin(t = 20, r = 10, b = 10, l = 10),
      panel.grid    = element_blank()
    ) +
    scale_fill_brewer(palette = "Set2") +
    expand_limits(y = max(summary_df$Count) * 1.1)
}

# --- 5. Faceted bar plots by demographic group
plot_q2_facet(q2_data, "Gender",     "Skincare Product Discovery – by Gender")
plot_q2_facet(q2_data, "Age",        "Skincare Product Discovery – by Age Group")
plot_q2_facet(q2_data, "Occupation", "Skincare Product Discovery – by Occupation")

# =======================================================
# --- Q3 Exploration: Discovery Frequency by Demographics
# =======================================================
# Prepare the data in long format
q3_long <- survey %>%
  select(ResponseId, Gender, Age, Occupation,
         Q3_1:Q3_6) %>%
  rename(
    TikTok = Q3_1,
    Instagram = Q3_2,
    OtherSocial = Q3_3,
    Traditional = Q3_4,
    InStore = Q3_5,
    FamilyFriends = Q3_6
  ) %>%
  pivot_longer(cols = TikTok:FamilyFriends,
               names_to = "Channel",
               values_to = "Score") %>%
  mutate(
    Score = as.numeric(str_extract(Score, "^\\d")),
    Gender = factor(Gender),
    Age = factor(Age, levels = c(
      "Under 18", "18-24", "25-34", "35-44",
      "45-54", "55-64", "65 or Older"
    ), ordered = TRUE),
    Occupation = factor(Occupation))

# Plot showing mean discovery frequency per channel with error bars and numeric labels
ggplot(q3_long, aes(x = reorder(Channel, -Score, FUN = mean), y = Score, fill = Channel)) +
  stat_summary(
    geom = "bar",
    fun = mean
  ) +
  stat_summary(
    geom = "errorbar",
    fun.data = mean_se,
    width = 0.2
  ) +
  stat_summary(
    geom = "text",
    fun = mean,
    aes(label = sprintf("%.2f", ..y..)),
    vjust = -1,        
    size = 5
  ) +
  labs(
    title = "Average Discovery Frequency by Channel",
    y = "Mean Frequency (1 = Never, 5 = Very Frequently)",
    x = "Discovery Channel"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold"))

# Visualization: Discovery Frequency by Gender
ggplot(q3_long, aes(x = reorder(Channel, -Score, FUN = mean), y = Score, fill = Gender)) +
  stat_summary(
    geom = "bar",
    fun = mean,
    position = position_dodge(0.7)
  ) +
  stat_summary(
    geom = "text",
    fun = mean,
    aes(label = sprintf("%.2f", ..y..)),
    position = position_dodge(0.7),
    vjust = -0.5,
    size = 3,
    fontface = "bold"
  ) +
  labs(
    title = "Average Discovery Frequency by Channel and Gender",
    x = "Discovery Channel",             
    y = "Mean Frequency (1–5)"
  ) +
  theme_minimal(base_size = 13) +
  scale_fill_brewer(palette = "Set2") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1))

# Visualization: Discovery Frequency by Age Group
ggplot(q3_long, aes(x = reorder(Channel, -Score, FUN = mean), y = Score, fill = Age)) +
  stat_summary(
    geom = "bar",
    fun = mean,
    position = position_dodge(0.7)
  ) +
  stat_summary(
    geom = "text",
    fun = mean,
    aes(label = sprintf("%.2f", ..y..)),
    position = position_dodge(0.7),
    vjust = -0.5,
    size = 3,
    fontface = "bold"
  ) +
  labs(
    title = "Average Discovery Frequency by Channel and Age Group",
    x = "Discovery Channel", 
    y = "Mean Frequency (1–5)"
  ) +
  theme_minimal(base_size = 13) +
  scale_fill_brewer(palette = "Set2") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1))

# Visualization: Discovery Frequency by Occupation
ggplot(q3_long, aes(x = reorder(Channel, -Score, FUN = mean), y = Score, fill = Occupation)) +
  stat_summary(
    geom = "bar",
    fun = mean,
    position = position_dodge(0.7)
  ) +
  stat_summary(
    geom = "text",
    fun = mean,
    aes(label = sprintf("%.2f", ..y..)),
    position = position_dodge(0.7),
    vjust = -0.5,
    size = 3,
    fontface = "bold"
  ) +
  labs(
    title = "Average Discovery Frequency by Channel and Occupation",
    x = "Discovery Channel",        
    y = "Mean Frequency (1–5)"
  ) +
  theme_minimal(base_size = 13) +
  scale_fill_brewer(palette = "Set2") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1))

# ================================================================
# H1 – Frequency of Skincare Discovery and Social Media Engagement
# ================================================================
# STEP 1 - Clean Q4: Extract Numeric Score (Engagement)
survey <- survey %>%
  mutate(Q4_clean = as.numeric(str_extract(Q4, "^\\d")))

# STEP 2 - Create Moderator Variable
survey <- survey %>%
  mutate(SM_Engagement_Level = case_when(
    Q4_clean == 5 ~ "High",
    Q4_clean %in% c(1, 2, 3, 4) ~ "Low",
    TRUE ~ NA_character_
  ))

# STEP 3 - Descriptive Stats and Plots
# Count of engagement levels
survey %>%
  filter(!is.na(SM_Engagement_Level)) %>%
  count(SM_Engagement_Level) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1)) %>%
  rename(
    `Engagement Level` = SM_Engagement_Level,
    `Number of Respondents` = n)

# Overall bar plot
ggplot(
  survey %>% filter(!is.na(SM_Engagement_Level)),
  aes(x = SM_Engagement_Level, fill = SM_Engagement_Level)) +
  geom_bar(width = 0.6, show.legend = FALSE) +
  labs(
    title = "Overall Social Media Engagement Level",
    x = "Engagement Level",
    y = "Number of Respondents"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13)

# By Gender
ggplot(
  survey %>% filter(!is.na(SM_Engagement_Level), !is.na(Gender)),
  aes(x = SM_Engagement_Level, fill = SM_Engagement_Level)) +
  geom_bar() +
  facet_wrap(~ Gender) +
  labs(
    title = "Social Media Engagement by Gender",
    x = "Engagement Level",
    y = "Number of Respondents"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) 

# By Age
ggplot(
  survey %>% filter(!is.na(SM_Engagement_Level), !is.na(Age)),
  aes(x = SM_Engagement_Level, fill = SM_Engagement_Level)) +
  geom_bar() +
  facet_wrap(~ Age, nrow = 2) +
  labs(
    title = "Social Media Engagement by Age Group",
    x = "Engagement Level",
    y = "Number of Respondents"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13)

# By Occupation
ggplot(
  survey %>% filter(!is.na(SM_Engagement_Level), !is.na(Occupation)),
  aes(x = SM_Engagement_Level, fill = SM_Engagement_Level)
) +
  geom_bar() +
  facet_wrap(~ Occupation, scales = "free_y") +
  labs(
    title = "Social Media Engagement by Occupation",
    x = "Engagement Level",
    y = "Number of Respondents"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13)

# STEP 4 - Convert Discovery Frequency Ratings to Numeric
survey <- survey %>%
  mutate(
    TikTok = as.numeric(str_extract(Q3_1, "^\\d")),
    Instagram = as.numeric(str_extract(Q3_2, "^\\d")),
    OtherSocial = as.numeric(str_extract(Q3_3, "^\\d")),
    Traditional = rowMeans(
      across(c(Q3_4, Q3_6), ~as.numeric(str_extract(.x, "^\\d"))),
      na.rm = TRUE))

# STEP 5 - Compute Aggregated Discovery Scores
survey <- survey %>%
  mutate(
    SocialMedia_Discovery = rowMeans(across(c(TikTok, Instagram, OtherSocial)), na.rm = TRUE),
    Traditional_Discovery = Traditional)

# STEP 6 - Paired Comparison: Social Media vs Traditional
# Summary statistics
survey %>%
  summarise(
    Mean_Social = mean(SocialMedia_Discovery, na.rm = TRUE),
    Mean_Traditional = mean(Traditional_Discovery, na.rm = TRUE))

# Paired t-test
t.test(survey$SocialMedia_Discovery, survey$Traditional_Discovery, paired = TRUE)

# STEP 7 - Moderation Analysis
# Reshape dataset to long format
long_df <- survey %>%
  select(SM_Engagement_Level, SocialMedia_Discovery, Traditional_Discovery) %>%
  pivot_longer(
    cols = c(SocialMedia_Discovery, Traditional_Discovery),
    names_to = "Channel_Type",
    values_to = "Score"
  ) %>%
  mutate(
    SM_Engagement_Level = factor(SM_Engagement_Level, levels = c("Low", "High")),
    Channel_Type = factor(Channel_Type, levels = c("Traditional_Discovery", "SocialMedia_Discovery")))

# Mean plot by Channel and Engagement
ggplot(long_df, aes(x = Channel_Type, y = Score, fill = SM_Engagement_Level)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(0.7), width = 0.6) +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(0.7), width = 0.2) +
  labs(
    title = "Discovery Frequency by Channel and Social Media Engagement",
    x = "Channel Type",
    y = "Mean Discovery Frequency",
    fill = "Engagement Level"
  ) +
  scale_fill_manual(values = c("Low" = "#6BAED6", "High" = "#FF6F61")) +
  theme_minimal(base_size = 13)

# STEP 8 – Separate Regressions per Platform
# Instagram
model_instagram <- lm(Instagram ~ SM_Engagement_Level, data = survey)
summary(model_instagram)

# TikTok
model_tiktok <- lm(TikTok ~ SM_Engagement_Level, data = survey)
summary(model_tiktok)

# OtherSocial
model_other <- lm(OtherSocial ~ SM_Engagement_Level, data = survey)
summary(model_other)

# OtherSocial
model_traditional <- lm(Traditional ~ SM_Engagement_Level, data = survey)
summary(model_traditional) 

# STEP 9 – Test Interactions
# Reshape to long
long_df_platform <- survey %>%
  select(SM_Engagement_Level, Instagram, TikTok, OtherSocial, Traditional) %>%
  pivot_longer(cols = c(Instagram, TikTok, OtherSocial, Traditional),
               names_to = "Platform", values_to = "Score") %>%
  mutate(
    SM_Engagement_Level = factor(SM_Engagement_Level, levels = c("Low", "High")),
    Platform = factor(Platform))

# Fit model with interaction
fit_platform <- lm(Score ~ SM_Engagement_Level * Platform, data = long_df_platform)
summary(fit_platform)

# ==================================================
#                 CBC  H2 – H6 (MNL)
# ==================================================
# STEP 1 – Reshape Data to Long Format (from CBC Tasks)
cbc_questions <- paste0("Q", 5:14)
survey_cbc <- survey[, c("ResponseId", cbc_questions)]

long_dt <- map_dfr(cbc_questions, function(q) {
  choices <- survey_cbc[, c("ResponseId", q)] %>%
    rename(choice = !!q) %>%
    mutate(Task = q)
  cbc_q <- cbc %>% filter(Question == q)
  left <- cbc_q %>% filter(Brand == "Brand A")
  right <- cbc_q %>% filter(Brand == "Brand B")
  
  map_dfr(1:nrow(choices), function(i) {
    resp_id <- choices$ResponseId[i]
    chosen <- choices$choice[i]
    chosen_pair <- case_when(
      chosen == "Brand A" ~ c(1, 0),
      chosen == "Brand B" ~ c(0, 1),
      TRUE ~ c(NA, NA)
    )
    
    tibble(
      RespondentID = resp_id,
      Task = q,
      Brand = c("A", "B"),
      Chosen = chosen_pair,
      Influencer_Type = c(left$Influencer_Type, right$Influencer_Type),
      Platform = c(left$Platform, right$Platform),
      Follower_Count = c(left$Follower_Count, right$Follower_Count),
      Shared_pers_exp = c(left$Shared_pers_ex, right$Shared_pers_ex),
      Discount = c(left$Discount, right$Discount)
    )
  })
})

# STEP 2 – Prepare Data for Multinomial Logit (MNL) Models
long_dt <- long_dt %>%
  mutate(
    choice = Chosen == 1,
    alt = Brand,
    id = paste0(RespondentID, "_", Task),
    Platform = factor(Platform),
    Follower_Count = factor(Follower_Count),
    Shared_pers_exp = factor(Shared_pers_exp),
    Discount = factor(Discount),
    Influencer_Type = factor(Influencer_Type))

mlogit_data <- mlogit.data(long_dt,
                           choice = "choice",
                           shape = "long",
                           alt.var = "alt",
                           id.var = "id")

# Save cleaned dataset (long format) for further analysis
write_csv(long_dt, "long_dt.csv")

# STEP 3 – Check Multicollinearity (VIF)
vif(lm(choice ~ Platform + Follower_Count + Influencer_Type + Shared_pers_exp + Discount, data = long_dt))

# STEP 4 – Estimate Separate MNL Models for H2 to H6
model_H2 <- mlogit(choice ~ Platform, data = mlogit_data)
summary(model_H2)

model_H3 <- mlogit(choice ~ Follower_Count, data = mlogit_data)
summary(model_H3)

model_H4 <- mlogit(choice ~ Influencer_Type, data = mlogit_data)
summary(model_H4)

model_H5 <- mlogit(choice ~ Shared_pers_exp, data = mlogit_data)
summary(model_H5)

model_H6 <- mlogit(choice ~ Discount, data = mlogit_data)
summary(model_H6)

# STEP 5 – Estimate Full MNL Model
model_full <- mlogit(choice ~ Platform + 
                       Follower_Count +
                       Influencer_Type +
                       Shared_pers_exp +
                       Discount,
                     data = mlogit_data)

summary(model_full)

# Variable Importance for Full MNL Model
# --- 1. Extract the coefficients
mnl_coefs <- summary(model_full)$coefficients

# --- 2. Convert to tidy tibble
mnl_df <- tibble::enframe(mnl_coefs, name = "term", value = "Estimate")

# --- 3. Separate Attribute and Level cleanly
mnl_df <- mnl_df %>%
  mutate(
    Attribute = case_when(
      grepl("Discount", term) ~ "Discount",
      grepl("Follower", term) ~ "Follower Count",
      grepl("Influencer", term) ~ "Influencer Type",
      grepl("Platform", term) ~ "Platform",
      grepl("Shared", term) ~ "Shared Experience",
      TRUE ~ "(Intercept)"
    ),
    Level = case_when(
      grepl("Discount", term) ~ sub("Discount", "", term),
      grepl("Follower", term) ~ sub("Follower_Count", "", term),
      grepl("Influencer", term) ~ sub("Influencer_Type", "", term),
      grepl("Platform", term) ~ sub("Platform", "", term),
      grepl("Shared", term) ~ sub("Shared_pers_exp", "", term),
      TRUE ~ term
    )
  )

# --- 4. Remove intercept
mnl_df <- mnl_df %>%
  filter(Attribute != "(Intercept)")

# --- 5. Add baseline levels explicitly (Estimate = 0)
baseline_levels <- tibble(
  Attribute = c("Discount", "Follower Count", "Influencer Type", "Platform", "Shared Experience"),
  Level = "Baseline",
  Estimate = 0
)

mnl_df_all <- bind_rows(mnl_df, baseline_levels)

# --- 6. Compute Range and Relative Importance
importance_mnl <- mnl_df_all %>%
  group_by(Attribute) %>%
  summarise(
    Range = max(Estimate) - min(Estimate)
  ) %>%
  mutate(
    RelativeImportance = 100 * Range / sum(Range))

# --- 7. Print result
print(importance_mnl)

# ===================================================
#       Full MNL Model vs Simple MNL Comparison
# ===================================================
# Likelihood ratio tests comparing each single-attribute model to the full model
lrtest(model_H2, model_full)   # Platform vs Full Model
lrtest(model_H3, model_full)   # Follower Count vs Full Model
lrtest(model_H4, model_full)   # Influencer Type vs Full Model
lrtest(model_H5, model_full)   # Experience vs Full Model
lrtest(model_H6, model_full)   # Discount vs Full Model

# Compute Akaike Information Criterion (AIC) for all models
AIC(model_H2, model_H3, model_H4, model_H5, model_H6, model_full)

# Convert AIC results into a tidy dataframe
aic_values <- AIC(model_H2, model_H3, model_H4, model_H5, model_H6, model_full)

aic_df <- as.data.frame(aic_values) %>%
  tibble::rownames_to_column(var = "Model") %>%
  rename(AIC = AIC) %>%
  mutate(
    Model = dplyr::recode(
      Model,
      "model_H2"   = "H2: Platform",
      "model_H3"   = "H3: Follower Count",
      "model_H4"   = "H4: Influencer Type",
      "model_H5"   = "H5: Experience",
      "model_H6"   = "H6: Discount",
      "model_full" = "Full Model"
    )
  ) %>%
  arrange(desc(AIC))  # sort from highest to lowest AIC

# Preview the table for verification
print(aic_df)

# Create the barplot comparing model fit
ggplot(aic_df, aes(x = reorder(Model, -AIC), y = AIC, fill = Model)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  coord_flip() +
  geom_text(aes(label = round(AIC, 1)), hjust = -0.1, size = 4) +
  labs(
    title = "Model Fit Comparison (AIC)",
    x = "Model",
    y = "Akaike Information Criterion (AIC)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, max(aic_df$AIC) * 1.1)

# STEP 7 – Hausman-McFadden Tests of IIA Assumption
# Compare the full model to each restricted model to assess IIA violations
hmftest(model_full, model_H2)  
hmftest(model_full, model_H3)
hmftest(model_full, model_H4)
hmftest(model_full, model_H5)
hmftest(model_full, model_H6)

# ==================================================================
#   HB MODELING: Estimating Individual-Level Preferences (H2 - H6)
# ==================================================================
# STEP 1 - Load and prepare the long-format CBC data
cbc_long <- read_csv("long_dt.csv") %>%
  mutate(Chosen = as.numeric(Chosen))  # ensure 'Chosen' is numeric (0 or 1)

# STEP 2 - Dummy-code all attribute levels (excluding baselines)
cbc_long <- dummy_cols(
  cbc_long,
  select_columns = c("Platform", "Follower_Count", "Shared_pers_exp", "Discount", "Influencer_Type"),
  remove_first_dummy = TRUE,            # drops baseline level
  remove_selected_columns = TRUE        # removes original columns
)

# STEP 3 - Select dummy-coded variables for estimation
x_vars <- c(
  grep("^Platform_", names(cbc_long), value = TRUE),
  grep("^Follower_Count_", names(cbc_long), value = TRUE),
  grep("^Shared_pers_exp_", names(cbc_long), value = TRUE),
  grep("^Discount_", names(cbc_long), value = TRUE),
  grep("^Influencer_Type_", names(cbc_long), value = TRUE)
)

# STEP 4 - Reshape CBC data for bayesm format
p <- 2  # number of alternatives per choice task
cbc_split <- split(cbc_long, cbc_long$RespondentID)

lgtdata <- vector("list", length(cbc_split))
names(lgtdata) <- names(cbc_split)

for (i in seq_along(cbc_split)) {
  df <- cbc_split[[i]] %>% arrange(Task, Brand)
  tasks <- unique(df$Task)
  
  # Vector of choices: 1 = Brand A, 2 = Brand B
  y_i <- df %>%
    group_by(Task) %>%
    summarise(choice = if_else(Chosen[Brand == "A"] == 1, 1, 2), .groups = "drop") %>%
    pull(choice)
  
  # Matrix of stacked attributes (A followed by B for each task)
  X_i <- map_dfr(tasks, function(t) {
    df %>% filter(Task == t) %>%
      arrange(Brand) %>%
      select(all_of(x_vars))
  }) %>% as.matrix()
  
  stopifnot(nrow(X_i) == p * length(y_i))  # sanity check
  lgtdata[[i]] <- list(y = y_i, X = X_i)
}

cat("Number of respondents in HB model:", length(lgtdata), "\n")

# STEP 5 - Define prior distributions and MCMC parameters
set.seed(123)
prior <- list(ncomp = 1)  # single-component normal mixture
mcmc <- list(R = 10000, keep = 10)  # 10,000 draws, saving every 10th

# STEP 6a - Run Hierarchical Bayes MNL estimation
hb_result <- rhierMnlRwMixture(
  Data = list(p = p, lgtdata = lgtdata),
  Prior = prior,
  Mcmc = mcmc)

# STEP 6b - Extract individual-level posterior means
# Compute posterior means per respondent
individual_means <- apply(hb_result$betadraw, c(2, 3), mean)
ind_df <- as.data.frame(t(individual_means))
colnames(ind_df) <- colnames(lgtdata[[1]]$X)

# Compute overall posterior means for each attribute level
posterior_means <- apply(hb_result$betadraw, 2, mean)

# Create posterior_df
posterior_df <- data.frame(
  Attribute = colnames(lgtdata[[1]]$X),
  Utility = posterior_means)

# STEP 7a - Prepare baseline levels
baseline_levels <- data.frame(
  Attribute = c("Platform", "Follower", "Influencer", "Shared", "Discount"),
  Level = c("Instagram", "38K","BeautyInfluencer", "None", "No Discount"),
  Utility = 0)

# STEP 7b - Create posterior_long for combining with baseline
# (This version keeps Level as NA for compatibility)
posterior_long <- posterior_df %>%
  mutate(Level = NA) %>%
  select(Attribute, Level, Utility)

# STEP 7c - Visualize aggregate part-worth utilities
ggplot(posterior_df, aes(x = reorder(Attribute, Utility), y = Utility, fill = Utility > 0)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = round(Utility, 2)),
    hjust = ifelse(posterior_df$Utility > 0, -0.1, 1.1),
    size = 4
  ) +
  coord_flip() +
  labs(
    title = "Figure 3: HB Estimated Part-Worth Utilities",
    x = "Attribute Level",
    y = "Utility (log-odds)"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#66c2a5",
               "FALSE" = "#fc8d62")
  ) +
  theme_minimal(base_size = 13) 

# STEP 7d: Convert individual-level estimates to long format for plotting
individual_long <- ind_df %>%
  tidyr::pivot_longer(
    everything(),
    names_to = "Attribute",
    values_to = "Utility")

# Plot histograms showing the distribution of part-worth utilities
ggplot(individual_long, aes(x = Utility)) +
  geom_histogram(
    bins = 30,
    fill = "#4169E1",
    color = "white",
    alpha = 0.8
  ) +
  facet_wrap(~ Attribute, scales = "free") +
  labs(
    title = "Distribution of Individual-Level Part-Worth Utilities",
    x = "Utility (log-odds)",
    y = "Number of Respondents"
  ) +
  theme_minimal(base_size = 13) 

# STEP 8a - Split and clean Attribute and Level
posterior_long <- posterior_df %>%
  tidyr::separate(Attribute, into = c("Attribute", "Level"), sep = "_", extra = "merge") %>%
  mutate(
    Attribute = case_when(
      Attribute %in% c("Follower", "Follower_Count") ~ "Follower Count",
      Attribute %in% c("Influencer", "Influencer_Type") ~ "Influencer Type",
      Attribute %in% c("Shared", "Shared_pers_exp") ~ "Shared Experience",
      Attribute == "Discount" ~ "Discount",
      Attribute == "Platform" ~ "Platform",
      TRUE ~ Attribute))

# STEP 8b - Add baseline levels manually (coherent with Table 3)
baseline_levels <- tibble::tibble(
  Attribute = c("Platform", "Follower Count", "Influencer Type", "Shared Experience", "Discount"),
  Level = c("Instagram", "38K", "Beauty Influencer", "None", "No Discount"),
  Utility = 0)

# STEP 8c - Combine and compute Relative Importance
full_utilities <- bind_rows(posterior_long, baseline_levels)

importance_df <- full_utilities %>%
  group_by(Attribute) %>%
  summarise(Range = max(Utility) - min(Utility)) %>%
  mutate(RelativeImportance = round(Range / sum(Range) * 100, 1)) %>%
  filter(Range != 0) %>%
  arrange(desc(RelativeImportance))

# Print results
print(importance_df)

# STEP 9 - Cluster individuals based on their HB utilities
set.seed(123)
clusters <- kmeans(ind_df, centers = 3)

# Assign and relabel clusters in one step
ind_df$cluster <- factor(
  clusters$cluster,
  levels = c(1, 2, 3),
  labels = c("The Trustkeepers", "The Steadies", "The Sceptics"))

# Compute frequency and percentage of each cluster
cluster_distribution <- ind_df %>%
  count(cluster) %>%
  mutate(percent = round(100 * n / sum(n), 1))

# Display the distribution
print(cluster_distribution)

# Convert data to long format for cluster visualization
ind_long <- ind_df %>%
  pivot_longer(
    cols = -cluster,
    names_to = "Attribute",
    values_to = "Utility")

# Calculate mean utility per cluster and attribute
means_df <- ind_long %>%
  group_by(cluster, Attribute) %>%
  summarise(mean_utility = mean(Utility), .groups = "drop")

# Plot with facets and mean utility lines
ggplot(ind_long, aes(x = Utility, fill = cluster)) +
  geom_density(alpha = 0.4) +
  geom_vline(
    data = means_df,
    aes(xintercept = mean_utility, color = cluster),
    linetype = "dashed",
    size = 0.7
  ) +
  facet_wrap(~ Attribute, scales = "free") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Part-Worth Utilities by Cluster",
    x = "Utility (log-odds)",
    y = "Density",
    fill = "Cluster",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"))


# =============================================
#       MNL and HB Performance Comparison
# =============================================
# MNL Holdout Prediction and Hit Rates
# --- 1. Add Unique Task Identifier (concatenate RespondentID and Task)
long_dt <- long_dt %>%
  mutate(TaskID = paste0(RespondentID, "_", Task))

# Create list of unique tasks (each choice situation)
tasks_unique <- unique(long_dt$TaskID)

# --- 2. Random 70/30 Split of Tasks into Train and Holdout
set.seed(123)
train_tasks <- sample(tasks_unique, size = floor(0.7 * length(tasks_unique)))
holdout_tasks <- setdiff(tasks_unique, train_tasks)

# Subset data into training and holdout datasets
train_dt <- long_dt %>% filter(TaskID %in% train_tasks)
holdout_dt <- long_dt %>% filter(TaskID %in% holdout_tasks)

cat("Train N =", nrow(train_dt), "| Holdout N =", nrow(holdout_dt), "\n")

# --- 3. Fit MNL Model on Training Data
# Convert training data to mlogit format
train_mlogit <- mlogit.data(
  train_dt,
  choice = "choice",
  shape = "long",
  alt.var = "alt",
  id.var = "id")

# Estimate the Multinomial Logit model
model_mnl_train <- mlogit(
  choice ~ Platform + Follower_Count + Shared_pers_exp + Discount + Influencer_Type,
  data = train_mlogit)

# --- 4. Predict Probabilities on Holdout Data
holdout_mlogit <- mlogit.data(
  holdout_dt,
  choice = "choice",
  shape = "long",
  alt.var = "alt",
  id.var = "id")

# Predict choice probabilities
mnl_pred_probs <- predict(
  model_mnl_train,
  newdata = holdout_mlogit,
  type = "probabilities")

# Build dataframe of predictions and observed choices
mnl_pred_df <- data.frame(
  TaskID = holdout_dt$TaskID,
  alt = holdout_dt$alt,
  choice_observed = holdout_dt$choice,
  prob = as.numeric(mnl_pred_probs)
) %>%
  group_by(TaskID) %>%
  # Flag alternative with the highest predicted probability as "predicted choice"
  mutate(pred_choice = if_else(prob == max(prob), 1, 0)) %>%
  ungroup()

# --- 5. Compute Hit Rate (Accuracy on Holdout)
hit_rate_mnl <- mean(mnl_pred_df$choice_observed == mnl_pred_df$pred_choice)
cat("MNL Hit Rate:", round(hit_rate_mnl * 100, 2), "%\n")

# Optional checks
summary(mnl_pred_df$prob)
str(train_dt)
table(round(mnl_pred_df$prob,3))

# --- 6. Repeated Holdout Splits (5 random partitions) to check robustness
set.seed(123)
hit_rates <- numeric(5)

for (i in 1:5) {
  # New random split
  train_tasks_i <- sample(tasks_unique, size = floor(0.7 * length(tasks_unique)))
  holdout_tasks_i <- setdiff(tasks_unique, train_tasks_i)
  
  train_dt_i <- long_dt %>% filter(TaskID %in% train_tasks_i)
  holdout_dt_i <- long_dt %>% filter(TaskID %in% holdout_tasks_i)
  
  # Refit MNL model
  model_mnl_i <- mlogit(
    choice ~ Platform + Follower_Count + Shared_pers_exp + Discount + Influencer_Type,
    data = mlogit.data(
      train_dt_i,
      choice = "choice",
      shape = "long",
      alt.var = "alt",
      id.var = "id"
    )
  )
  
  # Predict on holdout
  pred_probs_i <- predict(
    model_mnl_i,
    newdata = mlogit.data(
      holdout_dt_i,
      choice = "choice",
      shape = "long",
      alt.var = "alt",
      id.var = "id"
    ),
    type = "probabilities"
  )
  
  pred_df_i <- data.frame(
    TaskID = holdout_dt_i$TaskID,
    alt = holdout_dt_i$alt,
    choice_observed = holdout_dt_i$choice,
    prob = as.numeric(pred_probs_i)
  ) %>%
    group_by(TaskID) %>%
    mutate(pred_choice = if_else(prob == max(prob), 1, 0)) %>%
    ungroup()
  
  # Compute hit rate for this split
  hit_rates[i] <- mean(pred_df_i$choice_observed == pred_df_i$pred_choice)
}

cat("Mean MNL Hit Rate:", round(mean(hit_rates)*100, 2), "%\n")

# =========================================
#           HB Holdout Prediction
# =========================================
# --- 1. Prepare Training Data for HB Estimation
cbc_train <- train_dt %>%
  select(-TaskID) %>%
  mutate(Chosen = as.numeric(Chosen))

# Dummy-code categorical attributes (omit baselines)
cbc_train <- dummy_cols(
  cbc_train,
  select_columns = c("Platform", "Follower_Count", "Shared_pers_exp", "Discount", "Influencer_Type"),
  remove_first_dummy = TRUE,
  remove_selected_columns = TRUE)

# Collect dummy variable names
x_vars <- c(
  grep("^Platform_", names(cbc_train), value = TRUE),
  grep("^Follower_Count_", names(cbc_train), value = TRUE),
  grep("^Shared_pers_exp_", names(cbc_train), value = TRUE),
  grep("^Discount_", names(cbc_train), value = TRUE),
  grep("^Influencer_Type_", names(cbc_train), value = TRUE))

# --- 2. Reshape data into list of respondents
cbc_split <- split(cbc_train, cbc_train$RespondentID)

lgtdata <- vector("list", length(cbc_split))
names(lgtdata) <- names(cbc_split)

# Build lgtdata input for HB estimation
for (i in seq_along(cbc_split)) {
  df <- cbc_split[[i]] %>% arrange(Task, Brand)
  tasks <- unique(df$Task)
  
  y_i <- df %>%
    group_by(Task) %>%
    summarise(choice = if_else(Chosen[Brand == "A"] == 1, 1, 2), .groups = "drop") %>%
    pull(choice)
  
  X_i <- map_dfr(tasks, function(t) {
    df %>% filter(Task == t) %>%
      arrange(Brand) %>%
      select(all_of(x_vars))
  }) %>% as.matrix()
  
  stopifnot(nrow(X_i) == 2 * length(y_i))
  lgtdata[[i]] <- list(y = y_i, X = X_i)
}

# --- 3. Estimate Hierarchical Bayes Model
set.seed(123)
hb_result <- rhierMnlRwMixture(
  Data = list(p = 2, lgtdata = lgtdata),
  Prior = list(ncomp = 1),
  Mcmc = list(R = 10000, keep = 10)
)

# Posterior mean coefficients (aggregate level)
posterior_means <- apply(hb_result$betadraw, 2, mean)

# --- 4. Predict on Holdout Data
X_holdout <- model.matrix(
  ~ Platform + Follower_Count + Shared_pers_exp + Discount + Influencer_Type,
  data = holdout_dt
)[, -1]

# Compute predicted utility for each alternative
holdout_dt$Utility <- as.numeric(X_holdout %*% posterior_means)

# Convert utilities to probabilities (softmax within task)
holdout_dt <- holdout_dt %>%
  group_by(TaskID) %>%
  mutate(
    expU = exp(Utility),
    prob = expU / sum(expU),
    pred_choice = if_else(prob == max(prob), 1, 0)
  ) %>%
  ungroup()

# Compute hit rate
hit_rate_hb <- mean(holdout_dt$choice == holdout_dt$pred_choice)
cat("HB Hit Rate:", round(hit_rate_hb * 100, 2), "%\n")


# =======================================
# H7 – Stable Pricing & Brand Credibility
# =======================================

# NOTE: Although questions Q15 to Q19 were randomized in the survey, for the purpose of analysis, 
# they are presented and interpreted in the original order as shown in the questionnaire 
# (Appendix A).

# STEP 1 – Clean Q18 and Q19: ensure correct numeric conversion
survey <- survey %>%
  mutate(
    Q18 = str_extract(Q18, "[1-5]") %>% as.numeric(),   # Q18: Agreement with credibility statement (1–5)
    Q19 = str_extract(Q19, "[1-7]") %>% as.numeric()    # Q19: Brand preference scale (1–7)
  )

# STEP 2 – Frequency + Percentage table for Q18
q18_freq <- survey %>%
  count(Q18) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))

print(q18_freq)

# STEP 3 – Bar plot for Q18 with percentages
ggplot(q18_freq, aes(x = factor(Q18), y = n)) +
  geom_bar(stat = "identity", fill = "#6BAED6", width = 0.7) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            vjust = -0.5, size = 4.2) +
  labs(
    title = "Q18: Agreement – Stable Pricing Indicates Brand Credibility",
    x = "Agreement Scale (1 = Strongly Disagree, 5 = Strongly Agree)",
    y = "Number of Respondents") +
  ylim(0, max(q18_freq$n) * 1.15) +  
  theme_minimal(base_size = 13) 

# STEP 4 – Frequency table for Q19 (Brand Preference)
survey %>%
  count(Q19) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1)) %>%
  print()

# STEP 5 – Bar plot for Q19 with percentage labels
survey %>%
  count(Q19) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = factor(Q19), y = Percentage)) +
  geom_bar(stat = "identity", fill = "darkslateblue") +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5) +
  labs(
    title = "Q19: Brand Preference Based on Pricing Strategy",
    subtitle = "1 = Strong Preference for Brand A (Stable), 7 = Strong Preference for Brand B (Discount)",
    x = "Preference Scale",
    y = "Percentage (%)"
  ) +
  theme_minimal(base_size = 13) 

# STEP 6 – One-sample t-test: Is Q18 > midpoint (3)?
t.test(survey$Q18, mu = 3)

# STEP 7 – Paired t-test Q18 vs Q19 (approximate comparison)
t.test(survey$Q18, survey$Q19, paired = TRUE)

# STEP 8 – Spearman correlation
cor.test(survey$Q18, survey$Q19, method = "spearman", exact = FALSE)

# STEP 9 – Simple linear regression: Q18 ~ Q19
lm_model <- lm(Q18 ~ Q19, data = survey)
summary(lm_model)

# STEP 10 – Ordinal Logistic Regression
# Prepare variables
survey <- survey %>%
  mutate(
    Q18_ord = factor(Q18, ordered = TRUE),
    Gender = factor(Gender),
    Age = factor(Age),
    Occupation = factor(Occupation),
    Nationality_dummy = ifelse(Nationality == "Italian", 1, 0)
  )

# Run ordinal logit model
ordinal_model <- clm(Q18_ord ~ Q19 + Gender + Age + Occupation + Nationality_dummy, data = survey)
summary(ordinal_model)


# ===================================================
#    Post-Task Perceptions (Likert-Based Results) 
# ===================================================

# NOTE: Although questions Q15 to Q19 were randomized in the survey, for the purpose of analysis, 
# they are presented and interpreted in the original order as shown in the questionnaire 
# (Appendix A).

# ==================================
# --- Q15: Trust in Influencer types
# ==================================
# Reshape to long format
q15_long <- survey %>%
  select(ResponseId, Q15_1, Q15_2, Q15_3) %>%
  pivot_longer(
    cols = starts_with("Q15"),
    names_to = "Influencer_Type",
    values_to = "Trust_Score"
  ) %>%
  mutate(
    Influencer_Type = case_when(
      Influencer_Type == "Q15_1" ~ "Beauty Influencer",
      Influencer_Type == "Q15_2" ~ "Cosmetologist",
      Influencer_Type == "Q15_3" ~ "Dermatologist"
    ),
    Trust_Score = as.numeric(str_extract(Trust_Score, "^[0-9]")))

# Descriptive statistics for Q15
q15_summary <- q15_long %>%
  group_by(Influencer_Type) %>%
  summarise(
    Mean = round(mean(Trust_Score, na.rm = TRUE), 2),
    SD = round(sd(Trust_Score, na.rm = TRUE), 2),
    N = sum(!is.na(Trust_Score)))

print(q15_summary)

# --- Q15: Barplot of trust distribution ---
ggplot(q15_long, aes(x = factor(Trust_Score), fill = Influencer_Type)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Influencer_Type) +
  labs(
    title = "Trust in Skincare Recommendations by Influencer Type",
    x = "Trust Level (1 = Not at all, 5 = Completely)",
    y = "Number of Respondents"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13)

# ==========================================
# --- Q16: Impact of Follower Count on Trust
# ==========================================
survey <- survey %>%
  mutate(Q16_clean = as.numeric(str_extract(Q16, "^\\d")))

# Descriptive stats Q16
q16_summary <- survey %>%
  summarise(
    Mean = round(mean(Q16_clean, na.rm = TRUE),2),
    SD = round(sd(Q16_clean, na.rm = TRUE),2),
    N = sum(!is.na(Q16_clean)))

print(q16_summary)

# Barplot Q16
ggplot(survey, aes(x = factor(Q16_clean))) +
  geom_bar(fill="#66c2a5") +
  geom_text(stat="count", aes(label=..count..), vjust=-0.3) +
  labs(
    title = "Impact of Follower Count on Trust in Skincare Recommendations",
    x = "Trust Impact (1 = Not at all, 5 = Completely)",
    y = "Number of Respondents"
  ) +
  theme_minimal(base_size=13)

# ==================================================
# --- Q17: Perception of Product Value with Discount
# ==================================================
survey <- survey %>%
  mutate(Q17_clean = as.numeric(str_extract(Q17, "^\\d")))

# Descriptive stats Q17
q17_summary <- survey %>%
  summarise(
    Mean = round(mean(Q17_clean, na.rm = TRUE),2),
    SD = round(sd(Q17_clean, na.rm = TRUE),2),
    N = sum(!is.na(Q17_clean)))

print(q17_summary)

# Barplot Q17
ggplot(survey, aes(x = factor(Q17_clean))) +
  geom_bar(fill="#fc8d62") +
  geom_text(stat="count", aes(label=..count..), vjust=-0.3) +
  labs(
    title = "Impact of Discounts on Perceived Product Value",
    x = "Perception Impact (1 = Much less valuable, 5 = Great deal)",
    y = "Number of Respondents"
  ) +
  theme_minimal(base_size=13)


# ============================
#     Tables (in the Text)
# ============================
# -----------------------------------
# Table 1 -  Attributes' Levels Table
# -----------------------------------
# Create the data
attributes_df <- tibble::tibble(
  Attribute = c(
    "Influencer Type",
    "Social Media Platform",
    "Follower Count",
    "Shared Personal Experience",
    "Discount Code"
  ),
  Levels = c(
    "Beauty Influencer, Cosmetologist or Dermatologist",
    "Instagram or TikTok",
    "38K, 195K or 900K",
    "Positive or No Shared Experience",
    "None, 10% or 20%"))

# Build gt table
attributes_df %>%
  gt() %>%
  tab_header(
    title = md("**Table 1. Attributes and Levels Used in the Conjoint Experiment**")
  ) %>%
  cols_label(
    Attribute = "Attribute",
    Levels = "Levels"
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold",
    column_labels.font.weight = "bold"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. The table summarizes the attributes and levels included in the choice-based conjoint design.")

# --------------------------------------
# Table 2 - Full Multinomial Logit Model 
# --------------------------------------
# Prepare the data frame with results (H4 moved above H5)
full_model_table <- tibble(
  Predictor = c(
    "(Intercept)",
    "Platform: TikTok (H2)",
    "Follower Count: 38K (H3)",
    "Follower Count: 900K (H3)",
    "Influencer: Cosmetologist (H4)",
    "Influencer: Dermatologist (H4)",
    "Shared Personal Experience: Positive (H5)",
    "Discount: 20% (H6)",
    "Discount: No Discount (H6)"
  ),
  Estimate = c(
    -6.445,
    -6.421,
    3.575,
    0.520,
    1.011,
    1.968,
    10.109,
    -1.717,
    3.743
  ),
  `Std. Error` = c(
    0.326,
    0.260,
    0.221,
    0.245,
    0.081,
    0.143,
    0.421,
    0.278,
    0.145
  ),
  `z-value` = c(
    -19.80,
    -24.68,
    16.15,
    2.12,
    12.55,
    13.75,
    24.00,
    -6.17,
    25.82
  ),
  `p-value` = c(
    "<0.001 ***",
    "<0.001 ***",
    "<0.001 ***",
    "0.034 *",
    "<0.001 ***",
    "<0.001 ***",
    "<0.001 ***",
    "<0.001 ***",
    "<0.001 ***"))

# Create APA-style gt table
full_model_table %>%
  gt() %>%
  tab_header(
    title = md("**Table 2. Full Multinomial Logit Model Predicting Brand Choice**")
  ) %>%
  cols_label(
    Predictor = "Predictor",
    Estimate = "Estimate",
    `Std. Error` = "Std. Error",
    `z-value` = "z value",
    `p-value` = "p-value"
  ) %>%
  fmt_number(
    columns = c(Estimate, `Std. Error`, `z-value`),
    decimals = 3
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. *** p < .001, ** p < .01, * p < .05. Estimates are log-odds relative to the reference categories (Instagram, 185K followers, no experience shared, baseline discount, beauty influencer). McFadden R^2: 0.38365.")

# -------------------------------------------
# Table 3 - HB Estimated Part-Worth Utilities 
# -------------------------------------------
# Step 1: Clean posterior_df
posterior_long <- posterior_df %>%
  tidyr::separate(Attribute, into = c("Attribute", "Level"), sep = "_", extra = "merge") %>%
  mutate(
    Attribute = case_when(
      Attribute %in% c("Follower", "Follower_Count") ~ "Follower Count",
      Attribute %in% c("Influencer", "Influencer_Type") ~ "Influencer Type",
      Attribute %in% c("Shared", "Shared_pers_exp") ~ "Shared Experience",
      Attribute == "Discount" ~ "Discount",
      Attribute == "Platform" ~ "Platform",
      TRUE ~ Attribute),
    Level = case_when(
      Attribute == "Discount" & Level == "No" ~ "10%",  # fix the error
      TRUE ~ Level),
    Baseline = FALSE)

# Step 2: Add baseline levels
baseline_levels <- tibble(
  Attribute = c("Platform", "Follower Count", "Influencer Type", "Shared Experience", "Discount"),
  Level = c("Instagram", "38K", "Beauty Influencer", "None", "No Discount"),
  Utility = 0,
  Baseline = TRUE)

# Step 3: Combine & order baseline first per group in custom order
attribute_order <- c("Platform", "Follower Count", "Influencer Type", "Shared Experience", "Discount")

full_utilities <- bind_rows(posterior_long, baseline_levels) %>%
  mutate(
    Attribute = factor(Attribute, levels = attribute_order),
    Level = as.character(Level)
  ) %>%
  arrange(Attribute, desc(Baseline), Level)

# Step 4: Create the gt table
full_utilities %>%
  gt() %>%
  tab_header(
    title = md("**Table 3. HB Estimated Part-Worth Utilities (Baseline Levels Marked)**")
  ) %>%
  cols_label(
    Attribute = "Attribute",
    Level = "Level",
    Utility = "Utility (log-odds)",
    Baseline = "Baseline"
  ) %>%
  fmt_number(columns = Utility, decimals = 2) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. Baseline = TRUE indicates the reference category for each attribute; utility fixed to 0. These part-worth utilities quantify the relative impact of each attribute level on the likelihood of brand choice, expressed in log-odds units. Therefore, positive or negative values show how each factor increases or decreases the probability of selection compared to the baseline, rather than representing absolute probabilities or direct measures of perception.")


# =============================
#     Figures (in the Text)
# =============================
# ----------------------------------------------------------------
# Figure 3 and 4 - First Discovery and Overall Discovery Frequency
# ----------------------------------------------------------------
# - Figure 3: Horizontal bar chart showing the overall distribution of first-discovery channels with percentage labels
ggplot(q2_summary, aes(x = fct_infreq(Channel), y = Percentage, fill = Channel)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(round(Percentage, 1), "%")),
    hjust = -0.1,
    size = 4,
    color = "black"
  ) +
  coord_flip() +
  labs(
    title = "Figure 3: Where Did You First Discover a Skincare Product You Frequently Use?",
    subtitle = "Overall Distribution of First-Discovery Channels",
    x = "Discovery Channel",
    y = "Proportion of Respondents (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12),
    axis.text     = element_text(color = "black"),
    axis.title.x  = element_text(margin = margin(t = 10)),
    axis.title.y  = element_text(margin = margin(r = 10)),
    plot.margin   = margin(t = 20, r = 10, b = 10, l = 10),
    panel.grid    = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2") +
  expand_limits(y = max(q2_summary$Percentage) * 1.1)

# - Figure 4: Vertical bar chart showing the mean discovery frequency per channel (Likert scale) with error bars and numeric labels
ggplot(q3_long, aes(x = reorder(Channel, -Score, FUN = mean), y = Score, fill = Channel)) +
  stat_summary(
    geom = "bar",
    fun = mean
  ) +
  stat_summary(
    geom = "errorbar",
    fun.data = mean_se,
    width = 0.2
  ) +
  stat_summary(
    geom = "text",
    fun = mean,
    aes(label = sprintf("%.2f", ..y..)),
    vjust = -1,        
    size = 5
  ) +
  labs(
    title = "Figure 4: Average Discovery Frequency by Channel",
    y = "Mean Frequency (1 = Never, 5 = Very Frequently)",
    x = "Discovery Channel"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank())


# ==============================================
#     Appendix B - CBC Combinations Overview
# ==============================================
# Create APA-style gt table
cbc %>%
  gt() %>%
  tab_header(
    title = md("**Table 1. Overview of Choice-Based Conjoint Combinations**")
  ) %>%
  # Format all numeric columns to 2 decimals
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  # Style options
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    heading.title.font.weight = "bold",
    column_labels.font.weight = "bold",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  # Optional: Add note
  tab_source_note(
    source_note = "Note. All percentages are computed as a proportion of the final sample (N = 528).")


# =======================
#     APPENDIX Tables 
# =======================
# --------------------------------
# Table 1 - Sample Characteristics
# --------------------------------
# Summary data
sample_summary <- tibble(
  InitialParticipants = 576,
  Excluded = 48,
  FinalRespondents = 528,
  Female = 93.6,
  Male = 4.7,
  NonBinaryOther = 0.9,
  PreferNotToSay = 0.8,
  Under18 = 0.6,
  Age18_24 = 24.2,
  Age25_34 = 40.3,
  Age35_44 = 22.2,
  Age45Plus = 12.7,
  Italian = 93.8,
  OtherNationalities = 6.2,
  FullTime = 47.3,
  Students = 28.6,
  SelfEmployed = 8.7,
  PartTime = 6.3,
  Unemployed = 4.9,
  OtherOccupations = 4.2)

# Convert to long format
summary_long <- sample_summary %>%
  pivot_longer(cols = everything(),
               names_to = "Characteristic",
               values_to = "Value") %>%
  mutate(
    Characteristic = case_when(
      Characteristic == "InitialParticipants" ~ "Initial number of participants",
      Characteristic == "Excluded" ~ "Excluded respondents",
      Characteristic == "FinalRespondents" ~ "Final valid respondents",
      Characteristic == "Female" ~ "Gender: Female (%)",
      Characteristic == "Male" ~ "Gender: Male (%)",
      Characteristic == "NonBinaryOther" ~ "Gender: Non-binary/Other (%)",
      Characteristic == "PreferNotToSay" ~ "Gender: Prefer not to disclose (%)",
      Characteristic == "Under18" ~ "Age: Under 18 (%)",
      Characteristic == "Age18_24" ~ "Age: 18–24 (%)",
      Characteristic == "Age25_34" ~ "Age: 25–34 (%)",
      Characteristic == "Age35_44" ~ "Age: 35–44 (%)",
      Characteristic == "Age45Plus" ~ "Age: 45+ (%)",
      Characteristic == "Italian" ~ "Nationality: Italian (%)",
      Characteristic == "OtherNationalities" ~ "Nationality: Other (%)",
      Characteristic == "FullTime" ~ "Employment: Full-time (%)",
      Characteristic == "Students" ~ "Employment: Students (%)",
      Characteristic == "SelfEmployed" ~ "Employment: Self-employed (%)",
      Characteristic == "PartTime" ~ "Employment: Part-time (%)",
      Characteristic == "Unemployed" ~ "Employment: Unemployed (%)",
      Characteristic == "OtherOccupations" ~ "Employment: Other (%)",
      TRUE ~ Characteristic ))

# Generate gt table
summary_long %>%
  gt() %>%
  tab_header(
    title = md("**Table 1. Sample Characteristics of the Survey Respondents**")
  ) %>%
  cols_label(
    Characteristic = "Characteristic",
    Value = "Value"
  ) %>%
  fmt_number(
    columns = Value,
    decimals = 1
  ) %>%
  tab_source_note(
    source_note = "Note: Percentages reflect the distribution within the final sample (N = 528)."
  ) %>%
  opt_table_font(
    font = list(
      google_font("Times New Roman"),
      default_fonts()
    )
  ) %>%
  tab_options(
    table.font.size = px(12),
    heading.align = "left")

# ---------------------------------
# Table 2 - Social Media Engagement
# ---------------------------------
# Create tibble with counts and percentages
engagement_table <- tibble(
  `Engagement Level` = c("High", "Low"),
  `Number of Respondents` = c(220, 308)
) %>%
  mutate(`Percentage (%)` = round(`Number of Respondents` / sum(`Number of Respondents`) * 100, 1))

# Generate gt table
engagement_table %>%
  gt() %>%
  tab_header(
    title = md("**Table 2. Social Media Engagement Level of Respondents**")
  ) %>%
  cols_label(
    `Engagement Level` = "Engagement Level",
    `Number of Respondents` = "Number of Respondents",
    `Percentage (%)` = "Percentage (%)"
  ) %>%
  fmt_number(
    columns = c(`Number of Respondents`, `Percentage (%)`),
    decimals = 1
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. High engagement refers to respondents who reported using social media daily or more, while low engagement includes all other usage frequencies.")

# Print table
print(engagement_table)

# ------------------------------------
# Table 3 - Mean Discovery Frequencies
# ------------------------------------
# Data for means
mean_data <- tibble(
  Channel = c("Social Media", "Traditional Media"),
  Mean_Discovery_Frequency = c(2.86, 2.54))

# Generate gt table
mean_data %>%
  gt() %>%
  tab_header(
    title = md("**Table 3. Mean Discovery Frequency by Channel**")
  ) %>%
  cols_label(
    Channel = "Channel Type",
    Mean_Discovery_Frequency = "Mean Discovery Frequency"
  ) %>%
  fmt_number(
    columns = Mean_Discovery_Frequency,
    decimals = 2
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    column_labels.font.weight = "bold",
    table.border.top.color = "gray70",
    table.border.bottom.color = "gray70",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. Values represent the average frequency of skincare brand discovery across channels (1 = Never, 5 = Very Frequently).")

# -------------------------------
# Table 4 - Paired t-Test Results
# -------------------------------
# Prepare p-value label with stars
pvalue_numeric <- 4.908e-11
pvalue_label <- if (pvalue_numeric < .001) {
  "<0.001 ***"
} else if (pvalue_numeric < .01) {
  paste0(round(pvalue_numeric, 3), " **")
} else if (pvalue_numeric < .05) {
  paste0(round(pvalue_numeric, 3), " *")
} else {
  round(pvalue_numeric, 3)
}

# Create tibble with t-test results
ttest_table <- tibble(
  Statistic = c(
    "t-value",
    "Degrees of Freedom",
    "p-value",
    "Mean Difference",
    "95% CI Lower",
    "95% CI Upper"
  ),
  Value = c(
    round(6.7141, 3),
    "527",
    pvalue_label,
    round(0.3178662, 3),
    round(0.2248614, 3),
    round(0.4108709, 3)))

# Generate gt table
ttest_table %>%
  gt() %>%
  tab_header(
    title = md("**Table 4. Paired t-Test Comparing Social Media and Traditional Discovery Frequencies**")
  ) %>%
  cols_label(
    Statistic = "Statistic",
    Value = "Value"
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. * p < .05, ** p < .01, *** p < .001. The t-test compares mean discovery frequency across social media and traditional channels.")

# ---------------------------------------------------------------------
# Table 5 - Regression Model Predicting Discovery Frequency by Platform
# ---------------------------------------------------------------------
# Create tibble for regression results with TikTok before Other Social Media
regression_table <- tibble(
  Predictor = c(
    "(Intercept)",
    "SM Engagement (High)",
    "TikTok",
    "Other Social Media",
    "Traditional Media",
    "Engagement × TikTok",
    "Engagement × Other Social",
    "Engagement × Traditional"
  ),
  Estimate = c(3.747, 0.858, -1.623, -1.510, -1.170, -0.604, -0.845, -0.943),
  `Std. Error` = c(0.068, 0.105, 0.096, 0.096, 0.096, 0.149, 0.149, 0.149),
  `t-value` = c(55.088, 8.141, -16.877, -15.696, -12.169, -4.053, -5.669, -6.330),
  `p-value` = c(
    "<0.001 ***",
    "<0.001 ***",
    "<0.001 ***",
    "<0.001 ***",
    "<0.001 ***",
    "<0.001 ***",
    "<0.001 ***",
    "<0.001 ***" ))

# Create gt table
regression_table %>%
  gt() %>%
  tab_header(
    title = md("**Table 5. Regression Model Predicting Discovery Frequency by Platform (H1)**")
  ) %>%
  cols_label(
    Predictor = "Predictor",
    Estimate = "Estimate",
    `Std. Error` = "Std. Error",
    `t-value` = "t-value",
    `p-value` = "p-value"
  ) %>%
  fmt_number(
    columns = c(Estimate, `Std. Error`, `t-value`),
    decimals = 3
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. The model includes main effects and interaction terms. *** p < .001. The Reference Category for Engagement is High")

# --------------------------------
# Table 6 - Single MNL Regressions 
# --------------------------------
# Extract tidy results for each model
tidy_H2 <- tidy(model_H2) %>% mutate(Hypothesis = "H2: Platform")

tidy_H3 <- tidy(model_H3) %>% mutate(Hypothesis = "H3: Follower Count")
tidy_H4 <- tidy(model_H4) %>% mutate(Hypothesis = "H4: Influencer Type")
tidy_H5 <- tidy(model_H5) %>% mutate(Hypothesis = "H5: Shared Experience")
tidy_H6 <- tidy(model_H6) %>% mutate(Hypothesis = "H6: Discount")

# Combine all results into a single table and clean
all_models <- bind_rows(tidy_H2, tidy_H3, tidy_H4, tidy_H5, tidy_H6) %>%
  # Remove intercept rows
  filter(!str_detect(term, "Intercept")) %>%  
  # Round and format values
  mutate(
    Estimate = round(estimate, 3),
    `Std. Error` = round(std.error, 3),
    `z value` = round(statistic, 2),
    `p-value` = case_when(
      p.value < .001 ~ "<0.001 ***",
      p.value < .01 ~ paste0(round(p.value, 3), " **"),
      p.value < .05 ~ paste0(round(p.value, 3), " *"),
      TRUE ~ as.character(round(p.value, 3))
    )
  ) %>%
  select(Hypothesis, term, Estimate, `Std. Error`, `z value`, `p-value`)

# Create formatted gt table
all_models %>%
  gt() %>%
  tab_header(
    title = md("**Table 6. Multinomial Logit Model Results for H2–H6**")
  ) %>%
  cols_label(
    Hypothesis = "Hypothesis",
    term = "Predictor",
    Estimate = "Estimate",
    `Std. Error` = "Std. Error",
    `z value` = "z value",
    `p-value` = "p-value"
  ) %>%
  fmt_number(
    columns = c(Estimate, `Std. Error`, `z value`),
    decimals = 3
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = md(
      "Note. *** p < .001, ** p < .01, * p < .05. Positive coefficients indicate higher log-odds of selecting the brand relative to the reference category."))

# ---------------------------------------------
# Table 7: Variable Importance (MNL Full Model)
# ---------------------------------------------
importance_mnl %>%
  arrange(desc(RelativeImportance)) %>%
  gt() %>%
  tab_header(
    title = md("**Table 7. Relative Importance of Attributes (MNL Model)**")
  ) %>%
  cols_label(
    Attribute = "Attribute",
    Range = "Utility Range",
    RelativeImportance = "Relative Importance (%)"
  ) %>%
  fmt_number(
    columns = c(Range, RelativeImportance),
    decimals = 2
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = md(
      "Note. Relative importance computed as the range of utility estimates per attribute, normalized to sum to 100%.")
  )

# -------------------------------
# Table 8 - Likelihood Ratio Test 
# -------------------------------
# Example tibble for LR Test Results
lr_table <- tibble(
  Comparison = c(
    "Platform vs Full Model",
    "Follower Count vs Full Model",
    "Influencer Type vs Full Model",
    "Experience vs Full Model",
    "Discount vs Full Model"
  ),
  Chisq = c(2756.3, 1579.7, 1365.5, 2778, 1296.1),
  Df = c(7, 6, 6, 7, 6),
  p_value = c("<0.001 ***", "<0.001 ***", "<0.001 ***", "<0.001 ***", "<0.001 ***"))

# GT Table
lr_table %>%
  gt() %>%
  tab_header(
    title = md("**Table 8. Likelihood Ratio Tests Comparing Simpler Models to the Full Model**")
  ) %>%
  cols_label(
    Comparison = "Model Comparison",
    Chisq = "Chi-squared",
    Df = "Degrees of Freedom",
    p_value = "p-value"
  ) %>%
  fmt_number(columns = Chisq, decimals = 1) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. *** p < .001.")

# -------------
# Table 9 - AIC
# -------------
aic_df %>%
  arrange(AIC) %>%  # sort by AIC in ascending order
  gt() %>%
  tab_header(
    title = md("**Table 9. AIC Comparison Across Models**")
  ) %>%
  cols_label(
    Model = "Model",
    AIC = "AIC Value"
  ) %>%
  fmt_number(columns = AIC, decimals = 1) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. Lower AIC values indicate better model fit.")

# --------------
# Table 10 - IIA
# --------------
# Example tibble for Hausman-McFadden Test Results
hmf_table <- tibble(
  Comparison = c(
    "Full vs Platform-only",
    "Full vs Follower Count-only",
    "Full vs Influencer Type-only",
    "Full vs Experience-only",
    "Full vs Discount-only"
  ),
  Chisq = c(-637.72, -753.37, -492.56, -739.73, -809.51),
  Df = c(2, 3, 3, 2, 3),
  p_value = c("1.000", "1.000", "1.000", "1.000", "1.000"),
  Conclusion = c(
    "IIA not violated",
    "IIA not violated",
    "IIA not violated",
    "IIA not violated",
    "IIA not violated" ))

# GT Table
hmf_table %>%
  gt() %>%
  tab_header(
    title = md("**Table 10. Hausman-McFadden Tests for IIA Assumption**")
  ) %>%
  cols_label(
    Comparison = "Comparison",
    Chisq = "Chi-squared",
    Df = "Degrees of Freedom",
    p_value = "p-value",
    Conclusion = "Interpretation"
  ) %>%
  fmt_number(columns = Chisq, decimals = 2) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. High p-values indicate no evidence of IIA violation.")

# --------------------------------------------
# Table 11 - Relative Importance of Attributes
# --------------------------------------------
# STEP 1 – Rename columns
importance_df <- importance_df %>%
  rename(
    `Utility Range` = Range,
    `Relative Importance (%)` = RelativeImportance)

# STEP 2 – Remove zero-importance rows (if any)
importance_df_filtered <- importance_df %>%
  filter(`Utility Range` != 0)

# STEP 3 – Generate the table
importance_df_filtered %>%
  gt() %>%
  tab_header(
    title = md("**Table 11. Relative Importance of Attributes (HB)**")
  ) %>%
  cols_label(
    Attribute = "Attribute",
    `Utility Range` = "Utility Range",
    `Relative Importance (%)` = "Relative Importance (%)"
  ) %>%
  fmt_number(
    columns = c(`Utility Range`, `Relative Importance (%)`),
    decimals = 1
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. Relative importance was computed as the range of estimated utilities for each attribute.")

# --------------------
# Table 12 - MNL vs HB
# --------------------
# Hit Rate Table
model_comparison <- tibble::tibble(
  Metric = "Holdout Hit Rate (%)",
  `MNL Model` = 50.06,
  `HB Model` = 84.61)

# GT Table
model_comparison %>%
  gt() %>%
  tab_header(
    title = md("**Table 12. Predictive Accuracy Comparison Between MNL and HB**")
  ) %>%
  cols_label(
    Metric = "Metric",
    `MNL Model` = "MNL",
    `HB Model` = "Hierarchical Bayes"
  ) %>%
  fmt_number(
    columns = c(`MNL Model`, `HB Model`),
    decimals = 2
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. Holdout hit rate measures the percentage of correctly predicted choices on out-of-sample data.")

# -----------------------------------------
# Table 13 – Stable Pricing Perception
# -----------------------------------------
q18_table <- tibble(
  `Response (Q18)` = c(
    "1 = Strongly Disagree",
    "2 = Disagree",
    "3 = Neutral",
    "4 = Agree",
    "5 = Strongly Agree"
  ),
  `Count` = c(10, 62, 178, 213, 65),
  `Percentage (%)` = c(1.9, 11.7, 33.7, 40.3, 12.3))

q18_gt <- q18_table %>%
  gt() %>%
  tab_header(
    title = md("**Table 13. Stable Pricing Perceived Credibility**")
  ) %>%
  cols_label(
    `Response (Q18)` = "Response",
    `Count` = "Number of Respondents",
    `Percentage (%)` = "Percentage (%)"
  ) %>%
  fmt_number(
    columns = c(`Percentage (%)`),
    decimals = 1
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    heading.align = "left",
    column_labels.font.weight = "bold"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. Respondents rated their agreement with the statement 'Stable pricing increases brand credibility.'")

# Print table
q18_gt

# ---------------------------------------------
# Table 14 – Statistical Tests for Hypothesis 7
# ---------------------------------------------
# Create Table 
table14 <- tibble(
  `Test` = c(
    "One-sample t-test",
    "Paired-sample t-test",
    "Spearman Correlation"
  ),
  `Variables` = c(
    "Q18 vs. midpoint (3)",
    "Q18 vs. Q19",
    "Q18 & Q19"
  ),
  `Result` = c(
    "M = 3.49, 
    t(527) = 12.35",
    "Mean Diff. = 0.39",
    "ρ = – 0.42 (moderate, negative)"  # <--- Aggiornato con esempio di valore
  ),
  `p-value` = c(
    "< .001",
    "< .001",
    "< .001"
  ),
  `Interpretation` = c(
    "Agreement significantly above midpoint",
    "Credibility > Discount preference",
    "Higher credibility agreement predicts lower discount brand preference"))

# gt table
table14_gt <- table14 %>%
  gt() %>%
  tab_header(
    title = md("**Table 14. Statistical Tests Supporting H7**")
  ) %>%
  cols_label(
    `Test` = "Test",
    `Variables` = "Variables Involved",
    `Result` = "Statistical Results",
    `p-value` = "p-value",
    `Interpretation` = "Interpretation"
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    heading.align = "left",
    column_labels.font.weight = "bold"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. All tests support the hypothesis that stable pricing enhances perceived brand credibility.")

# Print table
table14_gt

# ---------------------------------------
# Table 15: Simple Linear Regression (H7)
# ---------------------------------------
# Create Table
table15 <- tibble(
  Predictor = c("Intercept", "Brand Preference (Q19)"),
  Estimate = c(4.1674, -0.2167),
  `Std. Error` = c(0.0783, 0.0222),
  `t value` = c(53.218, -9.744),
  `p-value` = c("<0.001 ***", "<0.001 ***"))

# gt Table
table15_gt <- table15 %>%
  gt() %>%
  tab_header(
    title = md("**Table 15. Simple Linear Regression Results for H7**")
  ) %>%
  cols_label(
    Predictor = "Predictor",
    Estimate = "Estimate",
    `Std. Error` = "Std. Error",
    `t value` = "t value",
    `p-value` = "p-value"
  ) %>%
  fmt_number(columns = c(Estimate, `Std. Error`, `t value`), decimals = 3) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    heading.align = "left",
    column_labels.font.weight = "bold"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = md("Note. *** p < .001. The model indicates that stronger preference for the discount-oriented brand is associated with lower credibility agreement.")
  )

# Print Table 15
table15_gt

# ------------------------------------------
# Table 16: Ordinal Logistic Regression (H7)
# ------------------------------------------
# Create Table
table16 <- tibble(
  Predictor = c(
    "Brand Preference (Q19)",
    "Gender: Male",
    "Gender: Non-Binary / Other",
    "Gender: Prefer not to say",
    "Age 25–34",
    "Age 35–44",
    "Age 45–54",
    "Age 55–64",
    "Age 65 or Older",
    "Age Under 18",
    "Occupation: Employed part-time",
    "Occupation: Other",
    "Occupation: Self-employed",
    "Occupation: Student",
    "Occupation: Unemployed",
    "Nationality: Non-Italian"
  ),
  Estimate = c(
    -0.5452, 0.1889, 0.2179, -1.0334,
    -0.1356, -0.2545, -0.6411, -0.6246, -2.1309, 0.0381,
    0.5298, 0.4998, 0.1839, 0.1277, -0.2359, 0.0930
  ),
  `Std. Error` = c(
    0.0565, 0.4002, 0.8116, 0.9126,
    0.2139, 0.3271, 0.3982, 0.6306, 0.7979, 1.2864,
    0.3725, 0.4472, 0.2910, 0.2752, 0.3821, 0.3621
  ),
  `z value` = c(
    -9.655, 0.472, 0.268, -1.133,
    -0.634, -0.778, -1.610, -0.990, -2.671, 0.030,
    1.422, 1.118, 0.632, 0.464, -0.618, 0.257
  ),
  `p-value` = c(
    "<0.001 ***", "0.637", "0.789", "0.257",
    "0.526", "0.437", "0.107", "0.322", "0.008 **", "0.976",
    "0.155", "0.264", "0.527", "0.643", "0.536", "0.798"))

# gt Table
table16_gt <- table16 %>%
  gt() %>%
  tab_header(
    title = md("**Table 16. Ordinal Logistic Regression Results for H7**")
  ) %>%
  cols_label(
    Predictor = "Predictor",
    Estimate = "Estimate",
    `Std. Error` = "Std. Error",
    `z value` = "z value",
    `p-value` = "p-value"
  ) %>%
  fmt_number(columns = c(Estimate, `Std. Error`, `z value`), decimals = 3) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    heading.align = "left",
    column_labels.font.weight = "bold"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = md("Note. *** p < .001, ** p < .01. Q18 was modeled as an ordinal outcome. Only brand preference (Q19) and the 65+ age group were statistically significant predictors.")
  )

# Print Table 16
table16_gt

# --------------------------------------
# Table 17 - Influencer Type Trust (Q15)
# --------------------------------------
# GT Table Q15
q15_summary %>%
  gt() %>%
  tab_header(
    title = md("**Table 17. Trust in Skincare Recommendations by Influencer Type**")
  ) %>%
  cols_label(
    Influencer_Type = "Influencer Type",
    Mean = "Mean Trust",
    SD = "Standard Deviation",
    N = "N"
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    source_note = "Note. Ratings were provided on a scale from 1 (Not at all) to 5 (Completely) in response to the question: 'How much do you trust skincare recommendations from each of the following types of influencers?'") 

# ========================
#     APPENDIX Figures 
# ========================
# ---------------------------------------------------
# Figure 1 - HB Estimated Part-Worth Utilities (Bars)
# ---------------------------------------------------
ggplot(posterior_df, aes(x = reorder(Attribute, Utility), y = Utility, fill = Utility > 0)) +
  geom_col(show.legend = FALSE) +
  # geom_text() removed
  coord_flip() +
  labs(
    title = "Figure 1: HB Estimated Part-Worth Utilities",
    x = "Attribute Level",
    y = "Utility (log-odds)"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#66c2a5", "FALSE" = "#fc8d62")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"))

# ----------------------------------------------------------
# Figure 2 - Distribution of Individual Part-Worth Utilities
# ----------------------------------------------------------
ggplot(individual_long, aes(x = Utility)) +
  geom_histogram(
    bins = 30,
    fill = "#4169E1",
    color = "white",
    alpha = 0.8
  ) +
  facet_wrap(~ Attribute, scales = "free") +
  labs(
    title = "Figure 2: Distribution of Individual-Level Part-Worth Utilities",
    x = "Utility (log-odds)",
    y = "Number of Respondents"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"))

# -----------------------------------------------------
# Figure 3 - Density of Part-Worth Utilities by Cluster
# -----------------------------------------------------
ggplot(ind_long, aes(x = Utility, fill = cluster)) +
  geom_density(alpha = 0.4) +
  geom_vline(
    data = means_df,
    aes(xintercept = mean_utility, color = cluster),
    linetype = "dashed",
    size = 0.7
  ) +
  facet_wrap(~ Attribute, scales = "free") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Figure 3: Distribution of Part-Worth Utilities by Cluster",
    x = "Utility (log-odds)",
    y = "Density",
    fill = "Cluster",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"))

# --------------------------------------------
# Figure 4 - Comparison of Estimated Utilities
# --------------------------------------------
# Create a tibble with estimated utilities from MNL and HB models for each attribute level
compare_df <- tibble::tibble(
  Attribute = c(
    "Platform_TikTok", "Follower_Count900K", "Follower_Count38K",
    "Influencer_Type_Dermatologist", "Influencer_Type_Cosmetologist",
    "Shared_pers_expPositive", "Discount_20%", "Discount_No"
  ),
  Utility_MNL = c(
    -6.42, 0.52, 3.58,
    1.97, 2.56,
    10.1, -1.72, 3.74
  ),
  Utility_HB = c(
    -6.81, 2.27, 3.58,
    -1.96, 1.01,
    8.08, -4.3, 3.74
  )
) %>%
  tidyr::pivot_longer(
    cols = starts_with("Utility"),
    names_to = "Model",
    values_to = "Utility"
  ) %>%
  mutate(
    Model = dplyr::recode(Model,
                          "Utility_MNL" = "MNL",
                          "Utility_HB"  = "HB"))
# Print Table
print(compare_df)

# Plot Figure
(ggplot(compare_df, aes(x = reorder(Attribute, Utility), y = Utility, fill = Model)) +
    geom_col(show.legend = FALSE) +  
    geom_text(                      
      aes(label = round(Utility, 2)),
      hjust = ifelse(compare_df$Utility > 0, -0.1, 1.1),
      size = 3.5,
      color = "black"
    ) +
    coord_flip() +              
    facet_wrap(~Model, ncol = 2) +  
    labs(
      title = "Figure 4: Estimated Part-Worth Utilities Comparison",
      x = "Attribute Level",
      y = "Utility (log-odds)"
    ) +
    scale_fill_brewer(palette = "Set2") +  
    theme_minimal(base_size = 13) +   
    expand_limits(y = -8) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid = element_blank()))

# --------------------------------------------
# Figure 5 - Comparison of Estimated Utilities
# --------------------------------------------
survey %>%
  count(Q19) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = factor(Q19), y = Percentage)) +
  geom_bar(stat = "identity", fill = "darkslateblue") +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5) +
  labs(
    title = "Figure 5: Brand Preference (Stable vs Discount Pricing) (Q19)",
    subtitle = "1 = Strong Preference for Brand A (Stable), 7 = Brand B (Discount)",
    x = "Preference Scale",
    y = "Percentage (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"))

