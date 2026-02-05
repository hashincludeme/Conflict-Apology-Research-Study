# ===============================================================================
# Research Project: Apology and Conflict Study Analysis
# Author: Keerthana
# Date: February 2026
# ===============================================================================
# This script tests 4 hypotheses about gender differences in apology scenarios
# ===============================================================================

# STEP 1: Install and load required packages
# ===============================================================================
cat("\n=== STEP 1: Installing and Loading Packages ===\n")

# List of required packages
required_packages <- c(
  "readxl",      # Read Excel files
  "dplyr",       # Data manipulation
  "tidyr",       # Data tidying
  "ggplot2",     # Visualization
  "car",         # Statistical tests (Levene's test)
  "effsize",     # Effect size calculations
  "psych",       # Descriptive statistics
  "emmeans",     # Post-hoc comparisons
  "janitor",     # Data cleaning
  "writexl"      # Export to Excel
)

# Install packages if not already installed
new_packages <- required_packages[
  !(required_packages %in% installed.packages()[, "Package"])
]
if (length(new_packages)) {
  cat("Installing packages:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages, repos = "https://cloud.r-project.org/")
}

# Load all packages
cat("Loading packages...\n")
invisible(lapply(required_packages, library, character.only = TRUE))
cat("All packages loaded successfully!\n\n")


# STEP 2: Load and examine the data
# ===============================================================================
cat("=== STEP 2: Loading Data ===\n")

# Set file path
data_file <- "Data - Winter 2026.xlsx"

# Read the data sheet
data <- read_excel(data_file, sheet = "Data")

# Read the variable key
var_key <- read_excel(data_file, sheet = "Key - variable meanings")

cat("Data loaded successfully!\n")
cat("Dataset dimensions:", nrow(data), "rows x", ncol(data), "columns\n\n")


# STEP 3: Data cleaning and preparation
# ===============================================================================
cat("=== STEP 3: Data Cleaning ===\n")

# Create clean dataset
df <- data %>%
  # Filter completed responses only (Finished is logical TRUE/FALSE)
  filter(Finished == TRUE) %>%
  mutate(
    # Gender: sex and target_sex are already "Male"/"Female"/"Other" text
    respondent_gender = sex,
    target_gender = target_sex,

    # Conflict type: real_imaginary is full text
    conflict_type = case_when(
      grepl("real", real_imaginary, ignore.case = TRUE) ~ "Real",
      grepl("imagin", real_imaginary, ignore.case = TRUE) ~ "Imaginary",
      TRUE ~ NA_character_
    ),

    # Cross-gender indicator
    cross_gender = case_when(
      respondent_gender == "Male" & target_gender == "Female" ~ "Male-Female",
      respondent_gender == "Female" & target_gender == "Male" ~ "Female-Male",
      respondent_gender == "Male" & target_gender == "Male" ~ "Male-Male",
      respondent_gender == "Female" & target_gender == "Female" ~ "Female-Female",
      TRUE ~ "Other"
    ),

    # Binary outcomes: text strings to numeric codes
    # outcome_binary1: "I apologize first, then ... apologizes." vs
    #                  "Neither I nor ... apologizes."
    outcome_binary1_num = case_when(
      grepl("I apologize first, then", outcome_binary1) ~ 1,
      grepl("Neither", outcome_binary1) ~ 2,
      TRUE ~ NA_real_
    ),
    # outcome_binary2: "Neither I nor ... apologizes." vs
    #                  "I apologize first, but ... does not"
    outcome_binary2_num = case_when(
      grepl("Neither", outcome_binary2) ~ 1,
      grepl("I apologize first, but", outcome_binary2) ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  # Filter only Male and Female respondents for main analyses
  filter(respondent_gender %in% c("Male", "Female"))

cat("Data cleaned successfully!\n")
cat("Final sample size:", nrow(df), "participants\n")
cat("Males:", sum(df$respondent_gender == "Male"), "\n")
cat("Females:", sum(df$respondent_gender == "Female"), "\n\n")


# STEP 4: Descriptive Statistics
# ===============================================================================
cat("=== STEP 4: Descriptive Statistics ===\n\n")

# Overall demographics
cat("Demographics Summary:\n")
cat("---------------------\n")
print(table(df$respondent_gender))
cat("\nAge statistics:\n")
print(summary(df$age))

# Conflict type by gender
cat("\n\nConflict Type by Gender:\n")
cat("------------------------\n")
conflict_table <- table(df$respondent_gender, df$conflict_type)
print(conflict_table)
print(prop.table(conflict_table, margin = 1))

cat("\n\n")


# ===============================================================================
# HYPOTHESIS 1: Gender and Negative Feelings
# ===============================================================================
# H1: Women will report significantly more negative feelings than men
#     when imagining a scenario where they apologize first but the other
#     person does not apologize back.
# ===============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("HYPOTHESIS 1: Gender and Negative Feelings\n")
cat(rep("=", 80), "\n", sep = "")

cat("\nResearch Question: Do women report more negative feelings than men\n")
cat("when they apologize first but the other person doesn't apologize back?\n\n")

# Prepare data for H1
h1_data <- df %>%
  filter(!is.na(feelings_youalone)) %>%
  select(ResponseId, respondent_gender, feelings_youalone)

cat("Sample size for H1:", nrow(h1_data), "\n")
cat("Males:", sum(h1_data$respondent_gender == "Male"), "\n")
cat("Females:", sum(h1_data$respondent_gender == "Female"), "\n\n")

# Descriptive statistics
cat("Descriptive Statistics:\n")
cat("-----------------------\n")
h1_desc <- h1_data %>%
  group_by(respondent_gender) %>%
  summarise(
    N = n(),
    Mean = mean(feelings_youalone, na.rm = TRUE),
    SD = sd(feelings_youalone, na.rm = TRUE),
    Median = median(feelings_youalone, na.rm = TRUE),
    Min = min(feelings_youalone, na.rm = TRUE),
    Max = max(feelings_youalone, na.rm = TRUE)
  )
print(h1_desc)

# Test for normality
cat("\n\nNormality Tests (Shapiro-Wilk):\n")
cat("--------------------------------\n")
male_feelings <- h1_data$feelings_youalone[h1_data$respondent_gender == "Male"]
female_feelings <- h1_data$feelings_youalone[h1_data$respondent_gender == "Female"]

if (length(male_feelings) >= 3) {
  shapiro_male <- shapiro.test(male_feelings)
  cat("Males: W =", round(shapiro_male$statistic, 4), ", p =",
      format.pval(shapiro_male$p.value, digits = 3), "\n")
} else {
  cat("Males: Sample size too small for Shapiro-Wilk test (n < 3)\n")
}

if (length(female_feelings) >= 3) {
  shapiro_female <- shapiro.test(female_feelings)
  cat("Females: W =", round(shapiro_female$statistic, 4), ", p =",
      format.pval(shapiro_female$p.value, digits = 3), "\n")
} else {
  cat("Females: Sample size too small for Shapiro-Wilk test (n < 3)\n")
}

# Test for homogeneity of variance
cat("\n\nLevene's Test for Homogeneity of Variance:\n")
cat("------------------------------------------\n")
h1_data$respondent_gender <- factor(h1_data$respondent_gender)
levene_h1 <- leveneTest(feelings_youalone ~ respondent_gender, data = h1_data)
print(levene_h1)

# Independent samples t-test
cat("\n\nIndependent Samples t-test:\n")
cat("---------------------------\n")
t_test_h1 <- t.test(feelings_youalone ~ respondent_gender, data = h1_data,
                    var.equal = FALSE)
print(t_test_h1)

# Effect size (Cohen's d)
cat("\n\nEffect Size (Cohen's d):\n")
cat("------------------------\n")
cohens_d_h1 <- effsize::cohen.d(female_feelings, male_feelings)
print(cohens_d_h1)

# Mann-Whitney U test (non-parametric alternative)
cat("\n\nMann-Whitney U Test (Non-parametric):\n")
cat("-------------------------------------\n")
wilcox_h1 <- wilcox.test(feelings_youalone ~ respondent_gender, data = h1_data)
print(wilcox_h1)

# Visualization
cat("\n\nCreating visualization for H1...\n")
h1_plot <- ggplot(h1_data, aes(x = respondent_gender, y = feelings_youalone,
                               fill = respondent_gender)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "red") +
  scale_fill_manual(values = c("Male" = "#4285F4", "Female" = "#EA4335")) +
  labs(
    title = "H1: Negative Feelings When Apologizing First (No Reciprocation)",
    subtitle = "Red diamond = mean; Box = median and quartiles",
    x = "Respondent Gender",
    y = "Feelings Rating (-30 = Very Negative, 30 = Very Positive)",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")

ggsave("H1_gender_feelings_plot.png", h1_plot,
       width = 10, height = 6, dpi = 300)

cat("H1 analysis complete!\n\n")


# ===============================================================================
# HYPOTHESIS 2: Real vs Imaginary Conflicts
# ===============================================================================
# H2: Men tend to recall real conflicts more than women;
#     women tend to imagine fictional conflicts more than men.
# ===============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("HYPOTHESIS 2: Real vs Imaginary Conflicts by Gender\n")
cat(rep("=", 80), "\n", sep = "")

cat("\nResearch Question: Do men recall real conflicts more than women,\n")
cat("while women imagine fictional conflicts more than men?\n\n")

# Prepare data for H2
h2_data <- df %>%
  filter(!is.na(conflict_type)) %>%
  select(ResponseId, respondent_gender, conflict_type)

cat("Sample size for H2:", nrow(h2_data), "\n\n")

# Contingency table
cat("Contingency Table:\n")
cat("------------------\n")
h2_table <- table(h2_data$respondent_gender, h2_data$conflict_type)
print(h2_table)

cat("\n\nRow Proportions (% within gender):\n")
cat("-----------------------------------\n")
h2_prop <- prop.table(h2_table, margin = 1)
print(round(h2_prop * 100, 2))

# Chi-square test
cat("\n\nChi-Square Test of Independence:\n")
cat("---------------------------------\n")
chi_h2 <- chisq.test(h2_table)
print(chi_h2)

# Effect size (Cramer's V)
cat("\n\nEffect Size (Cramer's V):\n")
cat("-------------------------\n")
cramers_v <- sqrt(chi_h2$statistic / sum(h2_table))
cat("Cramer's V =", round(cramers_v, 4), "\n")

# Fisher's exact test (for small sample sizes)
cat("\n\nFisher's Exact Test:\n")
cat("--------------------\n")
fisher_h2 <- fisher.test(h2_table)
print(fisher_h2)

# Visualization
cat("\n\nCreating visualization for H2...\n")
h2_plot_data <- h2_data %>%
  group_by(respondent_gender, conflict_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(respondent_gender) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100
  )

h2_plot <- ggplot(h2_plot_data, aes(x = respondent_gender, y = percentage,
                                    fill = conflict_type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0(round(percentage, 1), "%\n(n=", count, ")")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Real" = "#34A853", "Imaginary" = "#FBBC04")) +
  labs(
    title = "Hypothesis 2: Real vs Imaginary Conflicts by Gender",
    subtitle = paste0("Chi-sq = ", round(chi_h2$statistic, 2),
                     ", p = ", format.pval(chi_h2$p.value, digits = 3)),
    x = "Respondent Gender",
    y = "Percentage (%)",
    fill = "Conflict Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  ) +
  ylim(0, 110)

ggsave("H2_conflict_type_plot.png", h2_plot,
       width = 10, height = 6, dpi = 300)

cat("H2 analysis complete!\n\n")


# ===============================================================================
# HYPOTHESIS 3: Response Consistency
# ===============================================================================
# H3: Men show more inconsistency than women when answering repeated
#     preference questions about apology outcomes.
# ===============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("HYPOTHESIS 3: Response Consistency\n")
cat(rep("=", 80), "\n", sep = "")

cat("\nResearch Question: Do men show more inconsistency than women when\n")
cat("answering repeated preference questions about apology outcomes?\n\n")

# Prepare data for H3
# outcome_binary1_num: 1 = Both (you first), 2 = Neither
# outcome_binary2_num: 1 = Neither, 2 = You alone
h3_data <- df %>%
  filter(!is.na(outcome_binary1_num) & !is.na(outcome_binary2_num)) %>%
  select(ResponseId, respondent_gender,
         outcome_binary1_num, outcome_binary2_num) %>%
  mutate(
    # If they prefer "Both (you first)" over "Neither" in Q1,
    # they should also prefer "Neither" over "You alone" in Q2
    q1_prefers_apology = (outcome_binary1_num == 1),
    q2_prefers_neither = (outcome_binary2_num == 1),

    # Consistent: both TRUE or both FALSE
    consistent = (q1_prefers_apology == q2_prefers_neither),
    inconsistent = !consistent
  )

cat("Sample size for H3:", nrow(h3_data), "\n")
cat("Males:", sum(h3_data$respondent_gender == "Male"), "\n")
cat("Females:", sum(h3_data$respondent_gender == "Female"), "\n\n")

# Calculate inconsistency rates
cat("Inconsistency Rates:\n")
cat("--------------------\n")
h3_summary <- h3_data %>%
  group_by(respondent_gender) %>%
  summarise(
    N = n(),
    N_Inconsistent = sum(inconsistent),
    N_Consistent = sum(consistent),
    Inconsistency_Rate = mean(inconsistent) * 100
  )
print(h3_summary)

# Contingency table
cat("\n\nContingency Table:\n")
cat("------------------\n")
h3_table <- table(h3_data$respondent_gender, h3_data$inconsistent)
colnames(h3_table) <- c("Consistent", "Inconsistent")
print(h3_table)

# Chi-square test
cat("\n\nChi-Square Test:\n")
cat("-----------------\n")
chi_h3 <- chisq.test(h3_table)
print(chi_h3)

# Fisher's exact test
cat("\n\nFisher's Exact Test:\n")
cat("--------------------\n")
fisher_h3 <- fisher.test(h3_table)
print(fisher_h3)

# Logistic regression
cat("\n\nLogistic Regression (Inconsistency ~ Gender):\n")
cat("---------------------------------------------\n")
h3_data$gender_numeric <- ifelse(h3_data$respondent_gender == "Male", 1, 0)
logit_h3 <- glm(inconsistent ~ gender_numeric,
                data = h3_data, family = binomial)
print(summary(logit_h3))

# Odds ratio
cat("\n\nOdds Ratio (Male vs Female):\n")
cat("----------------------------\n")
or_h3 <- exp(coef(logit_h3)[2])
or_ci <- exp(confint(logit_h3)[2, ])
cat("OR =", round(or_h3, 3), "\n")
cat("95% CI: [", round(or_ci[1], 3), ",", round(or_ci[2], 3), "]\n")

# Visualization
cat("\n\nCreating visualization for H3...\n")
h3_plot <- ggplot(h3_summary, aes(x = respondent_gender,
                                  y = Inconsistency_Rate,
                                  fill = respondent_gender)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(round(Inconsistency_Rate, 1), "%\n(",
                               N_Inconsistent, "/", N, ")")),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Male" = "#4285F4", "Female" = "#EA4335")) +
  labs(
    title = "Hypothesis 3: Response Inconsistency by Gender",
    subtitle = paste0("Fisher's exact test: p = ",
                     format.pval(fisher_h3$p.value, digits = 3)),
    x = "Respondent Gender",
    y = "Inconsistency Rate (%)",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11),
    legend.position = "none"
  ) +
  ylim(0, max(h3_summary$Inconsistency_Rate) * 1.3 + 5)

ggsave("H3_consistency_plot.png", h3_plot,
       width = 10, height = 6, dpi = 300)

cat("H3 analysis complete!\n\n")


# ===============================================================================
# HYPOTHESIS 4: Cross-Gender Blame Attribution
# ===============================================================================
# H4: Women blame male colleagues more than men blame female colleagues
#     in cross-gender conflicts.
# ===============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("HYPOTHESIS 4: Cross-Gender Blame Attribution\n")
cat(rep("=", 80), "\n", sep = "")

cat("\nResearch Question: Do women blame male colleagues more than\n")
cat("men blame female colleagues in cross-gender conflicts?\n\n")

# blame_1 is a slider: 0-100
# Lower = blame the OTHER person more, Higher = blame YOURSELF more
# other_blame = 100 - blame_1

h4_data <- df %>%
  filter(cross_gender %in% c("Male-Female", "Female-Male")) %>%
  filter(!is.na(blame_1)) %>%
  mutate(
    other_blame = 100 - blame_1,
    self_blame = blame_1
  ) %>%
  select(ResponseId, respondent_gender, target_gender, cross_gender,
         blame_1, other_blame, self_blame)

cat("Sample size for H4:", nrow(h4_data), "\n")
cat("Male respondents with female targets:",
    sum(h4_data$cross_gender == "Male-Female"), "\n")
cat("Female respondents with male targets:",
    sum(h4_data$cross_gender == "Female-Male"), "\n\n")

# Descriptive statistics
cat("Descriptive Statistics (Other Blame Score):\n")
cat("--------------------------------------------\n")
h4_desc <- h4_data %>%
  group_by(cross_gender) %>%
  summarise(
    N = n(),
    Mean_Other_Blame = mean(other_blame, na.rm = TRUE),
    SD_Other_Blame = sd(other_blame, na.rm = TRUE),
    Median_Other_Blame = median(other_blame, na.rm = TRUE),
    Mean_Self_Blame = mean(self_blame, na.rm = TRUE),
    SD_Self_Blame = sd(self_blame, na.rm = TRUE)
  )
print(h4_desc)

# Test for normality
cat("\n\nNormality Tests:\n")
cat("----------------\n")
male_female_blame <- h4_data$other_blame[
  h4_data$cross_gender == "Male-Female"
]
female_male_blame <- h4_data$other_blame[
  h4_data$cross_gender == "Female-Male"
]

if (length(male_female_blame) >= 3) {
  shapiro_mf <- shapiro.test(male_female_blame)
  cat("Male->Female: W =", round(shapiro_mf$statistic, 4), ", p =",
      format.pval(shapiro_mf$p.value, digits = 3), "\n")
} else {
  cat("Male->Female: Too few observations (n =",
      length(male_female_blame), ")\n")
}

if (length(female_male_blame) >= 3) {
  shapiro_fm <- shapiro.test(female_male_blame)
  cat("Female->Male: W =", round(shapiro_fm$statistic, 4), ", p =",
      format.pval(shapiro_fm$p.value, digits = 3), "\n")
} else {
  cat("Female->Male: Too few observations (n =",
      length(female_male_blame), ")\n")
}

# Levene's test
cat("\n\nLevene's Test:\n")
cat("--------------\n")
h4_data$cross_gender <- factor(h4_data$cross_gender)
levene_h4 <- leveneTest(other_blame ~ cross_gender, data = h4_data)
print(levene_h4)

# Independent samples t-test
cat("\n\nIndependent Samples t-test:\n")
cat("---------------------------\n")
t_test_h4 <- t.test(other_blame ~ cross_gender, data = h4_data,
                    var.equal = FALSE)
print(t_test_h4)

# Effect size
cat("\n\nEffect Size (Cohen's d):\n")
cat("------------------------\n")
cohens_d_h4 <- effsize::cohen.d(female_male_blame, male_female_blame)
print(cohens_d_h4)

# Mann-Whitney U test
cat("\n\nMann-Whitney U Test:\n")
cat("--------------------\n")
wilcox_h4 <- wilcox.test(other_blame ~ cross_gender, data = h4_data)
print(wilcox_h4)

# Visualization
cat("\n\nCreating visualization for H4...\n")
h4_plot <- ggplot(h4_data, aes(x = cross_gender, y = other_blame,
                               fill = respondent_gender)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  stat_summary(fun = mean, geom = "point",
               shape = 23, size = 4, fill = "red") +
  scale_fill_manual(values = c("Male" = "#4285F4", "Female" = "#EA4335")) +
  scale_x_discrete(labels = c("Male-Female" = "Men blaming\nwomen",
                              "Female-Male" = "Women blaming\nmen")) +
  labs(
    title = "H4: Blame Attribution in Cross-Gender Conflicts",
    subtitle = "Red diamond = mean; Higher = blame other person more",
    x = "Cross-Gender Conflict Type",
    y = "Other Person Blame Score (0-100)",
    fill = "Respondent\nGender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11),
    legend.position = "right"
  ) +
  geom_hline(yintercept = 50, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  annotate("text", x = 0.6, y = 52, label = "Equal blame (50)",
           color = "gray40", size = 3)

ggsave("H4_blame_attribution_plot.png", h4_plot,
       width = 10, height = 6, dpi = 300)

cat("H4 analysis complete!\n\n")


# ===============================================================================
# SUMMARY OF RESULTS
# ===============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("SUMMARY OF ALL HYPOTHESES\n")
cat(rep("=", 80), "\n\n", sep = "")

# Create summary table
summary_results <- data.frame(
  Hypothesis = c(
    "H1: Women more negative feelings",
    "H2: Gender differences in conflict type",
    "H3: Men more inconsistent",
    "H4: Women blame men more"
  ),
  Test = c(
    "Independent t-test",
    "Chi-square test",
    "Fisher's exact test",
    "Independent t-test"
  ),
  Statistic = c(
    paste0("t = ", round(t_test_h1$statistic, 3)),
    paste0("Chi-sq = ", round(chi_h2$statistic, 3)),
    paste0("OR = ", round(fisher_h3$estimate, 3)),
    paste0("t = ", round(t_test_h4$statistic, 3))
  ),
  P_Value = c(
    format.pval(t_test_h1$p.value, digits = 3),
    format.pval(chi_h2$p.value, digits = 3),
    format.pval(fisher_h3$p.value, digits = 3),
    format.pval(t_test_h4$p.value, digits = 3)
  ),
  Effect_Size = c(
    paste0("d = ", round(abs(cohens_d_h1$estimate), 3)),
    paste0("V = ", round(cramers_v, 3)),
    paste0("OR = ", round(fisher_h3$estimate, 3)),
    paste0("d = ", round(abs(cohens_d_h4$estimate), 3))
  ),
  Supported = c(
    ifelse(t_test_h1$p.value < 0.05, "YES", "NO"),
    ifelse(chi_h2$p.value < 0.05, "YES", "NO"),
    ifelse(fisher_h3$p.value < 0.05, "YES", "NO"),
    ifelse(t_test_h4$p.value < 0.05, "YES", "NO")
  )
)

print(summary_results)

# Export summary to Excel
cat("\n\nExporting results to Excel...\n")
write_xlsx(summary_results, "hypothesis_summary.xlsx")

cat("\n", rep("=", 80), "\n", sep = "")
cat("ANALYSIS COMPLETE!\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Output files created:\n")
cat("- H1_gender_feelings_plot.png\n")
cat("- H2_conflict_type_plot.png\n")
cat("- H3_consistency_plot.png\n")
cat("- H4_blame_attribution_plot.png\n")
cat("- hypothesis_summary.xlsx\n\n")

cat("All analyses have been completed successfully!\n")
