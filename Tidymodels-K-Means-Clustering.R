# LOAD THE NECESSARY PACKAGES
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,janitor,broom,plotly)


# READ IN THE RAW DATA
employed <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv")

# TIDY THE DATA AND AGGREGATE THE EMPLOYMENT NUMBERS BY OCCUPATION AND RACE
employed_tidy <- employed %>%
  # REMOVE ROWS WHERE THE EMPLOY_N VALUE IS NA
  filter(!is.na(employ_n)) %>%
  group_by(occupation = paste(industry, minor_occupation), race_gender) %>%
  summarise(n = mean(employ_n)) %>%
  ungroup()

employment_demo <- employed_tidy %>%
  filter(race_gender %in% c("Women", "Black or African American", "Asian")) %>%
  pivot_wider(names_from = race_gender, values_from = n, values_fill = 0) %>%
  janitor::clean_names() %>%
  left_join(employed_tidy %>%
              filter(race_gender == "TOTAL") %>%
              select(-race_gender) %>%
              rename(total = n)) %>%
  filter(total > 1e3) %>%
  # CONVERT COUNTS TO PROPORTIONS
  mutate(across(c(asian, black_or_african_american, women), ~ . / (total)),
         # CONVERT TOTAL TO LOG DUE TO LOG NORMAL DISTRIBUTION OF VALUES
         total = log(total),
         # CENTER AND SCALE NUMERIC VALUES FOR K-MEANS AND CONVERT BACK TO NUMERIC VALUES
         across(where(is.numeric), ~ as.numeric(scale(.)))
  ) %>%
  # TIDY UP THE OCCUPATION VARIABLE SO THAT ALL VALUES ARE LOWER CASE AND UNDERSCORED
  mutate(occupation = snakecase::to_snake_case(occupation))

# RUN K-MEANS CLUSTERING ALGORITHM, REMOVING ANY NON-NUMERIC COLUMNS
employment_clust <- kmeans(select(employment_demo, -occupation), centers = 3)

# SUMMARISES THE COMPONENTS OF THE K-MEANS CLUSTER OBJECT
broom::tidy(employment_clust)

# JOINS THE MODEL OBJECT TO THE ORIGINAL DATASET TO SEE WHICH CLUSTER EACH ROW BELONGS TO
augment(employment_clust, employment_demo) %>%
  ggplot(aes(total, black_or_african_american, color = .cluster)) +
  geom_point()

# CHOOSING THE BEST VALUE FOR K
kclusts <-
  tibble(k = 1:9) %>%
  mutate(
    # 
    kclust = purrr::map(k, ~ kmeans(select(employment_demo, -occupation), .x)),
    # CREATE ONE ROW SUMMARIES FOR EACH VALUE OF K WITH SUMMARIES INCLUDING GOODNESS OF FIT, P-VALUES FOR HYPOTHESIS TEST ON RESIDULAS
    glanced = map(kclust, glance),
    # SUMMARISE THE COMPONENTS OF THE K-MEANS CLUSTER OBJECT
    tidied = map(kclust, tidy),
    # ADDS PREDICTED VALUE INFORMATION TO EACH OBSERVATION 
    augmented = map(kclust, augment, employment_demo)
  )

# PLOT THE TOTAL WITHINNESS SUM OF SQUARES FOR EACH VALUE OF K
kclusts %>%
  unnest(cols = c(glanced)) %>%
  ggplot(aes(k, tot.withinss)) +
  geom_line(alpha = 0.5, size = 1.2, color = "midnightblue") +
  geom_point(size = 2, color = "midnightblue") +
  labs(title = "Within groups Sums of Squares vs. the Number of Clusters", 
       x = "Number of Clusters", y = "Within groups Sums of Squares")

# UPDATE THE NUMBER OF CLUSTERS BASED ON THE WITHIN SUM OF SQUARES PLOT ABOVE
final_clust <- kmeans(select(employment_demo, -occupation), centers = 5)

# PLOT OF TOTAL PEOPLE EMPLOYED PER CATEGORY (WOMEN) AND COLOUR BY CLUSTER
p <- augment(final_clust, employment_demo) %>%
  ggplot(aes(total, women, color = .cluster, name = occupation)) +
  geom_point(alpha = 0.8)

ggplotly(p, height = 500)
