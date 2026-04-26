# =============================================================================
# DESCRIPTIVE STATISTICS — FINAL PROJECT
# Incarceration in the United States
# =============================================================================
#
# RESEARCH QUESTION:
#   Do U.S. counties with similar crime rates show comparable levels of
#   incarceration, or does incarceration vary substantially beyond what
#   crime levels alone can explain?
#
# HYPOTHESIS 1:
#   At similar levels of violent crime per 100k population, incarceration
#   rates across U.S. counties differ by a wide margin — meaning crime rates
#   explain only a limited share of the variation in how much counties
#   incarcerate.
#
# HYPOTHESIS 2:
#   The "excess" incarceration (the portion not explained by local crime
#   levels) is systematically higher in some regions and in rural counties
#   compared to the rest of the country.
#
# DATA SOURCE:
#   Vera Institute — Incarceration Trends dataset
#   https://github.com/rfordatascience/tidytuesday (2019-01-22)
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. LOAD LIBRARIES AND DATA
# -----------------------------------------------------------------------------

library(tidyverse)
library(plotly)
library(patchwork)
library(ggthemes)

prison_tb <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/main/data/2019/2019-01-22/incarceration_trends.csv"
) |>
  janitor::clean_names()


# -----------------------------------------------------------------------------
# 2. DATA PREPARATION
# -----------------------------------------------------------------------------
# We keep only columns relevant to our research question, and apply several
# filters to remove noisy or unreliable observations:
#   - counties with adult population < 10,000 (rates become unstable)
#   - counties where UCR crime data is missing or covers <50% of the population
#     (partial reporting would falsely show "zero crimes")
#   - rows with missing values in any key variable
#   - rows with zero incarceration (likely missing data coded as zero)
#
# We then compute two key metrics:
#   - incarceration_rate_100k_pop: jail + prison population per 100k adults
#   - violent_crime_rate_100k_pop: violent crimes per 100k people in the
#     reporting jurisdiction

prison_tb_ready <- prison_tb |>
  select(year, fips, state, county_name, region, urbanicity,
         total_pop_15to64, ucr_population,
         total_jail_pop, total_prison_pop, violent_crime) |>
  filter(total_pop_15to64 >= 10000,
         ucr_population > 0,
         ucr_population >= 0.5 * total_pop_15to64) |>
  drop_na(violent_crime, total_jail_pop, total_prison_pop,
          ucr_population, total_pop_15to64) |>
  mutate(
    incarceration_rate_100k_pop = (total_jail_pop + total_prison_pop) /
      total_pop_15to64 * 100000,
    violent_crime_rate_100k_pop = violent_crime / ucr_population * 100000
  ) |>
  filter(incarceration_rate_100k_pop > 0)

# To answer the research question, we average each county across all years.
# This gives us one stable observation per county — reducing noise from
# year-to-year fluctuations.
# We also compute inc_to_crime_ratio: how many incarcerated people per one
# violent crime. This is our proxy for "excess incarceration" — the larger
# the ratio, the more a county jails relative to its actual crime level.

prison_tb_avg <- prison_tb_ready |>
  summarise(
    violent_crime_rate_100k_pop = mean(violent_crime_rate_100k_pop),
    incarceration_rate_100k_pop = mean(incarceration_rate_100k_pop),
    .by = c(fips, state, county_name, region, urbanicity)
  ) |>
  mutate(inc_to_crime_ratio = incarceration_rate_100k_pop /
           violent_crime_rate_100k_pop)


# =============================================================================
# HYPOTHESIS 1
# =============================================================================
# CLAIM: Counties with similar crime rates differ enormously in how much
#        they incarcerate.
#
# APPROACH: Plot every county's average crime rate against its average
#           incarceration rate. If H1 holds, the cloud of points should be
#           wide — at any vertical slice (counties with the same crime
#           rate), incarceration values should span a large range.
# -----------------------------------------------------------------------------

# Pick a "reference" crime rate to highlight the spread.
# We will find the lowest- and highest-incarcerating counties within ±10%
# of this rate, and label them on the plot.
target_crime <- 300

extremes <- prison_tb_avg |>
  filter(between(violent_crime_rate_100k_pop,
                 target_crime * 0.9, target_crime * 1.1)) |>
  filter(incarceration_rate_100k_pop == min(incarceration_rate_100k_pop) |
           incarceration_rate_100k_pop == max(incarceration_rate_100k_pop)) |>
  mutate(label = paste0(county_name, ", ", state, "\n",
                        round(incarceration_rate_100k_pop), " per 100k"))

ratio <- round(max(extremes$incarceration_rate_100k_pop) /
                 min(extremes$incarceration_rate_100k_pop), 1)

# --- Plot for H1 -------------------------------------------------------------
H1 <- ggplot(prison_tb_avg,
             aes(x = log10(violent_crime_rate_100k_pop),
                 y = log10(incarceration_rate_100k_pop))) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 1) +
  geom_vline(xintercept = log10(target_crime), linetype = "dashed",
             color = "gray50", alpha = 0.6) +
  geom_point(data = extremes, color = "darkorange", size = 3) +
  geom_label(data = extremes, aes(label = label),
             size = 3, fontface = "bold", nudge_x = 0.2) +
  annotate("text",
           x = log10(target_crime) - 0.7,
           y = mean(log10(extremes$incarceration_rate_100k_pop)),
           label = paste0(ratio, "× difference\nat the same crime rate"),
           color = "darkorange", fontface = "italic", size = 4, hjust = 0) +
  labs(
    x = "Violent crime rate per 100k (log10)",
    y = "Incarceration rate per 100k (log10)",
    title = "Crime rate weakly predicts incarceration rate across U.S. counties",
    subtitle = "Each point = one county, averaged across all years"
  ) +
  theme_economist() +
  scale_color_economist()

H1

# -----------------------------------------------------------------------------
# WHAT WE SEE (H1):
#   The trend line slopes upward — counties with more crime do, on average,
#   incarcerate more people. But the cloud of points around the line is
#   extremely wide. At a violent crime rate of about 300 per 100k (the dashed
#   line), incarceration rates range from ~260 per 100k in Lincoln County, WV
#   to ~4,065 per 100k in Stone County, MS — a 15.6× difference between two
#   counties with essentially the same level of crime.
#
# CONCLUSION (H1): SUPPORTED.
#   Crime rate explains the direction but not the magnitude of incarceration.
#   The vast majority of variation in how much U.S. counties incarcerate
#   comes from something other than how much crime they experience.
#   This raises the natural follow-up question: what does explain it?
# =============================================================================


# =============================================================================
# HYPOTHESIS 2
# =============================================================================
# CLAIM: The "excess" incarceration is not random — it concentrates in
#        specific kinds of counties (Southern, rural).
#
# APPROACH: Use the incarceration-to-crime ratio (inc_to_crime_ratio) as a
#           proxy for excess. Compare its distribution across regions and
#           across urbanicity categories. If H2 holds, some groups should
#           have visibly higher medians than others.
# -----------------------------------------------------------------------------

# --- Plot: by region ---------------------------------------------------------
H2_region <- prison_tb_avg |>
  mutate(region = fct_reorder(region, inc_to_crime_ratio, .fun = median)) |>
  ggplot(aes(x = region, y = inc_to_crime_ratio, fill = region)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_y_log10() +
  labs(x = NULL,
       y = "Incarceration per 1 violent crime (log scale)",
       title = "By region") +
  theme_economist() +
  scale_fill_economist() +
  theme(legend.position = "none")

# --- Plot: by urbanicity -----------------------------------------------------
H2_urban <- prison_tb_avg |>
  mutate(urbanicity = fct_reorder(urbanicity, inc_to_crime_ratio, .fun = median)) |>
  ggplot(aes(x = urbanicity, y = inc_to_crime_ratio, fill = urbanicity)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_y_log10() +
  labs(x = NULL,
       y = NULL,
       title = "By urbanicity") +
  theme_economist() +
  scale_fill_economist() +
  theme(legend.position = "none")

# --- Combine -----------------------------------------------------------------
H2_region + H2_urban +
  plot_annotation(
    title = "Excess incarceration concentrates in the South/Midwest and in rural counties",
    subtitle = "Each box = distribution of incarceration-to-crime ratios across counties"
  )

# -----------------------------------------------------------------------------
# WHAT WE SEE (H2):
#
# By region:
#   Median ratios: Northeast ≈ 2.5, West ≈ 2.5, South ≈ 3.0, Midwest ≈ 3.2.
#   Counties in the South and Midwest jail roughly 25–30% more people per
#   violent crime than counties in the Northeast and West. The boxes overlap
#   substantially, but the shift in medians is clear and consistent.
#
# By urbanicity:
#   The pattern is much sharper. Median ratios climb monotonically:
#   urban ≈ 1.3, small/mid ≈ 2.5, suburban ≈ 2.7, rural ≈ 3.5.
#   Rural counties incarcerate roughly 2.7× more people per violent crime
#   than urban counties. The boxes are also wider for rural counties —
#   meaning the variation among rural counties is itself enormous.
#
# CONCLUSION (H2): SUPPORTED, with refinement.
#   The original hypothesis pointed to the South, but the data shows the
#   excess is shared by both Southern AND Midwestern counties — together
#   forming a continental, non-coastal pattern. More importantly,
#   urbanicity is a stronger predictor of excess than region: a rural
#   county jails far more people per crime than an urban one, regardless
#   of which part of the country it sits in.
# =============================================================================


# =============================================================================
# OVERALL FINDINGS
# =============================================================================
# 1. Crime rate weakly predicts incarceration rate.
#    Counties with the same level of violent crime can differ in
#    incarceration by an order of magnitude (up to 15× in our example).
#
# 2. The "excess" is geographically structured.
#    Southern and Midwestern counties incarcerate more relative to crime
#    than Northeastern and Western ones. Rural counties incarcerate far
#    more than urban ones — a consistent gradient with no overlap in trend.
#
# 3. Crime data alone cannot justify current incarceration patterns.
#    If crime were the main driver, the spread within crime levels would
#    be small and the regional/urban differences would disappear after
#    accounting for crime. Neither is the case.
#
# RECOMMENDATIONS:
#   - Reform efforts should be geographically targeted, not nationwide
#     uniform. The largest gains come from addressing rural and
#     South/Midwest counties where the gap between crime and incarceration
#     is widest.
#   - Policymakers should investigate the local drivers of excess
#     incarceration: sentencing practices, prosecutorial discretion,
#     pretrial detention rates, and the role of jails as economic
#     infrastructure in rural areas.
#   - Future research should bring in additional county-level data
#     (poverty, race composition, local court practices) to explain
#     what currently looks like unexplained variation.
# =============================================================================