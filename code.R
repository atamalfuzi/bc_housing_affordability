# Packages ----
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

options(timeout = 300)

# Data ----
statcan_table <- function(pid) {
  zip <- file.path(tempdir(), paste0(pid, ".zip"))
  download.file(sprintf("https://www150.statcan.gc.ca/n1/tbl/csv/%s-eng.zip", pid), zip, mode = "wb", quiet = TRUE)
  if (!identical(readBin(zip, "raw", 2), charToRaw("PK"))) {
    stop("Statistics Canada did not return table ", pid, " (the site may be down for maintenance); try again later.")
  }
  read_csv(unzip(zip, paste0(pid, ".csv"), exdir = tempdir()), show_col_types = FALSE)
}

# 11-10-0009-01 Selected income characteristics of census families (T1 Family File)
income <- statcan_table("11100009") %>%
  filter(grepl("British Columbia", GEO),
         grepl("S05", DGUID),  # census metropolitan areas and agglomerations only
         `Family characteristics` == "Median total income, all families") %>%
  transmute(year = as.integer(REF_DATE), area = sub(", British Columbia", "", GEO), income = VALUE) %>%
  filter(year <= 2023)  # the README describes 2023; raise this when you update it

# 34-10-0133-01 CMHC average rents (October survey of purpose-built rental apartments and row houses)
rent <- statcan_table("34100133") %>%
  filter(grepl("British Columbia", GEO),
         grepl("S05", DGUID),
         `Type of structure` == "Row and apartment structures of three units and over",
         `Type of unit` == "Two bedroom units") %>%
  transmute(year = as.integer(REF_DATE), area = sub(", British Columbia", "", GEO), rent = VALUE)

# 34-10-0145-01 CMHC conventional 5-year mortgage lending rate, monthly -> annual average
rate <- statcan_table("34100145") %>%
  group_by(year = as.integer(substr(REF_DATE, 1, 4))) %>%
  summarise(rate = mean(VALUE) / 100)

# BC Property Transfer Tax Data (Open Government Licence - British Columbia), one dataset per year.
# Median fair market value of market transactions, by regional district and month.
# The regional district standing in for each census metropolitan area:
districts <- c("METRO VANCOUVER" = "Vancouver", "CAPITAL" = "Victoria", "CENTRAL OKANAGAN" = "Kelowna")

transfer_year <- function(year) {
  resources <- jsonlite::fromJSON(paste0(
    "https://catalogue.data.gov.bc.ca/api/3/action/package_show?id=property-transfer-tax-data-", year
  ))$result$resources
  url <- resources$url[tolower(resources$format) == "csv" &
                         grepl("regional.district.monthly", resources$name, ignore.case = TRUE)]
  if (length(url) != 1) {
    stop("Expected one regional district CSV in the ", year, " property transfer data, found ", length(url), ".")
  }
  read_csv(url, col_types = cols_only(RegionalDistrict = "c", tot_mkt_trans = "d", md_FMV = "d")) %>%
    mutate(year = year, district = toupper(sub("^[0-9]+ - ", "", RegionalDistrict)))
}

# Annual price: monthly medians weighted by number of sales, an approximation of the annual median
price <- bind_rows(lapply(2016:max(income$year), transfer_year)) %>%
  filter(district %in% names(districts)) %>%
  group_by(year, area = districts[district]) %>%
  summarise(price = weighted.mean(md_FMV, tot_mkt_trans), months = n(), .groups = "drop")

stopifnot(all(price$months == 12))

# Ratios ----
# Monthly payment per dollar borrowed: 25-year amortization, Canadian semi-annual compounding
payment_factor <- function(rate, years = 25) {
  j <- (1 + rate / 2)^(1 / 6) - 1
  j / (1 - (1 + j)^(-12 * years))
}

base_year <- 2008  # first year with income data for every census agglomeration
down_payment <- 0.2

ratios <- income %>%
  inner_join(rent, by = c("year", "area")) %>%
  left_join(price, by = c("year", "area")) %>%
  left_join(rate, by = "year") %>%
  filter(year >= base_year, !is.na(income), !is.na(rent)) %>%
  mutate(
    rent_to_income = 12 * rent / income,
    own_to_income = 12 * (1 - down_payment) * price * payment_factor(rate) / income
  ) %>%
  select(area, year, income, rent, rent_to_income, price, rate, own_to_income) %>%
  arrange(area, year)

latest_year <- max(ratios$year)

# All eight BC census metropolitan areas should be here, and Vancouver, Victoria and Kelowna
# should have an ownership cost for every year from 2016
stopifnot(c("Vancouver", "Victoria", "Kelowna", "Abbotsford-Mission", "Kamloops", "Nanaimo", "Chilliwack", "Prince George") %in% ratios$area,
          sum(!is.na(ratios$own_to_income)) == 3 * (latest_year - 2016 + 1))

write_csv(ratios %>% mutate(across(c(rent_to_income, rate, own_to_income), \(x) round(x, 4)), price = round(price)),
          file.path("output", "affordability_index.csv"))

# Figures ----
ink <- "#0b0b0b"
muted <- "#52514e"
blue <- "#2a78d6"
orange <- "#eb6834"
grey <- "#a8a7a2"

theme_chart <- theme_minimal(base_size = 13) +
  theme(text = element_text(colour = ink),
        axis.text = element_text(colour = muted),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(colour = muted),
        plot.caption = element_text(colour = muted, hjust = 0),
        plot.caption.position = "plot",
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.justification = "left",
        legend.location = "plot",
        legend.title = element_blank())

statcan_note <- "Statistics Canada tables 11-10-0009-01, 34-10-0133-01"

# Figure rent_to_income ----
# Rent-to-income ratio in the base and latest year, for every area with both
dumbbell <- ratios %>%
  filter(year %in% c(base_year, latest_year)) %>%
  group_by(area) %>%
  filter(n() == 2) %>%
  ungroup() %>%
  # sort areas by their latest-year ratio
  mutate(area = reorder(area, rent_to_income * (year == latest_year), sum),
         year = factor(year))

rose <- dumbbell %>%
  group_by(area) %>%
  summarise(up = diff(rent_to_income[order(year)]) > 0)

rent_plot <- ggplot(dumbbell, aes(rent_to_income, area)) +
  geom_line(aes(group = area), colour = grey, linewidth = 0.8) +
  geom_point(aes(colour = year), size = 3) +
  scale_colour_manual(values = setNames(c(grey, blue), c(base_year, latest_year))) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0.03, 0.06))) +
  labs(title = sprintf("Rent takes a bigger share of family income in %d of %d BC urban areas", sum(rose$up), nrow(rose)),
       subtitle = sprintf("Average two-bedroom rent as a share of median census family income, %d and %d", base_year, latest_year),
       x = NULL, y = NULL,
       caption = paste0("Rent: October average in purpose-built rental apartments and row houses (CMHC), times 12.\nSource: ", statcan_note, ".")) +
  theme_chart +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path("figures", "rent_to_income.png"), rent_plot, bg = "white", width = 9, height = 9)

# Figure own_vs_rent ----
# Mortgage payment on a median-priced home vs two-bedroom rent, as a share of income
trend <- ratios %>%
  filter(!is.na(own_to_income)) %>%
  pivot_longer(c(own_to_income, rent_to_income), names_to = "measure", values_to = "share") %>%
  mutate(measure = recode(measure,
                          own_to_income = "Mortgage payment on a median-priced home",
                          rent_to_income = "Average two-bedroom rent"),
         area = factor(area, levels = unname(districts)))

trend_plot <- ggplot(trend, aes(year, share, colour = measure)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~area, nrow = 1) +
  scale_colour_manual(values = c(blue, orange)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(2016, latest_year, by = 2)) +
  labs(title = "Higher rates pushed the cost of owning past 40% of the median family's income",
       subtitle = "Housing cost as a share of median census family income",
       x = NULL, y = NULL,
       caption = paste0("Mortgage: 20% down, 25-year amortization, CMHC conventional 5-year rate (yearly average).\n",
                        "Price: median value of market sales in the regional district (Metro Vancouver, Capital, Central Okanagan).\n",
                        "Source: ", statcan_note, ", 34-10-0145-01; BC Property Transfer Tax Data.")) +
  theme_chart +
  theme(panel.spacing = unit(1.5, "lines"),
        strip.text = element_text(face = "bold", hjust = 0, size = 12))

ggsave(file.path("figures", "own_vs_rent.png"), trend_plot, bg = "white", width = 11, height = 6)

# Figure own_cost_drivers ----
# Exact log decomposition: own_to_income is proportional to price * payment_factor(rate) / income,
# so its log change is the sum of a price, a rate and an income term. Each term's share of the
# log change is applied to the change in percentage points, so the bars add up to the net change.
drivers <- ratios %>%
  filter(!is.na(own_to_income), year %in% c(2016, 2021, latest_year)) %>%
  group_by(area) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(period = paste0(lag(year), " to ", year),
         total = 100 * (own_to_income - lag(own_to_income)),
         log_total = log(own_to_income / lag(own_to_income)),
         `House price` = log(price / lag(price)),
         `Mortgage rate` = log(payment_factor(rate) / payment_factor(lag(rate))),
         `Family income` = -log(income / lag(income))) %>%
  ungroup() %>%
  filter(!is.na(total)) %>%
  pivot_longer(c(`House price`, `Mortgage rate`, `Family income`), names_to = "driver", values_to = "log_part") %>%
  mutate(points = total * log_part / log_total,
         driver = factor(driver, levels = c("House price", "Mortgage rate", "Family income")),
         area = factor(area, levels = rev(unname(districts))))

# The three parts should add up to the net change
stopifnot(all(abs(tapply(drivers$points, paste(drivers$area, drivers$period), sum) -
                    tapply(drivers$total, paste(drivers$area, drivers$period), mean)) < 1e-9))

drivers_plot <- ggplot(drivers, aes(points, area)) +
  geom_vline(xintercept = 0, colour = muted) +
  geom_col(aes(fill = driver), width = 0.6) +
  geom_point(data = distinct(drivers, area, period, total), aes(total), size = 3, colour = ink) +
  facet_wrap(~period, nrow = 1) +
  scale_fill_manual(values = c(orange, blue, grey)) +
  scale_x_continuous(labels = scales::label_number(suffix = " pts")) +
  labs(title = sprintf("Prices pushed ownership costs up until 2021; interest rates did from 2021 to %d", latest_year),
       subtitle = "Change in the mortgage payment on a median-priced home, in percentage points of median family income. The dot is the net change.",
       x = NULL, y = NULL,
       caption = paste0("Each bar is that factor's share of the change in logs; the three add up to the net change. Rising income lowers the share.\n",
                        "Source: ", statcan_note, ", 34-10-0145-01; BC Property Transfer Tax Data.")) +
  theme_chart +
  theme(panel.spacing = unit(1.5, "lines"),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 12))

ggsave(file.path("figures", "own_cost_drivers.png"), drivers_plot, bg = "white", width = 11, height = 4.5)
