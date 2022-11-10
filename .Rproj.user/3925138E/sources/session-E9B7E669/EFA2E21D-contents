library(readr)
library(tidyverse)
library(tidycensus)
library(ggridges)
library(ggrepel)
library(patchwork)

Affordable_Housing <- read_csv("Affordable_Housing.csv")
View(Affordable_Housing)

thads2013 <- 
  read_table('thads2013n.txt')

thads2013_clean <- 
  read_csv("thads2013n.csv") %>% 
  filter(REGION == "'1'")


class(thads2013_clean$REGION)

census_vars <-
  tidycensus::load_variables(
    year = 2021,
    dataset = 'acs1')


census_vars %>% 
  filter(str_detect(concept, 'GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME')) %>% 
  select(concept) %>% 
  distinct()

# get the variable id for median monthly housing costs 

census_vars %>%   
  filter(concept == 'GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS') %>% 
  pull(name)


# county ------------------------------------------------------------------


acs_2021_county <-
  tidycensus::get_acs(
    geography = 'county',
    variables = c('B25070_001', 'B25070_002',"B25070_001",
                  "B25070_002", "B25070_003", "B25070_004",
                  "B25070_005", "B25070_006", "B25070_007",
                  "B25070_008", "B25070_009", "B25070_010",
                  "B25070_011"),
    year = 2021,
    survey = 'acs1') %>% 
  select(-moe) %>% 
  pivot_wider(
    names_from = variable,
    values_from = estimate) %>% 
  filter(str_detect(NAME, 
                    'District of Columbia|Maryland|Virginia')) %>% 
  mutate(state = 
           case_when(
             NAME = str_detect(NAME, 'Maryland') ~ 'maryland',
             NAME = str_detect(NAME, 'Virginia') ~ 'virginia',
             NAME = str_detect(NAME, 'District of Columbia') ~ 'DC'))





acs_2021_county %>% 
  ggplot(aes(x = B25070_010,
             y = state)) +
  geom_density_ridges()



# tract -------------------------------------------------------------------



acs_2020_tract_dc <-
  tidycensus::get_acs(
    geography = 'tract',
    variables = c('B25070_001', 'B25070_002',"B25070_001",
                  "B25070_002", "B25070_003", "B25070_004",
                  "B25070_005", "B25070_006", "B25070_007",
                  "B25070_008", "B25070_009", "B25070_010",
                  "B25070_011"),
    year = 2020,
    state = 'dc',
    survey = 'acs5') %>% 
  select(-moe) %>% 
  pivot_wider(
    names_from = variable,
    values_from = estimate) %>% 
  filter(str_detect(NAME, 
                    'District of Columbia|Maryland|Virginia')) %>% 
  mutate(state =
           case_when(NAME = str_detect(NAME, 'District of Columbia') ~ 'DC'))


acs_2020_tract_va <-
  tidycensus::get_acs(
    geography = 'tract',
    variables = c('B25070_001', 'B25070_002',"B25070_001",
                  "B25070_002", "B25070_003", "B25070_004",
                  "B25070_005", "B25070_006", "B25070_007",
                  "B25070_008", "B25070_009", "B25070_010",
                  "B25070_011"),
    year = 2020,
    state = 'va',
    survey = 'acs5') %>% 
  select(-moe) %>% 
  pivot_wider(
    names_from = variable,
    values_from = estimate) %>% 
  filter(str_detect(NAME, 
                    'District of Columbia|Maryland|Virginia')) %>% 
  mutate(state = 
           case_when(
             NNAME = str_detect(NAME, 'Virginia') ~ 'virginia'))

acs_2020_tract_md <-
  tidycensus::get_acs(
    geography = 'tract',
    variables = c('B25070_001', 'B25070_002',"B25070_001",
                  "B25070_002", "B25070_003", "B25070_004",
                  "B25070_005", "B25070_006", "B25070_007",
                  "B25070_008", "B25070_009", "B25070_010",
                  "B25070_011"),
    year = 2020,
    state = 'md',
    survey = 'acs5') %>% 
  select(-moe) %>% 
  pivot_wider(
    names_from = variable,
    values_from = estimate) %>% 
  filter(str_detect(NAME, 
                    'District of Columbia|Maryland|Virginia')) %>% 
  mutate(state = 
           case_when(
             NAME = str_detect(NAME, 'Maryland') ~ 'maryland'))


acs_2020_tract <- 
  acs_2020_tract_dc %>% 
  full_join(acs_2020_tract_md) %>% 
  full_join(acs_2020_tract_va) %>% 
  mutate(
    gross_rent_below_20 =  B25070_002 + B25070_003 + B25070_004,
    gross_rent_20_40 = 
      B25070_005 + B25070_006 + B25070_007 + B25070_008 + B25070_009)

plot1 <- 
  acs_2020_tract %>% 
  ggplot(aes(x = B25070_010,
             y = state)) +
  geom_density_ridges_gradient() +
  geom_density_ridges(aes(x = gross_rent_below_20),
                      fill = 'purple',
                      alpha = 0.3) +
  geom_density_ridges(aes(x = gross_rent_20_40),
                      fill = 'dodgerblue',
                      alpha = 0.3) +
  theme(panel.background = element_blank(),
        axis.title.y = element_blank())

plot2 <-
  acs_2020_tract %>% 
  ggplot(aes(x = B25070_010,
             y = state)) +
  geom_density_ridges_gradient() +
  theme(panel.background = element_blank(),
        axis.title.y = element_blank())

plot3 <-
  acs_2020_tract %>% 
  ggplot(aes(x = gross_rent_below_20,
             y = state)) +
  geom_density_ridges(fill = 'purple',
                      alpha = 0.3) +
  theme(panel.background = element_blank(),
        axis.title.y = element_blank())

plot4 <-
  acs_2020_tract %>% 
  ggplot(aes(x = gross_rent_20_40,
             y = state)) +
  geom_density_ridges(fill = 'dodgerblue',
                      alpha = 0.3) +
  theme(panel.background = element_blank(),
        axis.title.y = element_blank())

patchwork <- plot1 / (plot2 + plot3 + plot4)



# state -------------------------------------------------------------------



acs_2021_state <-
  tidycensus::get_acs(
    geography = 'state',
    variables = c('B25070_001', 'B25070_002',"B25070_001",
                  "B25070_002", "B25070_003", "B25070_004",
                  "B25070_005", "B25070_006", "B25070_007",
                  "B25070_008", "B25070_009", "B25070_010",
                  "B25070_011"),
    year = 2021,
    survey = 'acs1') %>% 
  select(-moe) %>% 
  pivot_wider(
    names_from = variable,
    values_from = estimate) %>% 
  filter(str_detect(NAME, 
                    'District of Columbia|Maryland|Virginia'))%>% 
  mutate(
    gross_rent_below_20 =  B25070_002 + B25070_003 + B25070_004,
    gross_rent_20_40 = 
      B25070_005 + B25070_006 + B25070_007 + B25070_008 + B25070_009) 

acs_2021_state %>% 
  ggplot(aes(y = NAME))+
  geom_col(aes(x = B25070_010),
           fill = "#FF5858",
           alpha = 0.5,
           width = 0.2,
           position = position_nudge(y = 0.2)) +
  geom_col(aes(x = gross_rent_20_40),
           fill = '#7DE5ED',
           width = 0.2,
           position = position_nudge(y = 0)) +
  geom_col(aes(x = gross_rent_below_20),
           fill = '#82CD47',
           width = 0.2,
           position = position_nudge(y = -0.2)) +
  scale_x_continuous(breaks = c(0, 50000, 100000, 150000, 200000, 250000,
                                300000, 350000, 400000, 450000, 500000)) +
  labs(x = 'Number of household in thousand',
       title = 'Number of households categorized by rent affordability in the Washington metropolitan area',
       subtitle = 'Household categorized by gross rent as a percentage of household income in the past 12 months') + 
  theme(panel.background = element_blank(),
        axis.title.y = element_blank())


