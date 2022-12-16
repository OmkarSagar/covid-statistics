library(tidyverse)

#Reading in data
polls_adjusted <-
  readr::read_csv('covid_approval_polls_adjusted.csv')

#Grouping by subject and creating new column which is the mean of approve_adjusted
polls_adjusted <- polls_adjusted %>%
  dplyr::group_by(subject) %>%
  dplyr::mutate(average_approval = mean(approve_adjusted, na.rm = T)) %>%
  dplyr::ungroup()

#Creating graph of adjusted approval rating
approval_graph <- polls_adjusted %>%
  ggplot(aes(x = approve_adjusted)) +
  geom_histogram() +
  labs(x = 'Approval',
       y = 'Count',
       title = 'Adjusted Approval Ratings') +
  theme_minimal()

#Splitting graph to dispplay rating Biden and Trump
approval_graph_facet <- polls_adjusted %>%
  filter(subject == 'Biden' | subject == 'Trump') %>%
  ggplot(aes(x = approve_adjusted, fill = subject)) +
  geom_histogram() +
  facet_wrap(~subject)+
  labs(
    x = "Approval",
    y = "Count",
    fill = "President",
    title = "American approval of Biden and Trump's response to coronavirus",
    subtitle = "From 2020-2022"
  ) +
  theme_minimal()

#Adding a vertical line to show the mean of both Biden and Trump's approvals
mean_line <- approval_graph_facet +
  geom_vline(aes(xintercept = average_approval), linetype = 'dashed')

#Changing the represented colors to match Biden and Trump's respective party colors
approval_final <- mean_line +
  scale_fill_manual(values = c("#008FD5", "#FF2700")) +
  theme(legend.position = "bottom")

#Creating a column which is enddate in 'date' datatype, creating a column which is the approve_adjusted divided by 100, filtering the data so party only contains Democrats, Republicans, and Independents 
polls_q3 <- polls_adjusted %>%
  mutate(end_date = lubridate::mdy(enddate)) %>%
  mutate(approve_fraction = approve_adjusted / 100) %>%
  filter(party == 'D' | party == 'R' | party == 'I')

#Visualizing the approval rating of president's handling of covid-19
polls_q3 %>%
  ggplot(aes(x = end_date, y = approve_fraction, color = party)) +
  geom_point() +
  scale_color_manual(values = c("#008FD5", "#77AB43", "#FF2700")) +
  geom_smooth() +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  geom_vline(aes(xintercept = as.numeric(as.Date("2021-01-20"))), linetype = 'dashed') +
  labs(
    x = ' ',
    y = ' ',
    title = "Omkar Hanamsagar: Approval of President's Handling of Covid−19 Pandemic",
    subtitle = "From 2020−2022",
    color = 'Party'
  )

#Reading in toplines data
toplines <- readr::read_csv('covid_approval_toplines.csv')

#Filtering subject to only include Biden, party to only include D/R/I
toplines_biden <- toplines %>%
  filter(subject == 'Biden') %>%
  filter(party == 'D' | party == 'R' | party == 'I') %>%
  mutate(model_date = lubridate::mdy(modeldate))

#Creating a new column which is equal to Democrats, Republicans, and Independents if party is D, R and I respectively. Creating new column which is the fraction of approve_estimate column
toplines_biden <- toplines_biden %>%
  mutate(party_description = ifelse(
    party == 'D',
    'Democrats',
    ifelse(
      party == 'R',
      'Republicans',
      ifelse(party == 'I', 'Independents', 0)
    )
  )) %>%
  mutate(approve_estimate_frac = approve_estimate / 100)

#Visualizing the changes in Biden's approval rating by political party since his inauguration
library(ggrepel)
library(scales)
library(lubridate)
final_graph<-toplines_biden %>%
  mutate(label = ifelse(model_date == max(model_date),
                        party_description, NA_character_)) %>%
  ggplot(aes(x=model_date, y=approve_estimate_frac, color=party)) + ## need to fill in
  geom_line() +
  geom_text_repel(aes(label = label),
                  nudge_x = 10, na.rm = T,
                  xlim = as_date(c("2022-07-01", "2022-10-01"))) +
  geom_vline(aes(xintercept = as_date("2021-01-20")), linetype = "dashed")+ ## need to fill in
  annotate("text", x = as_date("2021-01-20"), y = 0.05,
           label = "Biden sworn into office", size = 3,
           hjust = -0.1) +
  scale_color_manual(values = c("#008FD5", "#77AB43", "#FF2700")) + ## need to fill in
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(.1,1), clip = "off") +
  scale_x_date(limits = c(as_date("2020-12-01"), as_date("2022-10-01"))) +
  labs(title="Do Americans approve of Biden response to the coronavirus crisis?",
       subtitle= "A calculation of the share of all Americans who approve of the handling of the coronavirus outbreak",
       x="",y="", fill="") + ## need to fill in
  theme_minimal() +
  theme(legend.position = "none") ## need to fill in
final_graph

#Reading in covid concern data
covid_concern <- read_csv('covid_concern_toplines.csv')

#### Reformatting modeldate column into mdy
covid_concern <- covid_concern %>%
  mutate(modeldate = mdy(modeldate))

#### Arranging very_estimate column in descending order 
covid_concern %>%
  arrange(desc(very_estimate))

#### Changing level of concern estimates into fractions
covid_concern <- covid_concern %>%
  mutate(very_estimate = very_estimate/100) %>%
  mutate(somewhat_estimate = somewhat_estimate/100) %>%
  mutate(not_very_estimate = not_very_estimate/100) %>%
  mutate(not_at_all_estimate = not_at_all_estimate/100)

#### Adding column that cleans up classification of where concern lies 
covid_concern <- covid_concern %>%
  mutate(concern = ifelse(subject == 'concern-economy', 'Concerned About Economy',ifelse(subject == 'concern-infected', 'Concerned About Getting Infected', 0)))

#### Visualizing concern level over the economy and getting infected
concern_level_by_type <- covid_concern %>%
  ggplot(aes(x = modeldate)) +
  facet_wrap( ~ concern) +
  geom_line(aes(y = very_estimate, color = 'coral2')) +
  geom_line(aes(y = somewhat_estimate, color = 'forestgreen')) +
  geom_line(aes(y = not_very_estimate, color = 'darkorchid')) +
  geom_line(aes(y = not_at_all_estimate, color = 'cornflowerblue')) +
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(
    name = 'Concern Level',
    values = c('coral2' = 'coral2', 'forestgreen' =
                 'forestgreen', 'darkorchid' =
                 'darkorchid', 'cornflowerblue' =
                 'cornflowerblue'),
    labels = c('Very Concerned', 'Somewhat Concerned', 'Not Very Concerned', 'Not At All Concerned')
  ) +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2020-02-15"), as.Date("2021-08-01"))) +
  labs( x = '',
        y = '',
        title = 'Concern level over the economy and getting infected',
        subtitle = 'From 2020-2021')

#### Visualizing when the concern level about the economy peaked
concern_peak_economy <- covid_concern %>%
  filter(subject == 'concern-economy') %>%
  ggplot(aes(x = modeldate)) +
  geom_line(aes(y = very_estimate, color = 'black')) +
  scale_y_continuous(labels = scales::percent) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-29"))), linetype = 'dashed') +
  scale_x_date(limits = c(as.Date("2020-02-15"), as.Date("2020-04-10"))) +
  labs( x = '',
        y = '',
        title = 'Peak concern level over the economy',
        subtitle = '(March 29, 2020)') +
  annotate(
    "text",
    x = as.Date("2020-03-07"),
    y = .63,
    label = "Peak Concern About the Economy",
    size = 2.80,
    hjust = -0.1) +
  theme_minimal()

#### Visualizing when the concern level about getting infected peaked
concern_peak_infected <- covid_concern %>%
  filter(subject == 'concern-infected') %>%
  ggplot(aes(x = modeldate)) +
  geom_line(aes(y = very_estimate, color = 'black')) +
  scale_y_continuous(labels = scales::percent) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2021-01-30"))), linetype = 'dashed') +
  scale_x_date(limits = c(as.Date("2021-01-15"), as.Date("2021-03-15"))) +
  labs( x = '',
        y = '',
        title = 'Peak concern level over getting infected',
        subtitle = '(January 30, 2021)') +
  annotate(
    "text",
    x = as.Date("2021-01-30"),
    y = .63,
    label = "Peak Concern About being Infected",
    size = 2.80,
    hjust = -0.1) +
  theme_minimal()

#### Filtering subject to only be about getting infected
infected_concern <- covid_concern %>%
  filter(subject == 'concern-infected')

#### Visualizing the approval rating of the president's handling of Covid-19
polls_q3 %>%
  ggplot(aes(x = end_date, y = approve_fraction, color = party, se = 3), se = 3) +
  geom_point(alpha = .15, se = 3) +
  scale_color_manual(values = c("#008FD5", "#77AB43", "#FF2700")) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3.5),span = .3, se = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  geom_vline(aes(xintercept = as.numeric(as.Date("2021-01-20"))), linetype = 'dashed') +
  labs(
    x = ' ',
    y = ' ',
    title = "Omkar Hanamsagar: Approval of President's Handling of Covid−19 Pandemic",
    subtitle = "From 2020−2022",
    color = 'Party'
  )


