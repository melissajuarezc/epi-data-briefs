#======================================================================================================================================
# Graded Assignment 4 Code

# Melissa Juarez, mj2945
#======================================================================================================================================
# RQ: How does mammogram use differ by race and ethnicity among women over 40 in New York 
# City, and does having a regular primary healthcare provider impact mammogram use? 
# 
# outcome: mammogram1yr19
# dependent variables: pcp19, newrace6
#
#load packages needed
library(tidyverse)
library(janitor)
library(lubridate)
library(haven)
library(here)
library(kableExtra)
library(scales)

#read my dataset
chs_df <- read_sas(here::here('data/chs2019_public.sas7bdat'))

# explore variables of interest & remove those whose responses are not applicable
# based on codebook
tabyl(chs_df$mammogram1yr19, show_na = FALSE) 
  ## we see that about the same % of respondents report either not having or having
  ## receiving a mammogram within the past year
tabyl(chs_df$pcp19, show_na = FALSE)
  ## we see that most respondents (86.1%) report having one person or more persons 
  ##  as your personal doctor or health care provider. good!
tabyl(chs_df$newrace6, show_na = FALSE)
  ## over 80% respondents are Black Non-Hispanic, Hispanic, or White non-Hispanic

# explore population of interest (women over 40) by subsetting for female and age 40/+
pop <- chs_df %>% filter(birthsex == 2) %>% filter(!is.na(age40new))

#mutate columns to reveal categories
pop <- pop %>%
  mutate(mammogram1yr19 = case_when(
    mammogram1yr19 == 1 ~ "Yes",
    mammogram1yr19 == 2 ~ "No"
  )) %>%
  mutate(pcp19 = case_when(
    pcp19 == 1 ~ "Yes",
    pcp19 == 2 ~ "No"
  )) %>%
  mutate(newrace6 = case_when(
    newrace6 == 1 ~ 'White Non-Hispanic',
    newrace6 == 2 ~ "Black Non-Hispanic",
    newrace6 == 3 ~ "Hispanic",
    newrace6 == 4 ~ "Asian/PI Non-Hispanic",
    newrace6 == 5 ~ "N. African/Middle Eastern, non-Hispanic",
    newrace6 == 6 ~ "Other Non-Hispanic"
  ))

# ================================================================
# Description of Population
# ================================================================
tabyl(pop$mammogram1yr19, show_na = FALSE) 
## we see that about the same % of women over 40 report either not having or having
## receiving a mammogram within the past year
tabyl(pop$pcp19, show_na = TRUE)
## we see that most respondents (92.5%) report having one person or more persons 
##  as your personal doctor or health care provider. good!
tabyl(pop$newrace6, show_na = FALSE)
tabyl(pop$age40new, show_na = FALSE)

# ================================================================
# Comparison 1: Percentage of Mammogram use by race and ethnicity
# ================================================================
# Table and chart below show that the largest groups who get mammograms are
# White, non-Hispanics (34%), Hispanics (28%), and Black Non-Hispanics (26.4)%
# Something worth noting is that among Asian non-Hispanic respondents, the majority
# of them (54.5% == 168/308) have not received a mammogram within the past year. this is the 
# largest difference between all racial groups.
tabyl(pop, newrace6, mammogram1yr19, show_na = FALSE) %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title("combined") %>%
  knitr::kable(format = "html", caption = 'White, non-Hispanic women make up the largest groups for those
               who did and did not receive a mammogram.') %>%
  kableExtra::kable_styling()

## turning our output above into a dataframe to feed into a ggplot for our bar graphs
race_mamm <- data.frame(table(pop$mammogram1yr19, pop$newrace6))
race_mamm <- race_mamm %>% 
  filter(Var1 == 'Yes') %>% 
  mutate(percent = case_when(
    ## percentages have the mammogram sample size by response in denominator
    Var1 == "Yes" ~ (Freq/1621*100),
    Var1 == "No" ~ (Freq/1623*100)
  )) %>% 
  rename(
    mammogram1yr19 = Var1,
    newrace6 = Var2
  )
                    
# creating plot using the above data
ggplot(race_mamm, aes(mammogram1yr19, percent, fill = newrace6)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="title")


# ================================================================
# Comparison 2: Percentage of Mammogram use by PCP status
# ================================================================
# The table for mammogram use and pcp status below shows that about 68.9% of women 
# over 40 who do not have a pcp also report not having a mammogram test within
# the past year. For women over 40 who do have a pcp, 48.3% report not having
# a mammogram within the last year. when women over 40 have access to a pcp,
# their likelihood of getting a mammogram increases by 20%.

tabyl(pop, mammogram1yr19, pcp19, show_na = FALSE) %>% 
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title("combined") %>%
  knitr::kable(format = "html", caption = 'Healthcare Inequity: Not having a PCP decreases likelihood of 
               obtaining mammogram.') %>%
  kableExtra::kable_styling()

## iow the majority of women samples with a PCP have gotten a mammogram w/in the 
## past year (percentages are too close to assert statistical significant difference
## without a hypothesis test) whereas the majority of women  who do not have a pcp
## also do not get a mammogram. pcps can be the frontline to detecting when someone
## might be at risk and helping patients set up appointments.

# turn into a graph
pcp_mamm <- data.frame(table(pop$mammogram1yr19, pop$pcp19))
pcp_mamm <- pcp_mamm %>% 
  #filter(Var1 == 'Yes') %>% 
  mutate(percent = case_when(
    ## percentages have the mammogram sample size by response in denominator
    Var2 == "No" ~ Freq/241*100,
    Var2 == "Yes" ~ Freq/2991*100
  )) %>% 
  rename(
    mammogram1yr19 = Var1,
    pcp19 = Var2
  )

# creating plot using the above data
ggplot(pcp_mamm, aes(pcp19, percent, fill = mammogram1yr19)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Healthcare Inequity: Those who have a PCP are more 
       likely to have obtained a mammogram.")


# ================================================================
# Comparison 3: Who is more likely to not have a pcp? 
# ================================================================
# although we only should show 2 figures, it might be beneficial to 
# talk about potential next steps in research and see who is most affected
# by healthcare accesibility:
tabyl(pop, newrace6, pcp19, show_na = FALSE) %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title("combined") %>%
  knitr::kable(format = "html", caption = 'Racial Inequity: Hispanics are 
               the most likely group to not have a PCP, among women over 40.') %>%
  kableExtra::kable_styling()

## table above shows that the largest group among people who do not have a PCP
## are Hispanic women, whereas the largest group among people who do have a PCP
## are White non-Hispanic women. Thus, we see that Hispanic women are 
## disproportionately affected when it comes to access to a PCP, compared to Black 
## and White non-Hispanic women.
  ## specifically: 10.8% of Hispanic women over 40 do not have a PCP, compared to 
  ## 7.7% Asian women, 6.7 and 6.5  N.African/Middle Eastern and white women, respectively
  ## and 5.5% Black women.
  ## the groups with the highest likelihood of having a PCP were other non-hisp, Black non-hisp
  ## and white non-hisp (in order from highest to lowest)

## in a graph form:
pcp_race <- data.frame(table(pop$newrace6, pop$pcp19))
pcp_race <- pcp_race %>% 
  #filter(Var1 == 'Yes') %>% 
  mutate(percent = case_when(
    ## percentages have the mammogram sample size by response in denominator
    Var2 == "No" ~ Freq/243*100,
    Var2 == "Yes" ~ Freq/3027*100
  )) %>% 
  rename(
    newrace6 = Var1,
    pcp19 = Var2
  )

# creating plot using the above data
ggplot(pcp_race, aes(pcp19, percent, fill = newrace6)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Racial Inequity: Hispanics are the most likely group 
       to not have a PCP, among women over 40.")

