#======================================================================================================================================
# Graded Assignment 5 Code

# group 19
#======================================================================================================================================
# RQ: How does birth control use differ by race and ethnicity among women between 
# ages 25-44 in New York City? How does the association between income and long 
# lasting birth control use and short lasting birth control use differ by race and ethnicity?
# 
# outcome: merging(bthcntrltype19 & bthcontrollastsex19)
# dependent variables: newrace6 &  imputed_pov200
#
#load packages needed
library(tidyverse)
library(janitor)
library(haven)
library(here)
library(kableExtra)
library(scales)
library(mStats)
library(epitools)
library(tableone)

#read my dataset
chs_df <- read_sas(here::here('data/chs2019_public.sas7bdat'))

# explore variables of interest & remove those whose responses are not applicable
# based on codebook
tabyl(chs_df$bthcntrltype19, show_na = FALSE) 
  ## 71% respondents (women 18-45 and men 18-65) did not use birth control whose 
  ## burden fall on women at last time of vaginal sex
  ## for those that do, the most common types are pills and the uid/implant.
tabyl(chs_df$bthcontrollastsex19, show_na = FALSE)
  ## 56% of respondents (women 18-45 and men 18-65) used contraceptives (including 
  ## condoms) at last time of vaginal sex, whereas 44% did not
tabyl(chs_df$newrace6, show_na = FALSE)
  ## over 80% respondents (all) are Black Non-Hispanic, Hispanic, or White non-Hispanic
tabyl(chs_df$imputed_pov200, show_na = FALSE)
  ## 58% of respondents (all) report an income that is greater than or equal to
  ## 200% of the federal poverty line, while 42% report an income lower than it. 

# Q: How many people used who responded No to bthcntrltype19 used condoms?
xtabs(~ bthcntrltype19 + bthcontrollastsex19, data = chs_df) 
  ## A: Out of the ppl categorized as 'No Contraception' for `bthcntrltype19`,
  ## 916 used condoms and 1594 used no form of contraception at all.


## Data manipulation to create a new variable for birth control type which
## includes condoms as a valid birth control type.
chs_df <- chs_df %>%
  mutate(allbrthcntrltypes19 = case_when(
    ## 1st Q: no contraception (not including condoms), 2nd Q: No contraception (including condoms)
    bthcntrltype19 == 1 & bthcontrollastsex19 == 2 ~ "No Contraception",
    ## 1st Q: no contraception (not including condoms), 2nd Q: Yes contraception (including condoms)
    bthcntrltype19 == 1 & bthcontrollastsex19 == 1 ~ "Condoms",
    bthcntrltype19 == 2 ~ "Pill only",
    bthcntrltype19 == 3 ~ "Pill + other methods",
    bthcntrltype19 == 4 ~ "Shots, vaginal ring, or patch",
    bthcntrltype19 == 5 ~ "IUD or implant",
    bthcntrltype19 == 6 ~ "EC",
    bthcntrltype19 == 7 ~ "Other",
    bthcntrltype19 == 8 ~ "Sterilization"
  ))

tabyl(chs_df$allbrthcntrltypes19, show_na = FALSE)
  ## now we see that 25.8% of respondents (men 18-65 + women 18-45 -- this is not our
  ## population of interest btw) use condoms and 45% use no contraception.
  ## other %s for forms of BC stay the same.


# explore population of interest (women ages 25-44) by subsetting for female and agegroup ==2 
# we see that our sample size is n = 1345.
pop <- chs_df %>% filter(birthsex == 2) %>% 
  filter(agegroup == 2) %>% 
  filter(wswexclusive == 2) ## women who have had sex w someone of opposite sex in past 12mo.

#mutate columns to reveal categories of race & income
pop <- pop %>%
  mutate(newrace6 = case_when(
    newrace6 == 1 ~ 'White Non-Hispanic',
    newrace6 == 2 ~ "Black Non-Hispanic",
    newrace6 == 3 ~ "Hispanic",
    newrace6 == 4 ~ "Asian/PI Non-Hispanic",
    newrace6 == 5 ~ "N. African/Middle Eastern, non-Hispanic",
    newrace6 == 6 ~ "Other Non-Hispanic"
  )) %>%
  mutate(imputed_pov200 = case_when(
    imputed_pov200 == 1 ~ "<200% FPL",
    imputed_pov200 == 2 ~ ">=200% FPL"
  ))
 
pop <- pop %>% mutate(imputed_pov200 = fct_relevel(imputed_pov200,  "<200% FPL", ">=200% FPL"),
         allbrthcntrltypes19 = fct_relevel(allbrthcntrltypes19, "No Contraception", "Condoms", "Pill only",
                                           "Shots, vaginal ring, or patch", "IUD or implant", "EC", 
                                           "Sterilization", "Other"))


# ==============================================================================
# Description of Population: women ages 25-44 (who have sex w someone of opposite sex)
# ==============================================================================
# n = 1301 with 44 NA values in the allbrthcontrol column

tabyl(pop$allbrthcntrltypes19, show_na = FALSE) 
  ## among women ages 25-44, 44% report using no contraception at all, while 56%
  ## uses contraception at last time of vaginal sex. 
tabyl(pop$newrace6, show_na = FALSE)
  ## the largest racial or ethnic group among women ages 25-44 are Hispanic, 
  ## making up 35% of all women ages 25-44. Followed by White, non-Hisp (28%),
  ## and Black non-Hisp (21%). This is different than the racial breakdowns of 
  ## NYC as a whole
tabyl(pop$imputed_pov200, show_na = FALSE) 
  ## 56% of women 25-44 reported an income greater than or equal to 200% of the   
  ## FPL, while 44% reported earning less.

## Table 1
# Create a variable list which we want in Table 1
listVars <- c("allbrthcntrltypes19", "newrace6", "imputed_pov200")
table1 <- CreateTableOne(vars = listVars, data = pop, factorVars = listVars)
a <- print(table1, quote = TRUE, noSpaces = TRUE)
as.data.frame(a)


# ==============================================================================
# Comparison 1: Percentage of BC use by race and ethnicity
# ==============================================================================

# turn yes/no birth control into a binary variable
pop <- pop %>%
  mutate(bcbinary = case_when(
    allbrthcntrltypes19 != "No Contraception" ~ 'Used Contraception',
    allbrthcntrltypes19 == "No Contraception" ~ 'No Contraception'
  ))

#create a table of the crosstabs of variables newrace6 and bcbinary
tabyl(pop, newrace6, bcbinary, show_na = FALSE) %>%
  adorn_totals("col") %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title("combined") %>%
  knitr::kable(format = "html", caption = 'Asian women use contraception in the highest percentages, 
               followed by White, non-Hispanic women.') %>%
  kableExtra::kable_styling()

## turning our output above into a dataframe to feed into a ggplot for our bar graphs
bcbin_race <- table(pop$newrace6, pop$bcbinary)
bcbin_race <- bcbin_race %>% 
  data.frame %>% 
  #filter(Var1 == 'Yes') %>% 
  mutate(percent = case_when(
    Var1 == "Asian/PI Non-Hispanic" ~ (Freq/sum(bcbin_race[1,])*100),
    Var1 == "Black Non-Hispanic" ~ (Freq/sum(bcbin_race[2,])*100),
    Var1 == "Hispanic" ~ (Freq/sum(bcbin_race[3,])*100),
    Var1 == "N. African/Middle Eastern, non-Hispanic" ~ (Freq/sum(bcbin_race[4,])*100),
    Var1 == "Other Non-Hispanic" ~ (Freq/sum(bcbin_race[5,])*100),
    Var1 == "White Non-Hispanic" ~ (Freq/sum(bcbin_race[6,])*100),
  )) %>% 
  rename(
    BirthControlUse = Var2,
    RaceEthnicity = Var1
  )

# creating plot using the above data
ggplot(bcbin_race, aes(RaceEthnicity, percent, fill = BirthControlUse)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Asian women use contraception in the highest percentages, 
       followed by White, non-Hispanic women.") +
  coord_flip()

## Analytical Interpretation: Chi-Squared Test
data = table(pop$newrace6, pop$bcbinary)
chisq.test(data) 
  ## p-value 0.1617 -> no compelling evidence to say that there is a statistically
  ## significant association between race and birth control usage.
riskratio.wald(data, rev="both")
oddsratio.wald(data, rev="both")

# ==============================================================================
# Comparison 2: Income on Birth Control Type, strata = race
# ==============================================================================
# now that we are grouping by long-lasting and short-lasting, we are reducing our
# sample size, since people who use EC, other, & no contraception are not included.

# turn long-lasting/short-lasting into binary variable
pop <- pop %>%
  mutate(bclength = case_when(
    allbrthcntrltypes19 %in% c("IUD or implant", "Sterilization") ~ 'Long-lasting',
    allbrthcntrltypes19 %in% c("Condoms", "Pill only", "Shots, vaginal ring, or patch") ~ 'Short-lasting'
  ))

## stacked bar plots
tabyl(pop, imputed_pov200, bclength, newrace6, show_na = FALSE) %>%
  adorn_totals("col") %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title("combined") %>%
  knitr::kable(format = "html", caption = 'title') %>%
  kableExtra::kable_styling()

xyz = table(pop$imputed_pov200, pop$bclength, pop$newrace6)
xyz <- xyz %>% 
  data.frame %>% 
  rename(
    IncomeLevel = Var1,
    BirthControlLength = Var2,
    Race = Var3, 
    Percent = Freq
  )

ggplot(xyz, aes(fill=BirthControlLength, y=Percent, x=IncomeLevel)) + 
  geom_bar(position="fill", stat="identity") +
  labs(title="Long-lasting birth control is more common than short-lasting birth control") +
  facet_grid(~ Race) 

## This graph is too long? Consider splitting into 2 rows of 3 graphs
xyz_1 = xyz[c(1:12),]
xyz_2 = xyz[c(13:24),]

ggplot(xyz_1, aes(fill=BirthControlLength, y=Percent, x=IncomeLevel)) + 
  geom_bar(position="fill", stat="identity") +
  facet_grid(~ Race) 
ggplot(xyz_2, aes(fill=BirthControlLength, y=Percent, x=IncomeLevel)) + 
  geom_bar(position="fill", stat="identity") +
  facet_grid(~ Race) 

## test of heterogeneity
install.packages('mStats')
library(mStats)
mhor(pop, imputed_pov200, bclength, strata = newrace6, digits = 2)
  ## p value is 0.47, meaning that race does not significantly affect the 
  ## association between income and bclength

