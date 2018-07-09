library(tidyverse)
library(forcats)

#Explore the distribution of rincome (reported income).
#What makes the default bar chart hard to understand? 
#How could you improve the plot?
gss_cat %>%
  count(rincome)

ggplot(gss_cat, aes(rincome)) +
  geom_bar()

#What is the most common relig in this survey? What’s the most common partyid?

gss_cat %>%
  count(relig, denom)

<-gss_cat %>%
  count(partyid)

#Which relig does denom (denomination) apply to? How can you find out with a table? How can you find out with a visualisation?

ggplot(gss_cat) + 
  geom_bar(aes(x = relig, fill = denom))

#There are some suspiciously high numbers in tvhours. Is the mean a good summary?
#A - should probably use median

#For each factor in gss_cat identify whether the order of the levels is arbitrary or principled.
# Principled - year, age, income, tvhours
# Arbitrary - marital, race, partyid, relig, denom
gss_cat %>%
  count(tvhours)

ggplot(gss_cat, aes(tvhours)) +
  geom_bar()

#Why did moving “Not applicable” to the front of the levels move it to the bottom of the plot?
#looks like it reversed the order of the factors

#How have the proportions of people identifying as Democrat, Republican, and Independent changed over time?

by_year <- gss_cat %>%
  filter(!is.na(partyid)) %>%
  group_by(year) %>%
  count() 

by_partyid_yr <- gss_cat %>%
  filter(!is.na(partyid)) %>%
  select(year, partyid) %>%
  mutate(partyidcoll = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  group_by(year)%>%
  count()%>%
  rename(partyid_yr = n)

  right_join(x=by_year, y=partyid)
  mutate(prop = n / by_year$n)

ggplot(by_partyid, aes(year, prop, colour = fct_reorder2(partyid, year, prop))) +
  geom_line() +
  labs(colour = "partyid")

#How could you collapse rincome into a small set of categories?
gss_cat %>%
  mutate(rincome = fct_collapse(rincome,
                                other = c("No answer", "Don't know", "Refused"),
                                high = c("$25000 or more", "$20000 - 24999"),
                                med = c("$15000 - 19999", "$10000 - 14999"),
                                low = c("$8000 to 9999", "$7000 to 7999", "$6000 to 6999", "$5000 to 5999",
                                        "$4000 to 4999", "$3000 to 3999", "$1000 to 2999", "Lt $1000")
  )) %>%
  count(rincome)

ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()

#Pat & Nick
by_year <- gss_cat %>%
  filter(!is.na(year)) %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                Republican = c("Strong republican", "Not str republican"),
                                Independent = c("Ind,near rep", "Independent", "Ind,near dem"),
                                Democrat = c("Not str democrat", "Strong democrat"))) %>% 
  group_by(year, partyid) %>%
  count() %>%
  group_by(year) %>%  ###
  mutate(prop = n / sum(n)) %>%
  filter(partyid != "other")


ggplot(by_year, aes(year, prop, colour = partyid)) +
  geom_line(na.rm = TRUE)

ggplot(by_year, aes(year, prop, colour = fct_reorder2(partyid, year, prop))) +
  geom_line() +
  labs(colour = "partyid")

#Kaitlin & Marc
by_year <- gss_cat %>% filter(!is.na(year)) %>% 
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat"))) %>% 
  group_by(partyid, year)  %>% count() %>% mutate(prop = n / sum(n)) 
ggplot(by_year, aes(year, prop)) +geom_line(aes(color=partyid))

#will
partyid_year_count <- gss_cat %>%
  filter(!is.na(partyid)) %>% 
  select(year, partyid) %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  group_by(year) %>% 
  count() %>% 
  rename(partyid_year_total = n)
by_year <- gss_cat %>% 
  filter(!is.na(partyid)) %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  group_by(year, partyid) %>% 
  count() %>%
  left_join(partyid_year_count, by = "year") %>%  
  mutate(prop = n / partyid_year_total)
by_year %>% 
  filter(partyid != "other") %>% 
  ggplot(aes(year, prop, color = partyid)) +
  geom_line(na.rm = T)

