
setwd(here::here())

library(tidyverse)
library(ggridges)
dat <- read.csv("data/Exploring Perceptions of Bottled Water Consumption_November 28, 2023_14.59.csv")

theme_set(theme_bw())

dat <- dat %>%
  filter(Finished == "TRUE") %>%
  filter(Age > 17)


## Primary Source by Gender Categories ####

dat <- dat %>%
  mutate(across('PrimarySource', str_replace, 'Glass bottled/canned/boxed water', 'Other')) %>%
  mutate(across('Gender', str_replace, 'Gender not listed. My gender is', "Other"))

dat %>%
  filter(Gender != "") %>%
ggplot(mapping = aes(x = PrimarySource, fill = Gender)) +
  geom_bar(position="dodge") +
  labs(title = "Counts of Water Source by Gender Category")



dat_sum <- dat %>%
  filter(Gender != "") %>%
  filter(Gender != "Prefer not to answer") %>%
  mutate(Gender = case_when(Gender == "Other" ~ "Other", 
                            Gender == "Transgender man" ~ "Other",
                            Gender == "Transgender woman" ~ "Other", 
                            Gender == "Woman" ~ "Woman",
                            Gender == "Man" ~ "Man", 
                            Gender == "Non-binary" ~ "Non-binary")) %>%
  group_by(Gender, PrimarySource) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) 


dat_sum %>%
  filter(PrimarySource != "Prefer not to answer") %>%
ggplot(mapping = aes(x = Gender, 
                     y = freq,
                     fill = PrimarySource)) +
  geom_col(color = "black") +
  labs(title = "Proportion of each Source within Gender Categories")


## Age ####
dat$Age <- as.numeric(dat$Age)

dat <- dat%>%
  filter(!is.na(Age))


ggplot(data = dat, 
       mapping = aes(y = Age, x = PrimarySource)) +
  geom_boxplot(fill="#93E9BE")  +
  labs(title= "Distribution of Ages for Different Water Sources")+
  theme(plot.title = element_text(hjust = 0.5))

## Race ####

ggplot(data = dat, 
       mapping = aes(x = PrimarySource, fill = Race)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

## School Year ####

dat <- dat %>%
  filter(SchoolYear != "")
dat$SchoolYear <- as.factor(dat$SchoolYear)

                     
dat$SchoolYear <- factor(dat$SchoolYear, c("Freshman", "Sophomore", "Junior", "Senior",
                           "Master", "PhD", "Other", "Prefer not to answer"))

dat %>%
  filter(PrimarySource != "I don't drink water" & PrimarySource != "Prefer not to answer") %>%
ggplot(mapping = aes(x = PrimarySource, fill = SchoolYear)) +
  geom_bar(position = "dodge", color = "black")


dat_year <- dat %>%
  filter(SchoolYear != "Other" & SchoolYear != "Prefer not to answer") %>%
  filter(PrimarySource == "Other" | PrimarySource == "Tap water" | 
           PrimarySource == "Plastic bottled water" ) %>%
  group_by(SchoolYear, PrimarySource) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) 


ggplot(data = dat_year, 
       mapping = aes(x = SchoolYear, 
                     y = freq,
                     fill = PrimarySource)) +
  geom_col(color = "black") +
  labs(title = "Proportion of each Source within School Year")+
  theme(plot.title = element_text(hjust = 0.5))



## Political Affiliation ####

dat$PoliticalAffil <- factor(dat$PoliticalAffil, c("Consistently liberal","Mostly liberal", "Mixed", 
                                                   "Mostly conservative", "Consistently conservative","Other"))
dat %>%
  filter(PrimarySource != "I don't drink water" & PrimarySource != "Prefer not to answer") %>%
  filter(PoliticalAffil !="Prefer not to answer") %>%
  ggplot(mapping = aes(x = PrimarySource, fill = PoliticalAffil)) +
  geom_bar(position = "dodge", color = "black")



dat_pol <- dat %>%
  filter(PoliticalAffil != "Prefer not to answer") %>%
  filter(PrimarySource == "Other" | PrimarySource == "Tap water" | 
           PrimarySource == "Plastic bottled water" ) %>%
  group_by(PoliticalAffil, PrimarySource) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) 

ggplot(data = dat_pol, 
       mapping = aes(x = PoliticalAffil, 
                     y = freq,
                     fill = PrimarySource)) +
  geom_col(color = "black") +
  labs(title = "Proportion of Preferred Source within Political Affiliation")+
  theme(plot.title = element_text(hjust = 0.5))


## Rankings ####

dat_rank <- dat %>%
  pivot_longer(30:36, 
               names_to = "Question",
               values_to = "Ranking") 

dat_rank$Ranking <- as.numeric(dat_rank$Ranking)
ggplot(data = dat_rank, 
       mapping = aes(x = Ranking,
                     y = Question, 
                     fill = Question)) +
  geom_density_ridges(alpha = .5)





## 

ggplot(data = dat,
       mapping = aes(x = HomeTapSafe,
                     fill = PrimarySource)) +
  geom_bar()


ggplot(data = dat,
       mapping = aes(x = PlasticBottleSafe,
                     fill = PrimarySource)) +
  geom_bar()


ggplot(data = dat,
       mapping = aes(x = GovtReliable,
                     fill = PrimarySource)) +
  geom_bar()


ggplot(data = dat,
       mapping = aes(x = ChoiceOtherCommunity,
                     fill = PrimarySource)) +
  geom_bar()


## Model ####

dat <- dat %>%
  mutate(PrimarySource = case_when(PrimarySource == "Tap water" ~ 1, 
                                   PrimarySource == "Plastic bottled water" ~ 0,
                              PrimarySource == "I don't drink water" ~5, 
                              PrimarySource =="Prefer not to answer" ~ 5,
                              PrimarySource == "Other" ~ 5 ))

datmod <- dat %>%
  filter(PrimarySource != 5)

bottle_mod <-  glm(PrimarySource ~ Age, 
                                  family = binomial(link = "logit"), 
                                  data = datmod)


ggplot(data = datmod,
       aes(x = Age, 
           y = PrimarySource, 
           color = Gender)) +
  geom_point() +
  stat_smooth(method = "glm", 
              aes(fill = Gender),
              alpha=.1,
              method.args = 
                list(family = binomial(link = "logit"))) +
  facet_wrap(vars(sex)) +
  scale_color_manual(values = c("deeppink3", "blue3")) +
  scale_fill_manual(values = c("deeppink3", "blue3"))



















