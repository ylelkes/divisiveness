library(tidyverse)
library(data.table)
library(lubridate)
library(ggrepel)

#setwd("[directory where replication files are located]")

# load in annotated twitter data
tweets_bert <- fread("twitter_bert.csv")
tweets_human <- fread("twitter_human.csv")


# convert twitter data from tweet-level to day-level averages of identity mentions
## converting human-labeled tweets to day-level averages
tweets_human_daily <- tweets_human %>%
  mutate(date = date(ymd_hms(datetime))) %>%
  select(date, political, religion, gender, raceethnic, implicit) %>%
  pivot_longer(cols = c(political, religion, gender, raceethnic, implicit), names_to = "Identity", values_to = "Frequency") %>%
  group_by(date, Identity) %>%
  summarize(percent = mean(Frequency, na.rm=TRUE)) %>%
  mutate(Identity = case_when(str_detect(Identity, "race") ~ "Race/ethnicity",
                              str_detect(Identity, "gen") ~ "Gender/sexuality",
                              str_detect(Identity, "pol") ~ "Political",
                              str_detect(Identity, "impl") ~ "Implicit race/ethnicity",
                              str_detect(Identity, "relig") ~ "Religion"),
         Annotation = "Human")

## converting BERT-labeled tweets to day-level averages
tweets_bert_daily <- tweets_bert %>%
  group_by(date = date(ymd_hms(datetime))) %>%
  summarize(gender_pred = mean(gender_pred, na.rm=TRUE),
            political_pred = mean(political_pred, na.rm=TRUE),
            raceethnic_pred = mean(raceethnic_pred, na.rm=TRUE)) %>%
  pivot_longer(cols = c(gender_pred, political_pred, raceethnic_pred), names_to = "Identity", values_to = "Frequency") %>%
  mutate(Identity = case_when(str_detect(Identity, "race") ~ "Race/ethnicity",
                              str_detect(Identity, "gender") ~ "Gender/sexuality",
                              str_detect(Identity, "pol") ~ "Political"))



# creating tibble to store day-level smoothed estimates of average identity mentions
date_range <- tibble(date = seq(as.Date("2007-03-06"), as.Date("2021-12-31"),by="days"))

# for each identity, predicting smoothed averages for each date in the date range
## this smooth is only done for the BERT estimates, as the human-labeled data contains far fewer observations
date_range$race <- predict(loess(Frequency ~ as.numeric(date),
                                 tweets_bert_daily %>% filter(Identity == "Race/ethnicity"),
                                 span=.04),date_range)

date_range$gender <- predict(loess(Frequency ~ as.numeric(date),
                                   tweets_bert_daily %>% filter(Identity == "Gender/sexuality"),
                                   span=.04),date_range)

date_range$political <- predict(loess(Frequency ~ as.numeric(date),
                                      tweets_bert_daily %>% filter(Identity == "Political"),
                                      span=.04),date_range)
# due to weak classifier performance, smoothed estimates of identity mentions are not shown for
# "religion" or "implicit race/ethnicity"
date_range$implicit <- NA

date_range$religion <- NA

# Wrangling the smoothed identity estimates to prepare data for plotting
# Also adding text for the events that are noted in the figure

date_range_combined <- date_range %>%
  pivot_longer(cols = c("race", "gender", "political","implicit", "religion"),
               names_to = "Identity", values_to = "fit") %>%
  mutate(Identity = case_when(Identity == "race" ~ "Race/ethnicity",
                              Identity == "gender" ~ "Gender/sexuality",
                              Identity == "political" ~ "Political",
                              Identity == "implicit" ~ "Implicit race/ethnicity",
                              Identity == "religion" ~ "Religion"),
         Annotation = "BERT",
         label = case_when(date == mdy("07-13-2013") &
                             str_detect(Identity, "ethnic") ~ "George Zimmerman\nacquittal",
                           date == mdy("06-15-2015") &
                             str_detect(Identity, "ethnic") ~ "Trump comments\non immigrants",
                           date == mdy("05-25-2020") &
                             str_detect(Identity, "ethnic") ~ "Killing of George Floyd",
                           date == mdy("08-09-2014") &
                             str_detect(Identity, "ethnic") ~ "Killing of\nMichael Brown",
                           date == mdy("01-27-2017") &
                             str_detect(Identity, "ethnic") ~ "Trump signs\n'Muslim Ban'",
                           date == mdy("03-23-2013") &
                             Identity == "Gender/sexuality" ~ "Women allowed to\nserve in combat",
                           date == mdy("06-26-2015") &
                             Identity == "Gender/sexuality" ~ "Gay marriage\nSupreme Court ruling",
                           date == mdy("10-07-2016") &
                             Identity == "Gender/sexuality" ~ "Access Hollywood\ntape",
                           date == mdy("01-21-2017") &
                             Identity == "Gender/sexuality" ~ "Women's\nMarch",
                           date == mdy("10-15-2017") &
                             Identity == "Gender/sexuality" ~ "#MeToo Twitter\nmovement",

                           date == mdy("11-4-2008") &
                             Identity == "Political" ~ "Obama elected",
                           date == mdy("11-4-2012") &
                             Identity == "Political" ~ "Obama reelected",
                           date == mdy("11-4-2016") &
                             Identity == "Political" ~ "Trump elected",
                           date == mdy("11-4-2020") &
                             Identity == "Political" ~ "Biden elected",

                           date == mdy("11-2-2010") &
                             Identity == "Political" ~ "Midterm\nelection",
                           date == mdy("11-4-2014") &
                             Identity == "Political" ~ "Midterm\nelection",
                           date == mdy("11-6-2018") &
                             Identity == "Political" ~ "Midterm\nelection",
                           date == mdy("6-16-15") &
                             Identity == "Political" ~ "Trump announces\ncandidacy",
                           date == mdy("12-18-19") &
                             Identity == "Political" ~ "Trump first\nimpeachment"))

# manually adding "fit" values for the implicit race/ethnicity labels
# as this identity category does not have BERT-based predictions and thus the
# function above does not automatically assign heights for these labels

date_range_combined <- date_range_combined %>%
  mutate(fit = case_when(
    str_detect(Identity, "Implicit") & str_detect(label, "Zimmerman") ~ 0.009,
    str_detect(Identity, "Implicit") & str_detect(label, "Brown") ~ 0.0302,
    str_detect(Identity, "Implicit") & str_detect(label, "immigrants") ~ 0.0248,
    str_detect(Identity, "Implicit") & str_detect(label, "Muslim") ~ 0.029,
    str_detect(Identity, "Implicit") & str_detect(label, "Floyd") ~ 0.022,
    TRUE ~ fit))



# setting width the red bands that denote events
time_before = 14
time_after = 14

add_bar <- function(event_date, identity){
  geom_ribbon(data = date_range_combined %>%
                filter(Identity == identity) %>%
                filter(between(date, mdy(event_date)-time_before,
                               mdy(event_date)+time_after)),
              aes(ymin=0,ymax=fit), fill = "red",alpha=.7, col="black")
}

# defining function to return a plot, given an identity
make_plot <- function(identity, ymax){
  max = max(date_range_combined[date_range_combined$Identity ==identity,"fit"], na.rm = TRUE)

  p <- date_range_combined %>%
    filter(Identity == identity) %>%
    ggplot(aes(x=date))+
    geom_ribbon(aes(ymin=0,ymax=fit),alpha=.15, col="black")+
    geom_line(aes(linetype=Annotation,y=fit), size=.75, col="black")+
    geom_text_repel(aes(y = fit, label = label),
                    nudge_y=.2,
                    nudge_x = 175,
                    box.padding = .5
    )+
    geom_smooth(data = tweets_human_daily %>% filter(Identity == identity),
                aes(y = percent, linetype=Annotation),
                method="loess",span=.1, color = "black", se=FALSE)+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(ymd("20070101"), ymd("20211231")))+
    scale_y_continuous(labels = scales::percent)+
    ylab("Percent of tweets mentioning social group")+
    xlab("")+
    facet_wrap(facets="Identity", ncol=1)+
    theme_bw()+
    theme(legend.position="none")+
    coord_cartesian(ylim=c(0, ymax))

  if(!(str_detect(identity, "Implicit")) & !(str_detect(identity, "Religion"))){
    for(date in date_range_combined %>%
      filter(Identity==identity,
             !is.na(label)) %>%
      mutate(date = format(date, "%m-%d-%Y")) %>%
      pull(date)){
    p <- p + add_bar(date, identity)
  }
  }
  return(p)
}

# Generate Figure 1

f1a <- make_plot("Race/ethnicity", .07)

ggsave(f1a, device = "png",
       height = 6,
       width = 8,
       units = "in",
       filename = "f1a.png")

f1b <- make_plot("Implicit race/ethnicity", .07)
ggsave(f1b, device = "png",
       height = 6,
       width = 8,
       units = "in",
       filename = "f1b.png")

f1c <- make_plot("Political", .13)
ggsave(f1c, device = "png",
       height = 6,
       width = 8,
       units = "in",
       filename = "f1c.png")

f1d <- make_plot("Gender/sexuality", .07)
ggsave(f1d, device = "png",
       height = 6,
       width = 8,
       units = "in",
       filename = "f1d.png")

f1e <- make_plot("Religion", .03)
ggsave(f1e, device = "png",
       height = 6,
       width = 8,
       units = "in",
       filename = "f1e.png")
