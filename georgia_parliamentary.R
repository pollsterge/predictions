library(extrafont); library(dplyr); library(ggplot2); library(rstan); library(reshape2); library(stringr); library(lubridate); library(readxl); library(highcharter); library(tidyr)
library(ggiraph)

options(mc.cores = parallel::detectCores())

setwd("D:\\Dropbox\\pollster.ge\\Geo Parl 2020\\Polls\\modeling\\jim_savage")


### First set up GD polls

polls <- read_excel("Parliamentary 2020.xlsx", sheet = "data")%>%
  filter(!WAVEID %in% c("W1", "W5"))%>%
  filter(!PARTYCODE %in% c("DK",
                           "NOPARTY",
                           "NOTASKED",
                           "NOTDECIDED",
                           "REFUSE",
                           # "OTHER",
                           "UNDECIDED"
  ))%>%
  mutate(
    PARTYCODE = case_when(
      PARTYCODE == "SHENEBA" ~ "LELO",
      T ~ as.character(PARTYCODE)
    ),
    PARTYCODE = case_when(
      PARTYCODE == "FREEDEM" ~ "EUROGEO",
      T ~ as.character(PARTYCODE)
    ),
    
    field_last_day = as.Date(`Field last day`),
    weight=Sys.Date()-field_last_day
  )%>%
  group_by(WAVEID)%>%
  mutate(
    Percent = Percent/sum(Percent),
    sigma=(AME/4)*0.01
  )%>%
  arrange(field_last_day)%>%
  ungroup()%>%
  mutate(
    sigma= tidyr::replace_na(sigma, mean(sigma, na.rm=T)))
  
### Make first for GD

# The polling data
# One row for each day, one column for each poll on that day, -9 for missing values
Y_dream <- polls %>%
  filter(PARTYCODE == "GD")%>%
  arrange(field_last_day)%>%
  mutate(N = 1:n())%>%
  filter(N > 8) %>% dcast(field_last_day  ~ N, value.var = "Percent") %>% 
  dplyr::select(-field_last_day ) %>% 
  as.data.frame %>% as.matrix
Y_dream[is.na(Y_dream)] <- -9

Y_unm <- polls %>%
  filter(PARTYCODE == "UNM")%>%
  arrange(field_last_day)%>%
  mutate(N = 1:n())%>%
  filter(N > 8) %>% dcast(field_last_day ~ N, value.var = "Percent") %>% 
  dplyr::select(-field_last_day) %>% 
  as.data.frame %>% as.matrix
Y_unm[is.na(Y_unm)] <- -9

Y_eurogeo <- polls %>%
  filter(PARTYCODE == "EUROGEO")%>%
  arrange(field_last_day)%>%
  mutate(N = 1:n())%>%
  group_by(WAVEID)%>%
  summarize(field_last_day=first(field_last_day),
            Percent=sum(Percent),
            N=first(N))%>%
  ungroup()%>%
  arrange(N)%>%
  mutate(N = 1:n())%>%
  filter(N > 8) %>% dcast(field_last_day ~ N, value.var = "Percent") %>% 
  dplyr::select(-field_last_day) %>% 
  as.data.frame %>% as.matrix
Y_eurogeo[is.na(Y_eurogeo)] <- -9

Y_apg <- polls %>%
  filter(PARTYCODE == "APG")%>%
  arrange(field_last_day)%>%
  mutate(N = 1:n())%>%
  filter(N > 8) %>% dcast(field_last_day ~ N, value.var = "Percent") %>% 
  dplyr::select(-field_last_day) %>% 
  as.data.frame %>% as.matrix
Y_apg[is.na(Y_apg)] <- -9


Y_agm <- polls %>%
  filter(PARTYCODE == "NEWGEORGIA")%>%
  arrange(field_last_day)%>%
  mutate(N = 1:n())%>%
  filter(N > 8) %>% dcast(field_last_day ~ N, value.var = "Percent") %>% 
  dplyr::select(-field_last_day) %>% 
  as.data.frame %>% as.matrix
Y_agm[is.na(Y_agm)] <- -9

Y_lab <- polls %>%
  filter(PARTYCODE == "LABOR")%>%
  arrange(field_last_day)%>%
  mutate(N = 1:n())%>%
  filter(N > 8) %>% dcast(field_last_day ~ N, value.var = "Percent") %>% 
  dplyr::select(-field_last_day) %>% 
  as.data.frame %>% as.matrix
Y_lab[is.na(Y_lab)] <- -9


# Do the same for margin of errors for those polls

sigma <- polls %>%
  filter(PARTYCODE == "GD")%>%
  arrange(field_last_day)%>%
  mutate(N = 1:n())%>%
  filter(N > 8) %>% dcast(field_last_day ~ N, value.var = "sigma")%>% 
  dplyr::select(-field_last_day)%>% 
  as.data.frame %>% as.matrix
sigma[is.na(sigma)] <- -9

# Run the two models

dream_model <- stan("state_space_polls.stan", 
                    data = list(T = nrow(Y_dream), 
                                polls = ncol(Y_dream), 
                                Y = Y_dream, 
                                sigma = sigma,
                                initial_prior = 0.3864),
                    control = list(adapt_delta = 0.999999999999),
                    iter=2000)



unm_model <- stan("state_space_polls.stan", 
                  data = list(T = nrow(Y_unm), 
                              polls = ncol(Y_unm), 
                              Y = Y_unm, 
                              sigma = sigma,
                              initial_prior = 0.3774),
                  control = list(adapt_delta = 0.999999))

eurogeo_model <- stan("state_space_polls.stan", 
                   data = list(T = nrow(Y_eurogeo), 
                               polls = ncol(Y_eurogeo), 
                               Y = Y_eurogeo, 
                               sigma = sigma,
                               initial_prior = 0.1097),
                   control = list(adapt_delta = 0.999999))

apg_model <- stan("state_space_polls.stan", 
                      data = list(T = nrow(Y_apg), 
                                  polls = ncol(Y_apg), 
                                  Y = Y_apg, 
                                  sigma = sigma,
                                  initial_prior = 0.0656),
                      control = list(adapt_delta = 0.999999))

# Pull the state vectors

mu_dream <- rstan::extract(dream_model, pars = "mu", permuted = T)[[1]] %>% 
  as.data.frame

mu_unm <- rstan::extract(unm_model, pars = "mu", permuted = T)[[1]] %>% 
  as.data.frame

mu_eurogeo <- rstan::extract(eurogeo_model, pars = "mu", permuted = T)[[1]] %>% 
  as.data.frame

mu_apg <- rstan::extract(apg_model, pars = "mu", permuted = T)[[1]] %>% 
  as.data.frame
# Rename to get dates

names <- polls %>%
  filter(PARTYCODE == "APG")%>%
  mutate(N = 1:n())%>%
  filter(N > 8)



names(mu_dream) <- unique(paste0(names$field_last_day))
names(mu_unm) <- unique(paste0(names$field_last_day))
names(mu_eurogeo) <- unique(paste0(names$field_last_day))
names(mu_apg) <- unique(paste0(names$field_last_day))


### Georgian chart

mu_dream$party <- "ქართული ოცნება"
mu_unm$party <- "ერთიანი ნაციონალური მოძრაობა"
mu_eurogeo$party <- "ევროპული საქართველო"
mu_apg$party <- "პატრიოტთა ალიანსი"

preds <- rbind(mu_dream, mu_unm, mu_eurogeo, mu_apg)%>%
  mutate(
    party = factor(party, levels=c("ქართული ოცნება", "ერთიანი ნაციონალური მოძრაობა", "ევროპული საქართველო", "პატრიოტთა ალიანსი"))
  )

analysis <- preds %>%
  subset(select=c(`2020-09-07`, party))%>%
  as.data.frame()

names(analysis) <- c("value", "party")

analysis %>%
  group_by(party)%>%
  mutate(row = row_number()) %>%
  ungroup()%>%
  pivot_wider(names_from = party, values_from=value) -> party_grouped

names(party_grouped) <- c("row", "gd", "unm", "eg", "pa")
### Count what is the chance of GD losing to opposition

nrow(party_grouped[party_grouped$`gd` > 0.5, ])/nrow(party_grouped)
nrow(party_grouped[party_grouped$`gd` > 0.4, ])/nrow(party_grouped)
nrow(party_grouped[party_grouped$`gd` >= 0.75, ])/nrow(party_grouped)
nrow(party_grouped[party_grouped$`gd` < party_grouped$`unm`, ])/nrow(party_grouped)



ggplot(preds, aes(`2020-09-07`, group=party, fill=party, color=party))+
  geom_density_interactive(aes(y = (..count..)/sum(..count..), tooltip=party), alpha=0.5)+
  scale_fill_manual(values=c( "#195ea2", "#dc082b", "#003a75", "#e7b031"))+
  scale_color_manual(values=c( "#195ea2", "#dc082b", "#003a75", "#e7b031"))+
  geom_vline(xintercept = 0.5)+
  geom_vline(xintercept = 0.4)+
  geom_vline(xintercept = 0.01)+
  geom_vline(xintercept = median(mu_dream$`2020-09-07`), color="#0077be", linetype = "longdash")+
  geom_vline(xintercept = median(mu_unm$`2020-09-07`), color="#dc082b", linetype = "longdash")+
  geom_vline(xintercept = median(mu_eurogeo$`2020-09-07`), color="#003a75", linetype = "longdash")+
  geom_vline(xintercept = median(mu_apg$`2020-09-07`), color="#e7b031", linetype = "longdash")+
  annotate("text", x = median(mu_dream$`2020-09-07`), y = 0.009,  color="#0077be", label = sprintf("%0.f", round(median(mu_dream$`2020-09-07`)*100, digits = 0)))+
  annotate("text", x = median(mu_unm$`2020-09-07`), y = 0.009,  color="#dc082b", label = sprintf("%0.f", round(median(mu_unm$`2020-09-07`)*100, digits = 0)))+
  annotate("text", x = median(mu_eurogeo$`2020-09-07`), y = 0.009,  color="#003a75", label = sprintf("%0.f", round(median(mu_eurogeo$`2020-09-07`)*100, digits = 0)))+
  annotate("text", x = median(mu_apg$`2020-09-07`), y = 0.009,  color="#e7b031", label = sprintf("%0.f", round(median(mu_apg$`2020-09-07`)*100, digits = 0)))+
  annotate("text", x = 0.01, y = 0.008,  label = "1%-იანი\nსაარჩევნო\nბარიერი", family="BPG Excelsior Exp")+
  annotate("text", x = 0.4, y = 0.008,  label = "40%-იანი\nჩამკეტი\nბარიერი", family="BPG Excelsior Exp")+
  scale_x_continuous(labels=function(x)x*100, limits=c(0, 1))+
  # facet_wrap(~party)
  labs(# title="პოლიტიკური პარტიების სიმულირებული პროპორცია",
       # subtitle="წარწერებზე მოცემულია სიმულაციათა მედიანური მნიშვნელობები",
       caption="ეფუძნება 2000 სიმულაციას,\nაპრიორულ ალბათობად მიჩნეულია\n2017 და 2018 წლის არჩევნების შედეგები",
       x="%")+
  theme_plot+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) -> gg

ggsave("predictions.png", gg, height=8, width=12)


tooltip_css <- "background-color:white;font-family:BPG Excelsior Exp;padding:10px;border-radius:10px 20px 10px 20px;"

gg_a <- girafe(ggobj=gg, width_svg = 9, height_svg = 6)
gg_a <- girafe_options(gg_a, opts_tooltip(css = tooltip_css, opacity = .75))
gg_a

htmlwidgets::saveWidget(gg_a, "model_ka.html")


### English chart

mu_dream$party <- "Georgian Dream"
mu_unm$party <- "United National Movement"
mu_eurogeo$party <- "European Georgia"
mu_apg$party <- "Alliance of Patriots"

preds <- rbind(mu_dream, mu_unm, mu_eurogeo, mu_apg)%>%
  mutate(
    party = factor(party, levels=c("Georgian Dream", "United National Movement", "European Georgia",
                                   "Alliance of Patriots"))
  )

ggplot(preds, aes(`2020-09-07`, group=party, fill=party, color=party))+
  geom_density_interactive(aes(y = (..count..)/sum(..count..), tooltip=party), alpha=0.5)+
  scale_fill_manual(values=c( "#195ea2", "#dc082b", "#003a75", "#e7b031"))+
  scale_color_manual(values=c( "#195ea2", "#dc082b", "#003a75", "#e7b031"))+
  geom_vline(xintercept = 0.5)+
  geom_vline(xintercept = 0.4)+
  geom_vline(xintercept = 0.01)+
  geom_vline(xintercept = median(mu_dream$`2020-09-07`), color="#0077be", linetype = "longdash")+
  geom_vline(xintercept = median(mu_unm$`2020-09-07`), color="#dc082b", linetype = "longdash")+
  geom_vline(xintercept = median(mu_eurogeo$`2020-09-07`), color="#003a75", linetype = "longdash")+
  geom_vline(xintercept = median(mu_apg$`2020-09-07`), color="#e7b031", linetype = "longdash")+
  annotate("text", x = median(mu_dream$`2020-09-07`), y = 0.009,  color="#0077be", label = sprintf("%0.f", round(median(mu_dream$`2020-09-07`)*100, digits = 0)))+
  annotate("text", x = median(mu_unm$`2020-09-07`), y = 0.009,  color="#dc082b", label = sprintf("%0.f", round(median(mu_unm$`2020-09-07`)*100, digits = 0)))+
  annotate("text", x = median(mu_eurogeo$`2020-09-07`), y = 0.009,  color="#003a75", label = sprintf("%0.f", round(median(mu_eurogeo$`2020-09-07`)*100, digits = 0)))+
  annotate("text", x = median(mu_apg$`2020-09-07`), y = 0.009,  color="#e7b031", label = sprintf("%0.f", round(median(mu_apg$`2020-09-07`)*100, digits = 0)))+
  annotate("text", x = 0.01, y = 0.008,  label = "1%-threshold", family="BPG Excelsior Exp")+
  annotate("text", x = 0.4, y = 0.008,  label = "40%-closing threshold", family="BPG Excelsior Exp")+
  scale_x_continuous(labels=function(x)x*100, limits=c(0, 1))+
  # facet_wrap(~party)
  labs(# title="პოლიტიკური პარტიების სიმულირებული პროპორცია",
    # subtitle="წარწერებზე მოცემულია სიმულაციათა მედიანური მნიშვნელობები",
    caption="Based on 2000 simlations,\n2017 and 2018 election results are considered as priors",
    x="%")+
  theme_plot+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) -> gg

ggsave("predictions.png", gg, height=8, width=12)


tooltip_css <- "background-color:white;font-family:BPG Excelsior Exp;padding:10px;border-radius:10px 20px 10px 20px;"

gg_a <- girafe(ggobj=gg, width_svg = 9, height_svg = 6)
gg_a <- girafe_options(gg_a, opts_tooltip(css = tooltip_css, opacity = .75))
gg_a

htmlwidgets::saveWidget(gg_a, "model_en.html")

