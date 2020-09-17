library(tidyverse)
library(readxl)
library(extrafont)
library(ggiraph)

### Define plot theme

setwd("D:\\Dropbox\\pollster.ge\\Geo Parl 2020\\Polls\\modeling\\jim_savage")

theme_plot <- theme(
  axis.text.y = element_text(colour="black", size = 14, family = "BPG Excelsior Exp"),
  axis.text.x = element_text(colour="black", size = 14, family="BPG Excelsior Exp"),
  axis.title.x = element_text(size=14, family = "BPG Excelsior Caps"),
  axis.title.y = element_text(size=14, family = "BPG Excelsior Caps"),
  strip.text  = element_text(size=14, family = "BPG Excelsior Caps"),
  legend.title = element_text(size=8, family = "BPG Excelsior Caps"),
  legend.text = element_text(size=8, family = "BPG Excelsior Exp"),
  plot.caption = element_text(size=10, family = "BPG Excelsior Exp"),
  plot.subtitle = element_text(hjust = 0.5, colour = "Black", size=10, family = "BPG Excelsior Caps"),
  plot.title = element_text(hjust = 0.5, colour = "Black", size=12, family = "BPG Excelsior Caps"),
  panel.border = element_rect(fill=NA, linetype = "solid", colour = "black"),
  panel.background = element_rect(fill = NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

## 
src <- readxl::read_excel("Parliamentary 2020.xlsx", sheet="data")

# Georgian-language prediction chart

src %>% 
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
    Percent = Percent/sum(Percent)
  )%>%
  group_by(PARTYCODE)%>%
  summarize(
    proportion = weighted.mean(Percent, 1/as.numeric(weight))
  )%>%
  ungroup()%>%
  filter(proportion > 0.01)%>%
  filter(PARTYCODE != "OTHER")%>%
  mutate(PARTYCODE = factor(PARTYCODE, levels=c("GD", "UNM", "EUROGEO", "APG", "LABOR", "NEWGEORGIA", "LELO",
                                                "CIVICMO", "GIRCHI", "DMUG", "FORJUSTICE", "VICTORIOUSGEORGIA",
                                                "FREEGEO"),
                                       labels=c("ქართული ოცნება", "ენმ", "ევროპული საქართველო", "პატრიოტთა ალიანსი",
                                                "ლეიბორისტები", "სტრატეგია აღმაშენებელი", "ლელო", "მოქალაქეები",
                                                "გირჩი", "ერთიანი საქართველო", "სამართლიანობისთვის", "გამარჯვებული საქართველო",
                                                "თავისუფალი საქართველო")))%>%
  ggplot(aes(proportion, forcats::fct_reorder(PARTYCODE, proportion), fill=PARTYCODE))+
  geom_col_interactive(aes(tooltip=paste0(PARTYCODE, ": ", round(proportion*100, 0), "%")))+
  geom_text(aes(proportion, PARTYCODE, label=round(proportion*100, 0)),
            vjust = 0.4, hjust=-1, size = 4, 
            position=position_dodge(width = 0.5), family="BPG Excelsior Exp")+
  scale_fill_manual(values = c("#195ea2", "#dc082b", "#003a75", "#e7b031",
                               "#e1073b", "#fc5000", "#f0ce0d", "#8ac650",
                               "#327f37", "#0f5cbb", "#019541", "#d92c1c",
                               "#a30032"))+
  scale_x_continuous(labels = scales::percent_format(), limits=c(0,1))+
  labs(title="პარტიული რეიტინგების შეწონილი საშუალო მნიშვნელობა, 2020 წლის 16 ივლისისთვის",
       x="პროგნოზირებული პროპორცია")+
  theme_plot+
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_blank()
  ) -> gg

gg

tooltip_css <- "background-color:white;font-family:BPG Excelsior Exp;padding:10px;border-radius:10px 20px 10px 20px;"

gg_a <- girafe(ggobj=gg, width_svg = 9, height_svg = 6)
gg_a <- girafe_options(gg_a, opts_tooltip(css = tooltip_css, opacity = .75))
gg_a
htmlwidgets::saveWidget(gg_a, "weighted_average_ka.html")

# English-language prediction chart

src %>% 
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
    Percent = Percent/sum(Percent)
  )%>%
  group_by(PARTYCODE)%>%
  summarize(
    proportion = weighted.mean(Percent, 1/as.numeric(weight))
  )%>%
  ungroup()%>%
  filter(proportion > 0.01)%>%
  filter(PARTYCODE != "OTHER")%>%
  mutate(PARTYCODE = factor(PARTYCODE, levels=c("GD", "UNM", "EUROGEO", "APG", "LABOR", "NEWGEORGIA", "LELO",
                                                "CIVICMO", "GIRCHI", "DMUG", "FORJUSTICE", "VICTORIOUSGEORGIA",
                                                "FREEGEO"),
                            labels=c("Georgian Dream", "UNM", "European Georgia", "Alliance of Patriots",
                                     "Labor", "Strategy Aghmashenebeli", "Lelo", "Citizens",
                                     "Girchi", "United Georgia", "For Justice",
                                     "Victorious Georgia",
                                     "Free Georgia")))%>%
  ggplot(aes(proportion, forcats::fct_reorder(PARTYCODE, proportion), fill=PARTYCODE))+
  geom_col_interactive(aes(tooltip=paste0(PARTYCODE, ": ", round(proportion*100, 0), "%")))+
  geom_text(aes(proportion, PARTYCODE, label=round(proportion*100, 0)),
            vjust = 0.4, hjust=-1, size = 4, 
            position=position_dodge(width = 0.5), family="BPG Excelsior Exp")+
  scale_fill_manual(values = c("#195ea2", "#dc082b", "#003a75", "#e7b031",
                               "#e1073b", "#fc5000", "#f0ce0d", "#8ac650",
                               "#327f37", "#0f5cbb", "#019541", "#d92c1c",
                               "#a30032"))+
  scale_x_continuous(labels = scales::percent_format(), limits=c(0,1))+
  labs(title="პარტიული რეიტინგების შეწონილი საშუალო მნიშვნელობა, 2020 წლის 16 ივლისისთვის",
       x="Predicted proportion")+
  theme_plot+
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_blank()
  ) -> gg

gg

tooltip_css <- "background-color:white;font-family:BPG Excelsior Exp;padding:10px;border-radius:10px 20px 10px 20px;"


gg_a <- girafe(ggobj=gg, width_svg = 9, height_svg = 6)
gg_a <- girafe_options(gg_a, opts_tooltip(css = tooltip_css, opacity = .75))
gg_a
htmlwidgets::saveWidget(gg_a, "weighted_average_en.html")

### Georgian-language chart for the dynamics

src %>% 
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
    Percent = Percent/sum(Percent)
  )%>%
  ungroup()%>%
  # View()
  filter(PARTYCODE %in% c("GD", "UNM", "EUROGEO", "APG"))%>%
  mutate(PARTYCODE = factor(PARTYCODE, levels=c("GD", "UNM", "EUROGEO", "APG"),
                            labels=c("ქართული ოცნება", "ენმ", "ევროპული საქართველო", "პატრიოტთა ალიანსი")))%>%
  ggplot(aes(field_last_day, Percent, group=PARTYCODE, color=PARTYCODE))+ #, shape=PARTYCODE))+
  stat_smooth(aes(fill=PARTYCODE), alpha=0.1)+
  geom_point_interactive(aes(tooltip=paste0(Source_KA, ", ", Date_KA,  "\n", PARTYCODE, ": ", round(Percent*100, 0), "%"))
                        )+
  # geom_point()+
  scale_fill_manual(values=c( "#195ea2", "#dc082b", "#003a75", "#e7b031"))+
  scale_color_manual(values=c( "#195ea2", "#dc082b", "#003a75", "#e7b031"))+
  # scale_x_date(limits=as.Date(c("2016-07-01", "2020-09-30")))+
  scale_y_continuous(labels = scales::percent_format(), limits=c(0,1))+
  labs(title="პარტიული რეიტინგები, 2017-2020",
       x="Projected proportion",
       y="Party names")+
  theme_plot+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    # axis.text.x = element_blank(),
    plot.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
  ) -> gg
gg

tooltip_css <- "background-color:white;font-family:BPG Excelsior Exp;padding:10px;border-radius:10px 20px 10px 20px;"


gg_a <- girafe(ggobj=gg, width_svg = 9, height_svg = 6)
gg_a <- girafe_options(gg_a, opts_tooltip(css = tooltip_css, opacity = .75))
gg_a
htmlwidgets::saveWidget(gg_a, "dynamics_ka.html")


### English-language chart for the dynamics

src %>% 
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
    Percent = Percent/sum(Percent)
  )%>%
  ungroup()%>%
  # View()
  filter(PARTYCODE %in% c("GD", "UNM", "EUROGEO", "APG"))%>%
  mutate(PARTYCODE = factor(PARTYCODE, levels=c("GD", "UNM", "EUROGEO", "APG"),
                            labels=c("Georgian Dream", "UNM", "European Georgia", "Alliance of Patriots")))%>%
  ggplot(aes(field_last_day, Percent, group=PARTYCODE, color=PARTYCODE))+ #, shape=PARTYCODE))+
  stat_smooth(aes(fill=PARTYCODE), alpha=0.1)+
  geom_point_interactive(aes(tooltip=paste0(Source_EN, ", ", Date_EN,  "\n", PARTYCODE, ": ", round(Percent*100, 0), "%"))
  )+
  # geom_point()+
  scale_fill_manual(values=c( "#195ea2", "#dc082b", "#003a75", "#e7b031"))+
  scale_color_manual(values=c( "#195ea2", "#dc082b", "#003a75", "#e7b031"))+
  # scale_x_date(limits=as.Date(c("2016-07-01", "2020-09-30")))+
  scale_y_continuous(labels = scales::percent_format(), limits=c(0,1))+
  labs(title="პარტიული რეიტინგები, 2017-2020",
       x="Projected proportion",
       y="Party names")+
  theme_plot+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    # axis.text.x = element_blank(),
    plot.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
  ) -> gg

gg

tooltip_css <- "background-color:white;font-family:BPG Excelsior Exp;padding:10px;border-radius:10px 20px 10px 20px;"

gg_a <- girafe(ggobj=gg, width_svg = 9, height_svg = 6)
gg_a <- girafe_options(gg_a, opts_tooltip(css = tooltip_css, opacity = .75))
gg_a

htmlwidgets::saveWidget(gg_a, "dynamics_en.html")

