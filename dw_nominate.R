library(tidyverse)
library(forcats)
#library(readr)
library(rvest)
library(stringi)
#library(stringr)
library(magrittr)
#library(dplyr)
#library(tidyr)
#library(ggplot2)
library(scales)
library(ggrepel)
library(ggExtra)
library(ggthemes)
library(ggridges)

years <- data_frame(Year = seq(from = 1789, to = 2017, by = 2),
                    Congress = seq(from = 1, to = 115, by = 1))

states_df <- "http://k7moa.com/state_codes_icpsr.htm" %>% 
  read_html() %>% 
  html_node(xpath = "//pre") %>% 
  html_text() %>% 
  str_split(pattern = "\r\n") %>% 
  extract2(1) %>% 
  str_trim(side = "right") %>% 
  as_data_frame()

states_df <- lapply(states_df, MARGIN = 1, FUN = stri_split_regex, pattern = "[:space:]", n = 3L) %>% 
  unlist() %>% 
  matrix(., ncol = 3, byrow = TRUE) %>% 
  as_data_frame() %>% 
  setNames(c("State_ICPSR", "State_Abbr", "State_Name")) %>% 
  mutate(State_ICPSR = as.integer(State_ICPSR))

party_df <- "http://k7moa.com/party3.htm" %>% 
  read_html() %>% 
  html_node(xpath = "//pre") %>% 
  html_text() %>% 
  str_split(pattern = "\r\n") %>% 
  extract2(1) %>% 
  str_trim(side = "both") %>% 
  as_data_frame %>% 
  filter(value != "")

party_df <- lapply(party_df, MARGIN = 1, FUN = stri_split_regex, pattern = "[:space:]", n = 2L) %>% 
  unlist() %>% 
  matrix(., ncol = 2, byrow = TRUE) %>% 
  as_data_frame() %>% 
  setNames(c("Party_Code", "Party_Name")) %>% 
  mutate(Party_Code = as.integer(Party_Code)) %>% 
  mutate(Party_Name = str_trim(Party_Name))

# from \url{http://k7moa.com/Weekly_Constant_Space_DW-NOMINATE_Scores.htm}
# 1.  Congress Number
# 2.  ICPSR ID Number:  5 digit code assigned by the ICPSR as
# corrected by Howard Rosenthal and myself.
# 3.  State Code:  2 digit ICPSR State Code.
# 4.  Congressional District Number (0 if Senate or President)
# 5.  State Name
# 6.  Party Code:  100 = Dem., 200 = Repub. (See PARTY3.DAT)
# 7.  Occupancy:  ICPSR Occupancy Code -- 0=only occupant; 1=1st occupant; 2=2nd occupant; etc.
# 8.  Last Means of Attaining Office:  ICPSR Attain-Office Code -- 1=general election;
# 2=special election; 3=elected by state legislature; 5=appointed
# 9.  Name
# 10.  1st Dimension Coordinate
# 11.  2nd Dimension Coordinate
# 12.  Log-Likelihood
# 13.  Number of Votes
# 14.  Number of Classification Errors
# 15.  Geometric Mean Probability
dw_names <- c("Congress",
              "ICPSR_ID",
              "State_ICPSR",
              "Congressional_District",
              "State_Name",
              "Party_Code",
              "ICPSR_Occupancy",
              "ICPSR_Attain_Office",
              "Name",
              "Dim_1",
              "Dim_2",
              "LogLikelihood",
              "Number_Votes",
              "Number_Classification_Errors",
              "Geometric_Mean_Probability")

#dw_df <- read_table(file = "ftp://k7moa.com/junkord/HANDSL01114A20_STAND_ALONE_31.DAT", col_names = FALSE) %>% 
#  setNames(dw_names)

#View(dw_df[which(grepl(x = dw_df$Name.x, pattern = "PAUL")),])
ICPSR_df <- data_frame(ICPSR_ID = c(14252, 14627, 3658, 40502, 40105, 
                                    6939, 15448, 14921, 40300, 14101, 
                                    29147, 29939, 15054, 15431, 41304, 
                                    21142, 10713, 41102, 20325, 20529,
                                    21345, 20307, 20949, 15445, 14290,
                                    41104),
                       Name = c("McDonald", "Gingrich", "Goldwater", "Obama", "Clinton", 
                                "Nixon", "Pelosi", "McConnell", "Murkowski", "Biden", 
                                "Sanders", "Ryan", "Reid", "Lewis", "Cruz", 
                                "Huizinga", "Conyers", "Rubio", "King", "Green",
                                "Collins", "Nunes", "Chaffetz", "Smith", "Ron Paul",
                                "Rand Paul"))


dw_df <- read_fwf(file = "ftp://k7moa.com/junkord/HANDSL01114A20_STAND_ALONE_31.DAT",
                  col_positions = fwf_widths(c(4, 8, 4, 4, 9, 7, 3, 3, 14, 9, 9, 15, 7, 7, 9))) %>% 
  # col_positions = fwf_positions(c(2, 4), c(8, 12), c(15, 16), c(19, 20), c(23,29), c(33, 36),
  #                               c(39, 39), c(42, 42), c(46, 56), c(60, 65), c(69, 74), c(80, 89),
  #                               c(93, 96), c(101, 103), c(108, 112))) %>% 
  setNames(dw_names) %>% 
  mutate(Chamber = as.factor(ifelse(Congressional_District == 0, "Senate", "House"))) %>% 
  left_join(party_df, by = "Party_Code") %>% 
  left_join(states_df, by = "State_ICPSR") %>% 
  left_join(years, by = "Congress") %>% 
  left_join(ICPSR_df, by = "ICPSR_ID") %>% 
  mutate(Party_Name = as.factor(Party_Name),
         State_Abbr = as.factor(State_Abbr),
         State_Name = as.factor(State_Name.y),
         State_Name.x = NULL,
         State_Name.y = NULL)

# dw_df <- read.table(file = "ftp://k7moa.com/junkord/HANDSL01114A20_STAND_ALONE_31.DAT", sep = "", header = FALSE, stringsAsFactors = FALSE) %>% 
#   setNames(dw_names)

# my_plot <- dw_df %>% 
#   filter(Congress == 114) %>% 
#   ggplot() +
#   aes(x = Dim_1, y = Dim_2, colour = as.factor(Party_Name)) +
#   geom_point() +
#   labs(x = "First Dimension (Party)",
#        y = "Second Dimension ('Outsiders')") +
#   geom_text_repel(aes(label = Name), size = 1) +
#   facet_grid(Chamber ~ .)
# 
# my_plot
# 
# ggMarginal(my_plot)
# 
# dw_df %>% 
#   ggplot() +
#   aes(x = Dim_1, y = Dim_2, colour = Congress) +
#   geom_point(alpha = 0.5) +
#   labs(x = "First Dimension (Party)",
#        y = "Second Dimension ('Outsiders')") +
#   facet_grid(Chamber ~ .)
# 
# 
# dw_df %>% 
#   dplyr::filter(Congress >= 85) %>% 
#   group_by(Party_Name, Congress, Chamber, Year) %>% 
#   summarize(Mean_1 = mean(Dim_1), #Dim_2 = mean(Dim_2),
#             Min_1 = min(Dim_1), Max_1 = max(Dim_1),
#             Fifth = quantile(Dim_1, probs = 0.05), NinetyFifth = quantile(Dim_1, probs = 0.95)) %>% 
#   #gather(Measure, Value, Mean_1:NinetyFifth, -Congress, -Party_Name) %>% 
#   ggplot(aes(x = Year, colour = Party_Name)) +
#   geom_line(aes(y = Mean_1)) +
#   geom_line(aes(y = Min_1)) +
#   geom_line(aes(y = Max_1)) +
#   geom_line(aes(y = Fifth)) +
#   geom_line(aes(y = NinetyFifth)) +
#   facet_grid(. ~ Chamber)

png(filename = "figs/Congress34-115.png", width = 760, height = 480, type = "cairo-png")
dw_df %>% 
  filter(Congress >= 34, Party_Name %in% c("Republican", "Democrat", "Independent")) %>% 
  ggplot(aes(x = Year, y = Dim_1, colour = Party_Name)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = c(`Republican` = "red", `Democrat` = "blue", `Conservative` = "black",
                                `Ind. Democrat` = "orange", `Independent` = "grey"), name = "Party") +
  labs(y = "Liberal <-> Conservative",
       x = "Year",
       title = "America's Ideological History since the Civil War",
       subtitle = "Constant DW-Nominate scores",
       caption = "copyright 2017 Thomas Hopper, data from Lewis, McCarty, Poole, Rosenthal") +
  facet_grid(. ~ Chamber) +
  theme_tufte() +
  theme(text = element_text(family = "Helvetica"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(fill = "grey90", colour = "grey90"),
        strip.text = element_text(size = 10),
        plot.caption = element_text(size = 5))
dev.off()

# geom_line(data = (dw_df %>% filter(Congress >= 34, Party_Name %in% c("Republican", "Democrat")) %>% 
#             group_by(Party_Name, Congress, Chamber, Year) %>% 
#             summarize(Mean_Dim_1 = mean(Dim_1))), 
#           aes(y = Mean_Dim_1)) +

png(filename = "figs/Congress89-115.png", width = 760, height = 480, type = "cairo-png")
dw_df %>% 
  filter(Year >= 1965, Party_Name %in% c("Republican", "Democrat", "Independent")) %>% 
  ggplot(aes(x = Year, y = Dim_1, colour = Party_Name)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = c(`Republican` = "red", `Democrat` = "blue", `Conservative` = "black",
                                `Ind. Democrat` = "orange", `Independent` = "grey"), name = "Party") +
  labs(y = "Liberal <-> Conservative",
       x = "Year",
       title = "America's Ideological History since 1965",
       subtitle = "Constant DW-Nominate scores",
       caption = "copyright 2017 Thomas Hopper, data from Lewis, McCarty, Poole, Rosenthal") +
  facet_grid(. ~ Chamber) +
  theme_tufte() +
  theme(text = element_text(family = "Helvetica"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(fill = "grey90", colour = "grey90"),
        strip.text = element_text(size = 10),
        plot.caption = element_text(size = 5))
dev.off()

dw_label_df <- dw_df %>% 
  filter(Year >= 1965, !is.na(Name.y)) %>% 
  group_by(Name.y, Dim_1, Party_Name, Chamber) %>% 
  summarize(Year = first(Year))

png(filename = "figs/Congress89-115_names.png", width = 760, height = 480, type = "quartz", antialias = "subpixel", pointsize = 18, res = 92)
dw_df %>% 
  filter(Year >= 1965, Party_Name %in% c("Republican", "Democrat", "Independent")) %>% 
  ggplot(aes(x = Year, y = Dim_1, colour = Party_Name)) +
  geom_point(alpha = 0.05, size = 1) +
  geom_line(stat = "smooth", method = "loess", se = FALSE, alpha = 0.1, size = 1) +
  geom_point(data = dw_label_df) +
  geom_text_repel(data = dw_label_df, aes(label = Name.y)) +
  scale_color_manual(values = c(`Republican` = "red", `Democrat` = "blue", `Conservative` = "black",
                                `Ind. Democrat` = "orange", `Independent` = "grey50"), name = "Party") +
  labs(y = "Liberal <-> Conservative",
       x = "Year",
       title = "America's Ideological History since 1965",
       subtitle = "Constant DW-Nominate scores",
       caption = "copyright 2017 Thomas Hopper, DW-Nominate data from Lewis, McCarty, Poole, Rosenthal") +
  facet_grid(. ~ Chamber) +
  theme_tufte() +
  theme(text = element_text(family = "Helvetica"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(fill = "grey90", colour = "grey90"),
        strip.text = element_text(size = 10),
        plot.caption = element_text(size = 5))
dev.off()

png(filename = "figs/Congress89-115_v.png", width = 1280, height = 480, type = "quartz", antialias = "subpixel", pointsize = 18, res = 92)
dw_df %>% 
  filter(Year >= 1965, Party_Name %in% c("Republican", "Democrat", "Independent")) %>% 
  mutate(Year = fct_rev(as.factor(Year))) %>% 
  ggplot(aes(x = Dim_1, y = Year, fill = Party_Name)) +
  geom_density_ridges(alpha = 0.6, scale = 2, min_height = 0.003, size = 0.1) +
  theme_ridges() +
  #scale_y_reverse() +
  facet_grid(. ~ Chamber) +
  # scale_color_manual(values = c(`Republican` = "red", `Democrat` = "blue", `Conservative` = "black",
  #                               `Ind. Democrat` = "orange", `Independent` = "grey50"), name = "Party") +
  scale_fill_cyclical(breaks = c("Democrat", "Republican", "Independent", "Conservative", "Ind. Democrate"),
                      values = c(`Democrat` = "blue", `Republican` = "red", `Independent` = "grey50", `Conservative` = "black",
                                 `Ind. Democrat` = "orange"),
                      name = "Party", guide = "legend") +
  scale_y_discrete(breaks = c(1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015),
                   expand = c(0.01, 0)) +
  labs(x = "Liberal <-> Conservative",
       y = "Year",
       title = "Conservative Shift in Congress",
       subtitle = "Constant DW-Nominate scores",
       caption = "copyright 2017 Thomas Hopper, DW-Nominate data from Lewis, McCarty, Poole, Rosenthal, ggjoy by Claus O. Wilke") +
  theme(text = element_text(family = "Helvetica"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_rect(fill = "grey90", colour = "grey90"),
        strip.text = element_text(size = 10),
        plot.caption = element_text(size = 5))
dev.off()

# The conservative shift in Congress, and the hollowing-out of the middle, with #Rstats, #ggplot2 and #ggjoy.
