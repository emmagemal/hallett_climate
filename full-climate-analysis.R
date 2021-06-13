# Cape Hallett climate data visualization and analysis
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh

### Library ----
library(tidyverse)
library(lme4)
library(MuMIn)  
library(car)
library(lmerTest)
library(lmtest)
library(viridis)


# loading the data
climate2011 <- read.csv("Data/climate_long_2011-2018.csv")
climate2004 <- read.csv("Data/climate_long_2004-2010.csv")
climate1965 <- read.csv("Data/climate_long_1965-1973.csv")

### Data Manipulation 2011-2018 ----
head(climate2011)
str(climate2011)
summary(climate2011)

climate2011 <- climate2011 %>% 
                  na.omit() %>%   # removing NA's
                  filter(rH > 0) %>%  # removing incorrect rH values 
                  rename(year = Year,
                         temp = Temp) %>%  # renaming columns (lowercase)
                  mutate(VPD = as.numeric(VPD)) # changing VPD to numeric 
str(climate2011)

# adding season column
climate2011 <- climate2011 %>% 
                  mutate(season = case_when(year == "2011" & month == "11" ~ "2011/12",
                                            year == "2011" & month == "12" ~ "2011/12",
                                            year == "2012" & month == "1" ~ "2011/12",
                                            year == "2012" & month == "11" ~ "2012/13",
                                            year == "2012" & month == "12" ~ "2012/13",
                                            year == "2013" & month == "1" ~ "2012/13",
                                            year == "2013" & month == "11" ~ "2013/14",
                                            year == "2013" & month == "12" ~ "2013/14",
                                            year == "2014" & month == "1" ~ "2013/14",
                                            year == "2014" & month == "11" ~ "2014/15",
                                            year == "2014" & month == "12" ~ "2014/15",
                                            year == "2015" & month == "1" ~ "2014/15",
                                            year == "2015" & month == "11" ~ "2015/16",
                                            year == "2015" & month == "12" ~ "2015/16",
                                            year == "2016" & month == "1" ~ "2015/16",
                                            year == "2016" & month == "11" ~ "2016/17",
                                            year == "2016" & month == "12" ~ "2016/17",
                                            year == "2017" & month == "1" ~ "2016/17",
                                            year == "2017" & month == "11" ~ "2017/18",
                                            year == "2017" & month == "12" ~ "2017/18",
                                            year == "2018" & month == "1" ~ "2017/18")) %>% 
                  mutate(season = as.factor(season))

# removing 2012/13 season
climate2011 <- climate2011[!(climate2011$season == "2012/13"), ]


# fixing the time column to allow for easy change to POSIX class
climate2011 <- climate2011 %>% 
                  mutate(real_time = case_when(time == 0 ~ "00:00",
                                               time == 100 ~ "01:00",
                                               time == 200 ~ "02:00",
                                               time == 300 ~ "03:00",
                                               time == 400 ~ "04:00",
                                               time == 500 ~ "05:00",
                                               time == 600 ~ "06:00",
                                               time == 700 ~ "07:00",
                                               time == 800 ~ "08:00",
                                               time == 900 ~ "09:00",
                                               time == 1000 ~ "10:00",
                                               time == 1100 ~ "11:00",
                                               time == 1200 ~ "12:00",
                                               time == 1300 ~ "13:00",
                                               time == 1400 ~ "14:00",
                                               time == 1500 ~ "15:00",
                                               time == 1600 ~ "16:00",
                                               time == 1700 ~ "17:00",
                                               time == 1800 ~ "18:00",
                                               time == 1900 ~ "19:00",
                                               time == 2000 ~ "20:00",
                                               time == 2100 ~ "21:00",
                                               time == 2200 ~ "22:00",
                                               time == 2300 ~ "23:00"))

# creating date and date + time columns for time series plotting
climate2011 <- climate2011 %>% 
                  unite("date", c("year", "month", "day"), remove = FALSE, sep = "-") %>% 
                  unite("date_time", c("date", "real_time"), remove = FALSE, sep = " ") %>% 
                  mutate(date = as.Date(date)) %>% 
                  mutate(date_time = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M"))
str(climate2011)

### Data Manipulation 2004-2010 ----
head(climate2004)
str(climate2004)
summary(climate2004)

climate2004 <- climate2004 %>% 
                  na.omit() %>%  # removing NA's
                  filter(rH > 0) %>%  # removing any incorrect rH values 
                  mutate(date_time = as.POSIXct(date_time,   # changing to POSIX class
                                                format = "%d/%m/%Y %H:%M")) 
str(climate2004)

# adding season column
climate2004$year <- format(climate2004$date_time, format = "%Y")
climate2004$month <- format(climate2004$date_time, format = "%m")
climate2004$day <- format(climate2004$date_time, format = "%d")

climate2004 <- climate2004 %>% 
                  mutate(season = case_when(year == "2004" & month == "11" ~ "2004/5",
                                            year == "2004" & month == "12" ~ "2004/5",
                                            year == "2005" & month == "1" ~ "2005/6",
                                            year == "2005" & month == "11" ~ "2005/6",
                                            year == "2005" & month == "12" ~ "2005/6",
                                            year == "2006" & month == "1" ~ "2006/7",
                                            year == "2006" & month == "11" ~ "2006/7",
                                            year == "2006" & month == "12" ~ "2006/7",
                                            year == "2007" & month == "1" ~ "2007/8",
                                            year == "2007" & month == "11" ~ "2007/8",
                                            year == "2007" & month == "12" ~ "2007/8",
                                            year == "2008" & month == "1" ~ "2008/9",
                                            year == "2008" & month == "11" ~ "2008/9",
                                            year == "2008" & month == "12" ~ "2008/9",
                                            year == "2009" & month == "1" ~ "2009/10",
                                            year == "2009" & month == "11" ~ "2009/10",
                                            year == "2009" & month == "12" ~ "2009/10",
                                            year == "2010" & month == "1" ~ "2010/11",
                                            year == "2010" & month == "11" ~ "2010/11",
                                            year == "2010" & month == "12" ~ "2010/11")) %>% 
                  mutate(season = as.factor(season))

### Data Manipulation 1965-1973 ----
head(climate1965)
str(climate1965)
summary(climate1965)

climate1965 <- climate1965 %>% 
                  na.omit() %>%  # removing NA's
                  filter(rH > 0) %>%  # removing any incorrect rH values 
                  mutate(date_time = as.character(date_time)) %>% 
                  mutate(date_time = as.POSIXct(date_time,   # changing to POSIX class
                                                format = "%Y%m%d%H%M")) 

str(climate1965)

# adding season column
climate1965$year <- format(climate1965$date_time, format = "%Y")
climate1965$month <- format(climate1965$date_time, format = "%m")
climate1965$day <- format(climate1965$date_time, format = "%d")

climate1965 <- climate1965 %>% 
                  mutate(season = case_when(year == "1965" & month == "01" ~ "1964/5",
                                            year == "1966" & month == "10" ~ "1966/7",
                                            year == "1966" & month == "11" ~ "1966/7",
                                            year == "1966" & month == "12" ~ "1966/7",
                                            year == "1967" & month == "01" ~ "1966/7",
                                            year == "1967" & month == "02" ~ "1966/7",
                                            year == "1967" & month == "10" ~ "1967/8",
                                            year == "1967" & month == "11" ~ "1967/8",
                                            year == "1967" & month == "12" ~ "1967/8",
                                            year == "1968" & month == "01" ~ "1967/8",
                                            year == "1968" & month == "02" ~ "1967/8",
                                            year == "1968" & month == "10" ~ "1968/9",
                                            year == "1968" & month == "11" ~ "1968/9",
                                            year == "1968" & month == "12" ~ "1968/9",
                                            year == "1969" & month == "01" ~ "1968/9",
                                            year == "1969" & month == "02" ~ "1968/9",
                                            year == "1969" & month == "10" ~ "1969/70",
                                            year == "1969" & month == "11" ~ "1969/70",
                                            year == "1969" & month == "12" ~ "1969/70",
                                            year == "1970" & month == "01" ~ "1969/70",
                                            year == "1970" & month == "02" ~ "1969/70",
                                            year == "1970" & month == "10" ~ "1970/1",
                                            year == "1970" & month == "11" ~ "1970/1",
                                            year == "1970" & month == "12" ~ "1970/1",
                                            year == "1971" & month == "01" ~ "1970/1",
                                            year == "1971" & month == "02" ~ "1970/1",
                                            year == "1971" & month == "10" ~ "1971/2",
                                            year == "1971" & month == "11" ~ "1971/2",
                                            year == "1971" & month == "12" ~ "1971/2",
                                            year == "1972" & month == "01" ~ "1971/2",
                                            year == "1972" & month == "02" ~ "1971/2",
                                            year == "1972" & month == "11" ~ "1972/3",
                                            year == "1972" & month == "12" ~ "1972/3",
                                            year == "1973" & month == "01" ~ "1972/3",
                                            year == "1973" & month == "02" ~ "1972/3")) %>% 
                  mutate(season = as.factor(season))

### Combining Data Frames ----
str(climate2011)
str(climate2004)
str(climate1965)

# making year and month integers for 'climate2004' and 'climate1965'
climate2004 <- climate2004 %>% 
                  mutate(year = as.integer(year)) %>% 
                  mutate(month = as.integer(month)) %>% 
                  mutate(day = as.integer(day))

climate1965 <- climate1965 %>% 
                  mutate(year = as.integer(year)) %>% 
                  mutate(month = as.integer(month)) %>% 
                  mutate(day = as.integer(day))

combo <- full_join(climate2004, climate2011)
combo2 <- full_join(climate1965, combo)

# removing unnecessary columns
combo2 <- combo2 %>% 
            dplyr::select(date_time, temp, rH, year, month, day, season)

summary(combo2)

### Creating Summary Data ----
sum_year <- combo2 %>% 
              group_by(year) %>% 
              summarise(avg_temp = mean(temp),
                        min_temp = min(temp),
                        max_temp = max(temp),
                        avg_rh = mean(rH),
                        min_rh = min(rH),
                        max_rh = max(rH))

sum_season <- combo2 %>% 
                group_by(season) %>% 
                summarise(avg_temp = mean(temp),
                          min_temp = min(temp),
                          max_temp = max(temp),
                          avg_rh = mean(rH),
                          min_rh = min(rH),
                          max_rh = max(rH))

# splitting the dataframes into temperature and rH
sum_year_temp <- sum_year %>% 
                    dplyr::select(year, avg_temp, min_temp, max_temp)
sum_season_temp <- sum_season %>% 
                      dplyr::select(season, avg_temp, min_temp, max_temp)

sum_year_rh <- sum_year %>% 
                  dplyr::select(year, avg_rh, min_rh, max_rh)
sum_season_rh <- sum_season %>% 
                    dplyr::select(season, avg_rh, min_rh, max_rh)

# creating long versions for multi-variable plotting
sum_year_temp_long <- sum_year_temp %>% 
                          pivot_longer(cols = c(2:4),
                                       names_to = "type",
                                       values_to = "temp")
sum_season_temp_long <- sum_season_temp %>% 
                          pivot_longer(cols = c(2:4),
                                       names_to = "type",
                                       values_to = "temp")

sum_year_rh_long <- sum_year_rh %>% 
                      pivot_longer(cols = c(2:4),
                                   names_to = "type",
                                   values_to = "rH")
sum_season_rh_long <- sum_season_rh %>% 
                        pivot_longer(cols = c(2:4),
                                     names_to = "type",
                                     values_to = "rH")


# adding a grouping variable for plotting (to produce gap between old and new data)
sum_year_temp_long$grouping <- ifelse(sum_year_temp_long$year < 2004, 1, 2)
sum_year_rh_long$grouping <- ifelse(sum_year_rh_long$year < 2004, 1, 2)

# adding NA values to produce a gap between old and new data, but for season 
# creating a dataframe of NA's to insert into the original 
season <- c("1990/1", "1991/2", "1992/3", "1993/4", "1994/5", "1995/6", "1996/7")
avg_temp <- c(NA, NA, NA, NA, NA, NA, NA)
max_temp <- c(NA, NA, NA, NA, NA, NA, NA)
min_temp <- c(NA, NA, NA, NA, NA, NA, NA)
avg_rh <- c(NA, NA, NA, NA, NA, NA, NA)
max_rh <- c(NA, NA, NA, NA, NA, NA, NA)
min_rh <- c(NA, NA, NA, NA, NA, NA, NA)

season_na <- tibble(season, avg_temp, min_temp, max_temp, avg_rh, max_rh, min_rh)
str(season_na)

# inserting the NA dataframe
combo_na <- full_join(combo, season_na)
combo_na <- full_join(combo_na, climate1965)

#### could potentially delete everything below and all other summarized bits 
sum_season_na <- combo_na %>% 
                    group_by(season) %>% 
                    summarise(avg_temp = mean(temp),
                              min_temp = min(temp),
                              max_temp = max(temp),
                              avg_rh = mean(rH),
                              min_rh = min(rH),
                              max_rh = max(rH))

sum_season_temp_na <- sum_season_na %>% 
                        dplyr::select(season, avg_temp, min_temp, max_temp)
sum_season_rh_na <- sum_season_na %>% 
                      dplyr::select(season, avg_rh, min_rh, max_rh)

sum_season_temp_long <- sum_season_temp %>% 
                          pivot_longer(cols = c(2:4),
                                       names_to = "type",
                                       values_to = "temp")
sum_season_rh_long <- sum_season_rh %>% 
                        pivot_longer(cols = c(2:4),
                                     names_to = "type",
                                     values_to = "rH")

### Active Days Calculations ----
active_combo <- combo2 %>% 
                    filter(temp > -1) %>% 
                    filter(rH > 85)

sum_active <- active_combo %>% 
                group_by(year, month) %>% 
                summarise(avg_temp = mean(temp),
                          sd_temp = sd(temp),
                          avg_rh = mean(rH),
                          sd_rh = sd(rH),
                          n_active_days = n_distinct(day)) %>% 
                mutate(month = as.factor(month))



# adding seasons to the active days summary 
sum_active <- sum_active %>% 
                mutate(season = case_when(year == "1965" & month == "1" ~ "1964/5",
                                          year == "1967" & month == "12" ~ "1967/8",
                                          year == "1969" & month == "12" ~ "1969/70",
                                          year == "1970" & month == "1" ~ "1969/70",
                                          year == "1971" & month == "1" ~ "1970/1",
                                          year == "1971" & month == "2" ~ "1970/1",
                                          year == "1972" & month == "2" ~ "1971/2",
                                          year == "1972" & month == "12" ~ "1972/3",
                                          year == "1973" & month == "1" ~ "1972/3",
                                          year == "2004" & month == "12" ~ "2004/5",
                                          year == "2005" & month == "12" ~ "2005/6",
                                          year == "2007" & month == "12" ~ "2007/8",
                                          year == "2008" & month == "12" ~ "2007/8",
                                          year == "2010" & month == "12" ~ "2010/11",
                                          year == "2011" & month == "12" ~ "2011/12",
                                          year == "2012" & month == "1" ~ "2011/12",
                                          year == "2012" & month == "12" ~ "2012/13",
                                          year == "2013" & month == "1" ~ "2012/13",
                                          year == "2013" & month == "12" ~ "2013/14",
                                          year == "2014" & month == "1" ~ "2013/14",
                                          year == "2015" & month == "12" ~ "2015/16",
                                          year == "2016" & month == "1" ~ "2015/16",
                                          year == "2016" & month == "12" ~ "2016/17",
                                          year == "2018" & month == "1" ~ "2017/18")) %>% 
                mutate(season = as.factor(season))

summary(sum_active)

### Data Visualization 
# creating a theme for consistency
climate_theme <- theme_bw() +
                    theme(axis.text.x = element_text(angle = 60, hjust = 1),
                          axis.title.x = 
                            element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                          axis.title.y = 
                            element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
                    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

### Data Visualization - Temperature ---- 
# temp over time (non-faceted, boxplot)  
(temp_boxplot_gap <- ggplot(combo_na, aes(x = season, y = temp)) +
                        geom_boxplot(fill = "#ADD8E6") +
                        ylab(label = "Temperature (˚C)") +
                        xlab(label = "Season") +
                        climate_theme +
                        theme(legend.position = "none",
                              panel.grid.minor = element_blank(),
                              panel.grid.major.x = element_blank(),
                              axis.text = element_text(size = c(10, 10, 10, 10, 10, 10, 10, 10,
                                                       0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 10, 
                                                       10, 10, 10, 10, 10, 10, 10, 10)),
                              axis.ticks.x = element_line(size = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                                                                   0.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 
                                                                   0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                                                                   0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                                                                   0.5))))

ggsave("Figures/boxplot_season_temp.png", plot = temp_boxplot_gap, 
       width = 7.5, height = 6, units = "in")


(temp_boxplot <- ggplot(combo2, aes(x = season, y = temp)) +  # no gap between old and new data
                    geom_boxplot(fill = "#ADD8E6") +
                    ylab(label = "Temperature (˚C)") +
                    xlab(label = "Season") +
                    climate_theme +
                    theme(legend.position = "none",
                          panel.grid.minor = element_blank(),
                          panel.grid.major.x = element_blank()))

ggsave("Figures/boxplot_season_nogap.png", plot = temp_boxplot, 
       width = 7.5, height = 6, units = "in")


# temp over time per season - FOR APPENDIX 
(temp_facet <- ggplot(combo2, aes(x = date_time, y = temp, fill = season)) +   
                  geom_line(aes(color = season)) +
                  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") +
                  facet_wrap(vars(season), ncol = 3, scales = "free_x") +
                  ylab(label = "Temperature (˚C)") +
                  xlab(label = "Time") +
                  climate_theme + 
                  theme(legend.position = "none") +
                  scale_fill_viridis(discrete = TRUE) +
                  scale_color_viridis(discrete = TRUE))

ggsave("Figures/temp_appendix.png", plot = temp_facet, 
       width = 9, height = 10, units = "in")


# average, minimum and maximum temperature over time (by year) 
minor <- seq(1965, 2015, by = 5)   # making minor gridlines for the plot 

(temp_year_avg <- ggplot(sum_year_temp_long, aes(x = year, y = temp, 
                                                 color = type, shape = type)) +
                    geom_vline(xintercept = minor, color = "grey92") +                 
                    geom_point(size = 2.5) +  
                    geom_line(aes(group = interaction(type, grouping))) +  # how I added the gap
                    geom_hline(yintercept = 0, linetype = "dashed") +
                    ylab(label = "Temperature (˚C)") +
                    xlab(label = "Year") +
                    climate_theme + 
                    theme(panel.grid.minor = element_blank(),
                          panel.grid.major.x = element_blank(),
                          legend.title = element_blank()) +
                    scale_color_manual(values = c("#004452", "#B5BA4F", "#FF6D33"),
                                       labels = c("Mean", "Maximum", "Minimum")) + 
                    scale_shape_discrete(labels = c("Mean", "Maximum", "Minimum")) +
                    scale_x_continuous(n.breaks = 8))

ggsave("Figures/temp_summary_year.png", plot = temp_year_avg, 
       width = 7.5, height = 6, units = "in")

# adding a mixed effects model to the plot
temp_model <- lmer(temp ~ year + (1|month) + (1|season), data = combo2, REML = F)

pred.mm <- ggpredict(temp_model, terms = c("year"))  

(temp_year_model <- ggplot(pred.mm) + 
                      geom_vline(xintercept = minor, color = "grey92") +                   
                      geom_line(aes(x = x, y = predicted), color = "#004452") +   # slope
                      geom_ribbon(aes(x = x, ymin = conf.low, 
                                      ymax = conf.high), 
                                  fill = "lightgrey", alpha = 0.5) +  # error band
                      geom_point(size = 2.5, data = sum_year_temp_long, aes(x = year, y = temp, 
                                                                            color = type, 
                                                                            shape = type)) +
                      geom_line(data = sum_year_temp_long, aes(x = year, y = temp, 
                                                               color = type, 
                                                               group = interaction(type, 
                                                                                   grouping))) +
                      geom_hline(yintercept = 0, linetype = "dashed") +
                      ylab(label = "Temperature (˚C)") +
                      xlab(label = "Year") +
                      climate_theme +
                      theme(panel.grid.minor = element_blank(),
                            panel.grid.major.x = element_blank(),
                            legend.title = element_blank()) +
                      scale_color_manual(values = c("#004452", "#B5BA4F", "#FF6D33"),
                                         labels = c("Mean", "Maximum", "Minimum")) + 
                      scale_shape_discrete(labels = c("Mean", "Maximum", "Minimum")) +
                      scale_x_continuous(n.breaks = 8))

ggsave("Figures/temp_summary_year_model.png", plot = temp_year_model, 
       width = 7.5, height = 6, units = "in")


# average, minimum and maximum temperature over time (by season) 
(temp_season_sum_gap <- ggplot(sum_season_na, aes(x = season, y = temp, 
                                                  color = type, shape = type)) +
                          geom_point(size = 2.5) +  
                          geom_line(aes(group = type)) +
                          geom_hline(yintercept = 0, linetype = "dashed") +
                          ylab(label = "Temperature (˚C)") +
                          xlab(label = "Season") +
                          climate_theme +
                          theme(panel.grid.major.x = element_blank(),
                                legend.title = element_blank(),
                                axis.text = element_text(size = c(10, 10, 10, 10, 10, 10, 10, 10,
                                                                  0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 
                                                                  10, 10, 10, 10, 10, 10, 10, 10, 
                                                                  10, 10)),
                                axis.ticks.x = element_line(size = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                                                                     0.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 
                                                                     0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                                                                     0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                                                                     0.5))) +
                          scale_color_manual(values = c("#004452", "#B5BA4F", "#FF6D33"),
                                             labels = c("Mean", "Maximum", "Minimum")) + 
                          scale_shape_discrete(labels = c("Mean", "Maximum", "Minimum")))

ggsave("Figures/temp_summary_season.png", plot = temp_season_sum_gap, 
       width = 7.5, height = 6, units = "in")


(temp_season_sum <- ggplot(sum_season_temp_long, aes(x = season, y = temp, 
                                                     color = type, shape = type)) +
                      geom_point(size = 2.5) +  
                      geom_line(aes(group = type)) +
                      geom_hline(yintercept = 0, linetype = "dashed") +
                      ylab(label = "Temperature (˚C)") +
                      xlab(label = "Season") +
                      climate_theme +
                      theme(panel.grid.major.x = element_blank(),
                            legend.title = element_blank()) +
                      scale_color_manual(values = c("#004452", "#B5BA4F", "#FF6D33"),
                                         labels = c("Mean", "Maximum", "Minimum")) + 
                      scale_shape_discrete(labels = c("Mean", "Maximum", "Minimum")))

ggsave("Figures/temp_summary_season_nogap.png", plot = temp_season_sum, 
       width = 7.5, height = 6, units = "in")


### Data Visualization - Relative Humidity ----
# rH over time (non-faceted, boxplot) - FOR PUBLICATION 
(rh_boxplot_gap <- ggplot(combo_na, aes(x = season, y = rH)) +
                      geom_boxplot(fill = "#598492") +
                      ylab(label = "Relative Humidity (%)") +
                      xlab(label = "Season") +
                      climate_theme +
                      theme(legend.position = "none",
                            panel.grid.minor = element_blank(),
                            panel.grid.major.x = element_blank(),
                            axis.text = element_text(size = c(10, 10, 10, 10, 10, 10, 10, 10,
                                                              0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 10, 
                                                              10, 10, 10, 10, 10, 10, 10, 10)),
                            axis.ticks.x = element_line(size = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                                                                 0.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 
                                                                 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                                                                 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                                                                 0.5))))

ggsave("Figures/boxplot_season_rH.png", plot = rh_boxplot_gap, 
       width = 7.5, height = 6, units = "in")

# rH over time per season - FOR PUBLICATION APPENDIX ??
(rh_facet <- ggplot(combo2, aes(x = date_time, y = rH, fill = season)) +   
                geom_line(aes(color = season)) +
                scale_x_datetime(date_labels = "%b", date_breaks = "1 month") +
                facet_wrap(vars(season), ncol = 3, scales = "free_x") +
                ylab(label = "Relative Humidity (%)") +
                xlab(label = "Time") +
                climate_theme +
                theme(legend.position = "none") +
                scale_color_viridis(discrete = TRUE))

ggsave("Figures/rH_appendix.png", plot = rh_facet, 
       width = 8, height = 10, units = "in")


### Data Analysis for Temperature ----
## Simple linear models 
temp_m <- lm(temp ~ year, data = combo2)
summary(temp_m)  # p < 2e-16, is significant
                 # adjusted R2 = 0.00193
anova(temp_m)

# checking model assumptions 
plot(temp_m)  # not very normally distributed 
hist(resid(temp_m))  # skewed, violates model assumptions 
bptest(temp_m)  # no heteroskedasticity though

# creating a null model to compare it to
temp_null <- lm(temp ~ 1, data = combo2)
AIC(temp_m, temp_null)  # linear model is slightly better 

## Mixed effects models 
# if std error > estimate = year doesn't explain much of the variation
# can calculate % of leftover variation that's explained by the random effects 
# estimate for year = after controlling for the random effects 

temp_mixed <- lmer(temp ~ year + (1|year), data = combo2, REML = F)
temp_month_mixed <- lmer(temp ~ year + (1|month), data = combo2, REML = F)
temp_day_mixed <- lmer(temp ~ year + (1|day), data = combo2, REML = F)
temp_season_mixed <- lmer(temp ~ year + (1|season), data = combo2, REML = F)
temp_month_season <- lmer(temp ~ year + (1|month) + (1|season), data = combo2, REML = F)
temp_month_date <- lmer(temp ~ year + (1|month) + (1|date_time), data = combo2, REML = F)

AIC(temp_mixed, temp_month_mixed, temp_day_mixed, temp_season_mixed)
  # month as random effect is best

anova(temp_month_mixed, temp_month_season)  # month_season is best
anova(temp_month_mixed, temp_month_date)    # month_date is better
AIC(temp_month_season, temp_month_date)     # month_season is best  

# creating null models to compare it with
temp_m_null <- lmer(temp ~ 1 + (1|month) + (1|season), data = combo2, REML = F)

# anova states which model is better at capturing the data
anova(temp_m_null, temp_month_season) 
# model with year as a fixed effect is slightly better than the null models   

anova(temp_month_season, temp_m)  # temp_month_season is best 

summary(temp_month_season)   # year: -0.0260 (std error: 0.0106), small effect size 
confint(temp_month_season)   # year: -0.0478 to -0.00418 
# 95% confident the correlation between year and temperature is between those 2 values 

# calculating pseudo-R2 for mixed effects model 
r.squaredGLMM(temp_month_season)  # marginal R^2 associated with year (fixed effect) = 0.00287
                                  # conditional R^2 associated with fixed + random = 0.763
# r2_nakagawa(temp_month_season) = 'performance' - same output

# comparing to the null model (only random effects)
summary(temp_m_null)   # null is almost significant (p = 0.0538)
r.squaredGLMM(temp_m_null)


### Data Analysis for Relative Humidity ----
## Simple linear models 
rh_m <- lm(rH ~ year, data = combo2)
summary(rh_m)  # p < 2e-16, significant 
               # adjusted R2 = 0.0869

# checking model assumptions 
plot(rh_m)  
hist(resid(rh_m))  # slightly skewed potentially 
bptest(rh_m)  # no heteroskedasticity 

# creating a null model to compare it to
rh_null <- lm(rH ~ 1, data = combo2)
AIC(rh_m, rh_null)  # linear model is better 


## Mixed effects models 
rh_mixed <- lmer(rH ~ year + (1|year), data = combo2)
rh_month_mixed <- lmer(rH ~ year + (1|month), data = combo2)
rh_day_mixed <- lmer(rH ~ year + (1|day), data = combo2)
rh_season_mixed <- lmer(rH ~ year + (1|season), data = combo2)

AIC(rh_mixed, rh_month_mixed, rh_day_mixed, rh_season_mixed)
# month as random effect is best

rh_month_season <- lmer(rH ~ year + (1|month) + (1|season), data = combo2, REML = F)
rh_month_day <- lmer(rH ~ year + (1|month) + (1|day), data = combo2, REML = F)

AIC(rh_month_mixed, rh_month_season, rh_month_day)  # month_day is best 

# creating null models to compare it with
rh_m_null <- lmer(rH ~ 1 + (1|month) + (1|day), data = combo2, REML = F)
rh_m_null2 <- lmer(rH ~ 1 + (1|month), data = combo2, REML = F)

# anova states which model is better at capturing the data
anova(rh_m_null, rh_m_null2)
anova(rh_m_null, rh_month_day)  # month_day model is better

summary(rh_month_day)   # year: 0.322 (std error: 0.00482)
confint(rh_month_day)   # year: 0.1243 to 0.1977 
# 95% confident the correlation between year and rH is between those 2 values 

# calculating pseudo-R2 for mixed effects model 
r.squaredGLMM(rh_month_day)   # marginal R^2 associated with year (fixed effect) = 0.0824
                              # conditional R^2 associated with fixed + random = 0.140
# r2_nakagawa(temp_month_season) = 'performance' - same output

# comparing to the null model (only random effects)
summary(rh_m_null)   # null is significant (p = 7.21e-6)
r.squaredGLMM(rh_m_null)
