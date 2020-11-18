
rm(list = ls())
fig.dir <- "/Users/martakaras/Dropbox/My_Website/static/img/2019-03-22-project-adeptdata"

# install.packages("adeptdata")
library(adeptdata)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)


## -----------------------------------------------------------------------------

acc_walking_IU %>%
  filter(time_s < 6, subj_id == acc_walking_IU$subj_id[1]) %>%
  mutate(loc_id = factor(
    loc_id, 
    levels = c("left_wrist", "left_hip", "left_ankle", "right_ankle"),
    labels = c("Left wrist", "Left hip", "Left ankle", "Right ankle"))) %>%
  melt(id.vars = c("subj_id", "loc_id", "time_s")) %>%
  ggplot(aes(x = time_s, y = value, color = variable)) + 
  geom_line() + 
  facet_wrap(~ loc_id, ncol = 2) + 
  theme_bw(base_size = 9) + 
  labs(x = "Exercise time [s]", 
       y = "Amplitude [g]", 
       color = "Sensor\naxis",
       title = "Raw accelerometry data of walking (100 Hz)") 

plt.path <- file.path(fig.dir, "plot_acc_walking_IU.jpeg")
ggsave(plt.path, width = 18, height = 12, units = "cm")



## -----------------------------------------------------------------------------

t1 <- ymd_hms("2018-10-25 18:07:00", tz = "UTC") 
t2 <- ymd_hms("2018-10-25 18:20:30", tz = "UTC") 
t3 <- ymd_hms("2018-10-25 18:22:00", tz = "UTC") 

acc_running %>%
  filter((date_time >= t1 & date_time < t1 + as.period(4, "seconds")) | 
           (date_time >= t2 & date_time < t2 + as.period(4, "seconds")) | 
           (date_time >= t3 & date_time < t3 + as.period(4, "seconds")) ) %>%
  mutate(loc_id = factor(
    loc_id, 
    levels = c("left_hip", "left_ankle"),
    labels = c("Left hip", "Left ankle"))) %>%
  melt(id.vars = c("date_time", "loc_id")) %>%
  mutate(date_time_floor = paste0(
    "Minute start: ", floor_date(date_time, unit = "minutes"))) %>%
  ggplot(aes(x = date_time, y = value, color = variable)) + 
  geom_line(size = 0.5) + 
  facet_grid(loc_id ~ date_time_floor, scales = "free_x") + 
  theme_bw(base_size = 9) + 
  labs(x = "Time [s]", 
       y = "Acceleration [g]", 
       color = "Sensor\naxis",
       title = "Raw accelerometry data (100 Hz)") 

plt.path <- file.path(fig.dir, "acc_running.jpeg")
ggsave(plt.path, width = 20, height = 12, units = "cm")



## -----------------------------------------------------------------------------

data.frame(
  x = rep(seq(0, 1, length.out = 200), 2),
  y = c(stride_template$left_ankle[[2]][1, ],
        stride_template$left_ankle[[2]][2, ]),
  group = c(rep(1, 200), rep(2, 200))) %>%
  ggplot(aes(x = x, y = y, group = group)) + 
  geom_line() +
  facet_grid(group ~ .) + 
  theme_bw(base_size = 9) + 
  labs(x = "Template phase", 
       y = "Vector magnitude [g]", 
       title = "Walking stride templates (left ankle)") 
  

plt.path <- file.path(fig.dir, "stride_template.jpeg")
ggsave(plt.path, width = 10, height = 12, units = "cm")





