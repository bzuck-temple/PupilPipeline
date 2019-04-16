### for resting state data
bz4min <-read.csv("bz4gazer.csv", header =T)


pup_files1 <-arrange(bz4min, subject, trial, time) %>% 
  filter(time>= 0, time <= 15000)

ggplot(data=pup_files1, aes(x=time, y=pupil)) + geom_point() + ylim(400, 1200)

pup_extend<- pup_files1 %>% 
  group_by(subject, trial) %>% 
  mutate(extendpupil=extend_blinks(pupil, fillback=100, fillforward=100, hz=1000))

ggplot(data=pup_extend, aes(x=time, y=extendpupil)) + geom_point() + ylim(400, 1200)

pup_interp <- interpolate_pupil(
  pup_extend, 
  extendblinks = TRUE, 
  type = "linear")

ggplot(data=pup_interp, aes(x=time, y=interp)) + geom_point() + ylim(400, 1200)

rolling_mean_pupil_average <- as.data.frame(pup_interp) %>% #must be in a data.frame
  select(
    subject, trial, pupil, 
    time, interp, sample_message) %>%
  mutate(movingavgpup = moving_average_pupil(interp, n = 5))

ggplot(data=rolling_mean_pupil_average, aes(x=time, y=movingavgpup)) + geom_point() + ylim(400, 1200)

baseline_pupil <- baseline_correction_pupil(
  rolling_mean_pupil_average, 
  pupil_colnames = "movingavgpup", 
  baseline_window = c(0, 500))

ggplot(data=baseline_pupil, aes(x=time, y=baselinecorrectedp)) + geom_point() + ylim(-300, 300)

timebinsmm <- rolling_mean_pupil_average  %>% 
  mutate(pupilmm = (movingavgpup * 5)/5570.29)

ggplot(data=timebinsmm, aes(x=time, y=pupilmm)) + geom_point() + ylim(.5, 1.1)

pup_missing <- count_missing_pupil(pup_files1, missingthresh = .2)

puphist <- ggplot(pup_extend, aes(x = extendpupil)) + 
  geom_histogram(aes(y = ..count..), colour = "green", binwidth = 0.5)  + 
  geom_vline(xintercept = 2500, linetype="dotted") +
  geom_vline(xintercept = 5100, linetype="dotted") + 
  xlab("Pupil Size") + 
  ylab("Count") + 
  theme_bw()

print(puphist)

pup_outliers <- timebinsmm %>%
  # based on visual inspection
  dplyr::filter(interp >= 0, interp <= 1500) #change back to 2500 5100

ggplot(data=pup_outliers, aes(x=time, y=pupilmm)) + geom_point() + ylim(.5, 1.1)

mad_removal <- pup_outliers %>%
  group_by(subject, trial) %>%
  mutate(speed=speed_pupil(interp,time)) %>%
  mutate(MAD=calc_mad(speed)) %>%
  filter(speed < MAD)

ggplot(data=mad_removal, aes(x=time, y=pupilmm)) + geom_point() + ylim(.5, 1.1)

baseline_pupil_onset <- mad_removal %>% 
  group_by(subject, trial) %>%  
  mutate(time_zero= onset_pupil(time, sample_message, event = c("Simple_Start") )) %>%
  ungroup() %>% 
  filter(time_zero >= 0, time_zero <= 15000) %>%
  select(
    subject, trial, time, time_zero, 
    sample_message, pupilmm)

ggplot(data=baseline_pupil_onset, aes(x=time_zero, y=pupilmm)) + geom_point() + ylim(.5, 1.1)

timebins1 <- downsample_pupil(baseline_pupil_onset, bin.length=200)

ggplot(data=timebins1, aes(x=time_zero, y=pupilmm)) + geom_point() + ylim(.5, 1.1)

agg_subject<- timebins1 %>% 
  dplyr::group_by(subject,timebins) %>% 
  dplyr::summarise(aggbaseline=mean(pupilmm)) %>% 
  ungroup()

ggplot(data=agg_subject, aes(x=timebins, y=aggbaseline)) + geom_point() + geom_line(size = 1) + ylim(.5, 1.1)

rmarkdown::render("RestingStatePipeline.R")
