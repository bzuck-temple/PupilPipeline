pupil_path <- system.file("extdata", "Pupil_file1.xls", package = "gazer")
pupil_files<-read.table(pupil_path)

behave_data<-behave_pupil(pupil_files, omiterrors = FALSE, behave_colnames = c("subject","script","alteration", "trial", "target","accuracy","rt", "block", "cb"))

itemacc <- behave_data %>% 
     group_by(target) %>% 
      summarise(
           # overall item accuracy and word condition only
             meanitemacc = mean(accuracy[block>0 & alteration=="word"])
       ) 

subacc <- behave_data %>% 
    group_by(subject) %>% 
    summarise(
        #subject accuracy and word condition only
         meansubacc = mean(accuracy[block > 0 & alteration == "word"])
    ) 

dataraw1 <- merge(pupil_files, itemacc) # merge into main ds

dataraw2 <- merge(dataraw1, subacc) # merge into main ds

pupil_files1 <- dataraw2 %>%
  # filter out practice blocks, incorrect responses, nonword trials, low item and subj acc
  filter(block > 0, accuracy == 1, alteration == "word", meanitemacc > .60, meansubacc > .74)

pup_files1 <-arrange(pupil_files1, subject, target, trial, time)
 
pup_extend<- pup_files1 %>% 
  group_by(subject, trial) %>% 
  mutate(extendpupil=extend_blinks(pupil, fillback=100, fillforward=100, hz=250))

pup_interp <- interpolate_pupil(
  pup_extend, 
  extendblinks = TRUE, 
  type = "linear")

rolling_mean_pupil_average <- as.data.frame(pup_interp) %>% #must be in a data.frame
  select(
    subject, trial, target, pupil, script, alteration, 
    time, interp, sample_message) %>%
mutate(movingavgpup = moving_average_pupil(interp, n = 5))

baseline_pupil <- baseline_correction_pupil(
  rolling_mean_pupil_average, 
  pupil_colnames = "movingavgpup", 
  baseline_window = c(500, 1000)
)


timebinsmm <- rolling_mean_pupil_average  %>% 
  mutate(pupilmm = (movingavgpup * 5)/5570.29)

pup_missing <- count_missing_pupil(pup_files1, missingthresh = .2)

puphist <- ggplot(pup_extend, aes(x = extendpupil)) + 
  geom_histogram(aes(y = ..count..), colour = "green", binwidth = 0.5)  + 
  geom_vline(xintercept = 2500, linetype="dotted") +
  geom_vline(xintercept = 5100, linetype="dotted") + 
  xlab("Pupil Size") + 
  ylab("Count") + 
  theme_bw()

print(puphist)

 

baseline_pupil_onset <- baseline_pupil %>% 
  dplyr::group_by(subject, trial) %>%  
  dplyr::mutate(time_zero= onset_pupil(time, sample_message, event = c("target"))) %>%
  ungroup() %>% 
  dplyr::filter(time_zero >= 0, time_zero <= 3000) %>%
  select(
    subject, trial, time, script, time_zero, 
    sample_message, baselinecorrectedp
  )

timebins1 <- downsample_pupil(baseline_pupil_onset, bin.length=200)

agg_subject<- timebins1 %>% 
  dplyr::group_by(subject, script,timebins) %>% 
  dplyr::summarise(aggbaseline=mean(baselinecorrectedp)) %>% 
  ungroup()

ggplot(agg_subject,aes(timebins, aggbaseline)) +
  geom_line(aes(linetype=script)) +
   theme_bw()
jamie.theme <- theme_bw() + theme(axis.line = element_line(colour = "black"), 
                                  panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
                                  panel.border = element_blank(), panel.background = element_blank(), legend.title = element_blank())
mycolors <- c("goldenrod2", "black")
agg_subject$timebins <-as.numeric(agg_subject$timebins)
agg_subject$aggbaseline <-as.numeric(agg_subject$aggbaseline)
agg_subject$script <-as.factor(agg_subject$script)
agg_subject$subject <-as.factor(agg_subject$subject)
ggplot(subset(agg_subject, script == "Print"), aes(timebins, aggbaseline, color = subject)) + stat_summary(fun.y = mean, geom = "line", size = 0.5) + stat_summary(fun.data = mean_se, geom = "pointrange", size = 0.2)
