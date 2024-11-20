#Comparing variables

#Overall performance by winter
Perf_Winters %>%
  ggplot(aes(x= Ov4_Tot,y = Un4_Tot,colour=winter)) +
  geom_point()

a <- Perf_Winters %>%
  ggplot(aes(x =winter,y=Perc_Ov4_Tot*100)) +
  geom_bar(stat="identity") +
  labs(y = "%", x = "Winters", title = "Percenage of A&E Admissions over 4 hours, each winter since 2010-11")

apply_custom_theme(a)  +
  scale_color_manual(values = "#12436D")


Joined <- Perf_Winters %>%
  left_join(Activity_Winters,by="winter")

Joined %>%
  ggplot(aes(x= Adm_T1,y = 100*Perc_Ov4_T1,label = winter)) +
  geom_point() +
  geom_text() +
  labs(y = "% waiting more than 4 hours", x = "Total number of T1 attendances", title = "Increase in attendance leads to higher proportion of long waits") +
  scale_x_continuous(labels = scales::comma)

Pre_Covid <- Joined %>%
  filter(!(winter %in% c("20-21","21-22","22-23","23-24")))

Post_Covid <- Joined %>%
  filter(winter %in% c("21-22","22-23","23-24"))

model_Pre <- lm(Perc_Ov4_T1 ~ Adm_T1, data = Pre_Covid)
summary(model_Pre)


model_Post <- lm(Perc_Ov4_T1 ~ Adm_T1, data = Post_Covid)
summary(model_Post)


