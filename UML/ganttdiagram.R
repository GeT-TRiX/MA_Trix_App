
# https://stackoverflow.com/questions/3550341/gantt-charts-with-r

library(DiagrammeR)
mermaid("
gantt
dateFormat  YYYY-MM-DD
title A Very Nice Gantt Diagram

section Basic Tasks
This is completed             :done,          first_1,    2014-01-06, 2014-01-08
This is active                :active,        first_2,    2014-01-09, 3d
Do this later                 :               first_3,    after first_2, 5d
Do this after that            :               first_4,    after first_3, 5d

section Important Things
Completed, critical task      :crit, done,    import_1,   2014-01-06,24h
Also done, also critical      :crit, done,    import_2,   after import_1, 2d
Doing this important task now :crit, active,  import_3,   after import_2, 3d
Next critical task            :crit,          import_4,   after import_3, 5d

section The Extras
First extras                  :active,        extras_1,   after import_4,  3d
Second helping                :               extras_2,   after extras_1, 20h
More of the extras            :               extras_3,   after extras_1, 48h
")



library(DiagrammeR)
mermaid("
        gantt
        dateFormat  YYYY-MM-DD
        title A Very Nice Gantt Diagram
        
        section Basic Tasks
        This is completed             :done,          first_1,    2014-01-06, 2014-01-08
        This is active                :active,        first_2,    2014-01-09, 3d
        Do this later                 :               first_3,    after first_2, 5d
        Do this after that            :               first_4,    after first_3, 5d
        
        section Important Things
        Completed, critical task      :crit, done,    import_1,   2014-01-06,24h
        Also done, also critical      :crit, done,    import_2,   after import_1, 2d
        Doing this important task now :crit, active,  import_3,   after import_2, 3d
        Next critical task            :crit,          import_4,   after import_3, 5d
        
        section The Extras
        First extras                  :active,        extras_1,   after import_4,  3d
        Second helping                :               extras_2,   after extras_1, 20h
        More of the extras            :               extras_3,   after extras_1, 48h
        ")



df <- data.frame(task = c("Recherche bibliographique", "Elaboration d'un cahier des charges", "Conception de l'architecture",
                          "Implémentation sous R", "Implémentation sous Shiny", 
                          "Déploiement de MATRiX en ligne", "Maintenance de MATRiX", "Optimisation du code", "Rédaction du rapport de stage"),
                 status = c("active", "active", "active"),
                 pos = c("first_1", "first_2", "first_3","first_4", "first_5","first_6", "first_7", "first_8", "first_9"),
                 start = c("03-05", "03-05", "after first_1"),
                 end = c("08-31", "3d", "5d"))
View(df)
 mermaid(
  paste0(
    # mermaid "header", each component separated with "\n" (line break)
    "gantt", "\n", 
    "dateFormat   YYYY-MM-DD", "\n", 
    "title A Very Nice Gantt Diagram", "\n",
    # unite the first two columns (task & status) and separate them with ":"
    # then, unite the other columns and separate them with ","
    # this will create the required mermaid "body"
    paste(df %>%
            unite(i, task, status, sep = ":") %>%
            unite(j, i, pos, start, end, sep = ",") %>%
            .$j, 
          collapse = "\n"
    ), "\n"
  )
)

m <- mermaid(
   paste0(
     # mermaid "header", each component separated with "\n" (line break)
     "gantt", "\n", 
     "dateFormat  MM-DD", "\n", 
     "title A Very Nice Gantt Diagram", "\n",
     # unite the first two columns (task & status) and separate them with ":"
     # then, unite the other columns and separate them with ","
     # this will create the required mermaid "body"
     paste(df %>%
             unite(i, task, status, sep = ":") %>%
             unite(j, i, pos, start, end, sep = ",") %>%
             .$j, 
           collapse = "\n"
     ), "\n"
   )
 )

m$x$config = list(ganttConfig = list(
  axisFormatter = list(list(
    "%b " 
    ,htmlwidgets::JS(
      'function(d){ return d.getDay() == 1 }' 
    )
  ))
))

m


df <- data.frame(task = c("Recherche bibliographique", "Elaboration d'un cahier des charges",
                          "Conception de l'architecture",
                          "Implémentation sous R", "Implémentation sous Shiny", 
                          "Déploiement de MATRiX en ligne", "Maintenance de MATRiX", "Optimisation du code", "Rédaction du rapport de stage"),
                 status = c("active", "active", "active"),
                 pos = c("first_1", "first_2", "first_3","first_4", "first_5","first_6", "first_7", "first_8", "first_9"),
                 start = c("03-05", "03-05", "after first_1"),
                 end = c("08-31", "3d", "5d"))


library(reshape2)
library(ggplot2)
rm(list = ls())
task1 <- c('Recherche bibliographique', '2018-03-05', '2018-03-25')
task2 <- c('Elaboration d"un cahier des charges', '2018-03-25', '2018-04-15')
task3 <- c('Conception de l"architecture', '2018-04-15', '2018-04-30')
task4 <- c('Implémentation du code source', '2018-04-20', '2018-06-30')
task5 <- c('Déploiement de MATRiX en ligne', '2018-05-10', '2018-06-20')
task6 <- c('Bêta test', '2018-05-20', '2018-05-22')
task7 <- c('Optimisation/Correction des bugs', '2018-06-20', '2018-08-20')
task8 <- c('Maintenance de MATRiX', '2018-06-20', '2018-07-01')
task9 <- c('Rédaction du rapport de stage', '2018-07-15', '2018-08-20')


# df <- as.data.frame(t(sapply(ls(pattern = '^task\\d'), function(x) eval(parse(text = x)))), row.names = FALSE)

df <- as.data.frame(rbind(task1, task2, task3, task4,task5, task6, task7, task8, task9))
names(df) <- c('task', 'start', 'end')
df$task <- factor(df$task, levels = df$task)
df$start <- as.Date(df$start)
df$end <- as.Date(df$end)
df_melted <- melt(df, measure.vars = c('start', 'end'))


# starting date to begin plot
start_date <- as.Date('2018-03-05')

ggplot(df_melted, aes(value, task)) + 
  geom_line(size = 10) +
  labs(x = '', y = '', title = "Diagramme des tâche effectuées") +
  theme_bw(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(colour="black", linetype = "dashed"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 0)) +
  scale_x_date(date_labels = "%Y %b")

# see ?strptime if you want your date in a different format
# see http://docs.ggplot2.org/current/scale_date.html if you want to adjust the x-axis


