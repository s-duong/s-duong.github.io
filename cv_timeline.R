# All packages used below must be installed first
library(devtools)
#devtools::install_github("laresbernardo/lares")
library(lares)
library(ggplot2)


today <- as.character(Sys.Date())


### Edit from here ###
cv <- data.frame(rbind(
  c("PhD in Cognitive Psychology", "University of Pittsburgh", "Education", "2018-08-01", today),
  c("MSc in Cognitive Psychology", "University of Pittsburgh", "Education", "2018-08-01", "2021-05-31"),
  c("BA in Psychology", "University of New Haven", "Education", "2013-08-01", "2017-05-31"),
  c("Family Math Researchers Dissertation Fellow", "Center for Family Math", "Fellowships", "2023-06-01", today),
  c("Graduate Research Fellow", "National Science Foundation", "Fellowships", "2019-08-01", "2023-08-31"),
  c("Statistician", "UPMC YFRP", "Work Experience", "2023-06-01", today),
  c("Data Analyst", "Trying Together", "Work Experience", "2022-09-01", today),
  c("Statistics and Programming Consultant", "Various", "Work Experience", "2021-12-01", today),
  c("Micro-Intern in Higher Education", "UCTL", "Work Experience", "2023-06-01", "2023-08-15"),
  c("Lab Manager", "University of Maryland, Lab for Early Social Cognition", "Work Experience", "2017-06-01", "2018-06-30"),
  c("Research Assistant (Psychology)", "Yale University, Social Cognitive Development Lab", "Work Experience", "2015-09-01", "2017-05-30"),
  c("Research Assistant (Math)", "University of New Haven, Math Department.", "Work Experience", "2015-05-01", "2017-05-30")
  # c("Extra1", "Place1", "Extra", "2015-05-01", today),
  # c("Extra2", "Place2", "Extra", "2019-01-01", today),
  # c("Extra3", NA, "Extra", "2019-12-01", today)
))
### Edit until here ###


order <- c("Role", "Place", "Type", "Start", "End")
colnames(cv) <- order


plot_timeline2 <- function(event, start, end = start + 1, label = NA, group = NA,
                           title = "Curriculum Vitae Timeline", subtitle = "Antoine Soetewey",
                           size = 7, colour = "orange", save = FALSE, subdir = NA) {
  df <- data.frame(
    Role = as.character(event), Place = as.character(label),
    Start = lubridate::date(start), End = lubridate::date(end),
    Type = group
  )
  cvlong <- data.frame(pos = rep(
    as.numeric(rownames(df)),
    2
  ), name = rep(as.character(df$Role), 2), type = rep(factor(df$Type,
                                                             ordered = TRUE
  ), 2), where = rep(
    as.character(df$Place),
    2
  ), value = c(df$Start, df$End), label_pos = rep(df$Start +
                                                    floor((df$End - df$Start) / 2), 2))
  maxdate <- max(df$End)
  p <- ggplot(cvlong, aes(
    x = value, y = reorder(name, -pos),
    label = where, group = pos
  )) +
    geom_vline(
      xintercept = maxdate,
      alpha = 0.8, linetype = "dotted"
    ) +
    labs(
      title = title,
      subtitle = subtitle, x = NULL, y = NULL, colour = NULL
    ) +
    #theme_minimal() +
    theme_classic() +
    #theme_light() +
    #theme_bw() +
    theme(panel.background = element_rect(
      fill = "white",
      colour = NA
    ), axis.ticks = element_blank(), panel.grid.major.x = element_line(
      linewidth = 0.25,
      colour = "grey80"
    ))
  if (!is.na(cvlong$type)[1] | length(unique(cvlong$type)) >
      1) {
    p <- p + geom_line(aes(color = type), linewidth = size) +
      facet_grid(type ~ ., scales = "free", space = "free") +
      guides(colour = "none") +
      scale_colour_manual(values = c("#619CFF", "#EF64A8", "#FFBA21")) +
      theme(strip.text.y = element_text(size = 10))
  } else {
    p <- p + geom_line(linewidth = size)
  }
  p <- p + geom_label(aes(x = label_pos),
                      colour = "black",
                      size = 2, alpha = 0.7
  )
  if (save) {
    file_name <- "cv_timeline.png"
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir), recursive = T)
      file_name <- paste(subdir, file_name, sep = "/")
    }
    p <- p + ggsave(file_name, width = 8, height = 6)
    message(paste("Saved plot as", file_name))
  }
  return(p)
}




plot_timeline2(
  event = cv$Role,
  start = cv$Start,
  end = cv$End,
  label = cv$Place,
  group = cv$Type,
  save = FALSE,
  subtitle = "Shirley Duong" # replace with your name
)