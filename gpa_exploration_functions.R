# filter to a class 
retrieve_data <- function(dat, subject, n) {
  gpadat <- dat %>% 
    filter(
      Subject == subject &
        Number == n
    )
  gpadat
}

get_plotly_gpadat <- function(gpadat, Instructors = NULL) {
  
  if (!is.null(Instructors)) {
    if (length(Instructors) > 8) {
      cat("Only first 8 instructors are included")
      Instructors <- Instructors[1:8]
    }
    gpadat <- dplyr::filter(gpadat, Primary.Instructor %in% Instructors)
  }
  
  n_instructor <- unique(gpadat$Primary.Instructor)
  
  if (length(n_instructor) > 8) {
    cat("Only Top 8 instructors who taught the most were included")
    instructor_table <- table(gpadat$Primary.Instructor)
    Instructors <- names(instructor_table)[order(instructor_table, decreasing = TRUE)[1:8]]
    gpadat <- dplyr::filter(gpadat, Primary.Instructor %in% Instructors)
  }
  
  gpadat %>% mutate(
    HoverText = paste0(
      Course.Title,
      "\n", Primary.Instructor,
      "\nTerm: ", YearTerm, 
      "\n", "Class Size: ", Size, 
      "\n", "Average: ", round(Mean_GPA,2), 
      "\n", "SD: ", round(SD_GPA, 2),
      "\n", "% Received A: ", round(perc_A)
    )
  )
  
}

overall_avg <- function(gpadat) {
  weighted.mean(gpadat$Mean_GPA, gpadat$Size)
}

# Average GPA
plotly_average_gpa <- function(plotly_gpadat, subject, n) {
  
  inst_mean <- plotly_gpadat %>% 
    group_by(Primary.Instructor) %>%
    summarise(
      Average_GPA = weighted.mean(Mean_GPA, Size),
      Total_Size = sum(Size)
    ) %>%
    ungroup() %>%
    mutate(
      HoverText = paste0(Primary.Instructor, "\nAverage GPA: ", round(Average_GPA, 2), "\n", "Total Class Size: ", Total_Size)
    )
  
  g <- ggplot(plotly_gpadat, aes(color = Primary.Instructor)) +
    geom_hline(data = inst_mean, aes(yintercept = Average_GPA, color = Primary.Instructor, text = HoverText), lty = "dashed") +
    geom_hline(yintercept = overall_avg(plotly_gpadat), lty = "dashed") +
    geom_point(aes(x = YearTerm, y = Mean_GPA, size = Size, text = HoverText), alpha = 0.9) +
    scale_size_continuous(guide = FALSE) +
    theme(
      axis.text.x = element_text(angle = 45),
      panel.background = element_rect(fill = "white"), 
      axis.line = element_line(colour = "black"), 
      panel.grid = element_line(colour = "lightgrey")
    ) +
    ggtitle(label = paste(subject, n, "Average GPA")) +
    scale_color_brewer(palette = "Dark2") +
    xlab("Year-Term") +
    ylab("GPA")
  
  ggp <- ggplotly(g, tooltip = "text")
  ggp$x$data[[nrow(inst_mean) + 1]]$text <- paste("Overall Average:", round(overall_avg(plotly_gpadat), 2))
  ggp
  
}


# By Percentage Received A
plotly_percentage_A <- function(plotly_gpadat, subject, n) {
  inst_mean <- plotly_gpadat %>% 
    group_by(Primary.Instructor) %>%
    summarise(
      Average_perc = weighted.mean(perc_A, Size),
      Total_Size = sum(Size)
    ) %>%
    ungroup() %>%
    mutate(
      HoverText = paste0(Primary.Instructor, "\nPercentage: ", round(Average_perc, 2), "\n", "Total Class Size: ", Total_Size)
    )
  
  g <- ggplot(plotly_gpadat, aes(color = Primary.Instructor)) +
    geom_hline(data = inst_mean, aes(yintercept = Average_perc, color = Primary.Instructor, text = HoverText), lty = "dashed") +
    geom_point(aes(x = YearTerm, y = perc_A, size = Size, text = HoverText), alpha = 0.9) +
    scale_size_continuous(guide = FALSE) +
    theme(
      axis.text.x = element_text(angle = 45),
      panel.background = element_rect(fill = "white"), 
      axis.line = element_line(colour = "black"), 
      panel.grid = element_line(colour = "lightgrey")
    ) +
    ggtitle(label = paste(subject, n, "% Received A")) +
    scale_color_brewer(palette = "Dark2") +
    xlab("Year-Term") +
    ylab("%")
  
  ggp <- ggplotly(g, tooltip = "text")
  ggp
}

summarise_by_inst <- function(gpadat) {
  gpadat <- gpadat %>%
    group_by(Primary.Instructor) %>%
    summarise(
      `Mean GPA` = weighted.mean(Mean_GPA, Size),
      `Total Class Size` = sum(Size)
    )
  
  gpadat[order(gpadat$`Total Class Size`),]
}