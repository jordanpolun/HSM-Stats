## ----Setup, include=FALSE------------------------------------------------
library(babynames)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(lubridate)

## ----Reading in data-----------------------------------------------------
# Reading in data from CSV

master_df <-
  read.csv(
    file.choose(),
    header = T,
    stringsAsFactors = F
  )

# Converting Created.On to datetime/POSIXct
master_df$Created.On <-
  as.POSIXct(master_df$Created.On, format = "%m/%d/%Y %H:%M")

# Converting Modified.On to datetime/POSIXct
master_df$Modified.On <-
  as.POSIXct(master_df$Modified.On, format = "%m/%d/%Y %H:%M")



## ----Anonymizing employees-----------------------------------------------
# Creating lookup table of babynames to anonymize data
#From https://stackoverflow.com/questions/49307530/how-do-you-anonymize-a-vector-in-a-way-that-generates-human-readable-output-in-r

anonymize <- F

if (anonymize) {
  lookup <- babynames %>% filter(year == 2010) %>%
    top_n(1000, n) %>%
    sample_n(length(unique(master_df$Owner))) %>%
    select(Name = name) %>%
    bind_cols(Owner = unique(master_df$Owner))
  
  # Merging lookup with master_df to add Name column
  master_df <- merge(master_df, lookup, by = "Owner")
  
  # Removing Owner column to maintain anonymity
  master_df <- select(master_df,-Owner)
  
} else {
  colnames(master_df)[colnames(master_df) == "Owner"] <- "Name"
}

# Sorting by datetime old -> new
master_df <-
  master_df[order(master_df$Modified.On, decreasing = F), ]


## ----Categorizing tasks--------------------------------------------------
# If a task uses any common Service Request phrases, it is probably a Service Request
# From https://stackoverflow.com/questions/35962426/multiple-strings-with-str-detect-r

master_df <- master_df %>%
  mutate(Task.Class = ifelse(str_detect(tolower(Subject), paste(c("deploy", "reimage", "move", "return", "connect"), collapse = '|')), "Service Request", 
                             ifelse(str_detect(tolower(Subject), paste(c("class", "p1", "priority", "lab"), collapse = '|')), "Classroom Call", "Other")),
         Month.Complete = paste(month(master_df$Modified.On, label=T), year(master_df$Modified.On)),
         Date.Complete = stamp("September 1, 2019")(master_df$Modified.On),
         Quarter.Complete = paste("Q", quarter(master_df$Modified.On), " ", year(master_df$Modified.On), sep=""),
         Week.Complete = (week(master_df$Modified.On) %% 4) + 1,
         Completion.Time = as.double(master_df$Modified.On - master_df$Created.On, units = "days"))



## ----Create employee DataFrame-------------------------------------------

employee_df <- master_df %>%
  group_by(Name) %>%
  summarize(Start.Date = min(Modified.On),
            End.Date = max(Modified.On),
            Num.Tasks = n(),
            Days.To.Complete = mean(Completion.Time)) %>%
  mutate(Days.Worked = as.double(End.Date - Start.Date, units="days"),
         Tasks.Per.Day = Num.Tasks/Days.Worked,
         Task.Percentage = 100 * Num.Tasks/(nrow(filter(master_df, Modified.On >= Start.Date, Modified.On <= End.Date))))

p1s = (filter(master_df, Task.Class == "Classroom Call") %>% count(Name)) %>% rename("Classroom.Calls" = n)
srs = (filter(master_df, Task.Class == "Service Request") %>% count(Name)) %>% rename("Service.Requests" = n)
others = (filter(master_df, Task.Class == "Other") %>% count(Name)) %>% rename("Others" = n)

employee_df <- inner_join(employee_df, p1s, by="Name")
employee_df <- inner_join(employee_df, srs, by="Name")
employee_df <- inner_join(employee_df, others, by="Name")

# Add in employee worth... or how much more they do than the average
employee_df <- employee_df %>%
  mutate(Employee.Worth = Tasks.Per.Day/mean(Tasks.Per.Day))

# Remove HelpDesk and brand new employees (No one averages 30 tasks per day or 0.1 tasks per day, so there must be a small sample size)
employee_df <- employee_df %>%
  filter(Tasks.Per.Day >= 0.1, Tasks.Per.Day <= 30)

## ----Generating employee reports-----------------------------------------
for (row in 1:nrow(employee_df)) {
  employee_row = employee_df[row, ]
  employee_tasks_df <- filter(master_df, Name == employee_row$Name)
  tenure_master_df <- filter(master_df, Modified.On > employee_row$Start.Date, Modified.On < employee_row$End.Date)
  everyone_else_employee_df <-
    filter(employee_df, Name != employee_row$Name)
  
  pdf(NULL)

  # Monthly line graph
  employee_tasks_per_month_plot <- function() {
    
    employee_monthly_data <- count(employee_tasks_df, Month.Complete)
    employee_average <- mean(employee_monthly_data$n)
    oss_monthly_data <- tenure_master_df %>%
                        group_by(Month.Complete) %>%
                        summarize(Num.Workers = n_distinct(Name))
    
    oss_average <- nrow(tenure_master_df)/(length(unique(tenure_master_df$Month.Complete)))/mean(oss_monthly_data$Num.Workers)
    
    employee_df[row, "Employee.Worth"] <- employee_average/oss_average
    
    employee_monthly_tasks <- ggplot(employee_monthly_data, aes(x=Month.Complete, y=n, group=1)) +
      
      # Plot employee's line
      geom_point(stat="identity") + 
      geom_line(stat="identity") +
      
      # Add employee's average line
      geom_hline(aes(yintercept=employee_average, color=paste("Average", round(employee_average, 1))), linetype="dashed", show.legend = T) +
      
      # Add OSS's average line
      geom_hline(aes(yintercept=oss_average, color=paste("OSS average", round(oss_average, 1))), linetype="dashed", show.legend = T) +
      
      # Start graph at zero
      ylim(0, NA) +
      
      # Maintain chronological order
      scale_x_discrete(limits=unique(employee_tasks_df$Month.Complete)) +
      
      # Add labels
      labs(x="Month", y="Tasks Completed", title="Tasks Completed per Month") +
      
      # Change colors of lines
      scale_color_manual(values=c("red", "blue")) +
      
      # Tilt xlabels so it's visible and moving legend for more room
      theme(axis.text.x = element_text(angle = 90),
            legend.title = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(hjust=0.5))
    
    print(employee_monthly_tasks)
    return(employee_monthly_tasks)

  }


  #Task categories pie chart
  employee_categories_pie_chart <- function() {
    
    p1s <- employee_row$Classroom.Calls
    others <- employee_row$Others
    srs <- employee_row$Service.Requests
    
    categories_df <- data.frame("Category" = c("Classroom Call", "Other", "Service Request"),
                                "Value" = c(p1s, others, srs)) %>%
      
      # Making ggplot not auto-sort and adding labels and positions of labels
      arrange(desc(Category)) %>%
      mutate(lab.ypos = cumsum(Value) - 0.5*Value,
             labels = paste(Category, "\n", round(Value/sum(Value) * 100, 1), "%", sep=""))
    
    category_pie_chart <- ggplot(categories_df, aes(x="", y=Value, fill=Category)) +
      geom_bar(stat="identity", width=1, color="black") +
      coord_polar("y", start=0) +
      
      # Add title
      labs(x="", y="", title="Task Breakdown by Category") +
      
      # Adding percentages
      geom_text(aes(y=lab.ypos, label=labels), size=3) +
      
      # Manually setting colors for slices
      scale_fill_manual(values=c("hotpink", "lightgreen", "lightblue")) +
      
      # Adjusting text size, moving legend to bottom, removing axes
      theme(legend.position = "none",
            plot.title = element_text(hjust=0.5),
            axis.ticks=element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank())
    
    print(category_pie_chart)
    return (category_pie_chart)

  }


  # Employee Worth stripchart function
  employee_worth_stripchart <- function() {
    
    average <- mean(employee_df$Employee.Worth)
    stat_stripchart <- ggplot(everyone_else_employee_df, aes(x="Status", y=Employee.Worth)) +
      geom_jitter(position=position_jitter(0.1)) + 
      
      geom_point(data=employee_row, aes(x="Status", y=Employee.Worth), fill = "yellow", color="black", shape=23, size=5) + 
      geom_segment(data=employee_row, aes(x=0, y=Employee.Worth, xend=1, yend=Employee.Worth)) +
      geom_text(data = employee_row, aes(x=0.5, y=Employee.Worth, label=paste(Name, round(Employee.Worth, 2)), vjust=-0.5, hjust=1)) +
      
      geom_hline(data=employee_df, aes(yintercept=average), linetype="dashed", color="red") +
      geom_text(data = data.frame(), aes(x=0.5, y=average, label=paste("Average", round(average, 2)), vjust=-0.5)) +
      
      # Add title
      labs(x="", y="Employee Worth", title=paste(employee_row$Name, "Employee Worth")) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x= element_blank())
    
    print(stat_stripchart)
    return(stat_stripchart)
  }
  
  
  # Tasks Per Day stripchart function
  tasks_per_day_stripchart <- function() {
    
    average <- mean(employee_df$Tasks.Per.Day)
    stat_stripchart <- ggplot(everyone_else_employee_df, aes(x="Status", y=Tasks.Per.Day)) +
      geom_jitter(position=position_jitter(0.1)) + 
      
      geom_point(data=employee_row, aes(x="Status", y=Tasks.Per.Day), fill = "yellow", color="black", shape=23, size=5) + 
      geom_segment(data=employee_row, aes(x=0, y=Tasks.Per.Day, xend=1, yend=Tasks.Per.Day)) +
      geom_text(data = employee_row, aes(x=0.5, y=Tasks.Per.Day, label=paste(Name, round(Tasks.Per.Day, 2)), vjust=-0.5, hjust=1)) +
      
      geom_hline(data=employee_df, aes(yintercept=average), linetype="dashed", color="red") +
      geom_text(data = data.frame(), aes(x=0.5, y=average, label=paste("Average", round(average, 2)), vjust=-0.5)) +
      
      # Add title
      labs(x="", y="Tasks Per Day", title=paste(employee_row$Name, "Tasks Per Day")) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x= element_blank())
    
    print(stat_stripchart)
    return(stat_stripchart)
  }
  
  
  # Task Percentage stripchart function
  task_percentage_stripchart <- function() {
    
    average <- mean(employee_df$Task.Percentage)
    stat_stripchart <- ggplot(everyone_else_employee_df, aes(x="Status", y=Task.Percentage)) +
      geom_jitter(position=position_jitter(0.1)) + 
      
      geom_point(data=employee_row, aes(x="Status", y=Task.Percentage), fill = "yellow", color="black", shape=23, size=5) + 
      geom_segment(data=employee_row, aes(x=0, y=Task.Percentage, xend=1, yend=Task.Percentage)) +
      geom_text(data = employee_row, aes(x=0.5, y=Task.Percentage, label=paste(Name, round(Task.Percentage, 2)), vjust=-0.5, hjust=1)) +
      
      geom_hline(data=employee_df, aes(yintercept=average), linetype="dashed", color="red") +
      geom_text(data = data.frame(), aes(x=0.5, y=average, label=paste("Average", round(average, 2)), vjust=-0.5, hjust=0)) +
      
      # Add title
      labs(x="", y="Task Percentage", title=paste(employee_row$Name, "Task Percentage")) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x= element_blank())
    
    print(stat_stripchart)
    return(stat_stripchart)
  }


  # Time to Complete distribution
  employee_completion_time_density_plot <- function() {

    outliers <- boxplot(employee_tasks_df$Completion.Time, plot = F)$out
    times_no_outliers <-
      filter(employee_tasks_df,!Completion.Time %in% outliers)
    average <- round(mean(times_no_outliers$Completion.Time), 1)
    
    oss_outliers <- boxplot(tenure_master_df$Completion.Time, plot = F)$out
    oss_times_no_outliers <-
      filter(tenure_master_df,!Completion.Time %in% oss_outliers)
    oss_average <- round(mean(oss_times_no_outliers$Completion.Time), 1)
        
    completion_time_plot <- ggplot(times_no_outliers, aes(x=Completion.Time)) +
      
      geom_density(fill="lightblue") +
      
      geom_vline(aes(xintercept=mean(Completion.Time)), linetype="dashed", color="blue") +
      geom_text(data=data.frame(), aes(x=average, y=0.1, label=paste(employee_row$Name, "Mean", average), vjust=-0.5, angle=90)) +
      
      geom_vline(data=oss_times_no_outliers, aes(xintercept=mean(Completion.Time)), linetype="dashed", color="red") +
      geom_text(data=data.frame(), aes(x=oss_average, y=0.01, label=paste("OSS Average", oss_average), vjust=-0.5, angle=90, hjust=0)) +
      
      labs(x="Days to Complete Task", title=paste(employee_row$Name, "Completion Time Density Plot"))

    print(completion_time_plot)
    return(completion_time_plot)
  }



  # Leaderboards
  employee_leaderboards <- function() {
    daily_leaderboard <-
      employee_tasks_df %>% count(Date.Complete) %>% arrange(desc(n)) %>% head(5)
    weekly_leaderboard <-
      employee_tasks_df %>% count(Month.Complete, Week.Complete) %>% arrange(desc(n)) %>% head(5)
    monthly_leaderboard <-
      employee_tasks_df %>% count(Month.Complete) %>% arrange(desc(n)) %>% head(5)

    grid.arrange(
      tableGrob(daily_leaderboard),
      tableGrob(weekly_leaderboard),
      tableGrob(monthly_leaderboard)
    )
  }


  # Save as PDF
  if(!is.null(dev.list())) dev.off()

  pdf(paste("Employee Data/", employee_row$Name, ".pdf", sep = ""))
  employee_tasks_per_month_plot()
  employee_categories_pie_chart()
  employee_worth_stripchart()
  tasks_per_day_stripchart()
  task_percentage_stripchart()
  employee_completion_time_density_plot()
  employee_leaderboards()

  if(!is.null(dev.list())) dev.off()

}


## ----Generating OSS Report-----------------------------------------------

pdf(NULL)

# Monthly line graph
oss_tasks_per_month_plot <- function() {
  
  apple_tasks <- filter(master_df, Team == "Apple")
  windows_tasks <- filter(master_df, Team == "Onsite Support")
  
  apple_monthly_data <- count(apple_tasks, Quarter.Complete)
  windows_monthly_data <- count(windows_tasks, Quarter.Complete)
  oss_monthly_data <- count(master_df, Quarter.Complete)

  monthly_tasks <- ggplot(oss_monthly_data, aes(x=Quarter.Complete, y=n, group=1)) +
    # Add OSS quarterly
    geom_point() + 
    geom_line() +
    
    # Add apple quarterly
    geom_point(data=apple_monthly_data, aes(x=Quarter.Complete, y=n), color="lightgreen") + 
    geom_line(data=apple_monthly_data, aes(x=Quarter.Complete, y=n), color="lightgreen") +
    
    # Add windows quarterly
    geom_point(data=windows_monthly_data, aes(x=Quarter.Complete, y=n), color="lightblue") + 
    geom_line(data=windows_monthly_data, aes(x=Quarter.Complete, y=n), color="lightblue") +
    
    # Maintain chronological order
    scale_x_discrete(limits=unique(master_df$Quarter.Complete)) +
    
    # Add labels
    labs(x="Quarter", y="Tasks Completed", title="Tasks Completed per Quarter by Team") +
    
    # Tilt xlabels so it's visible and moving legend for more room
    theme(axis.text.x = element_text(angle = 90),
          legend.position="bottom",
          plot.title = element_text(hjust=0.5))
  
  return(monthly_tasks)
  
}

# Pie charts
# Total categories pie chart
oss_categories_pie_chart <- function() {
  srs <- sum(employee_df$Service.Requests)
  p1s <- sum(employee_df$Classroom.Calls)
  other <- nrow(master_df) - srs - p1s
  
  categories_df <- data.frame("Category" = c("Classroom Call", "Other", "Service Request"),
                              "Value" = c(p1s, other, srs)) %>%
    # Making ggplot not auto-sort and adding labels and positions of labels
    arrange(desc(Category)) %>%
    mutate(lab.ypos = cumsum(Value) - 0.5*Value,
           labels = paste(Category, "\n", round(Value/sum(Value) * 100, 1), "%", sep=""))
  
  category_pie_chart <- ggplot(categories_df, aes(x="", y=Value, fill=Category)) +
    geom_bar(stat="identity", width=1, color="black") +
    coord_polar("y", start=0) +
    
    # Add title
    labs(x="", y="", title="Task Breakdown by Category") +
    
    # Adding percentages
    geom_text(aes(y=lab.ypos, label=labels), size=3) +
    
    # Manually setting colors for slices
    scale_fill_manual(values=c("hotpink", "lightgreen", "lightblue")) +
    
    # Adjusting text size, moving legend to bottom, removing axes
    theme(legend.position = "none",
          plot.title = element_text(hjust=0.5),
          axis.ticks=element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank())
  
  # print(category_pie_chart)
  return (category_pie_chart)
}

# Team breakdown
oss_team_breakdown_pie_chart <- function() {
  
  apple_tasks <- filter(master_df, Team == "Apple")
  windows_tasks <- filter(master_df, Team == "Onsite Support")
  
  a_p1s <- nrow(filter(apple_tasks, Task.Class == "Classroom Call"))
  a_srs <- nrow(filter(apple_tasks, Task.Class == "Service Request"))
  a_other <- nrow(apple_tasks) - a_srs - a_p1s
  
  w_p1s <- nrow(filter(windows_tasks, Task.Class == "Classroom Call"))
  w_srs <- nrow(filter(windows_tasks, Task.Class == "Service Request"))
  w_other <- nrow(windows_tasks) - w_srs - w_p1s
  
  categories_df <- data.frame("Category" = c("Apple Classroom Call", "Apple Other", "Apple Service Request",
                                        "Windows Classroom Call", "Windows Other", "Windows Service Request"),
                         "Value" = c(a_p1s, a_other, a_srs,
                                     w_p1s, w_other, w_srs))
  
  teams_pie_chart <- ggplot(categories_df, aes(x=2, y=as.numeric(Value), fill=Category)) +
    geom_bar(stat="identity", width=1, color="black") +
    coord_polar("y", start=0) +
    
    # Add title
    labs(x="", y="", title="Task Breakdown by Team") +
    
    # Manually setting colors for slices and editing legend labels
    scale_fill_manual(values=c("palegreen3", "palegreen1", "palegreen2", "lightblue3", "lightblue1", "lightblue2"),
                      breaks=categories_df$Category,
                      labels= paste(categories_df$Category, " ", round(categories_df$Value/nrow(master_df) * 100, 1), "%", sep="")) +
    
    # Adjusting text size, moving legend to bottom, removing axes
    theme(legend.position = "right",
          legend.text = element_text(size=7),
          plot.title = element_text(hjust=0.5),
          axis.ticks=element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank())
  
  # print(teams_pie_chart)
  return (teams_pie_chart)
}

# Leaderboards
oss_leaderboards <- function() {
  daily_leaderboard <-
    master_df %>% count(Name, Date.Complete) %>% arrange(desc(n)) %>% head(5)
  weekly_leaderboard <-
    master_df %>% count(Name, Month.Complete, Week.Complete) %>% arrange(desc(n)) %>% head(5)
  monthly_leaderboard <-
    master_df %>% count(Name, Month.Complete) %>% arrange(desc(n)) %>% head(5)
  
  grid.arrange(
    tableGrob(daily_leaderboard),
    tableGrob(weekly_leaderboard),
    tableGrob(monthly_leaderboard)
  )
}



# Save as PDF
if(!is.null(dev.list())) dev.off()

pdf("OSS Data.pdf")
oss_tasks_per_month_plot()
oss_categories_pie_chart()
oss_team_breakdown_pie_chart()
oss_leaderboards()

if(!is.null(dev.list())) dev.off()
