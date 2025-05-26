num_ticks <- 10

#***--------------------------------Normal resulting plots------------------------**#

NRHF_plot <- function(sample_results, modulations_back, fileName, saveNRHFplot) {
  
  plotTitleTHF <- paste0("NRHF based on FT (frequency = 0), ", modulations_back, " modulations")
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]

  NRHF_p <- ggplot(sample_results, aes(x = TRef)) +
          geom_line(aes(y = dc_value), color = "black", linewidth = 1.2) +     # Smoothed data with thicker line
          labs(
            title = plotTitleTHF,
            subtitle = subtitle,
            x = "Temperature (°C)",
            y = "Non Reversing Heat Flow (W/g)"
          ) +
          theme_minimal(base_size = 18) +  # Larger base font size for better readability
          theme(
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), # Center title with bold font
            axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
            axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),  # Bold and separated y-axis title
            axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
            axis.line = element_line(color = "black", size = 0.5),  # Black axis lines for better contrast
            panel.grid.major = element_line(color = "gray", size = 0.25),  # Light gray grid lines for a clean look
            panel.grid.minor = element_blank(),  # Minor grid lines removed
            plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
            axis.ticks = element_line(color = "black", size = 0.5)  # Ticks for axes
          ) +
          scale_x_continuous(expand = c(0.005, 0.005), breaks = scales::pretty_breaks(n = num_ticks)) +  # Set the number of x-axis ticks
          scale_y_continuous(
            expand = c(0.0002, 0.0002) # Remove space between plot and y-axis
          )  # This ensures the y-axis covers the full range of your data with extra space at the top

  if(saveNRHFplot == TRUE){
    ggsave(paste0(subtitle, " ", plotTitleTHF, ".png"), dpi = 600, width = 10, height = 10)
  }
 return(NRHF_p)  # <--- Ensure the function returns the ggplot object
 }

RevCp_plot <- function(sample_results, modulations_back, fileName, saveRevCpplot) {
  
  plottitleRevCp <- paste0("RevCp based on FT (1st harmonic), ", modulations_back, " modulations")
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]
  
    RevCp_p <- ggplot(sample_results, aes(x = TRef)) +
      geom_line(aes(y = reversing_heat_flow), color = "black", linewidth = 1.2) +     # Smoothed data with thicker line
      labs(
        title = plottitleRevCp,
        subtitle = subtitle,
        x = "Temperature (°C)",
        y = "Reversing Heat Capacity (J/g)"
      ) +
      theme_minimal(base_size = 18) +  # Larger base font size for better readability
      theme(
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), # Center title with bold font
        axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
        axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),  # Bold and separated y-axis title
        axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
        axis.line = element_line(color = "black", size = 0.5),  # Black axis lines for better contrast
        panel.grid.major = element_line(color = "gray", size = 0.25),  # Light gray grid lines for a clean look
        panel.grid.minor = element_blank(),  # Minor grid lines removed
        plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
        axis.ticks = element_line(color = "black", size = 0.5)  # Ticks for axes
      ) +
      scale_x_continuous(expand = c(0.005, 0.005), breaks = scales::pretty_breaks(n = num_ticks)) +  # Set the number of x-axis ticks
      scale_y_continuous(
        expand = c(0.0002, 0.0002) # Remove space between plot and y-axis
      )  # This ensures the y-axis covers the full range of your data with extra space at the top

  if(saveRevCpplot == TRUE) {
    ggsave(paste0(fileName, plottitleRevCp, ".png"), dpi = 600, width = 10, height = 10)
  }
  return(RevCp_p)  # <--- Ensure the function returns the ggplot object
    
} 


Manual_RevCp_plot <- function(average_heat_flow_per_pattern, modulations_back, fileName, savemanualRevCpplot) {
  
  plottitleRevCpmanual <- paste0("RevCp calculated manually, ", modulations_back, " modulations")
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]

  manRevCp_p <- ggplot(average_heat_flow_per_pattern, aes(x = Tref)) +
    geom_line(aes(y = RevCpManual), color = "black", linewidth = 1.2) +     # Smoothed data with thicker line
    labs(
      title = plottitleRevCpmanual,
      subtitle = subtitle,
      x = "Temperature (°C)",
      y = "Reversing Heat Capacity (J/g)"
    ) +
    theme_minimal(base_size = 18) +  # Larger base font size for better readability
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), # Center title with bold font
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),  # Bold and separated y-axis title
      axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
      axis.line = element_line(color = "black", size = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", size = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", size = 0.5)  # Ticks for axes
    ) +
    scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = num_ticks)) +  # Set the number of x-axis ticks
    scale_y_continuous(
      expand = c(0, 0)  # Remove space between plot and y-axis
    )  # This ensures the y-axis covers the full range of your data with extra space at the top
  if(savemanualRevCpplot == TRUE){
    ggsave(paste0(fileName, plottitleRevCpmanual, ".png"), dpi = 600, width = 10, height = 10)
  }
  return(manRevCp_p)  # <--- Ensure the function returns the ggplot object
}






#***--------------------------------Overlays------------------------**#

RevCp_NRHF_plot <- function(sample_results, modulations_back, fileName) {
  plot_ly(sample_results) %>%
    add_lines(
      x = ~TRef, 
      y = ~reversing_heat_flow, 
      name = "Reversing Heat Flow",
      line = list(color = "black", width = 1.2),
      yaxis = "y"
    ) %>%
    add_lines(
      x = ~TRef, 
      y = ~dc_value, 
      name = "DC Value",
      line = list(color = "red", width = 1.2),
      yaxis = "y2"
    ) %>%
    layout(
      title = list(
        text = plottitleRevCp,
        x = 0.5,  # Center title
        font = list(size = 20, family = "Arial", color = "black", weight = "bold")
      ),
      xaxis = list(
        title = "Temperature (°C)",
        titlefont = list(size = 18, family = "Arial", color = "black", weight = "bold"),
        tickfont = list(size = 18, family = "Arial", color = "black"),
        linecolor = "black", 
        linewidth = 0.5,
        mirror = TRUE
      ),
      yaxis = list(
        title = "Reversing Heat Capacity (J/g)",
        titlefont = list(size = 18, family = "Arial", color = "black", weight = "bold"),
        tickfont = list(size = 18, family = "Arial", color = "black"),
        linecolor = "black", 
        linewidth = 0.5,
        showgrid = TRUE,
        gridcolor = "gray",
        gridwidth = 0.25
      ),
      yaxis2 = list(
        title = "DC Value",
        titlefont = list(size = 18, family = "Arial", color = "black", weight = "bold"),
        tickfont = list(size = 18, family = "Arial", color = "black"),
        overlaying = "y",
        side = "right",
        showgrid = FALSE
      ),
      legend = list(
        x = 0,  # Position legend at the top-left
        y = 1,
        xanchor = "left",  # Anchor to the left of the plot
        yanchor = "top",  # Anchor to the top of the plot
        font = list(size = 16, family = "Arial", color = "black")
      ),
      margin = list(t = 100, r = 100, b = 100, l = 100)  # Padding around the plot
    )
}

#***--------------------------------Raw data------------------------**#

Original_data <- function(orgData, modulations_back, fileName) {
  
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]
  
  orgDataSlice <- orgData %>% 
    slice(seq(1, n(), by = 50))
  
  oD <- ggplot(orgDataSlice, aes(x = time, y = modHeatFlow)) +
    geom_line(color = "black", linewidth = 1) +  
    labs(
      title = "Raw, unmodified modulated total heat flow data", 
      subtitle = subtitle, 
      x = "Time (min)",
      y = "Heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", size = 0.5),
      panel.grid.major = element_line(color = "gray", size = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", size = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))
  
  # Convert to plotly and add hover text
  oD <- plotly::ggplotly(oD, tooltip = "text") %>%
    plotly::style(text = paste0(
      "Time: ", orgDataSlice$time, " min", "<br>",
      "Heat Flow: ", orgDataSlice$modHeatFlow, "<br>"
    ))
  
  return(oD)  
}


Datasteps_plot_1 <- function(d_steps_cleaned, modulations_back, fileName) {
  d_steps_cleanedSlice <- d_steps_cleaned %>% 
    slice(seq(1, n(), by = 50))
  
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]  
  
  
  dStepsCleanedSlice <- ggplot(d_steps_cleanedSlice, aes(x = time, y = modHeatFlow)) +
    geom_line(color = "black", size = 1) +  
    labs(
      title = "Cleaned raw total heat flow data after\nremoving temperatures between steps",
      subtitle = subtitle, 
      x = "Time (min)",
      y = "Heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", size = 0.5),
      panel.grid.major = element_line(color = "gray", size = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(320, 20, 20, 20),
      axis.ticks = element_line(color = "black", size = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))
  
  # Convert to plotly and add hover text
  dStepsCleanedSlice <- plotly::ggplotly(dStepsCleanedSlice, tooltip = "text") %>%
    plotly::style(text = paste0(
      "Time: ", d_steps_cleanedSlice$time, " min", "<br>",
      "Heat Flow: ", d_steps_cleanedSlice$modHeatFlow, "<br>"
    ))
  
  return(dStepsCleanedSlice)  
}



Datasteps_plot_prefinal <- function(d_steps_cleaned_2, modulations_back, fileName) {
  d_steps_cleaned_2Slice <- d_steps_cleaned_2 %>% 
    slice(seq(1, n(), by = 50))
  
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]  
  
  
  dStepsCleaned_2Slice <- ggplot(d_steps_cleaned_2Slice, aes(x = time, y = modHeatFlow)) +
    geom_line(color = "black", linewidth = 1) +  
    labs(
      title = "Cleaned raw total heat flow data after\nremoving noisy pattern at the end of each step",
      subtitle = subtitle, 
      x = "Time (min)",
      y = "Heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 20)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", size = 0.5),
      panel.grid.major = element_line(color = "gray", size = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", size = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = num_ticks)) 

  # Convert to plotly and add hover text
  dStepsCleaned_2Slice <- plotly::ggplotly(dStepsCleaned_2Slice, tooltip = "text") %>%
    plotly::style(text = paste0(
      "Time: ", d_steps_cleaned_2Slice$time, " min", "<br>",
      "Heat Flow: ", d_steps_cleaned_2Slice$modHeatFlow, "<br>"
    ))
  
  return(dStepsCleaned_2Slice)  
}



Datasteps_plot_final <- function(d_steps_cleaned_3, modulations_back, fileName) {
  d_steps_cleaned_3Slice <- d_steps_cleaned_3 %>% 
    slice(seq(1, n(), by = 1))
  
  plottitledStepsCleaned_3Slice <- paste0("Raw modulated heat flow data used in final calculation, ", modulations_back, " modulations")
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]  
  
  dStepsCleaned_3Slice <- ggplot(d_steps_cleaned_3Slice, aes(x = time, y = modHeatFlow)) +
    geom_line(color = "black", size = 1) +  
    labs(
      title = plottitledStepsCleaned_3Slice,
      subtitle = subtitle,
      x = "Time (min)",
      y = "Heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", size = 0.5),
      panel.grid.major = element_line(color = "gray", size = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", size = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))
  
  # Convert to plotly and add hover text
  dStepsCleaned_3Slice <- plotly::ggplotly(dStepsCleaned_3Slice, tooltip = "text") %>%
    plotly::style(text = paste0(
      "Time: ", d_steps_cleaned_3Slice$time, " min", "<br>",
      "Heat Flow: ", d_steps_cleaned_3Slice$modHeatFlow, "<br>",
      "TRef: ", d_steps_cleaned_3Slice$TRef
    ))
  
  return(dStepsCleaned_3Slice)  
}



#****-------------------------Extremas-------------------------------------*
Maxima_minima <- function(extramadf, modulations_back, fileName) {
  
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]  
  
  Max_min <- ggplot(extramadf, aes(x = time, y = modHeatFlow)) +
    geom_point(color = "black", size = 1) +  
    labs(
      title = "Maxima and minima of oscillations\nafter removing temperatures in between temperature steps",
      subtitle = subtitle,
      x = "Time (min)",
      y = "Heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", size = 0.5),
      panel.grid.major = element_line(color = "gray", size = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(320, 20, 20, 20),
      axis.ticks = element_line(color = "black", size = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))
  
  return(Max_min)  
}


Maxima_minima_1 <- function(extramadf2, modulations_back, fileName) {
  
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]  
  
  Max_min_1 <- ggplot(extramadf2, aes(x = time, y = modHeatFlow)) +
    geom_point(color = "black", size = 1) +  
    labs(
      title = "Maxima and minima of oscillations after 2 cleaning steps\n(removing noise after the last maximum + initial cleaning) ",
      subtitle = subtitle,
      x = "Time (min)",
      y = "Heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", size = 0.5),
      panel.grid.major = element_line(color = "gray", size = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(320, 20, 20, 20),
      axis.ticks = element_line(color = "black", size = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))

  return(Max_min_1)  
}

Maxima_minima_2 <- function(extramadf3, modulations_back, fileName) {
  
  plottitlemaxmin2 <- paste0("Maxima and minima of oscillations used in calculations, ", modulations_back, " modulations")
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]
  
  Max_min_2 <- ggplot(extramadf3, aes(x = time, y = modHeatFlow)) +
    geom_point(color = "black", size = 1) +  
    labs(
      title = plottitlemaxmin2,
      subtitle = subtitle,
      x = "Time (min)",
      y = "Heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", size = 0.5),
      panel.grid.major = element_line(color = "gray", size = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", size = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))
  
  return(Max_min_2)  
}

