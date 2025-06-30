num_ticks <- 10

#***--------------------------------Normal resulting plots------------------------**#

quasi_isothermal_NRHF_plot <- function(results, modulationsBack, fileName) {
  
  plotTitleTHF <- paste0("NRHF based on FT, ", modulationsBack, " modulations")
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]

  NRHF_p <- ggplot(results, aes(x = TRef)) +
          geom_line(aes(y = NRHF), color = "black", linewidth = 1.2) +     # Smoothed data with thicker line
          labs(
            title = plotTitleTHF,
            subtitle = subtitle,
            x = "Temperature (°C)",
            y = "Non Reversing Heat Flow (W/g)"
          ) +
          theme_minimal(base_size = 18) +  # Larger base font size for better readability
          theme(
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                      color = "black"), # Center title with bold font
            axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
            axis.title.y = element_text(size = 18, face = "bold", color = "black", 
                                        margin = margin(r = 10)),  # Bold and separated y-axis title
            axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
            axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
            panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
            panel.grid.minor = element_blank(),  # Minor grid lines removed
            plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
            axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
          ) +
          scale_x_continuous(expand = c(0.005, 0.005), 
                             breaks = scales::pretty_breaks(n = num_ticks)) +  # Set the number of x-axis ticks
          scale_y_continuous(
            expand = c(0.0002, 0.0002) # Remove space between plot and y-axis
          )  # This ensures the y-axis covers the full range of your data with extra space at the top

 return(NRHF_p)  # <--- Ensure the function returns the ggplot object
 }

quasi_isothermal_RevCp_plot <- function(results, modulationsBack, fileName) {
  
  plottitleRevCp <- paste0("RevCp based on FT, ", 
                           modulationsBack, " modulations")
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]
  
    RevCp_p <- ggplot(results, aes(x = TRef)) +
      geom_line(aes(y = RevCp), color = "black", linewidth = 1.2) +     # Smoothed data with thicker line
      labs(
        title = plottitleRevCp,
        subtitle = subtitle,
        x = "Temperature (°C)",
        y = "Reversing Heat Capacity (J/g)"
      ) +
      theme_minimal(base_size = 18) +  # Larger base font size for better readability
      theme(
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                  color = "black"), # Center title with bold font
        axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
        axis.title.y = element_text(size = 18, face = "bold", color = "black", 
                                    margin = margin(r = 10)),  # Bold and separated y-axis title
        axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
        axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
        panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
        panel.grid.minor = element_blank(),  # Minor grid lines removed
        plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
        axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
      ) +
      scale_x_continuous(expand = c(0.005, 0.005),
                         breaks = scales::pretty_breaks(n = num_ticks)) +  # Set the number of x-axis ticks
      scale_y_continuous(
        expand = c(0.0002, 0.0002) # Remove space between plot and y-axis
      )  # This ensures the y-axis covers the full range of your data with extra space at the top

  return(RevCp_p)  # <--- Ensure the function returns the ggplot object
    
} 


quasi_isothermal_Manual_RevCp_plot <- function(resultsNoFT, modulationsBack, fileName) {
  
  plottitleRevCpmanual <- paste0("RevCp no FT, ",
                                 modulationsBack, " modulations")
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]

  manRevCp_p <- ggplot(resultsNoFT, aes(x = Tref)) +
    geom_line(aes(y = RevCpManual), color = "black", linewidth = 1.2) +     # Smoothed data with thicker line
    labs(
      title = plottitleRevCpmanual,
      subtitle = subtitle,
      x = "Temperature (°C)",
      y = "Reversing Heat Capacity (J/g)"
    ) +
    theme_minimal(base_size = 18) +  # Larger base font size for better readability
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                color = "black"), # Center title with bold font
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
      axis.title.y = element_text(size = 18, face = "bold", color = "black", 
                                  margin = margin(r = 10)),  # Bold and separated y-axis title
      axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
      axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
    ) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = scales::pretty_breaks(n = num_ticks)) +  # Set the number of x-axis ticks
    scale_y_continuous(
      expand = c(0, 0)  # Remove space between plot and y-axis
    )  # This ensures the y-axis covers the full range of your data with extra space at the top
  
  return(manRevCp_p)  # <--- Ensure the function returns the ggplot object
}


#***--------------------------------Overlays------------------------**#

quasi_isothermal_RevCp_NRHF_plot <- function(results, modulationsBack, fileName) {
  plottitleoverlay <- paste0("Overlay of RevCp and NRHF, ", 
                             modulationsBack, " modulations")
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]
  
  plot_ly(results) %>%
    add_lines(
      x = ~TRef, 
      y = ~RevCp, 
      name = "Reversing Heat Flow",
      line = list(color = "black", width = 3),  # Line width matches ggplot linewidth = 1.2 (~3 px)
      yaxis = "y"
    ) %>%
    add_lines(
      x = ~TRef, 
      y = ~NRHF, 
      name = "NRHF",
      line = list(color = "red", width = 3),  # Match line width for consistency
      yaxis = "y2"
    ) %>%
    layout(
      title = list(
        text = paste0(
          "<span style='font-size:20px; font-weight:bold'>", 
          plottitleoverlay, "</span>", "<br><span style='font-size:18px;'>", 
          subtitle, "</span>"
        ),
        x = 0.5,
        xanchor = "center",
        yanchor = "top"
      ),
      xaxis = list(
        title = "Temperature (°C)",
        titlefont = list(size = 18, family = "Arial", color = "black", bold = TRUE),
        tickfont = list(size = 18, family = "Arial", color = "black"),
        linecolor = "black", 
        linewidth = 2,  # Axis line width matches ggplot axis.line (0.5 pt ~ 2 px)
        mirror = FALSE,
        ticks = "outside",
        showgrid = TRUE,
        gridcolor = "gray",
        gridwidth = 1  # Matches ggplot panel.grid.major = 0.25 (scaled to px)
      ),
      yaxis = list(
        title = paste0(
          c(rep("&nbsp;", 10), 
            "Reversing Heat Capacity (J/g)", 
            rep("&nbsp;", 10)), 
          collapse = ""
        ),
        titlefont = list(size = 18, family = "Arial", color = "black", bold = TRUE),
        tickfont = list(size = 18, family = "Arial", color = "black"),
        linecolor = "black",
        linewidth = 2,
        mirror = TRUE,
        ticks = "outside",
        showgrid = TRUE,
        gridcolor = "gray",
        gridwidth = 1,
        tickangle = 0,
        ticklen = 5,
        tickwidth = 1,
        tickcolor = "black"
      ),
      yaxis2 = list(
        title = "Non-reversing heat flow (W/g)",
        titlefont = list(size = 18, family = "Arial", color = "black", bold = TRUE),
        tickfont = list(size = 18, family = "Arial", color = "black"),
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        zeroline = FALSE,
        ticklen = 5,
        tickwidth = 1,
        tickcolor = "black",
        standoff = 500    # <-- Same fix for the right y-axis
      ),
      legend = list(
        x = 1.02,             # Just right of the plot area
        y = 1,
        xanchor = "left",     # Anchor the LEFT side of the legend box at x = 1.02
        yanchor = "top",
        font = list(size = 16, family = "Arial", color = "black")
      ),
      margin = list(t = 80, r = 200, b = 80, l = 200),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
}


#***--------------------------------Raw data------------------------**#

quasi_isothermal_Original_data <- function(orgData, modulationsBack, fileName) {
  
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
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", 
                                  margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", linewidth = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))
  
  # Convert to plotly and add hover text
  oD <- plotly::ggplotly(oD, tooltip = "text") %>%
    plotly::style(text = paste0(
      "Time: ", orgDataSlice$time, " min", "<br>",
      "Heat Flow: ", orgDataSlice$modHeatFlow, "<br>"
    ))
  
  return(oD)  
}


quasi_isothermal_Datasteps_plot_1 <- function(isolatedPatterns, modulationsBack, fileName) {
  isolatedPatternsSlice <- isolatedPatterns %>% 
    slice(seq(1, n(), by = 50))
  
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]  
  
  
  dStepsCleanedSlice <- ggplot(isolatedPatternsSlice, aes(x = time, y = modHeatFlow)) +
    geom_line(color = "black", size = 1) +  
    labs(
      title = "Cleaned raw total heat flow data after\nremoving temperatures between steps",
      subtitle = subtitle, 
      x = "Time (min)",
      y = "Heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black",
                                  margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", linewidth = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))
  
  # Convert to plotly and add hover text
  dStepsCleanedSlice <- plotly::ggplotly(dStepsCleanedSlice, tooltip = "text") %>%
    plotly::style(text = paste0(
      "Time: ", isolatedPatternsSlice$time, " min", "<br>",
      "Heat Flow: ", isolatedPatternsSlice$modHeatFlow, "<br>"
    ))
  
  return(dStepsCleanedSlice)  
}



quasi_isothermal_Datasteps_plot_prefinal <- function(deleteLastMax, modulationsBack, fileName) {
  deleteLastMaxSlice <- deleteLastMax %>% 
    slice(seq(1, n(), by = 50))
  
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]  
  
  
  dStepsCleaned_2Slice <- ggplot(deleteLastMaxSlice, aes(x = time, y = modHeatFlow)) +
    geom_line(color = "black", linewidth = 1) +  
    labs(
      title = "Cleaned raw total heat flow data after\nremoving noisy pattern at the end of each step",
      subtitle = subtitle, 
      x = "Time (min)",
      y = "Heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", 
                                  margin = margin(r = 20)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", linewidth = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = scales::pretty_breaks(n = num_ticks)) 

  # Convert to plotly and add hover text
  dStepsCleaned_2Slice <- plotly::ggplotly(dStepsCleaned_2Slice, tooltip = "text") %>%
    plotly::style(text = paste0(
      "Time: ", deleteLastMaxSlice$time, " min", "<br>",
      "Heat Flow: ", deleteLastMaxSlice$modHeatFlow, "<br>"
    ))
  
  return(dStepsCleaned_2Slice)  
}



quasi_isothermal_Datasteps_plot_final <- function(finalDataForAnalysis, modulationsBack, fileName) {
  finalDataForAnalysisSlice <- finalDataForAnalysis %>% 
    slice(seq(1, n(), by = 1))
  
  plottitledStepsCleaned_3Slice <- paste0("Raw modulated heat flow data used in final calculation, ", 
                                          modulationsBack, " modulations")
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]  
  
  dStepsCleaned_3Slice <- ggplot(finalDataForAnalysisSlice, aes(x = time, y = modHeatFlow)) +
    geom_line(color = "black", linewidth = 1) +  
    labs(
      title = plottitledStepsCleaned_3Slice,
      subtitle = subtitle,
      x = "Time (min)",
      y = "Heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold",
                                color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", 
                                  margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", linewidth = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))
  
  # Convert to plotly and add hover text
  dStepsCleaned_3Slice <- plotly::ggplotly(dStepsCleaned_3Slice, tooltip = "text") %>%
    plotly::style(text = paste0(
      "Time: ", finalDataForAnalysisSlice$time, " min", "<br>",
      "Heat Flow: ", finalDataForAnalysisSlice$modHeatFlow, "<br>",
      "TRef: ", finalDataForAnalysisSlice$TRef
    ))
  
  return(dStepsCleaned_3Slice)  
}



#****-------------------------Extremas-------------------------------------*
quasi_isothermal_Maxima_minima <- function(extramadf, modulationsBack, fileName) {
  
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
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", 
                                  margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", linewidth = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))
  
  return(Max_min)  
}


quasi_isothermal_Maxima_minima_1 <- function(extramadf2, modulationsBack, fileName) {
  
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
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", 
                                  margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", linewidth = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))

  return(Max_min_1)  
}

quasi_isothermal_Maxima_minima_2 <- function(extramadf3, modulationsBack, fileName) {
  
  plottitlemaxmin2 <- paste0("Maxima and minima of oscillations used in calculations, ", 
                             modulationsBack, " modulations")
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
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black",
                                  margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", linewidth = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))
  
  return(Max_min_2)  
}

#***--------------------------------Data to accommodate export-----------------------**#
quasi_isothermal_Original_dataggplot <- function(orgData, modulationsBack, fileName) {
  
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
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", 
                                  margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", linewidth = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))
  
  return(oD)  
}


quasi_isothermal_Datasteps_plot_1ggplot <- function(d_steps_cleaned, modulationsBack, fileName) {
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
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", 
                                  margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", linewidth = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))
  
  
  return(dStepsCleanedSlice)  
}



quasi_isothermal_Datasteps_plot_prefinalggplot <- function(deleteLastMax, modulationsBack, fileName) {
  deleteLastMaxSlice <- deleteLastMax %>% 
    slice(seq(1, n(), by = 50))
  
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]  
  
  
  dStepsCleaned_2Slice <- ggplot(deleteLastMaxSlice, aes(x = time, y = modHeatFlow)) +
    geom_line(color = "black", linewidth = 1) +  
    labs(
      title = "Cleaned raw total heat flow data after\nremoving noisy pattern at the end of each step",
      subtitle = subtitle, 
      x = "Time (min)",
      y = "Heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", 
                                  margin = margin(r = 20)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", linewidth = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = scales::pretty_breaks(n = num_ticks)) 
  
  return(dStepsCleaned_2Slice)  
}



quasi_isothermal_Datasteps_plot_finalggplot <- function(finalDataForAnalysis, modulationsBack, fileName) {
  finalDataForAnalysisSlice <- finalDataForAnalysis %>% 
    slice(seq(1, n(), by = 1))
  
  plottitledStepsCleaned_3Slice <- paste0("Raw modulated heat flow data used in final calculation, ", 
                                          modulationsBack, " modulations")
  subtitle <- unlist(strsplit(fileName, "[.]"))[1]  
  
  dStepsCleaned_3Slice <- ggplot(finalDataForAnalysisSlice, aes(x = time, y = modHeatFlow)) +
    geom_line(color = "black", size = 1) +  
    labs(
      title = plottitledStepsCleaned_3Slice,
      subtitle = subtitle,
      x = "Time (min)",
      y = "Heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", 
                                  margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", linewidth = 0.5)
    ) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = scales::pretty_breaks(n = num_ticks)) +
    scale_y_continuous(expand = c(0, 0))
  
  return(dStepsCleaned_3Slice)  
}
