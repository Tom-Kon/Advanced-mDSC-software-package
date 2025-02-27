library(ggplot2)
library(plotly)

NRHF_plot <- function(sample_results) {
  
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
    ggsave(paste0(fileName, plotTitleTHF, ".png"), dpi = 600, width = 10, height = 10)
  }
 return(NRHF_p)  # <--- Ensure the function returns the ggplot object
 }

RHF_plot <- function(sample_results) {
    RHF_p <- ggplot(sample_results, aes(x = TRef)) +
      geom_line(aes(y = reversing_heat_flow), color = "black", linewidth = 1.2) +     # Smoothed data with thicker line
      labs(
        title = plottitleRHF,
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

  if(saveRHFplot == TRUE) {
    ggsave(paste0(fileName, plottitleRHF, ".png"), dpi = 600, width = 10, height = 10)
  }
  return(RHF_p)  # <--- Ensure the function returns the ggplot object
    
} 


RHF_NRHF_plot <- function(sample_results) {
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
        text = plottitleRHF,
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


Datasteps_plot <- function(data_steps_cleaned_4) {
  datasteps_p <- ggplot(data_steps_cleaned_4, aes(x = time, y = heat_flow)) +
    geom_line(color = "black", linewidth = 1) +  
    labs(
      title = "Temperature vs. Time",
      x = "Time (min)",
      y = "Temperature (°C)"
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
  datasteps_p <- plotly::ggplotly(datasteps_p, tooltip = "text") %>%
    plotly::style(text = paste0(
      "Time: ", data_steps_cleaned_4$time, " min", "<br>",
      "Heat Flow: ", data_steps_cleaned_4$heat_flow, "<br>",
      "TRef: ", data_steps_cleaned_4$TrefCleaned4
    ))
  
  return(datasteps_p)  
}



Manual_RHF_plot <- function(average_heat_flow_per_pattern) {
    manRHF_p <- ggplot(average_heat_flow_per_pattern, aes(x = Tref)) +
      geom_line(aes(y = RevCpManual), color = "black", linewidth = 1.2) +     # Smoothed data with thicker line
      labs(
        title = plottitleRHFmanual,
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
  if(savemanualRHFplot == TRUE){
    ggsave(paste0(fileName, plottitleRHFmanual, ".png"), dpi = 600, width = 10, height = 10)
  }
    return(manRHF_p)  # <--- Ensure the function returns the ggplot object
}
