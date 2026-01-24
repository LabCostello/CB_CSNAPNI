NinputtoCdf <- tibble("year" = rep(rep(year_labels,each=21),3),
           "Nsource" = rep(c("Synthetic Fertilizer", "Manure", "N Fixation"),each=126),
           "crop"=rep(cropname, 18),
           "values" = c(c(colSums(CfertNwswEsynth[,,])),c(colSums(CfertNwE[,,]-CfertNwswEsynth[,,])),c(colSums(CfixNwswE[,,]))))

#(NinputtoCdf %>% filter(year>1997) %>% group_by(year,Nsource) %>% summarise(sum(values))) %>% ggplot(data=.) + geom_col(aes(x= factor(year),y=`sum(values)`,fill=Nsource))

sumCkgwsnoEdf <- tibble("year"=rep(year_labels,each=21),
           "crop"=rep(cropname,6),
           "values"=c(sumCkgwsnoE))

sumCareawsnoEdf <- tibble("year"=rep(year_labels,each=21),
                        "crop"=rep(cropname,6),
                        "values"=c(croparea))

scientific_format_custom <- function(digits = 2) {
  function(x) {
    if (any(x == 0, na.rm = TRUE)) {
      # Handle zeros separately
      result <- ifelse(x == 0, "0.0e+00", sprintf("%.1e", x))
    } else {
      result <- sprintf("%.1e", x)
    }
    return(result)
  }
}

scientific_format_custom <- function(digits = 2) {
  function(x) {
    if (any(x == 0, na.rm = TRUE)) {
      # Handle zeros separately
      result <- ifelse(x == 0, "0", sprintf("%.1e", x))
    } else {
      result <- sprintf("%.1e", x)
    }
    
    # Clean up the scientific notation
    result <- gsub("\\.0e", "e", result)  # Remove .0 before e
    result <- gsub("e\\+0?", "e", result)  # Remove + and leading zeros in exponent
    result <- gsub("e-0?", "e-", result)   # Keep - but remove leading zeros
    
    return(result)
  }
}

scientific_format_custom <- function(digits = 2, round_to_one_threshold = 0.3) {
  function(x) {
    if (any(x == 0, na.rm = TRUE)) {
      # Handle zeros separately
      result <- ifelse(x == 0, "0", sprintf("%.1e", x))
    } else {
      result <- sprintf("%.1e", x)
    }
    
    # Extract coefficient and exponent parts
    parts <- strsplit(result, "e")
    
    for (i in seq_along(parts)) {
      if (length(parts[[i]]) == 2) {
        coeff <- as.numeric(parts[[i]][1])
        exp_part <- parts[[i]][2]
        
        # Round coefficients close to 1 (between 1-threshold and 1+threshold)
        if (abs(coeff - 1) <= round_to_one_threshold) {
          result[i] <- paste0("1e", exp_part)
        }
      }
    }
    
    # Clean up the scientific notation
    result <- gsub("\\.0e", "e", result)  # Remove .0 before e
    result <- gsub("e\\+0?", "e", result)  # Remove + and leading zeros in exponent
    result <- gsub("e-0?", "e-", result)   # Keep - but remove leading zeros
    result <- gsub("e0$", "", result)      # Remove e0 at the end (1e0 becomes 1)
    
    return(result)
  }
}

library(ggplot2)
library(dplyr)
library(scales)

# Filter for specific crops and years
selected_crops <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,16,19,20)
selected_years <- 4:6

# Filter the nitrogen input data
plot_data <- NinputtoCdf %>%
  filter(crop %in% cropname[selected_crops] & year %in% year_labels[selected_years])

# Filter the carbon data for the same crops and years
production_data <- sumCkgwsnoEdf %>%
  filter(crop %in% cropname[selected_crops] & year %in% year_labels[selected_years])

# Calculate scaling factor for secondary axis
# Get max values to determine appropriate scaling
max_n <- max(plot_data %>% group_by(crop, year) %>% summarise(total = sum(values)) %>% pull(total))
max_c <- max(production_data$values)
scale_factor <- max_n / max_c

# Add scaled carbon values to production_data for plotting
production_data$scaled_values <- production_data$values * scale_factor

# Create the plot
ggplot() +
  # Primary data: stacked bars for nitrogen inputs
  geom_col(data = plot_data, 
           aes(x = factor(year), y = values, fill = Nsource)) +
  # Secondary data: points/lines for carbon
  geom_point(data = production_data, 
             aes(x = factor(year), y = scaled_values), 
             color = "red", size = 2, alpha = 0.7) +
  geom_line(data = production_data, 
            aes(x = factor(year), y = scaled_values, group = 1), 
            color = "red", linewidth = 1, alpha = 0.7) +
  # Faceting
  facet_wrap(~crop, scales = "free_y") +
  # Primary y-axis with scientific notation
  scale_y_continuous(
    name = "Nitrogen Input (million kg)",
    labels = function(x) format(x / 1000000, digits = 2),
    # Secondary y-axis
    sec.axis = sec_axis(
      trans = ~ . / scale_factor,
      name = "Crop production (million kg)",
      labels = function(x) format(x / 1000000, digits = 2)
    )
  ) +
  # Colors
  scale_fill_manual(values = c("Synthetic Fertilizer" = "steelblue", 
                               "Manure" = "orange", 
                               "N Fixation" = "forestgreen")) +
  # Labels and theme
  labs(
    x = "Year",
    fill = "Nitrogen Source"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.y.right = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.text.y.right = element_text(size = 11, color = "black"),
    strip.text = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )

ggsave("nitrogen_PROD_plot.png", 
       width = 13, height = 8, 
       dpi = 300, 
       bg = "white")


# Graph Comparison CSNANI vs CBWCSNANI ####
create_animal_plot <- function(data, animal_label, title) {
  # Order data consistently
  data_ordered <- with(data, data[order(year, Nsources, model),])
  
  # Create plot for the specified animal
  ggplot(data=data_ordered[data_ordered$animal == animal_label,], aes(x=model, y=values)) + 
    geom_bar_pattern(
      aes(pattern = Nsources, fill = Nsources),
      stat = "identity",
      color = "black",  # Add black outline to bars
      pattern_fill = "black",
      pattern_density = 0.2,
      pattern_spacing = 0.037
    ) +
    scale_pattern_manual(values = c("stripe","circle", "circle", "none", "none")) +
    scale_fill_manual(values = c("white", "grey", "white", "black", "white")) +
    facet_grid(~year) +
    labs(title = title,
         x = "",
         y = "kg N input/kg protein") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          legend.title = element_text(color = "black", size = 16, face = "bold"),
          legend.text = element_text(color = "black", size = 14),
          axis.text = element_text(size = 14),
          axis.title.y = element_text(size = 16),
          strip.text.x = element_text(size = 12))
}

# Usage:
beefcomp <- create_animal_plot(cmdtcsnapni3, meat_labels[1], "Beef")
dairycomp <- create_animal_plot(cmdtcsnapni3, meat_labels[2], "Milk")
porkcomp <- create_animal_plot(cmdtcsnapni3, meat_labels[3], "Pork")
layerscomp <- create_animal_plot(cmdtcsnapni3, meat_labels[6], "Eggs")
broilcomp <- create_animal_plot(cmdtcsnapni3, meat_labels[7], "Chicken")
turkcomp <- create_animal_plot(cmdtcsnapni3, meat_labels[8], "Turkey")

library(ggpubr)

figureR1 <- ggarrange(beefcomp,dairycomp,porkcomp,layerscomp, broilcomp,turkcomp,ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
figureR1

labels <- c(
  "Fertilizer (national)" = "Fertilizer (national)",
  "Inorganic Fertilizer (cbw)" = "Synthetic Fertilizer (cbw)",  # Display label changed
  "Manure Fertilizer (cbw)" = "Manure Fertilizer (cbw)",
  "N Fixation (national)" = "N Fixation (national)",
  "N Fixation (cbw)" = "N Fixation (cbw)",
  "Manure Management" = "Manure Management"
)

# Commodity-specific graph for CSNANI with patterns ####
create_animal_plot2 <- function(data, animal_label, title) {
  # Define custom labels
  
  
  # Order data consistently
  data_ordered <- with(data, data[order(year, Nsource),])
  
  # Create plot for the specified animal
  ggplot(data=data_ordered[data_ordered$animal == animal_label,], aes(x=animal,y=values)) + 
    geom_bar_pattern(
      aes(pattern = Nsource, fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      pattern_fill = "black",
      pattern_density = 0.2,
      pattern_spacing = 0.037
    ) +
    scale_x_discrete(labels = NULL) + 
    scale_pattern_manual(
      values = c("stripe","circle", "circle", "none", "none", "none"),
      labels = labels
    ) +
    scale_fill_manual(
      values = c("white", "grey", "white", "black", "grey", "white"),
      labels = labels
    ) +
    facet_grid(~year) +
    labs(title = title,
         x = "",
         y = "kg N input/kg protein") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          legend.title = element_text(color = "black", size = 16, face = "bold"),
          legend.text = element_text(color = "black", size = 14),
          axis.text = element_text(size = 14),
          axis.title.y = element_text(size = 16),
          strip.text.x = element_text(size = 12))
}



# Usage:
beefcomp2 <- create_animal_plot2(cmdtcsnapni4, meat_labels[1], "Beef")
dairycomp2 <- create_animal_plot2(cmdtcsnapni4, meat_labels[2], "Milk")
porkcomp2 <- create_animal_plot2(cmdtcsnapni4, meat_labels[3], "Pork")
layerscomp2 <- create_animal_plot2(cmdtcsnapni4, meat_labels[6], "Eggs")
broilcomp2 <- create_animal_plot2(cmdtcsnapni4, meat_labels[7], "Chicken")
turkcomp2 <- create_animal_plot2(cmdtcsnapni4, meat_labels[8], "Turkey")

figureR2 <- ggarrange(beefcomp2,dairycomp2,porkcomp2,layerscomp2, broilcomp2,turkcomp2,ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
figureR2

# Commodity-specific graph for CSNANI with colors ####
create_animal_plot3 <- function(data, animal_label, title) {
  # Order data consistently
  data_ordered <- with(data, data[order(year, Nsource),])
  
  # Filter data for the specified animal
  animal_data <- data_ordered[data_ordered$animal == animal_label,]
  
  # Calculate totals for each year
  totals <- aggregate(values ~ year, data = animal_data, FUN = sum)
  
  # Create plot for the specified animal
  ggplot(data=animal_data, aes(x=animal, y=values)) + 
    geom_bar(
      aes(fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      position = "stack",
      width = 0.7      # Slightly narrower bars for better appearance
    ) +
    # Add total values as text on top of each bar
    geom_text(
      data = totals,
      aes(x = 1, y = values, label = format(values, digits = 3)),
      position = position_stack(vjust = 1.05),
      size = 4.5,
      fontface = "bold"  # Make totals bold for better visibility
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.15))) +  # Add more space at top for labels
    scale_x_discrete(labels = NULL) + 
    scale_fill_manual(values = c(
      "Fertilizer (national)" = "#780000",
      "Inorganic Fertilizer (cbw)" = "#C1121F",
      "Manure Fertilizer (cbw)" = "#CD3E43",
      "N Fixation (national)" = "#003049",
      "N Fixation (cbw)" = "#669BBC",
      "Manure Management" = "#FDF0D5"
    ),
    labels = c(
      "Fertilizer (national)" = "Fertilizer (national)",
      "Inorganic Fertilizer (cbw)" = "Synthetic Fertilizer (cbw)",  # Display label changed
      "Manure Fertilizer (cbw)" = "Manure Fertilizer (cbw)",
      "N Fixation (national)" = "N Fixation (national)",
      "N Fixation (cbw)" = "N Fixation (cbw)",
      "Manure Management" = "Manure Management"
    )) +
    facet_grid(~year) +
    labs(title = title,
         x = "",
         y = "kg N input/kg protein") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(b = 12)),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 18, face = "bold"),
      legend.text = element_text(color = "black", size = 16),
      axis.text = element_text(size = 16),
      axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 12)),
      strip.text.x = element_text(size = 16, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),  # Remove minor grid lines for cleaner look
      panel.spacing = unit(1.5, "lines"),  # Add more space between facets
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}

beefcomp3 <- create_animal_plot3(cmdtcsnapni4, meat_labels[1], "Beef")
dairycomp3 <- create_animal_plot3(cmdtcsnapni4, meat_labels[2], "Milk")
porkcomp3 <- create_animal_plot3(cmdtcsnapni4, meat_labels[3], "Pork")
layerscomp3 <- create_animal_plot3(cmdtcsnapni4, meat_labels[6], "Eggs")
broilcomp3 <- create_animal_plot3(cmdtcsnapni4, meat_labels[7], "Chicken")
turkcomp3 <- create_animal_plot3(cmdtcsnapni4, meat_labels[8], "Turkey")

figureR3 <- ggarrange(beefcomp3,dairycomp3,porkcomp3,layerscomp3, broilcomp3,turkcomp3,ncol=3, nrow=2, common.legend = TRUE, legend="bottom")
figureR3

# Export for poster
ggsave("figureR3_poster2.png", figureR3, 
       width = 41.38, height = 25.86, units = "cm", dpi = 600)

# Total impact per commodity category graph SCENARIOS for CSNANI ####
datab3 <- get_scenario_results("baseline","cmdtcsnapni4") %>% filter(year == 2017)
datagp3 <- get_scenario_results("grass_profitability","cmdtcsnapni4") %>% filter(year == 2017)
datadc3 <- get_scenario_results("double_crop","cmdtcsnapni4") %>% filter(year == 2017)
datag2g3 <- get_scenario_results("biogas_grass_profit_cover_combo","cmdtcsnapni4") %>% filter(year == 2017)

# Combine all scenario data with custom scenario labels
combined_data3 <- bind_rows(
  datab3 %>% mutate(scenario = "Baseline"),
#  datagp3 %>% mutate(scenario = "Grass\nProfitability"),
#  datadc3 %>% mutate(scenario = "Double\nCrop"),
  datag2g3 %>% mutate(scenario = "Continuous cover")
)

# Modified function to facet by scenario instead of year
create_animal_plot3_scenarios <- function(data, animal_label, title) {
  # Order data consistently
  data_ordered <- with(data, data[order(scenario, Nsource),])
  
  # Filter data for the specified animal
  animal_data <- data_ordered[data_ordered$animal == animal_label,]
  
  # Calculate totals for each scenario
  totals <- aggregate(values ~ scenario, data = animal_data, FUN = sum)
  
  # Create plot for the specified animal
  ggplot(data=animal_data, aes(x=animal, y=values)) + 
    geom_bar(
      aes(fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      position = "stack",
      width = 0.7      # Slightly narrower bars for better appearance
    ) +
    # Add total values as text on top of each bar
    geom_text(
      data = totals,
      aes(x = 1, y = values, label = format(values, digits = 3)),
      position = position_stack(vjust = 1.05),
      size = 4.5,
      fontface = "bold"  # Make totals bold for better visibility
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.15))) +  # Add more space at top for labels
    scale_x_discrete(labels = NULL) + 
    scale_fill_manual(values = c(
      "Fertilizer (national)" = "#780000",
      "Inorganic Fertilizer (cbw)" = "#C1121F",
      "Manure Fertilizer (cbw)" = "#CD3E43",
      "N Fixation (national)" = "#003049",
      "N Fixation (cbw)" = "#669BBC",
      "Manure Management" = "#FDF0D5"
    ),
    labels = labels) +
    facet_grid(~scenario) +  # Changed from ~year to ~scenario
    labs(title = title,
         x = "",
         y = "kg N input/kg protein") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(b = 12)),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 18, face = "bold"),
      legend.text = element_text(color = "black", size = 16),
      axis.text = element_text(size = 16),
      axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 12)),
      strip.text.x = element_text(size = 12, face = "bold"),  # Smaller for longer scenario names
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),  # Remove minor grid lines for cleaner look
      panel.spacing = unit(1.5, "lines"),  # Add more space between facets
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}

# Create individual plots for each animal using meat_labels
beefcomp3_scenarios <- create_animal_plot3_scenarios(combined_data3, meat_labels[1], "Beef")
dairycomp3_scenarios <- create_animal_plot3_scenarios(combined_data3, meat_labels[2], "Milk")
porkcomp3_scenarios <- create_animal_plot3_scenarios(combined_data3, meat_labels[3], "Pork")
layerscomp3_scenarios <- create_animal_plot3_scenarios(combined_data3, meat_labels[6], "Eggs")
broilcomp3_scenarios <- create_animal_plot3_scenarios(combined_data3, meat_labels[7], "Chicken")
turkcomp3_scenarios <- create_animal_plot3_scenarios(combined_data3, meat_labels[8], "Turkey")

# Combine all plots using ggarrange with common legend
figureR3_scenarios <- ggarrange(
  beefcomp3_scenarios, dairycomp3_scenarios, porkcomp3_scenarios,
  layerscomp3_scenarios, broilcomp3_scenarios, turkcomp3_scenarios,
  ncol = 3, nrow = 2, 
  common.legend = TRUE, 
  legend = "bottom"
)

# Display the combined figure
figureR3_scenarios
ggsave("nitrogen_sources_cmdt_2017.png", 
       figureR3_scenarios, 
       width = 42.85, height = 25.5, units = "cm", dpi = 600)

# Total impact per commodity category graph for CSNANI with colors ####
create_animal_plot4 <- function(data, animal_label, title) {
  # Order data consistently
  data_ordered <- with(data, data[order(year, Nsource),])
  
  # Filter data for the specified animal
  animal_data <- data_ordered[data_ordered$animal == animal_label,]
  
  # Calculate totals for each year
  totals <- aggregate(totprodkgProt ~ year, data = animal_data, FUN = sum)
  
  # Create plot for the specified animal
  ggplot(data=animal_data, aes(x=animal, y=totprodkgProt)) + 
    geom_bar(
      aes(fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      position = "stack",
      width = 0.7      # Slightly narrower bars for better appearance
    ) +
    # Add total values as text on top of each bar
    geom_text(
      data = totals,
      aes(x = 1, y = totprodkgProt, label = format(totprodkgProt, scientific = FALSE, digits = 2)),
      position = position_stack(vjust = 1.05),
      size = 3.9,
      fontface = "bold"  # Make totals bold for better visibility
    ) +
    scale_y_continuous(
      labels = function(x) format(x, scientific = FALSE),
      expand = expansion(mult = c(0, 0.15))  # Add more space at top for labels
    ) +
    scale_x_discrete(labels = NULL) + 
    scale_fill_manual(values = c(
      "Fertilizer (national)" = "#780000",
      "Inorganic Fertilizer (cbw)" = "#C1121F",
      "Manure Fertilizer (cbw)" = "#CD3E43",
      "N Fixation (national)" = "#003049",
      "N Fixation (cbw)" = "#669BBC",
      "Manure Management" = "#FDF0D5"
    ), labels = labels) +
    facet_grid(~year) +
    labs(title = title,
         x = "",
         y= "Total N input (million kg N)")+
         #y = "Anthropogenic nitrogen (kg N)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(b = 12)),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 18, face = "bold"),
      legend.text = element_text(color = "black", size = 16),
      axis.text = element_text(size = 16),
      axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 12)),
      strip.text.x = element_text(size = 16, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),  # Remove minor grid lines for cleaner look
      panel.spacing = unit(1.5, "lines"),  # Add more space between facets
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}


data <- cmdtcsnapni4[cmdtcsnapni4$year>2007,]
data$totprodkgProt <- data$totprodkgProt/1000000
beefcomp4 <- create_animal_plot4(data, meat_labels[1], "Beef")
dairycomp4 <- create_animal_plot4(data, meat_labels[2], "Milk")
porkcomp4 <- create_animal_plot4(data, meat_labels[3], "Pork")
layerscomp4 <- create_animal_plot4(data, meat_labels[6], "Eggs")
broilcomp4 <- create_animal_plot4(data, meat_labels[7], "Chicken")
turkcomp4 <- create_animal_plot4(data, meat_labels[8], "Turkey")

figureR4 <- ggarrange(beefcomp4,dairycomp4,porkcomp4,layerscomp4, broilcomp4,turkcomp4,ncol=3, nrow=2, common.legend = TRUE, legend="bottom")
figureR4

# Total impact per commodity category graph for different scenarios ####

datab <- get_scenario_results("baseline","cmdtcsnapni4") %>% filter(year == 2017)
datagp <- get_scenario_results("grass_profitability","cmdtcsnapni4") %>% filter(year == 2017)
datadc <- get_scenario_results("double_crop","cmdtcsnapni4") %>% filter(year == 2017)
datag2g <- get_scenario_results("biogas_grass_profit_cover_combo","cmdtcsnapni4") %>% filter(year == 2017)

combined_data <- tibble(bind_rows(
  datab %>% mutate(scenario = "Baseline"),
#  datagp %>% mutate(scenario = "SWG"),
#  datadc %>% mutate(scenario = "WR"),
  datag2g %>% mutate(scenario = "Continuous cover")
)) %>% mutate(totprodkgProt = totprodkgProt/1000000)  # Convert to million kg N

combined_data <- combined_data %>% 
  mutate(Nsource = str_replace(Nsource, "Inorganic", "Synthetic"))

# Modified function to facet by scenario instead of year
create_animal_plot_scenarios <- function(data, animal_label, title) {
  # Order data consistently
  data_ordered <- with(data, data[order(scenario, Nsource),])
  
  # Filter data for the specified animal
  animal_data <- data_ordered[data_ordered$animal == animal_label,]
  
  # Calculate totals for each scenario
  totals <- aggregate(totprodkgProt ~ scenario, data = animal_data, FUN = sum)
  
  # Create plot for the specified animal
  ggplot(data=animal_data, aes(x=animal, y=totprodkgProt)) + 
    geom_bar(
      aes(fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      position = "stack",
      width = 0.7      # Slightly narrower bars for better appearance
    ) +
    # Add total values as text on top of each bar
    geom_text(
      data = totals,
      aes(x = 1, y = totprodkgProt, label = format(totprodkgProt, scientific = FALSE, digits = 3)),
      position = position_stack(vjust = 1.05),
      size = 6,
      fontface = "bold"  # Make totals bold for better visibility
    ) +
    scale_y_continuous(
      labels = function(x) format(x, scientific = FALSE),
      expand = expansion(mult = c(0, 0.15))  # Add more space at top for labels
    ) +
    scale_x_discrete(labels = NULL) + 
    scale_fill_manual(values = c(
      "Fertilizer (national)" = "#780000",
      "Inorganic Fertilizer (cbw)" = "#C1121F",
      "Manure Fertilizer (cbw)" = "#CD3E43",
      "N Fixation (national)" = "#003049",
      "N Fixation (cbw)" = "#669BBC",
      "Manure Management" = "#FDF0D5"
    ),
    labels = c(
      "Fertilizer (national)" = "Fertilizer (national)",
      "Inorganic Fertilizer (cbw)" = "Synthetic Fertilizer (cbw)",  # Display label changed
      "Manure Fertilizer (cbw)" = "Manure Fertilizer (cbw)",
      "N Fixation (national)" = "N Fixation (national)",
      "N Fixation (cbw)" = "N Fixation (cbw)",
      "Manure Management" = "Manure Management"
    )) +
    facet_grid(~scenario) +  # Facet by scenario
    labs(title = title,
         x = "",
         y = "Total N input (million kg N)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(b = 12)),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 18, face = "bold"),
      legend.text = element_text(color = "black", size = 18),
      axis.text = element_text(size = 18),
      axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 12)),
      strip.text.x = element_text(size = 14, face = "bold"),  # Smaller for longer scenario names
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),  # Remove minor grid lines for cleaner look
      panel.spacing = unit(1.5, "lines"),  # Add more space between facets
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}

# Create individual plots for each animal using meat_labels
beefcomp_scenarios <- create_animal_plot_scenarios(combined_data, meat_labels[1], "Beef")
dairycomp_scenarios <- create_animal_plot_scenarios(combined_data, meat_labels[2], "Milk")
porkcomp_scenarios <- create_animal_plot_scenarios(combined_data, meat_labels[3], "Pork")
layerscomp_scenarios <- create_animal_plot_scenarios(combined_data, meat_labels[6], "Eggs")
broilcomp_scenarios <- create_animal_plot_scenarios(combined_data, meat_labels[7], "Chicken")
turkcomp_scenarios <- create_animal_plot_scenarios(combined_data, meat_labels[8], "Turkey")

# Combine all plots using ggarrange with common legend
figureR4_scenarios <- ggarrange(
  beefcomp_scenarios, dairycomp_scenarios, porkcomp_scenarios,
  layerscomp_scenarios, broilcomp_scenarios, turkcomp_scenarios,
  ncol = 3, nrow = 2, 
  common.legend = TRUE, 
  legend = "bottom"
)

# Display the combined figure
figureR4_scenarios

ggsave("nitrogen_sources_all_animals_scenarios_2017.png", 
        figureR4_scenarios, 
        width = 44.4, height = 26.39, units = "cm", dpi = 600)

# Total impact per commodity category graph for CSNANI with pattern ####
create_animal_plot4 <- function(data, animal_label, title) {
  # Order data consistently
  data_ordered <- with(data, data[order(year, Nsource),])
  
  # Filter data for the specified animal
  animal_data <- data_ordered[data_ordered$animal == animal_label,]
  
  # Calculate totals for each year
  totals <- aggregate(totprodkgProt ~ year, data = animal_data, FUN = sum)
  
  # Create plot for the specified animal
  ggplot(data=animal_data, aes(x=animal, y=totprodkgProt)) + 
    geom_bar_pattern(
      aes(pattern = Nsource, fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      pattern_fill = "black",
      pattern_density = 0.2,
      pattern_spacing = 0.037,
      position = "stack",
      width = 0.7      # Slightly narrower bars for better appearance
    ) +
    scale_y_continuous(
      labels = function(x) format(x, scientific = TRUE),
      expand = expansion(mult = c(0, 0.15))  # Add more space at top for labels
    ) +
    scale_x_discrete(labels = NULL) + 
    scale_pattern_manual(values = c(
      "Fertilizer (national)" = "none",
      "Inorganic Fertilizer (cbw)" = "none",
      "Manure Fertilizer (cbw)" = "none",
      "N Fixation (national)" = "circle",
      "N Fixation (cbw)" = "circle",
      "Non-recoverable Manure" = "stripe"
    )) +
    scale_fill_manual(values = c(
      "Fertilizer (national)" = "black",
      "Synthetic Fertilizer (cbw)" = "white",
      "Manure Fertilizer (cbw)" = "grey",
      "N Fixation (national)" = "grey",
      "N Fixation (cbw)" = "white",
      "Non-recoverable Manure" = "white"
    )) +
    facet_grid(~year) +
    labs(title = title,
         x = "",
         y = "kg of N emission") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(b = 12)),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 18, face = "bold"),
      legend.text = element_text(color = "black", size = 16),
      axis.text = element_text(size = 16),
      axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 12)),
      strip.text.x = element_text(size = 16, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),  # Remove minor grid lines for cleaner look
      panel.spacing = unit(1.5, "lines"),  # Add more space between facets
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}
# For a high-quality PNG
#ggsave("figureR4_poster.png", figureR4, width = 10, height = 12, units = "in", dpi = 300)
# Export for poster
ggsave("figureR4_poster2.png", figureR4, 
       width = 41.38, height = 25.86, units = "cm", dpi = 600)

# Total impact per year category graph for CSNANI with colors ####
create_animal_plot_by_year <- function(data, year_value, title) {
  # Order data consistently
  data_ordered <- with(data, data[order(animal, Nsource),])
  
  # Filter data for the specified year
  year_data <- data_ordered[data_ordered$year == year_value,]
  
  # Create plot for the specified year with all animals
  ggplot(data=year_data, aes(x=animal, y=totprodkgProt)) + 
    geom_bar(
      aes(fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      position = "stack"
    ) +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +  # Format y-axis labels scientifically
    scale_fill_manual(values = c(
      "Fertilizer (national)" = "#780000",       
      "Fertilizer Inorganic (cbw)" = "#C1121F",  
      "Fertilizer Manure (cbw)" = "#CD3E43",     
      "Fixation (national)" = "#003049",         
      "Fixation (cbw)" = "#669BBC",              
      "Non-recoverable Manure" = "#FDF0D5"            
    )) +
    labs(title = paste(title, year_value),
         x = "",
         y = "kg N input") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          legend.title = element_text(color = "black", size = 16, face = "bold"),
          legend.text = element_text(color = "black", size = 14),
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Angled labels for better readability
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 16))
}

# Get unique years from your dataset
unique_years <- unique(cmdtcsnapni4$year)

# Create a plot for each year
year_plot1 <- create_animal_plot_by_year(cmdtcsnapni4, unique_years[1], "Animal Comparison -")
year_plot2 <- create_animal_plot_by_year(cmdtcsnapni4, unique_years[2], "Animal Comparison -")
year_plot3 <- create_animal_plot_by_year(cmdtcsnapni4, unique_years[3], "Animal Comparison -")
year_plot4 <- create_animal_plot_by_year(cmdtcsnapni4, unique_years[4], "Animal Comparison -")

# Arrange all plots in a 2x2 grid with common legend
figureR4_revised <- ggarrange(year_plot1, year_plot2, year_plot3, year_plot4, 
                              ncol=2, nrow=2, 
                              common.legend = TRUE, 
                              legend="bottom")

# Display the final arrangement
figureR4_revised

# To fix later graph Total impact per commodity category graph for CSNANI with pattern####
create_animal_plot4 <- function(data, animal_label, title, percent_change_data) {
  # Order data consistently
  data_ordered <- with(data, data[order(year, Nsource),])
  
  # Filter data for the specified animal
  animal_data <- data_ordered[data_ordered$animal == animal_label,]
  
  # Calculate totals for each year
  totals <- aggregate(totprodkgProt ~ year, data = animal_data, FUN = sum)
  
  # Filter percentage change data for the specified animal
  animal_percent <- percent_change_data[percent_change_data$animal == animal_label,]
  
  # Calculate scaling factors for secondary axis
  max_totprodkgProt <- max(totals$totprodkgProt)
  max_percent <- max(abs(animal_percent$percent_change)) * 1.2  # Add 20% for padding
  scaling_factor <- max_totprodkgProt / max_percent
  
  # Create plot for the specified animal
  ggplot(data=animal_data, aes(x=animal, y=totprodkgProt)) + 
    geom_bar_pattern(
      aes(pattern = Nsource, fill = Nsource),
      stat = "identity",
      color = "black",  # Add black outline to bars
      pattern_fill = "black",
      pattern_density = 0.1,
      pattern_spacing = 0.04,
      position = "stack",
      width = 0.7      # Slightly narrower bars for better appearance
    ) +
    # Add points to the percentage change line
    geom_point(
      data = animal_percent,
      aes(x = 1, y = percent_change * scaling_factor),
      color = "red",
      size = 3,
      shape = 18
    ) +
    # Add percentage values as text
    geom_text(
      data = animal_percent,
      aes(x = 1, y = percent_change * scaling_factor, 
          label = sprintf("%+.1f%%", percent_change)),
      color = "red",
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    scale_y_continuous(
      name = "kg of N emission",
      labels = function(x) format(x, scientific = TRUE),
      expand = expansion(mult = c(0, 0.2)),  # Add more space at top for labels
      sec.axis = sec_axis(
        ~./scaling_factor, 
        name = "% Change in Population (Base 2002)",
        labels = function(x) paste0(x, "%")
      )
    ) +
    scale_x_discrete(labels = NULL) + 
    scale_pattern_manual(values = c("stripe","circle", "circle", "none", "none", "none")) +
    scale_fill_manual(values = c("white", "grey", "white", "black", "grey", "white")) +
    facet_grid(~year) +
    labs(title = title,
         x = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(b = 12)),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 18, face = "bold"),
      legend.text = element_text(color = "black", size = 16),
      axis.text = element_text(size = 16),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 12)),
      axis.title.y.right = element_text(size = 18, face = "bold", color = "black", margin = margin(l = 12)),
      axis.text.y.right = element_text(color = "black"),
      strip.text.x = element_text(size = 16, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),  # Remove minor grid lines for cleaner look
      panel.spacing = unit(1.5, "lines"),  # Add more space between facets
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}

# First, calculate the percentage change data using 2002 as baseline
animpoptotal_percent <- animpoptotal_structured %>%
  group_by(animal) %>%
  filter(year>=2002) %>% 
  mutate(
    baseline = population[year == 2002],  # Get the 2002 value for each animal
    percent_change = (population - baseline) / baseline * 100  # Calculate % change
  ) %>%
  ungroup()

# Now create your plots with the updated function
beefcomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[1], "Beef", animpoptotal_percent)
dairycomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[2], "Milk", animpoptotal_percent)
porkcomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[3], "Pork", animpoptotal_percent)
layerscomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[6], "Eggs", animpoptotal_percent)
broilcomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[7], "Chicken", animpoptotal_percent)
turkcomp4 <- create_animal_plot4(cmdtcsnapni4, meat_labels[8], "Turkey", animpoptotal_percent)

# Create the combined figure
figureR4 <- ggarrange(beefcomp4, dairycomp4, porkcomp4, layerscomp4, broilcomp4, turkcomp4, 
                      ncol=3, nrow=2, common.legend = TRUE, legend="bottom")
figureR4

# Sankey Diagram ####
library(plotly)

majorgrains <- c(1:2,10:12)
othercrops <-  setdiff(1:21,majorgrains)
#cornbyproduct <- 19:21

# Define node names
nodes <- data.frame(name = c(
  "Synthetic Fertilizer",
  paste(cropareaharvested_key[majorgrains + 1, 1],"planted"),
  "other crops planted",
  "Nitrogen Fixation",
  "Imported Feed/Food",
  "Beef",
  "Dairy",
  "Swine",
  "Layers",
  "Broilers",
  "Turkey",
  "Animal commodity",
  "Manure",
  "Non-recoverable manure",
  "Manure Fertilizer",
  paste(cropareaharvested_key[majorgrains + 1, 1],"available"),
  "other crops available",
  "Byproduct imported",
  "Plant commodity"
#  "Corn byproducts planted",
#  "Corn byproducts available"
  ))

# Create source-target patterns more systematically
source_patterns <- c(
  rep(0, 6),            # Synthetic Fertilizer to crops planted
  rep(7, 6),            # Fixation to crops planted
  rep(8, 6),            # Imported pool to various crops available
  rep(19:23, 6),          # crops available to animals
  rep(24, 6),            # Crops available CGM to various animals
  19:24,                  # Available crops to Food
  9:14,                # Animal to Manure
  9:14,                # Animal to Food
  16, 16,               # Manure sending to Non-recoverable and Manure Fertilizer
  rep(18, 6),            # Manure Fertilizer to Crops planted
  1:6,                  # Crops produced to crops available
  8,                     # Imported feed food to animal byproducts
  rep(25, 6)           # Animal byproducts to animals
#  0,18,8,            # fertilizer to Corn byproducts
#  27,8,            # Corn byproducts and imported feed/food to Corn byproducts available
#  rep(28, 6)
)

target_patterns <- c(
  1:6,                  # Crops produced from Synthetic Fertilizer
  1:6,                  # Crops produced from Fixation
  19:24,                  # Available crops that were imported
  rep(9:14, each = 5), # Animal targets from major crops
  9:14,                # Animal targets from other crops
  rep(26, 6),           # Food as target (receiving from available crops)
  rep(16, 6),           # Manure as target
  rep(15, 6),           # Food as target again
  17, 18,               # Non recoverable manure and manure fertilizer
  1:6,                # Crops produced from Manure Fertilizer
  19:24,              # Crops available from crops produced
  25,                # Imported feed food to animal byproducts
  9:14                 # Animal byproducts to animals
#  27,27,27,                # Fertilizers to Corn byproducts planted
#  28,28,                # Corn byproducts to Corn byproducts available
#  9:14
)

# Calculate values more systematically
# Create a list to store values for each year
all_years_values <- list()

# Loop through 6 years
for(year in 1:6) {
  meatfeed <- data.frame("beef"= colSums(feedN4anim[c(1,10,11,13,15),,year]), # colSums(cropNtoanimtotal[c(1,10,11,13,15),,6])
                         "dairy"= colSums(feedN4anim[c(2,12,14,16),,year]),
                         "swine"= colSums(feedN4anim[c(3,4),,year]),
                         "layers"= (feedN4anim[5,,year]),
                         "broilers"= colSums(feedN4anim[c(7,8),,year]),
                         "turkey"= colSums(feedN4anim[c(6,9),,year]))
  
  meatfeedMG <- meatfeed[majorgrains,]
  meatfeedOC <- colSums(meatfeed[othercrops,])
  meatfeedCB <- colSums(meatfeed[cornbyproduct,])
  
  byproductimported <- (animpoptotal[,year]*(animNreq-rowSums(cropNtoanim[,,year])))
  
  values <- c(
    as.integer(CfertNinorgtot[majorgrains, year]), # Synth N
    sum(as.integer(CfertNinorgtot[othercrops, year])), # Synth N other crops
    colSums(CfixNwswE[, , year])[majorgrains], # Fix N
    sum(CfixNwswE[, othercrops, year]), # Fix N to other crops
    (colSums(feedN4meatimp[,,year]))[majorgrains], # Imported N
    sum((colSums(feedN4meatimp[,,year]))[othercrops]), # Imported N
    unlist(meatfeedMG),
    unlist(meatfeedOC),
    colSums(C4humanN[, , year])[majorgrains],
    sum(C4humanN[, othercrops, year]),
    sum(kgmanureNlrs[, c(1, 10, 11, 13, 15), year]),
    sum(kgmanureNlrs[, c(2, 12, 14, 16), year]),
    sum(kgmanureNlrs[, c(3, 4), year]),
    sum(kgmanureNlrs[, 5, year]),
    sum(kgmanureNlrs[, c(7, 8), year]),
    sum(kgmanureNlrs[, c(6, 9), year]),# sum(colSums(noanimdyncty[,c(6, 9),6])*t(animdatadyn[c(6, 9),10]))
    meatN[1,year], #beef animpoptotal_sort[1,6]*kgmeatperanim_avg[1,6]*ediblemeat[1]*meatdata[1,4]
    meatN[2,year], #milk
    meatN[3,year], #pork
    meatN[6,year], #eggs
    meatN[7,year], #chicken animpoptotal_sort[7,6]*kgmeatperanim_avg[7,6]*ediblemeat[7]*meatdata[7,4]
    meatN[8,year], #turkey
    sum(kgmanureNlrs[,animal_manure,year])-sum(kgmanureNrec450[,animal_manure,year]), # Here I added the turkey (6,9) to be non recoverable manure
      (colSums(kgmanureNlrsrecovnonavailableplant)+colSums(kgmanureNlrsavailableplantavailable))[year],
    as.integer(Cfertmantot[majorgrains, year]),
    sum(as.integer(Cfertmantot[othercrops, year])),
    ((colSums(Ckgws[,,year])*cropdata[,1]*cropdata[,2]))[majorgrains],
    sum(((colSums(Ckgws[,,year])*cropdata[,1]*cropdata[,2]))[othercrops]),
    sum(byproductimported[1:16]),
    c(sum(byproductimported[c(1, 10, 11, 13, 15)]),
      sum(byproductimported[c(2, 12, 14, 16)]),
      sum(byproductimported[c(3, 4)]),
      byproductimported[5],
      sum(byproductimported[c(7, 8)]),
      sum(byproductimported[c(6,9)]))
    #sum(as.integer(CfertNinorgtot[cornbyproduct, year])),
    #sum(as.integer(Cfertmantot[cornbyproduct, year])),
    #sum(((colSums(Ckgws[,,year])*cropdata[,1]*cropdata[,2]))[cornbyproduct])-sum(as.integer(CfertNinorgtot[cornbyproduct, year]))+sum(as.integer(Cfertmantot[cornbyproduct, year])),
    #sum(((colSums(Ckgws[,,year])*cropdata[,1]*cropdata[,2]))[cornbyproduct]),
    #sum((colSums(feedN4meatimp[,,year]))[cornbyproduct]), # Imported N
    #unlist(meatfeedOC)
  )
  
  all_years_values[[year]] <- values
}

# Convert to a matrix if needed (rows = variables, columns = years)
values_matrix <- do.call(cbind, all_years_values)
colnames(values_matrix) <- paste0("Year_", 1:n)
values_matrix[values_matrix[,1]<0,] <- 0
  
# Create links data frame in one step
links <- data.frame(
  source = source_patterns,
  target = target_patterns,
  value1997 = values_matrix[,1],
  value2002 = values_matrix[,2],
  value2007 = values_matrix[,3],
  value2012 = values_matrix[,4],
  value2017 = values_matrix[,5],
  value2022 = values_matrix[,6]
)
# Format numbers in a more readable way
format_numbers <- function(x) {
  ifelse(x >= 1e6, 
         paste0(round(x/1e6, 1), "M"),
         ifelse(x >= 1e3,
                paste0(round(x/1e3, 1), "K"),
                round(x, 1)))
}

# Calculate node values for 2002
node_values_2002 <- nodein_values_2002 <- nodeout_values_2002 <- numeric(length(nodes$name))
for(i in 1:length(nodes$name)) {
  # Sum of outgoing flows (as source)
  outgoing <- sum(links$value2002[links$source == (i-1)], na.rm = TRUE)
  # Sum of incoming flows (as target)  
  incoming <- sum(links$value2002[links$target == (i-1)], na.rm = TRUE)
  # Use the maximum of incoming or outgoing
  node_values_2002[i] <- max(outgoing, incoming, na.rm = TRUE)
  nodein_values_2002[i] <- incoming
  nodeout_values_2002[i] <- outgoing
}

# Calculate node values for 2022
node_values_2022 <- nodein_values_2022 <- nodeout_values_2022 <-  numeric(length(nodes$name))
for(i in 1:length(nodes$name)) {
  # Sum of outgoing flows (as source)
  outgoing <- sum(links$value2022[links$source == (i-1)], na.rm = TRUE)
  # Sum of incoming flows (as target)  
  incoming <- sum(links$value2022[links$target == (i-1)], na.rm = TRUE)
  # Use the maximum of incoming or outgoing
  node_values_2022[i] <- max(outgoing, incoming, na.rm = TRUE)
  nodein_values_2022[i] <- incoming
  nodeout_values_2022[i] <- outgoing
}

# Create labels with formatted values
node_labels_2002 <- paste0(nodes$name, "\n(", format_numbers(node_values_2002), ")")
node_labels_2002 <- paste0(
  nodes$name,
  ifelse(nodein_values_2002 > 0, paste0("\nIn: ", format_numbers(nodein_values_2002)), ""),
  ifelse(nodeout_values_2002 > 0, paste0("\nOut: ", format_numbers(nodeout_values_2002)), "")
)

node_labels_2022 <- paste0(nodes$name, "\n(", format_numbers(node_values_2022), ")")
node_labels_2022 <- paste0(nodes$name,
  #paste0(nodes$name," (",round(((nodeout_values_2022/nodein_values_2022)-1)*100),"%)"),
  ifelse(nodein_values_2022 > 0, paste0("\nIn: ", format_numbers(nodein_values_2022)), ""),
  ifelse(nodeout_values_2022 > 0, paste0("\nOut: ", format_numbers(nodeout_values_2022)), "")
)

# Create Sankey diagram for 2002 (Panel a)
fig_a <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    pad = 30,        # Increased padding for more space between nodes
    thickness = 25,   # Slightly thicker nodes
    line = list(color = "grey", width = 0.5),
    label = node_labels_2002  # Labels with formatted values for 2002
  ),
  link = list(
    source = links$source,
    target = links$target,
    value = links$value2002
  )
) %>% 
  layout(
    title = list(text = "<b>2002</b>",  # Bold title using HTML
                 font = list(size = 16)),
    font = list(size = 12)
  ) %>%
  config(displayModeBar = FALSE)

# Create Sankey diagram for 2022 (Panel b)
fig_b <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    pad = 30,        # Increased padding for more space between nodes
    thickness = 25,   # Slightly thicker nodes
    line = list(color = "grey", width = 0.5),
    label = node_labels_2022  # Labels with formatted values for 2022
  ),
  link = list(
    source = links$source,
    target = links$target,
    value = links$value2022
  )
) %>% 
  layout(
    title = list(text = "<b>2022</b>",  # Bold title using HTML
                 font = list(size = 16)),
    font = list(size = 12)
  ) %>%
  config(displayModeBar = FALSE)

# Display the plots
fig_a
fig_b
# Maps ####
library(ggplot2)
library(sf)

# Read shapefile
shapefile <- st_read("RawData/shp/P6Beta_v3_LRSegs_081516.shp")
cbw_shp <- shapefile[shapefile$CBW=="Y",]
cbw_shp <- cbw_shp %>% arrange(FIPS,LndRvrSeg)
rownames(cbw_shp) <- 1:nrow(cbw_shp)

ctiescbw_shp <- summarise(group_by(cbw_shp,cbw_shp$FIPS))

# Read dataframe
NANImap <- data.frame("LndRvrSeg" = CBW_lrs_shp$LndRvrSeg,
                      "NANIAtm" = NANIBtot[,1,5],
                      "NANIFertilizer"=NANIBtot[,3,5],
                      "NANIFix"= NANIBtot[,2,5],
                      "NANIFF" = NANIBtot[,4,5])

merged_data <- merge(cbw_shp, NANImap, by = "LndRvrSeg", all.x = TRUE)

# First, create the categorical variables with the improved breaks
merged_data <- merged_data %>%
  mutate(
    # For NANIAtm - based on quartiles and distribution
    NANIAtm_cat = cut(NANIAtm, 
                      breaks = c(-Inf, 5000, 15000, 50000, 150000, Inf),
                      labels = c("Very Low\n(< 5K)", "Low\n(5K - 15K)", "Medium\n(15K - 50K)", 
                                 "High\n(50K - 150K)", "Very High\n(> 150K)"),
                      include.lowest = TRUE),
    
    # For NANIFertilizer - heavily right-skewed
    NANIFertilizer_cat = cut(NANIFertilizer,
                             breaks = c(-Inf, 1000, 20000, 100000, 500000, Inf),
                             labels = c("Very Low\n(< 1K)", "Low\n(1K - 20K)", "Medium\n(20K - 100K)", 
                                        "High\n(100K - 500K)", "Very High\n(> 500K)"),
                             include.lowest = TRUE),
    
    # For NANIFix - similar to fertilizer but smaller range
    NANIFix_cat = cut(NANIFix,
                      breaks = c(-Inf, 1000, 15000, 90000, 400000, Inf),
                      labels = c("Very Low\n(< 1K)", "Low\n(1K - 15K)", "Medium\n(15K - 90K)", 
                                 "High\n(90K - 400K)", "Very High\n(> 400K)"),
                      include.lowest = TRUE),
    
    # For NANIFF - has negative values (deficit/surplus concept)
    NANIFF_cat = cut(NANIFF,
                     breaks = c(-Inf, -10000, 0, 50000, 200000, Inf),
                     labels = c("Large Deficit\n(< -10K)", "Small Deficit\n(-10K to 0)", 
                                "Small Surplus\n(0 - 50K)", "Medium Surplus\n(50K - 200K)", 
                                "Large Surplus\n(> 200K)"),
                     include.lowest = TRUE)
  )

# Define consistent color palette for the first three maps (progression from light to dark)
consistent_colors <- c(
  "Very Low\n(< 5K)" = "#FFF7EC",    # Very light cream
  "Very Low\n(< 1K)" = "#FFF7EC",    # Very light cream
  "Low\n(5K - 15K)" = "#FEE8C8",     # Light orange
  "Low\n(1K - 20K)" = "#FEE8C8",     # Light orange
  "Low\n(1K - 15K)" = "#FEE8C8",     # Light orange
  "Medium\n(15K - 50K)" = "#FDD49E", # Medium orange
  "Medium\n(20K - 100K)" = "#FDD49E", # Medium orange
  "Medium\n(15K - 90K)" = "#FDD49E",  # Medium orange
  "High\n(50K - 150K)" = "#FD8D3C",  # Dark orange
  "High\n(100K - 500K)" = "#FD8D3C", # Dark orange
  "High\n(90K - 400K)" = "#FD8D3C",  # Dark orange
  "Very High\n(> 150K)" = "#D94701", # Very dark orange/red
  "Very High\n(> 500K)" = "#D94701", # Very dark orange/red
  "Very High\n(> 400K)" = "#D94701"  # Very dark orange/red
)

# Special color palette for NANIFF (deficit in blue, surplus in orange)
naniff_colors <- c(
  "Large Deficit\n(< -10K)" = "#08519C",     # Dark blue
  "Small Deficit\n(-10K to 0)" = "#6BAED6", # Light blue
  "Small Surplus\n(0 - 50K)" = "#FEE8C8",   # Light orange
  "Medium Surplus\n(50K - 200K)" = "#FD8D3C", # Medium orange
  "Large Surplus\n(> 200K)" = "#D94701"     # Dark orange
)

# Plot A - Atmospheric Deposition
a <- ggplot() +
  geom_sf(data = merged_data, aes(fill = NANIAtm_cat), color = "#666666", size = 0.1) +
  geom_sf(data = ctiescbw_shp, color = "black", fill = NA, size = 0.3) +
  scale_fill_manual(
    name = "Atmospheric\nDeposition\n(kg N/yr)",
    values = consistent_colors,
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE
  ))

# Plot B - Fertilizer
b <- ggplot() +
  geom_sf(data = merged_data, aes(fill = NANIFertilizer_cat), color = "#666666", size = 0.1) +
  geom_sf(data = ctiescbw_shp, color = "black", fill = NA, size = 0.3) +
  scale_fill_manual(
    name = "Fertilizer\nApplication\n(kg N/yr)",
    values = consistent_colors,
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE
  ))

# Plot C - Nitrogen Fixation
c <- ggplot() +
  geom_sf(data = merged_data, aes(fill = NANIFix_cat), color = "#666666", size = 0.1) +
  geom_sf(data = ctiescbw_shp, color = "black", fill = NA, size = 0.3) +
  scale_fill_manual(
    name = "Nitrogen\nFixation\n(kg N/yr)",
    values = consistent_colors,
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE
  ))

# Plot D - Food and Feed (with special blue-orange color scheme)
d <- ggplot() +
  geom_sf(data = merged_data, aes(fill = NANIFF_cat), color = "#666666", size = 0.1) +
  geom_sf(data = ctiescbw_shp, color = "black", fill = NA, size = 0.3) +
  scale_fill_manual(
    name = "Food & Feed\nBalance\n(kg N/yr)",
    values = naniff_colors,
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE
  ))

library(patchwork)

# Combine the plots
(a + b) / (c + d)
a/b/c/d

# First, create the categorical variables with the improved breaks
merged_data <- merged_data %>%
  mutate(
    # For NANIAtm - based on quartiles and distribution
    NANIAtm_cat = cut(NANIAtm, 
                      breaks = c(-Inf, 5000, 15000, 50000, 150000, Inf),
                      labels = c("Very Low\n(< 5K)", "Low\n(5K - 15K)", "Medium\n(15K - 50K)", 
                                 "High\n(50K - 150K)", "Very High\n(> 150K)"),
                      include.lowest = TRUE),
    
    # For NANIFertilizer - heavily right-skewed
    NANIFertilizer_cat = cut(NANIFertilizer,
                             breaks = c(-Inf, 1000, 20000, 100000, 500000, Inf),
                             labels = c("Very Low\n(< 1K)", "Low\n(1K - 20K)", "Medium\n(20K - 100K)", 
                                        "High\n(100K - 500K)", "Very High\n(> 500K)"),
                             include.lowest = TRUE),
    
    # For NANIFix - similar to fertilizer but smaller range
    NANIFix_cat = cut(NANIFix,
                      breaks = c(-Inf, 1000, 15000, 90000, 400000, Inf),
                      labels = c("Very Low\n(< 1K)", "Low\n(1K - 15K)", "Medium\n(15K - 90K)", 
                                 "High\n(90K - 400K)", "Very High\n(> 400K)"),
                      include.lowest = TRUE),
    
    # For NANIFF - has negative values (deficit/surplus concept)
    NANIFF_cat = cut(NANIFF,
                     breaks = c(-Inf, -10000, 0, 50000, 200000, Inf),
                     labels = c("Large Deficit\n(< -10K)", "Small Deficit\n(-10K to 0)", 
                                "Small Surplus\n(0 - 50K)", "Medium Surplus\n(50K - 200K)", 
                                "Large Surplus\n(> 200K)"),
                     include.lowest = TRUE)
  )

# Define consistent color palette for the first three maps
consistent_colors <- c(
  "Very Low\n(< 5K)" = "#FFF7EC",    
  "Very Low\n(< 1K)" = "#FFF7EC",    
  "Low\n(5K - 15K)" = "#FEE8C8",     
  "Low\n(1K - 20K)" = "#FEE8C8",     
  "Low\n(1K - 15K)" = "#FEE8C8",     
  "Medium\n(15K - 50K)" = "#FDD49E", 
  "Medium\n(20K - 100K)" = "#FDD49E", 
  "Medium\n(15K - 90K)" = "#FDD49E",  
  "High\n(50K - 150K)" = "#FD8D3C",  
  "High\n(100K - 500K)" = "#FD8D3C", 
  "High\n(90K - 400K)" = "#FD8D3C",  
  "Very High\n(> 150K)" = "#D94701", 
  "Very High\n(> 500K)" = "#D94701", 
  "Very High\n(> 400K)" = "#D94701"  
)

# Special color palette for NANIFF
naniff_colors <- c(
  "Large Deficit\n(< -10K)" = "#08519C",     
  "Small Deficit\n(-10K to 0)" = "#6BAED6", 
  "Small Surplus\n(0 - 50K)" = "#FEE8C8",   
  "Medium Surplus\n(50K - 200K)" = "#FD8D3C", 
  "Large Surplus\n(> 200K)" = "#D94701"     
)

# Common theme for all plots - optimized for map visibility
common_theme <- theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.6, "cm"),        # Much smaller legend squares
    legend.key.width = unit(0.8, "cm"),       # Narrower legend keys
    legend.key.height = unit(0.4, "cm"),      # Shorter legend keys
    legend.margin = margin(5, 0, 5, 0),       # Less margin around legend
    legend.box.margin = margin(5, 0, 0, 0),   # Less space above legend
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(5, 5, 10, 5)         # Smaller plot margins
  )

# Plot A - Atmospheric Deposition
a <- ggplot() +
  geom_sf(data = merged_data, aes(fill = NANIAtm_cat), color = "#666666", size = 0.1) +
  geom_sf(data = ctiescbw_shp, color = "black", fill = NA, size = 0.3) +
  scale_fill_manual(
    name = "Atmospheric Deposition (kg N/yr)",
    values = consistent_colors,
    na.value = "grey90"
  ) +
  common_theme +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE
  ))

# Plot B - Fertilizer
b <- ggplot() +
  geom_sf(data = merged_data, aes(fill = NANIFertilizer_cat), color = "#666666", size = 0.1) +
  geom_sf(data = ctiescbw_shp, color = "black", fill = NA, size = 0.3) +
  scale_fill_manual(
    name = "Fertilizer Application (kg N/yr)",
    values = consistent_colors,
    na.value = "grey90"
  ) +
  common_theme +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE
  ))

# Plot C - Nitrogen Fixation
c <- ggplot() +
  geom_sf(data = merged_data, aes(fill = NANIFix_cat), color = "#666666", size = 0.1) +
  geom_sf(data = ctiescbw_shp, color = "black", fill = NA, size = 0.3) +
  scale_fill_manual(
    name = "Nitrogen Fixation (kg N/yr)",
    values = consistent_colors,
    na.value = "grey90"
  ) +
  common_theme +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE
  ))

# Plot D - Food and Feed
d <- ggplot() +
  geom_sf(data = merged_data, aes(fill = NANIFF_cat), color = "#666666", size = 0.1) +
  geom_sf(data = ctiescbw_shp, color = "black", fill = NA, size = 0.3) +
  scale_fill_manual(
    name = "Food & Feed Balance (kg N/yr)",
    values = naniff_colors,
    na.value = "grey90"
  ) +
  common_theme +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE
  ))

# Create the final combined plot
final_plot <- a / b / c / d

# Save as high-resolution PNG for paper
ggsave("nitrogen_maps_publication.png", 
       plot = final_plot,
       width = 8.5,      # Standard letter width
       height = 11,      # Standard letter height  
       units = "in",
       dpi = 600,        # High resolution for publication
       bg = "white")     # White background

# Alternative: Save as PDF (vector format, better for publications)
ggsave("nitrogen_maps_publication.pdf", 
       plot = final_plot,
       width = 8.5, 
       height = 11, 
       units = "in",
       bg = "white")

# Display the plot
final_plot

# Create plots for 2002 and 2022
a <- ggplot() +
  geom_sf(data = cbind(cbw_shp,TNcmdt), aes(fill = discrete2002))+
  geom_sf(data = ctiescbw_shp,  fill=NA, color = "black")+
  scale_fill_manual(values = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FD8D3C", "#D94701"), 
                    labels = c("0 - 5 ", 
                               "5 - 50 ", 
                               "50 - 500 ", 
                               "500 - 5000 ",
                               "5000 - 10000"))+
  labs(fill= "Commodity-specific Anthropogenic Nitrogen (tonnes N)", title = "2002")+
  theme(legend.title = element_text(color = "black", size = 16, face = "bold"),
        legend.text = element_text(color = "black", size = 14),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

b <- ggplot() +
  geom_sf(data = cbind(cbw_shp,TNcmdt), aes(fill = discrete2022))+
  geom_sf(data = ctiescbw_shp,  fill=NA, color = "black")+
  scale_fill_manual(values = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FD8D3C", "#D94701"), 
                    labels = c("0 - 5e+03", 
                               "5e+03 - 5e+04", 
                               "5e+04 - 5e+05", 
                               "5e+05 - 5e+06",
                               "5e+06 - 1e+07"))+
  labs(fill= "Commodity-specific Anthropogenic Nitrogen (kg N)", title = "2022")+
  theme(legend.title = element_text(color = "black", size = 16, face = "bold"),
        legend.text = element_text(color = "black", size = 14),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

# Combine the plots with a shared legend
combined_plot <- a  + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5))

combined_plot

ggsave("combined_plot.png", 
       plot = combined_plot,
       width = 12,      # Width for two side-by-side plots
       height = 6,      # Height for single row
       units = "in",
       dpi = 600,       
       bg = "white")


# First, calculate totals for each year and crop
