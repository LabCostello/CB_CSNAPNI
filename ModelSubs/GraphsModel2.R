# First, calculate totals for each year and crop
totals <- aggregate(values ~ year + crop, data = plot_data, FUN = sum)

# First graph #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
ggplot() +
  # Primary data: stacked bars for nitrogen inputs
  geom_col(data = plot_data, 
           aes(x = factor(year), y = values, fill = Nsource)) +
  # Add total values as text on top of each bar
  # geom_text(
  #   data = totals,
  #   aes(x = factor(year), y = values, label = round(values / 1000000, 1)),
  #   vjust = -0.5,
  #   size = 3.5,
  #   fontface = "bold"
  # ) +
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
    expand = expansion(mult = c(0, 0.15)),  # Add space at top for labels
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

####
# Second graph #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
create_animal_plot4 <- function(data, animal_label, title, population_data) {
  # Order data consistently
  data_ordered <- with(data, data[order(year, Nsource),])
  
  # Filter data for the specified animal
  animal_data <- data_ordered[data_ordered$animal == animal_label,]
  
  # Filter population data for the specified animal
  animal_pop <- population_data[population_data$animal == animal_label,]
  
  # Calculate totals for each year
  totals <- aggregate(totprodkgProt ~ year, data = animal_data, FUN = sum)
  
  # Calculate scale factor for secondary axis (multiply by 0.6 to keep line lower)
  scale_factor <- (max(totals$totprodkgProt) / max(animal_pop$population)) * 0.6
  
  # Scale the population values
  animal_pop$scaled_population <- animal_pop$population * scale_factor
  
  # Create plot for the specified animal
  ggplot() + 
    geom_bar(
      data = animal_data,
      aes(x = factor(year), y = totprodkgProt, fill = Nsource),
      stat = "identity",
      color = "black",
      position = "stack",
      width = 0.7
    ) +
    # Add total values as text on top of each bar
    geom_text(
      data = totals,
      aes(x = factor(year), y = totprodkgProt, label = round(totprodkgProt, 1)),
      vjust = -0.5,
      size = 3.9,
      fontface = "bold"
    ) +
    # Secondary data: points/lines for animal population
    geom_point(
      data = animal_pop, 
      aes(x = factor(year), y = scaled_population), 
      color = "red", size = 3, alpha = 0.7
    ) +
    geom_line(
      data = animal_pop, 
      aes(x = factor(year), y = scaled_population, group = 1), 
      color = "red", linewidth = 1, alpha = 0.7
    ) +
    scale_y_continuous(
      labels = function(x) format(x, scientific = FALSE),
      expand = expansion(mult = c(0, 0.15)),
      # Secondary y-axis for population
      sec.axis = sec_axis(
        trans = ~ . / scale_factor,
        labels = function(x) round(x / 1000000, 1)
      )
    ) +
    scale_fill_manual(values = c(
      "Fertilizer (national)" = "#780000",
      "Inorganic Fertilizer (cbw)" = "#C1121F",
      "Manure Fertilizer Available (cbw)" = "#CD3E43",
      "Manure Fertilizer Non-available (cbw)" = "#CD3E15",
      "N Fixation (national)" = "#003049",
      "N Fixation (cbw)" = "#669BBC",
      "Non-recoverable Manure (cbw)" = "#FDF0D5"
    ), labels = labels) +
    labs(
      title = title,
      x = "Year",
      fill = "Nitrogen Source"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold", margin = margin(b = 12)),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 18, face = "bold"),
      legend.text = element_text(color = "black", size = 16),
      axis.text = element_text(size = 16),
      axis.title.y = element_blank(),  # Remove individual y-axis titles
      axis.title.y.right = element_blank(),  # Remove individual right y-axis titles
      axis.text.y.right = element_text(size = 16, color = "black"),
      axis.text.x = element_text(size = 16),
      strip.text.x = element_text(size = 16, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1.5, "lines"),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
      legend.position = "bottom"
    )
}

# Creating pop_data
# Create the wide format dataframe
pop_wide <- data.frame(
  animal = meat_labels,
  `1997` = animpoptotal_sort[,1],
  `2002` = animpoptotal_sort[,2],
  `2007` = animpoptotal_sort[,3],
  `2012` = animpoptotal_sort[,4],
  `2017` = animpoptotal_sort[,5],
  `2022` = animpoptotal_sort[,6],
  check.names = FALSE
)

# Reshape to long format
library(tidyr)
pop_data <- pivot_longer(
  pop_wide,
  cols = -animal,
  names_to = "year",
  values_to = "population"
)

# Convert year to numeric
pop_data$year <- as.numeric(pop_data$year)

# Filter for years > 2007
pop_data <- pop_data[pop_data$year > 2007,]

data <- cmdtcsnapni4[cmdtcsnapni4$year>2007,]
data$totprodkgProt <- data$totprodkgProt/1000000

# Create the plots
beefcomp4 <- create_animal_plot4(data, meat_labels[1], "Beef", pop_data)
dairycomp4 <- create_animal_plot4(data, meat_labels[2], "Milk", pop_data)
porkcomp4 <- create_animal_plot4(data, meat_labels[3], "Pork", pop_data)
layerscomp4 <- create_animal_plot4(data, meat_labels[6], "Eggs", pop_data)
broilcomp4 <- create_animal_plot4(data, meat_labels[7], "Chicken", pop_data)
turkcomp4 <- create_animal_plot4(data, meat_labels[8], "Turkey", pop_data)

# Arrange and add common axis labels
figureR4 <- ggarrange(beefcomp4, dairycomp4, porkcomp4, layerscomp4, broilcomp4, turkcomp4,
                      ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")

figureR4 <- annotate_figure(figureR4,
                            left = text_grob("Total N input (million kg N)", 
                                             rot = 90, size = 18, face = "bold"),
                            right = text_grob("Animal population (million heads)", 
                                              rot = 270, size = 18, face = "bold")
)

figureR4

ggsave("nitrogen_cmdtotal.png", 
       width = 16, height = 10, 
       dpi = 300, 
       bg = "white")

# Third graph - Sankey Diagram #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
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

# Calculate node values for 2012
node_values_2012 <- nodein_values_2012 <- nodeout_values_2012 <- numeric(length(nodes$name))
for(i in 1:length(nodes$name)) {
  # Sum of outgoing flows (as source)
  outgoing <- sum(links$value2012[links$source == (i-1)], na.rm = TRUE)
  # Sum of incoming flows (as target)  
  incoming <- sum(links$value2012[links$target == (i-1)], na.rm = TRUE)
  # Use the maximum of incoming or outgoing
  node_values_2012[i] <- max(outgoing, incoming, na.rm = TRUE)
  nodein_values_2012[i] <- incoming
  nodeout_values_2012[i] <- outgoing
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
node_labels_2012 <- paste0(nodes$name, "\n(", format_numbers(node_values_2012), ")")
node_labels_2012 <- paste0(
  nodes$name,
  ifelse(nodein_values_2012 > 0, paste0("\nIn: ", format_numbers(nodein_values_2012)), ""),
  ifelse(nodeout_values_2012 > 0, paste0("\nOut: ", format_numbers(nodeout_values_2012)), "")
)

node_labels_2022 <- paste0(nodes$name, "\n(", format_numbers(node_values_2022), ")")
node_labels_2022 <- paste0(nodes$name,
                           #paste0(nodes$name," (",round(((nodeout_values_2022/nodein_values_2022)-1)*100),"%)"),
                           ifelse(nodein_values_2022 > 0, paste0("\nIn: ", format_numbers(nodein_values_2022)), ""),
                           ifelse(nodeout_values_2022 > 0, paste0("\nOut: ", format_numbers(nodeout_values_2022)), "")
)

# Create Sankey diagram for 2012 (Panel a)
fig_a <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    pad = 30,        # Increased padding for more space between nodes
    thickness = 25,   # Slightly thicker nodes
    line = list(color = "grey", width = 0.5),
    label = node_labels_2012  # Labels with formatted values for 2012
  ),
  link = list(
    source = links$source,
    target = links$target,
    value = links$value2012
  )
) %>% 
  layout(
    title = list(text = "<b>2012</b>",  # Bold title using HTML
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



# Fourth graph - Map #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
consistent_colors <- c(
  "Very Low\n(0 - 5K)" = "#FFF7EC",    
  "Low\n(5K - 50K)" = "#FEE8C8",     
  "Medium\n(50K - 500K)" = "#FDD49E", 
  "High\n(500K - 5M)" = "#FD8D3C",  
  "Very High\n(5M - 10M)" = "#D94701"
)
library(patchwork)

# First, create all plots without legends (except one to extract the legend from)
a <- ggplot() +
  geom_sf(data = cbind(cbw_shp,TNcmdt), aes(fill = discrete2002))+
  geom_sf(data = ctiescbw_shp,  fill=NA, color = "black")+
  scale_fill_manual(values = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FD8D3C", "#D94701"),
                    labels = c("0 - 5e+03",
                               "5e+03 - 5e+04",
                               "5e+04 - 5e+05",
                               "5e+05 - 5e+06",
                               "5e+06 - 1e+07"))+
  labs(fill= "Anthropogenic Nitrogen (kg N)", title = "2002")+
  theme(legend.title = element_text(color = "black", size = 16, face = "bold"),
        legend.text = element_text(color = "black", size = 14),
        axis.text = element_text(size = 14)) +
  guides(fill = "none")  # Remove legend

b <- ggplot() +
  geom_sf(data = cbind(cbw_shp,TNcmdt), aes(fill = discrete2017))+
  geom_sf(data = ctiescbw_shp,  fill=NA, color = "black")+
  scale_fill_manual(values = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FD8D3C", "#D94701"),
                    labels = c("0 - 5e+03",
                               "5e+03 - 5e+04",
                               "5e+04 - 5e+05",
                               "5e+05 - 5e+06",
                               "5e+06 - 1e+07"))+
  labs(fill= "Anthropogenic Nitrogen (kg N)", title = "2017")+
  theme(legend.title = element_text(color = "black", size = 16, face = "bold"),
        legend.text = element_text(color = "black", size = 14),
        axis.text = element_text(size = 14)) +
  guides(fill = "none")  # Remove legend

c <- ggplot() +
  geom_sf(data = cbind(cbw_shp,TNcmdt), aes(fill = discrete2007))+
  geom_sf(data = ctiescbw_shp,  fill=NA, color = "black")+
  scale_fill_manual(values = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FD8D3C", "#D94701"),
                    labels = c("0 - 5e+03",
                               "5e+03 - 5e+04",
                               "5e+04 - 5e+05",
                               "5e+05 - 5e+06",
                               "5e+06 - 1e+07"))+
  labs(fill= "Anthropogenic Nitrogen (kg N)", title = "2007")+
  theme(legend.title = element_text(color = "black", size = 16, face = "bold"),
        legend.text = element_text(color = "black", size = 14),
        axis.text = element_text(size = 14)) +
  guides(fill = "none")  # Remove legend

d <- ggplot() +
  geom_sf(data = cbind(cbw_shp,TNcmdt), aes(fill = discrete2012))+
  geom_sf(data = ctiescbw_shp,  fill=NA, color = "black")+
  scale_fill_manual(values = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FD8D3C", "#D94701"),
                    labels = c("0 - 5e+03",
                               "5e+03 - 5e+04",
                               "5e+04 - 5e+05",
                               "5e+05 - 5e+06",
                               "5e+06 - 1e+07"))+
  labs(fill= "Anthropogenic Nitrogen (kg N)", title = "2012")+
  theme(legend.title = element_text(color = "black", size = 16, face = "bold"),
        legend.text = element_text(color = "black", size = 14),
        axis.text = element_text(size = 14)) +
  guides(fill = "none")  # Remove legend

e <- ggplot() +
  geom_sf(data = cbind(cbw_shp,TNcmdt), aes(fill = discrete2022))+
  geom_sf(data = ctiescbw_shp,  fill=NA, color = "black")+
  scale_fill_manual(values = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FD8D3C", "#D94701"),
                    labels = c("0 - 5e+03",
                               "5e+03 - 5e+04",
                               "5e+04 - 5e+05",
                               "5e+05 - 5e+06",
                               "5e+06 - 1e+07"))+
  labs(fill= "Anthropogenic Nitrogen (kg N)", title = "2022")+
  theme(legend.title = element_text(color = "black", size = 16, face = "bold"),
        legend.text = element_text(color = "black", size = 14),
        axis.text = element_text(size = 14)) +
  guides(fill = "none")  # Remove legend for now

# Combine the plots with equal sizing and common legend

combined_plot <- wrap_plots(a, b, c, d, e, ncol = 2, nrow = 3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title.position = "top",  # This puts the title above the legend
        legend.title = element_text(hjust = 0.5))  # Center the title

combined_plot

ggsave("combined_plot.png",
       plot = combined_plot,
       width = 12,      # Increased width for better proportion
       height = 14,     # Increased height
       units = "in",
       dpi = 600,        # High resolution for publication
       bg = "white")     # White background

# Fith graph - NANI Map ####
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
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = "bottom",
        legend.title.position = "top")

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