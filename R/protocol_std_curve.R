#### Defining utility functions for data processing and plotting ####
# Function to plot linear regression with confidence interval (se = TRUE) on graphs
geom_lm <- function(formula = y ~ x, colour = alpha('black', 0.75), 
                    linewidth = 0.25, linetype = 'dashed', se = TRUE, ...) {
  geom_smooth(formula = formula, se = se, method = 'lm', colour = colour,
              linewidth = linewidth, linetype = linetype, ...)
}

# Function to extract data points from df_raw_sda
extract_data <- function(row, col) {
  df_raw_sda[row, col]
}

# Function to generate position values of a 96-well plate
generate_positions <- function() {
  positions <- expand.grid(LETTERS[1:8], 1:12)
  positions <- paste0(positions$Var1, positions$Var2)
  positions <- str_sort(positions, numeric = TRUE)
  return(positions)
}

#### Importing data ####
df_raw_sda <- 
  read_csv(here('data', 'std_curve', paste0(protocol, '_std_curve.csv')), col_names = FALSE) # Raw absorbance data as exported by Softmax software
# Extracting each wavelength for which data were recorded
wavelength_list <- 
  as.numeric(unlist(strsplit(as.character(df_raw_sda[2, 16]), ' '))) # Softmax stores wavelength at this position
#### Importing assay and experiment metadata ####
# Experiment metadata
exp_metadata <- read_csv(here('data', 'std_curve', paste0(protocol, '_std_curve_metadata.csv')), col_names = TRUE) # Experiment metadata. Contain details of this particular experiment.
# Creating id for each dilution within samples
exp_metadata <- exp_metadata %>% 
  mutate(sample = ifelse(is.na(sample_dil), NA, paste(sample_ID, sample_dil, sep = '_')))
# Creating id for each replica dilution within sample and dilution
exp_metadata <- exp_metadata %>%
  group_by(sample, type) %>%
  mutate(replica = ifelse(is.na(sample), '', paste0(sample, '_', row_number())))
# Assay/method metadata
assay_metadata <- read_csv(here('data', 'std_curve', paste0(protocol, '_protocol_metadata.csv')), col_names = TRUE) # Assay metadata. Contain general details of this assay method.

#### Getting metadata ####
# Storing metadata into objects
var_name <- assay_metadata$var_name # Variable name
var_short <- assay_metadata$var_short # Shortened variable name
var_abbr <- assay_metadata$var_abbr # Variable abbreviation
var_conc_unit <- unique(subset(exp_metadata, !is.na(std_conc_unit))$std_conc_unit)
signal <- paste('Absorbance', assay_metadata$signal_formula) # Signal recorded
signal_name <- assay_metadata$signal_name
signal_exp <- assay_metadata$signal_expression
exp_date <- exp_metadata$date[1] # Date of the experiment
assay <- assay_metadata$assay # Assay name
today <- format(Sys.Date(), '%Y.%m.%d') # Getting today's date
color_high <- assay_metadata$color_high # Color for plotting
color_low <- assay_metadata$color_low # Color for plotting
signal_formula <- assay_metadata$signal_formula # Formula for calculating final signal to be used in calculations
blank_wells <- subset(exp_metadata, type == 'blank')$well # Identifies 'blank' wells
std_mol_name <- assay_metadata$std_mol_name # Name or id of the molecule used as standard

# Finding the original file name of the Softmax .sda file
end_row <- which(df_raw_sda[, 1] == '~End')
original_file <- strsplit(paste(df_raw_sda[end_row+1, 1]), ';')[[1]][1]

#### Extracting data into separate dataframes for each wavelength as a list of dataframes ####
# Creating empty list to store dataframes
dataframes <- list()

# Extracting data into a list of dataframes
for(wl in wavelength_list) {
  df_name <- paste0('df_', wl)
  positions <- generate_positions()
  row <- which(wavelength_list == wl) * 2 + 2  # Calculate the row based on wavelength_list index
  cols <- 3:98  # Assuming cols 3 to 98 are what we want, where softmax usually puts the data points
  # Extract data points
  absorbance <- sapply(cols, function(col) extract_data(row, col))
  # Create data frame
  df <- data.frame(
    well = positions,
    wavelength = rep(wl, length(absorbance))
  )
  # Transpose absorbance data into a single column
  df$absorbance <- melt(absorbance)$value
  # Convert absorbance column to numeric
  df$absorbance <- as.numeric(df$absorbance)
  dataframes[[df_name]] <- df
}

# Separating data frames from list into individual data frames
for (df_name in names(dataframes)) {
  assign(df_name, dataframes[[df_name]])
}

# Creating a single data frame with all wavelengths
df_all_wl <- bind_rows(dataframes, .id = 'column_label')

# Cleaning up
rm(absorbance, end_row, df, cols, df_name, positions, row, wl)

#### Calculating mean absorbance of the blank wells for each wavelength ####
# Initializing Mean Absorbance Storage 
mean_blank_abs <- setNames(numeric(length(wavelength_list)), wavelength_list)

# Loop for calculating mean absorbance of the blank wells for each wl
for (wl in wavelength_list) {
  df_name <- paste0('df_', wl)
  current_df <- dataframes[[df_name]]  # Access the dataframe from the list
  # Calculate mean absorbance for blank wells
  mean_absorbance <- current_df %>%
    filter(well %in% blank_wells) %>%
    summarize(mean_absorbance = mean(absorbance)) %>%
    pull(mean_absorbance)  # Extract the value
  
  # Store the mean absorbance in the vector
  mean_blank_abs[as.character(wl)] <- mean_absorbance
}

# Cleaning up
rm(df_name, current_df, mean_absorbance)

#### Correcting absorbance by that of blank wells for each wl ####
# Loop for subtracting blank absorbance for each wavelength
for (df_name in names(dataframes)) {
  # Extract the wavelength number from the dataframe name
  wl <- sub('df_', '', df_name)
  # Subtract mean abs of blank wells from all wells
  dataframes[[df_name]] <- dataframes[[df_name]] %>%
    select(well, absorbance) %>%
    rename(!!paste0('A', wl) := absorbance) %>% # Renames col
    mutate(!!paste0('A', wl) := !!sym(paste0('A', wl)) - mean_blank_abs[wl]) # Computes subtraction
}

# Separating dataframes into individual dataframes
for (df_name in names(dataframes)) {
  assign(df_name, dataframes[[df_name]])
}

# Cleaning up
rm(df_name)

# Merging data frames
df_signal <- Reduce(function(x, y) merge(x, y, by = 'well', all = TRUE), dataframes)

#### Calculating final signal to be used in further calculations ####
df_signal <- df_signal %>% 
  mutate(signal = eval(parse(text = signal_formula)))

#### Merging signal and metadata and sub-setting sample data ####
# Merging signal with metadata
df <- df_signal %>% left_join(., exp_metadata, by = 'well')

#### Building the standard curve - Subsetting data ####
df_std <- filter(df, type == 'std') # Subsets standard curve observations from data

#### Building the standard curve - Linear model ####
std_curve <- lm(std_conc ~ signal, data = df_std)
summary(std_curve)

#### Computing limit of detection ####
lm_lod <- lm(signal ~ std_conc, data = df_std)
lm_lod_summary <- summary(lm_lod)

# Obtain and Slope (b) and Standard Error of Estimate (sy/x)
lm_lod_a <- unname(coef(lm_lod)[1])  # Intercept
lm_lod_b <- unname(coef(lm_lod)[2])  # Slope
lm_lod_sy_x <- sqrt(sum(lm_lod_summary$residuals^2) / lm_lod_summary$df[2]) # Standard Error of Estimate (sy/x)

# Compute yLOD 
std_curve_yLOD <- unname((3 * lm_lod_sy_x) + lm_lod_a)

# Compute CLOD
std_curve_CLOD <- unname((std_curve_yLOD - lm_lod_a) / lm_lod_b)

# Alternative, direct calculation
std_curve_CLOD <- unname((3 * lm_lod_sy_x)/lm_lod_b)

### Computing Limit of Quantification (LOQ)  for the linear model ####
# Set signal-to-noise ratio for LOQ
SN_ratio_LOQ <- 10  # Ensure LOQ signal/noise ratio ≥ 10

# Compute LOQ
std_curve_CLOQ <- unname((SN_ratio_LOQ * lm_lod_sy_x) / lm_lod_b)

# Compute yLOQ
std_curve_yLOQ <- lm_lod_a + (SN_ratio_LOQ * lm_lod_sy_x)

#### Plotting - Microplate view - Tidying data and setting theme ####

# Reading data
df_plate_map <- read_csv(here('data', 'std_curve', paste0(protocol, '_plate_map.csv')), col_names = TRUE)

# Transforming well positions into row and column indexes
df_plate_map <- df_plate_map %>% 
  mutate(col = parse_number(well), 
         row = str_replace_all(well, '[:digit:]', ''))
LookUp <-  1:26
names(LookUp) = LETTERS
df_plate_map$row <- LookUp[df_plate_map$row]

rm(LookUp)

# Setting theme for plate plots
theme_plate_plot <- theme_few(base_size = 8) +
  theme(legend.position = 'none',
        axis.text.x = element_text(color = 'black'), 
        axis.text.y = element_text(color = 'black'), 
        axis.title.y = element_text(face='bold'),
        axis.title.x = element_text(face='bold'),
        axis.line = element_line(linewidth = 0.1),
        panel.border = element_rect(color = 'black', linewidth = 0.5), 
        axis.ticks.length.x = unit(0.2, 'mm'),
        axis.ticks.length.y = unit(0.2, 'mm')) 

#### Plotting - Microplate view - Plate map ####
# Use the code below to (i) count the number of grouping vars, 
# (ii) select the range from which to draw colors, and
# (iii) get the number of colors necessary within the range of colors
color_count <-  length(unique(na.omit(df$type))) # Counts the number of grouping vars
get_palette <- colorRampPalette(brewer.pal(9, 'Spectral')) # Defines the range from which to generate colors
get_palette(color_count) # Generates the colors 

p_plate_map <- 
  ggplot(data = df_plate_map) +
  geom_circle(aes(x0 = col, y0 = row, r = 0.45, fill = type),
              alpha = 0.85, color = 'gray35') +
  coord_equal() +
  scale_fill_manual(values = get_palette(color_count), na.value = 'gray85') +
  geom_text(aes(x = col, y = row, label = ifelse(is.na(label), '', paste0(label))), size = 3) + 
  scale_x_continuous(breaks = 1:12, expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(breaks = 1:8, labels = LETTERS[1:8], 
                     expand = expansion(mult = c(0.01, 0.01)), trans = reverse_trans()) +
  labs(x = 'Coluna', 
       y = 'Linha',
       fill = NULL) +
  theme_plate_plot + 
  theme(legend.position = 'bottom')

#### Plotting - Standard curve ####
# Setting a theme for plotting the standard curve
theme_std_curve <- 
  theme_few(base_size = 8) +
  theme(      
    legend.position = 'none',
    axis.text.x = element_text(color = 'gray15'), 
    axis.text.y = element_text(color = 'gray15'), 
    axis.title.x = element_text(face='bold'),
    axis.title.y = element_text(face='bold'),
    axis.line = element_line(linewidth = 0),
    panel.border = element_rect(linewidth = 0.4, color = 'gray15'), 
    axis.ticks.length.x = unit(0.5, 'mm'),
    axis.ticks.length.y = unit(0.5, 'mm'))

# Plotting the standard curve
p_std_curve <- 
  ggplot(df_std, aes(x = std_conc, y = signal)) +
  geom_lm(fill = 'gray85') +
  geom_point(aes(fill = signal), alpha = 0.75, shape = 21, color = 'black', stroke = 0.25, size = 4) +
  scale_fill_gradient(low = color_low, high = color_high) + 
  stat_poly_eq(formula = y ~ x,
               aes(label = after_stat(rr.label)),  
               label.y.npc = 0.95, size = 5, rr.digits = 4, vjust = 0.5) +
  stat_poly_eq(formula = y ~ x,
               aes(label = after_stat(eq.label)),  
               label.y.npc = 0.05, size = 5, vjust = 0.5, label.x.npc = 0.95) +
  geom_text_npc(aes(npcx = 0.05, npcy = 0.88, label = paste('CLoD =', round(std_curve_CLOD, 4), var_conc_unit)),
                check_overlap = TRUE, size = 5, vjust = 0.5) +
  geom_text_npc(aes(npcx = 0.05, npcy = 0.81, label = paste('CLoQ =', round(std_curve_CLOQ, 4), var_conc_unit)),
                check_overlap = TRUE, size = 5, vjust = 0.5) +
  labs(title = paste(assay, 'assay'),
       subtitle = paste0(exp_date, ' - Standard curve'),
       x = paste0(std_mol_name, ' (', var_conc_unit, ')'),
       y = parse(text = signal_exp)) +
  theme_std_curve
