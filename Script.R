# Set working directory
setwd("D:/GNR631 Mini Project/Leaf Area Index")

# Load libraries
lapply(
  c(
    "ggspatial",
    "scales",
    "hrbrthemes",
    'ggplot2',
    'readxl',
    'tidyverse',
    'plotly',
    'spatstat',
    'raster',
    'terra',
    'sp',
    'gstat',
    'fields',
    'tmap',
    'colorRamps',
    'rgl',
    'sf',
    'GGally',
    'CCA',
    'CCP',
    'tidyr',
    'reshape2',
    'zoo'
  ),
  require,
  character.only = TRUE
)

# install.packages("zoo")

# ______________________________________________________________________________
# function to read and clean the shape files
func.read.shp <- function(shape.file.path) {
  shp <- st_read(shape.file.path)
  shp <- st_make_valid(shp)
  shp <- st_simplify(shp)
}

# intersection function
func.intersect <- function(shp1, shp2, shp3) {
  intersection.1 <- st_intersection(shp1, shp2)
  intersection.2 <- st_intersection(shp3, intersection.1)
  return (intersection.2)
}

# function to add look up table data in the sf.object
func.lookup <- function(sf.object, table_das, table_hts) {
  looked.1 <- left_join(sf.object, table_das, by = "DAS")
  looked.2 <- left_join(looked.1, table_hts, by = "Height")
  return (looked.2)
}

# function to calculate Leaf Area Index Values
func.lai <- function(sf.object) {
  # Get the average VLADF and Leaf Angle
  sf.object <-
    sf.object %>% mutate(VLADF = (VLADF_DAS + VLADF_height) / 2)
  sf.object <-
    sf.object %>% mutate(LeafAngle = (leaf_angle_DAS + leaf_angle_height) / 2)
  
  # Getting the sine of the Leaf Angle
  sf.object <-
    sf.object %>% mutate(SineLeafAngle = sin(LeafAngle * pi / 180))
  
  # Get the Leaf Area Index value
  sf.object <-
    sf.object %>% mutate(LAI = ((VLADF * can_cover) / SineLeafAngle))
  
  # Normalize the LAI values  - Range Normalization
  # Range normalize the LAI variable
  sf.object$LAI_rn <-
    (sf.object$LAI - min(sf.object$LAI)) / (max(sf.object$LAI) - min(sf.object$LAI))
  
  return(sf.object)
}

# Function to save plots # A3 = 11.7 x 16.5 inches # A4	210 x 297	8.3 x 11.7
func.save_A4_Plot <- function(plotName) {
  # Convert variable name to string
  variable_string <- deparse(substitute(plotName))
  
  # Replace periods with underscores
  variable_string <- gsub("\\.", "_", variable_string)
  
  # Split the string into components
  variable_components <- strsplit(variable_string, "_")[[1]]
  
  # Extract the components
  plotType_string <- variable_components[1]
  date_string <- variable_components[2]
  
  # Convert date string to Date object
  date_object <- as.Date(date_string, format = "%d%b%Y")
  
  # Convert Date object back to string in the desired format
  formatted_date_string <- format(date_object, "%Y%m%d")
  
  # Convert Date object back to string in the desired format
  formatted_date_string <- format(date_object, "%Y%m%d")
  
  # Concatenate the plot name 
  formatted_plotName <- paste(plotType_string, formatted_date_string)
  
  # Concatenate strings
  variable <- paste0(formatted_plotName, ".jpeg")
  ggsave(variable, plot = plotName, width = 11.7, height = 8.3, dpi = 500)
  
  print(paste(variable, "saved in A4"))
}


# ______________________________________________________________________________

# load Look up table
lookup_table <- read_excel("LookupTable.xlsx")
lt.das <-
  lookup_table[c("DAS", "leaf_angle_DAS", "VLADF_DAS")] # Days after sowing lookup table
lt.hts <-
  lookup_table[c("Height", "leaf_angle_height", "VLADF_height")] # Crop height lookup table

# ______________________________________________________________________________

# set 01 - 21st Nov 2018

# set 01 - file paths
shape.fp.21Nov2018.gcc <- file.path("21 Nov 2018/canopy.shp")
shape.fp.21Nov2018.das <- file.path("21 Nov 2018/35DAS.shp")
shape.fp.21Nov2018.hts <- file.path("21 Nov 2018/21NovHeight.shp")

# Call the function and read the shape files
set1.21Nov2018.gcc <- func.read.shp(shape.fp.21Nov2018.gcc)
set1.21Nov2018.das <- func.read.shp(shape.fp.21Nov2018.das)
set1.21Nov2018.hts <- func.read.shp(shape.fp.21Nov2018.hts)


# intersection  # sequence matters, keep the gcc as shp3
sf.object.21Nov2018 <- func.intersect(shp1 = set1.21Nov2018.hts,
                                        shp2 = set1.21Nov2018.das,
                                        shp3 = set1.21Nov2018.gcc)


# update the height column to meters to match the lookup table
sf.object.21Nov2018 <-
  sf.object.21Nov2018 %>% mutate_at(vars(X21Nov), ~ round(. * 0.01, 1))

# rename columns to match the lookup table
sf.object.21Nov2018 <-
  sf.object.21Nov2018 %>% rename(DAS = X57DAS_DAS)
sf.object.21Nov2018 <-
  sf.object.21Nov2018 %>% rename(Height = X21Nov)

# Convert the common key column in sf.object to double type
sf.object.21Nov2018$DAS <- as.double(sf.object.21Nov2018$DAS)

# call the lookup function to add values to the sf object
sf.object.21Nov2018 <-
  func.lookup(sf.object.21Nov2018, lt.das, lt.hts)

# Calculate LAI values
sf.object.21Nov2018 <- func.lai(sf.object.21Nov2018)

# Calculate mean LAI
mean_lai_rn_21Nov2018 <- mean(sf.object.21Nov2018$LAI_rn)

plot.tittle.21Nov2018 <- "Leaf Area Index 21st Nov 2018"

head(sf.object.21Nov2018)


# set 02 - 12 Dec 2018

# set 02 - file paths
shape.fp.12Dec2018.gcc <- file.path("12 Dec 2018/can_cover.shp")
shape.fp.12Dec2018.das <- file.path("12 Dec 2018/57DAS.shp")
shape.fp.12Dec2018.hts <- file.path("12 Dec 2018/12DecHeight.shp")

# Call the function and read the shape files
set2.12Dec2018.gcc <- func.read.shp(shape.fp.12Dec2018.gcc)
set2.12Dec2018.das <- func.read.shp(shape.fp.12Dec2018.das)
set2.12Dec2018.hts <- func.read.shp(shape.fp.12Dec2018.hts)


# intersection  # sequence matters, keep the gcc as shp3
sf.object.12Dec2018 <- func.intersect(shp1 = set2.12Dec2018.hts,
                                      shp2 = set2.12Dec2018.das,
                                      shp3 = set2.12Dec2018.gcc)


# update the height column to meters to match the lookup table
sf.object.12Dec2018 <-
  sf.object.12Dec2018 %>% mutate_at(vars(X12Dec), ~ round(. * 0.01, 1))

# rename columns to match the lookup table
sf.object.12Dec2018 <-
  sf.object.12Dec2018 %>% rename(DAS = X57DAS_DAS)
sf.object.12Dec2018 <-
  sf.object.12Dec2018 %>% rename(Height = X12Dec)

# Convert the common key column in sf.object to double type
sf.object.12Dec2018$DAS <- as.double(sf.object.12Dec2018$DAS)

# call the lookup function to add values to the sf object
sf.object.12Dec2018 <-
  func.lookup(sf.object.12Dec2018, lt.das, lt.hts)

# Calculate LAI values
sf.object.12Dec2018 <- func.lai(sf.object.12Dec2018)

# Calculate mean LAI
mean_lai_rn_12Dec2018 <- mean(sf.object.12Dec2018$LAI_rn)

plot.tittle.12Dec2018 <- "Leaf Area Index 12th Dec 2018"



# set 03 - 7 Jan 2019

# set 03 - file paths
shape.fp.7Jan2019.gcc <- file.path("7Jan 2019/canopy.shp")
shape.fp.7Jan2019.das <- file.path("7Jan 2019/83DAS.shp")
shape.fp.7Jan2019.hts <- file.path("7Jan 2019/7JanHeight.shp")

# Call the function and read the shape files
set3.7Jan2019.gcc <- func.read.shp(shape.fp.7Jan2019.gcc)
set3.7Jan2019.das <- func.read.shp(shape.fp.7Jan2019.das)
set3.7Jan2019.hts <- func.read.shp(shape.fp.7Jan2019.hts)


# intersection  # sequence matters, keep the gcc as shp3
sf.object.7Jan2019 <- func.intersect(shp1 = set3.7Jan2019.hts,
                                     shp2 = set3.7Jan2019.das,
                                     shp3 = set3.7Jan2019.gcc)


# update the height column to meters to match the lookup table
sf.object.7Jan2019 <-
  sf.object.7Jan2019 %>% mutate_at(vars(X7Jan), ~ round(. * 0.01, 1))

# rename columns to match the lookup table
sf.object.7Jan2019 <- sf.object.7Jan2019 %>% rename(DAS = X101_DAS)
sf.object.7Jan2019 <- sf.object.7Jan2019 %>% rename(Height = X7Jan)

# Convert the common key column in sf.object to double type
sf.object.7Jan2019$DAS <- as.double(sf.object.7Jan2019$DAS)

# call the lookup function to add values to the sf object
sf.object.7Jan2019 <- func.lookup(sf.object.7Jan2019, lt.das, lt.hts)

# Calculate LAI values
sf.object.7Jan2019 <- func.lai(sf.object.7Jan2019)

# Calculate mean LAI
mean_lai_rn_7Jan2019 <- mean(sf.object.7Jan2019$LAI_rn)

plot.tittle.7Jan2019 <- "Leaf Area Index 7th Jan 2019"


# set 04 - 25 Jan 2019

# set 04 - file paths
shape.fp.25Jan2019.gcc <- file.path("25 Jan 2019/1mtr_can.shp")
shape.fp.25Jan2019.das <- file.path("25 Jan 2019/101DAS.shp")
shape.fp.25Jan2019.hts <- file.path("25 Jan 2019/25JanHeight.shp")

# Call the function and read the shape files
set4.25Jan2019.gcc <- func.read.shp(shape.fp.25Jan2019.gcc)
set4.25Jan2019.das <- func.read.shp(shape.fp.25Jan2019.das)
set4.25Jan2019.hts <- func.read.shp(shape.fp.25Jan2019.hts)


# intersection  # sequence matters, keep the gcc as shp3
sf.object.25Jan2019 <- func.intersect(shp1 = set4.25Jan2019.hts,
                                      shp2 = set4.25Jan2019.das,
                                      shp3 = set4.25Jan2019.gcc)


# update the height column to meters to match the lookup table
sf.object.25Jan2019 <-
  sf.object.25Jan2019 %>% mutate_at(vars(X25Jan), ~ round(. * 0.01, 1))

# rename columns to match the lookup table
sf.object.25Jan2019 <-
  sf.object.25Jan2019 %>% rename(DAS = X57DAS_DAS)
sf.object.25Jan2019 <-
  sf.object.25Jan2019 %>% rename(Height = X25Jan)

# Convert the common key column in sf.object to double type
sf.object.25Jan2019$DAS <- as.double(sf.object.25Jan2019$DAS)

# call the lookup function to add values to the sf object
sf.object.25Jan2019 <-
  func.lookup(sf.object.25Jan2019, lt.das, lt.hts)

# Calculate LAI values
sf.object.25Jan2019 <- func.lai(sf.object.25Jan2019)

# Calculate mean LAI
mean_lai_rn_25Jan2019 <- mean(sf.object.25Jan2019$LAI_rn)

plot.tittle.25Jan2019 <- "Leaf Area Index 25th Jan 2019"

# ______________________________________________________________________________
# Box and Violin Plot

# =================
# LAI
# =================

data.forBoxPlot.lai <-cbind(sf.object.21Nov2018$LAI, sf.object.12Dec2018$LAI, 
                      sf.object.7Jan2019$LAI, sf.object.25Jan2019$LAI)

# Convert matrix to data frame
data.forBoxPlot.lai <- as.data.frame(data.forBoxPlot.lai)
typeof(data.forBoxPlot.lai)
head(data.forBoxPlot.lai)

# Set column names
colnames(data.forBoxPlot.lai) <- c("LAI_21Nov2018", "LAI_12Dec2018", "LAI_7Jan2019", "LAI_25Jan2019")

# Reshape the data to long format
data.forBoxPlot.lai_long <- tidyr::pivot_longer(data.forBoxPlot.lai, 
                                            cols = starts_with("LAI"),
                                            names_to = "Date", 
                                            values_to = "LAI")

# Create a factor variable for Date
data.forBoxPlot.lai_long$Date <- factor(data.forBoxPlot.lai_long$Date, 
                                    levels = c("LAI_21Nov2018", "LAI_12Dec2018", "LAI_7Jan2019", "LAI_25Jan2019"),
                                    labels = c("21Nov2018", "12Dec2018", "7Jan2019", "25Jan2019"))

# Create the box plot

boxplot.lai <- 
  ggplot(data.forBoxPlot.lai_long, aes(x = Date, y = LAI, color = Date)) +
  geom_boxplot(        
    # # custom boxes
    # color="blue",
    # fill="blue",
    # alpha=0.2,
    
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
    ) +
  stat_summary(fun = "mean", geom = "point", shape = 18,
               size = 2, color = "black") +
  geom_line(stat = "summary", fun = "mean", aes(group = 1), color = "black", linetype = "dashed") +
  # geom_smooth(method = "loess", se = FALSE, color = "black") + # Add this line
  labs(title = "Box plots of Leaf Area Index",
       y = "Leaf Area Index",
       x = "Date")  +
  stat_boxplot(geom ='errorbar') + 
  # + scale_fill_manual(values = c("red", "blue", "green", "orange")) +
  theme_minimal()

boxplot.lai
# Save the plot as a PDF file
ggsave("boxplot_lai.jpeg", plot = boxplot.lai, width = 11.7, height = 8.3, units = "in", dpi = 500)

# violin chart
# Most basic violin chart
violin.chart.lai <- ggplot(data.forBoxPlot.lai_long, aes(x=LAI, y=Date, colour = Date)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(scale = "width",
              draw_quantiles = c(0.25, 0.5, 0.75),
              trim = FALSE) + 
  labs(title = "Violin Chart of Leaf Area Index",
       y = "Date",
       x = "Leaf Area Index") +
  theme_minimal()

violin.chart.lai
ggsave("violinChart_lai.jpeg", plot = violin.chart.lai, width = 11.7, height = 8.3, units = "in", dpi = 500)


# =================
# VLADF 
# =================

data.forBoxPlot.VLADF <-cbind(sf.object.21Nov2018$VLADF, sf.object.12Dec2018$VLADF, 
                            sf.object.7Jan2019$VLADF, sf.object.25Jan2019$VLADF)

# Convert matrix to data frame
data.forBoxPlot.VLADF <- as.data.frame(data.forBoxPlot.VLADF)
typeof(data.forBoxPlot.VLADF)
head(data.forBoxPlot.VLADF)
summary(data.forBoxPlot.VLADF)

# Set column names
colnames(data.forBoxPlot.VLADF) <- c("VLADF_21Nov2018", "VLADF_12Dec2018", "VLADF_7Jan2019", "VLADF_25Jan2019")

# Reshape the data to long format
data.forBoxPlot.VLADF_long <- tidyr::pivot_longer(data.forBoxPlot.VLADF, 
                                                cols = starts_with("VLADF"),
                                                names_to = "Date", 
                                                values_to = "VLADF")

# Create a factor variable for Date
data.forBoxPlot.VLADF_long$Date <- factor(data.forBoxPlot.VLADF_long$Date, 
                                        levels = c("VLADF_21Nov2018", "VLADF_12Dec2018", "VLADF_7Jan2019", "VLADF_25Jan2019"),
                                        labels = c("21Nov2018", "12Dec2018", "7Jan2019", "25Jan2019"))

head(data.forBoxPlot.VLADF_long)


# ylim.max <- round(max(data.forBoxPlot.VLADF_long$VLADF) / 5) * 5

ylim.max <- round(max(data.forBoxPlot.VLADF_long$VLADF)) + 0.5
ylim.min <- round(min(data.forBoxPlot.VLADF_long$VLADF)) - 0.5


# Create the box plot

boxplot.VLADF <- 
  ggplot(data.forBoxPlot.VLADF_long, aes(x = Date, y = VLADF, color = Date)) +
  geom_boxplot(        
    # # custom boxes
    # color="blue",
    # fill="blue",
    # alpha=0.2,
    
    # Notch?
    notch=FALSE,
    notchwidth = 0.8,
    
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
  ) +
  stat_summary(fun = "mean", geom = "point", shape = 18,
               size = 2, color = "black") +
  geom_line(stat = "summary", fun = "mean", aes(group = 1), color = "black", linetype = "dashed") +
  labs(title = "Box plots of VLADF",
       y = "Crop VLADF",
       x = "Date")  +
  stat_boxplot(geom ='errorbar') + 
  ylim(ylim.min, ylim.max) +
  # + scale_fill_manual(values = c("red", "blue", "green", "orange")) +
  theme_minimal() 
  # Adding x and y axis boxplot
  # geom_boxplot(data = NULL, aes(x = 0, y = 0), width = 0.1, color = "black", alpha = 0)

boxplot.VLADF
# Save the plot as a PDF file
ggsave("boxplot_VLADF.jpeg", plot = boxplot.VLADF, width = 11.7, height = 8.3, units = "in", dpi = 500)

# violin chart
# Most basic violin chart
violin.chart.VLADF <- ggplot(data.forBoxPlot.VLADF_long, aes(x=VLADF, y=Date, colour = Date)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(scale = "width",
              draw_quantiles = c(0.25, 0.5, 0.75),
              trim = FALSE) +
  labs(title = "Violin Chart of Crop VLADF",
       y = "Date",
       x = "VLADF") +
  theme_minimal()

violin.chart.VLADF
ggsave("violinChart_VLADF.jpeg", plot = violin.chart.VLADF, width = 11.7, height = 8.3, units = "in", dpi = 500)

# =================
# GCC 
# =================

data.forBoxPlot.GCC <-cbind(sf.object.21Nov2018$can_cover, sf.object.12Dec2018$can_cover, 
                              sf.object.7Jan2019$can_cover, sf.object.25Jan2019$can_cover)

# Convert matrix to data frame
data.forBoxPlot.GCC <- as.data.frame(data.forBoxPlot.GCC)
typeof(data.forBoxPlot.GCC)
head(data.forBoxPlot.GCC)
summary(data.forBoxPlot.GCC)

# Set column names
colnames(data.forBoxPlot.GCC) <- c("GCC_21Nov2018", "GCC_12Dec2018", "GCC_7Jan2019", "GCC_25Jan2019")

# Reshape the data to long format
data.forBoxPlot.GCC_long <- tidyr::pivot_longer(data.forBoxPlot.GCC, 
                                                  cols = starts_with("GCC"),
                                                  names_to = "Date", 
                                                  values_to = "GCC")

# Create a factor variable for Date
data.forBoxPlot.GCC_long$Date <- factor(data.forBoxPlot.GCC_long$Date, 
                                          levels = c("GCC_21Nov2018", "GCC_12Dec2018", "GCC_7Jan2019", "GCC_25Jan2019"),
                                          labels = c("21Nov2018", "12Dec2018", "7Jan2019", "25Jan2019"))

head(data.forBoxPlot.GCC_long)


# ylim.max <- round(max(data.forBoxPlot.GCC_long$GCC) / 5) * 5

ylim.max <- round(max(data.forBoxPlot.GCC_long$GCC)) 
ylim.min <- round(min(data.forBoxPlot.GCC_long$GCC)) 


# Create the box plot

boxplot.GCC <- 
  ggplot(data.forBoxPlot.GCC_long, aes(x = Date, y = GCC, color = Date)) +
  geom_boxplot(        
    # # custom boxes
    # color="blue",
    # fill="blue",
    # alpha=0.2,
    
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
  ) +
  stat_summary(fun = "mean", geom = "point", shape = 18,
               size = 2, color = "black") +
  geom_line(stat = "summary", fun = "mean", aes(group = 1), color = "black", linetype = "dashed") +
  labs(title = "Box plots of Green Canpoy Cover",
       y = "Green Canpoy Cover",
       x = "Date")  +
  stat_boxplot(geom ='errorbar') + 
  ylim(ylim.min, ylim.max) +
  # + scale_fill_manual(values = c("red", "blue", "green", "orange")) +
  theme_minimal() 
# Adding x and y axis boxplot
# geom_boxplot(data = NULL, aes(x = 0, y = 0), width = 0.1, color = "black", alpha = 0)

boxplot.GCC
# Save the plot as a PDF file
ggsave("boxplot_GCC.jpeg", plot = boxplot.GCC, width = 11.7, height = 8.3, units = "in", dpi = 500)

# violin chart
# Most basic violin chart
violin.chart.GCC <- ggplot(data.forBoxPlot.GCC_long, aes(x=GCC, y=Date, colour = Date)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(scale = "width",
              draw_quantiles = c(0.25, 0.5, 0.75),
              trim = FALSE) +
  labs(title = "Violin Chart of Crop GCC",
       y = "Date",
       x = "GCC") +
  theme_minimal()

violin.chart.GCC
ggsave("violinChart_GCC.jpeg", plot = violin.chart.GCC, width = 11.7, height = 8.3, units = "in", dpi = 500)


# =================
# Height 
# =================

data.forBoxPlot.Height <-cbind(sf.object.21Nov2018$Height, sf.object.12Dec2018$Height, 
                            sf.object.7Jan2019$Height, sf.object.25Jan2019$Height)

# Convert matrix to data frame
data.forBoxPlot.Height <- as.data.frame(data.forBoxPlot.Height)
typeof(data.forBoxPlot.Height)
head(data.forBoxPlot.Height)
summary(data.forBoxPlot.Height)

# Set column names
colnames(data.forBoxPlot.Height) <- c("Height_21Nov2018", "Height_12Dec2018", "Height_7Jan2019", "Height_25Jan2019")

# Reshape the data to long format
data.forBoxPlot.Height_long <- tidyr::pivot_longer(data.forBoxPlot.Height, 
                                                cols = starts_with("Height"),
                                                names_to = "Date", 
                                                values_to = "Height")

# Create a factor variable for Date
data.forBoxPlot.Height_long$Date <- factor(data.forBoxPlot.Height_long$Date, 
                                        levels = c("Height_21Nov2018", "Height_12Dec2018", "Height_7Jan2019", "Height_25Jan2019"),
                                        labels = c("21Nov2018", "12Dec2018", "7Jan2019", "25Jan2019"))

head(data.forBoxPlot.Height_long)


# ylim.max <- round(max(data.forBoxPlot.Height_long$Height) / 5) * 5

ylim.max <- round(max(data.forBoxPlot.Height_long$Height)) +0.5
ylim.min <- round(min(data.forBoxPlot.Height_long$Height)) 


# Create the box plot

boxplot.Height <- 
  ggplot(data.forBoxPlot.Height_long, aes(x = Date, y = Height, color = Date)) +
  geom_boxplot(        
    # # custom boxes
    # color="blue",
    # fill="blue",
    # alpha=0.2,
    
    # Notch?
    notch=FALSE,
    notchwidth = 0.8,
    
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
  ) +
  stat_summary(fun = "mean", geom = "point", shape = 18,
               size = 2, color = "black") +
  geom_line(stat = "summary", fun = "mean", aes(group = 1), color = "black", linetype = "dashed") +
  labs(title = "Box plots of Crop Height",
       y = "Crop Height",
       x = "Date")  +
  stat_boxplot(geom ='errorbar') + 
  ylim(ylim.min, ylim.max) +
  # + scale_fill_manual(values = c("red", "blue", "green", "orange")) +
  theme_minimal() 
# Adding x and y axis boxplot
# geom_boxplot(data = NULL, aes(x = 0, y = 0), width = 0.1, color = "black", alpha = 0)

boxplot.Height
# Save the plot as a PDF file
ggsave("boxplot_Height.jpeg", plot = boxplot.Height, width = 11.7, height = 8.3, units = "in", dpi = 500)

# violin chart
# Most basic violin chart
violin.chart.Height <- ggplot(data.forBoxPlot.Height_long, aes(x=Height, y=Date, colour = Date)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(scale = "width",
              draw_quantiles = c(0.25, 0.5, 0.75),
              trim = FALSE) +
  labs(title = "Violin Chart of Crop Height",
       y = "Date",
       x = "Height") +
  theme_minimal()

violin.chart.Height
ggsave("violinChart_Height.jpeg", plot = violin.chart.Height, width = 11.7, height = 8.3, units = "in", dpi = 500)


# ______________________________________________________________________________
# Plots

# Declare the functions

func.hist <- function(sf.object, mean_lai_rn, plot.tittle) {
  # Create the histogram
  hist_plot <- ggplot(data = sf.object, aes(x = LAI_rn)) +
    geom_histogram(
      binwidth = 0.05,
      fill = "purple",
      color = "#e9ecef",
      alpha = 0.5
    ) +
    labs(x = "Leaf Area Index", y = "Frequency") +  # Custom axis labels
    # xlim(c(0, 8)) +  # Custom x limits
    ylim(c(0, 200)) +  # Custom y limits
    scale_x_continuous(breaks = seq(0, 1, by = .1),
                       labels = seq(0, 1, by = .1)) +  # Custom x ticks
    # scale_y_continuous(breaks = seq(0, 260, by = 10)) +  # Custom y ticks
    ggtitle(plot.tittle) +
    theme_ipsum() +
    theme(plot.title = element_text(size = 15)) +
    geom_vline(aes(xintercept = mean_lai_rn),
               color = "red",
               linewidth = 1) + # Mean Line
    geom_text(
      aes(
        x = mean_lai_rn - .075 ,
        y = 125,
        label = paste("Mean:", round(mean_lai_rn, 2))
      ),
      color = "red",
      size = 3,
      vjust = 1.5
    )  # Add mean label
  
  return(hist_plot)
}

func.map <- function(sf.object, plot.tittle) {
  plot <- # Plot with scale and north direction
    ggplot() +
    geom_sf(data = sf.object, aes(fill = LAI_rn)) +
    ggtitle(plot.tittle) +
    theme_minimal() +
    coord_sf(
      xlim = c(78.3986, 78.39915),
      ylim = c(17.3241, 17.3245),
      expand = TRUE
    ) +
    annotation_scale(location = "bl", width_hint = 0.25) +  # Add scale bar
    annotation_north_arrow(
      location = "br",
      which_north = "true",
      style = north_arrow_fancy_orienteering,
      pad_x = unit(0.1, "in"),
      pad_y = unit(0.1, "in")
    ) +  # Add north arrow
    scale_fill_viridis_c(name = "Normalised LAI") # Apply viridis color palette and legend title
  
  return(plot)
}


# ______________________________________________________________________________

# Call the function to generate plots
hist.21Nov2018 <- func.hist(sf.object.21Nov2018,
                              mean_lai_rn_21Nov2018,
                              plot.tittle.21Nov2018)

hist.12Dec2018 <- func.hist(sf.object.12Dec2018,
                            mean_lai_rn_12Dec2018,
                            plot.tittle.12Dec2018)

hist.7Jan2019 <- func.hist(sf.object.7Jan2019,
                           mean_lai_rn_7Jan2019,
                           plot.tittle.7Jan2019)

hist.25Jan2019 <- func.hist(sf.object.25Jan2019,
                            mean_lai_rn_25Jan2019,
                            plot.tittle.25Jan2019)

# Call the function to generate map
map.21Nov2018 <- func.map(sf.object.21Nov2018,
                            plot.tittle.21Nov2018)

map.12Dec2018 <- func.map(sf.object.12Dec2018,
                          plot.tittle.12Dec2018)

map.7Jan2019 <- func.map(sf.object.7Jan2019,
                         plot.tittle.7Jan2019)

map.25Jan2019 <- func.map(sf.object.25Jan2019,
                          plot.tittle.25Jan2019)

# View the plots
hist.21Nov2018
hist.12Dec2018
hist.7Jan2019
hist.25Jan2019

# View the map
map.21Nov2018
map.12Dec2018
map.7Jan2019
map.25Jan2019

# Save Plots 
func.save_A4_Plot(hist.21Nov2018)
func.save_A4_Plot(hist.12Dec2018)
func.save_A4_Plot(hist.7Jan2019)
func.save_A4_Plot(hist.25Jan2019)

func.save_A4_Plot(map.21Nov2018)
func.save_A4_Plot(map.12Dec2018)
func.save_A4_Plot(map.7Jan2019)
func.save_A4_Plot(map.25Jan2019)


# ______________________________________________________________________________
# Corelation Matrix .12Nov2018

df.21Nov2018 <- data.frame(sf.object.21Nov2018$VLADF,
                            sf.object.21Nov2018$LeafAngle,
                            sf.object.21Nov2018$LAI,
                            sf.object.21Nov2018$Height)

colnames(df.21Nov2018) <- c('VLADF', 'LeafAngle', 'LAI', 'Height')

df.corelation.12Nov2018 <- round(cor(df.21Nov2018), 2)

df.corelation.12Nov2018.melt <- melt(df.corelation.12Nov2018)

# Heatmap 
#create correlation heatmap
coor.plot.12Nov2018 <- 
  ggplot(data = df.corelation.12Nov2018.melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue3", high = "red2",
                       limit = c(-1,1), name="Correlation") +
  ggtitle("21 Nov 2018") + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())

coor.plot.12Nov2018
ggsave("coor_2018_11_21.jpeg", plot = coor.plot.12Nov2018, width = 11.7, height = 8.3, units = "in", dpi = 500)


# Corelation Matrix .12Dec2018

df.12Dec2018 <- data.frame(sf.object.12Dec2018$VLADF,
                           sf.object.12Dec2018$LeafAngle,
                           sf.object.12Dec2018$LAI,
                           sf.object.12Dec2018$Height)

colnames(df.12Dec2018) <- c('VLADF', 'LeafAngle', 'LAI', 'Height')

df.corelation.12Dec2018 <- round(cor(df.12Dec2018), 2)

df.corelation.12Dec2018.melt <- melt(df.corelation.12Dec2018)

# Heatmap 
#create correlation heatmap
coor.plot.12Dec2018 <- 
  ggplot(data = df.corelation.12Dec2018.melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue3", high = "red2",
                       limit = c(-1,1), name="Correlation") +
  ggtitle("12 Dec 2018") + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())

coor.plot.12Dec2018
ggsave("coor_2018_12_12.jpeg", plot = coor.plot.12Dec2018, width = 11.7, height = 8.3, units = "in", dpi = 500)


# Corelation Matrix .7Jan2019

df.7Jan2019 <- data.frame(sf.object.7Jan2019$VLADF,
                           sf.object.7Jan2019$LeafAngle,
                           sf.object.7Jan2019$LAI,
                           sf.object.7Jan2019$Height)

colnames(df.7Jan2019) <- c('VLADF', 'LeafAngle', 'LAI', 'Height')

df.corelation.7Jan2019 <- round(cor(df.7Jan2019), 2)

df.corelation.7Jan2019.melt <- melt(df.corelation.7Jan2019)

# Heatmap 
#create correlation heatmap
coor.plot.7Jan2019 <- 
  ggplot(data = df.corelation.7Jan2019.melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue3", high = "red2",
                       limit = c(-1,1), name="Correlation") +
  ggtitle("7 Jan 2019") + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())

coor.plot.7Jan2019
ggsave("coor_2019_01_07.jpeg", plot = coor.plot.7Jan2019, width = 11.7, height = 8.3, units = "in", dpi = 500)

# Corelation Matrix .25Jan2019

df.25Jan2019 <- data.frame(sf.object.25Jan2019$VLADF,
                          sf.object.25Jan2019$LeafAngle,
                          sf.object.25Jan2019$LAI,
                          sf.object.25Jan2019$Height)

colnames(df.25Jan2019) <- c('VLADF', 'LeafAngle', 'LAI', 'Height')

df.corelation.25Jan2019 <- round(cor(df.25Jan2019), 2)

df.corelation.25Jan2019.melt <- melt(df.corelation.25Jan2019)

# Heatmap 
#create correlation heatmap
coor.plot.25Jan2019 <- 
  ggplot(data = df.corelation.25Jan2019.melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue3", high = "red2",
                       limit = c(-1,1), name="Correlation") +
  ggtitle("25 Jan 2019") + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())

coor.plot.25Jan2019
ggsave("coor_2019_01_25.jpeg", plot = coor.plot.25Jan2019, width = 11.7, height = 8.3, units = "in", dpi = 500)
# ______________________________________________________________________________
# Hovmoller Plot

# =================
# Hovmoller
# =================

precision = 10

data.lon <- data.frame(cbind(sf.object.21Nov2018$XMAX,sf.object.21Nov2018$XMIN,
                             sf.object.12Dec2018$XMAX,sf.object.12Dec2018$XMIN,
                             sf.object.7Jan2019$XMAX,sf.object.7Jan2019$XMIN,
                             sf.object.25Jan2019$XMAX,sf.object.25Jan2019$XMIN))

df.lon <- data.frame(round(((data.lon$X1+data.lon$X2)/2), precision), 
                     round(((data.lon$X3+data.lon$X4)/2), precision),
                     round(((data.lon$X5+data.lon$X6)/2), precision),
                     round(((data.lon$X7+data.lon$X8)/2), precision))

colnames(df.lon) <- c("lon_21Nov2018", "lon_12Dec2018", "lon_7Jan2019", "lon_25Jan2019")

data.lat <- data.frame(cbind(sf.object.21Nov2018$YMAX,sf.object.21Nov2018$YMIN,
                             sf.object.12Dec2018$YMAX,sf.object.12Dec2018$YMIN,
                             sf.object.7Jan2019$YMAX,sf.object.7Jan2019$YMIN,
                             sf.object.25Jan2019$YMAX,sf.object.25Jan2019$YMIN))

df.lat <- data.frame(round(((data.lat$X1+data.lat$X2)/2), precision), 
                     round(((data.lat$X3+data.lat$X4)/2), precision),
                     round(((data.lat$X5+data.lat$X6)/2), precision),
                     round(((data.lat$X7+data.lat$X8)/2), precision))

colnames(df.lat) <- c("lat_21Nov2018", "lat_12Dec2018", "lat_7Jan2019", "lat_25Jan2019")

# data.graticule <- ((data.lon$X1+data.lon$X2)/2)+((data.lat$X1+data.lat$X2)/2), 

data.spatial.lai <-data.frame(cbind(sf.object.21Nov2018$LAI, sf.object.12Dec2018$LAI, 
                                          sf.object.7Jan2019$LAI, sf.object.25Jan2019$LAI, 
                                          df.lat$lat_21Nov2018, df.lon$lon_21Nov2018))



colnames(data.spatial.lai) <- c("LAI_21Nov2018", "LAI_12Dec2018", 
                                      "LAI_7Jan2019", "LAI_25Jan2019", 
                                      "lat_21Nov2018", "lon_21Nov2018" )

head(data.spatial.lai)

# Reshape data into long format
data.spatial.lai_long <- melt(data.spatial.lai, id.vars = c("lat_21Nov2018", "lon_21Nov2018"), variable.name = "Time", value.name = "LAI")

# Create Hovmöller plot - Latitude
hovplot.lat <- 
  ggplot(data.spatial.lai_long, aes(x = Time, y = lat_21Nov2018, fill = LAI)) +
  geom_tile() +
  scale_fill_gradient(low = "blue2", high = "red2") +
  labs(title = "Hovmöller Plot of Leaf Area Index",
       x = "Time",
       y = "Latitude",
       fill = "LAI") +
  # geom_text(aes(label = lon_21Nov2018), color = "black", size = 3, vjust = -0.5, hjust = 0.5) +
  theme_minimal()

hovplot.lat
ggsave("LAI_lat_Hovmöller_plot.jpeg", plot = hovplot.lat, width = 11.7, height = 8.3, units = "in", dpi = 500)

# Create Hovmöller plot - Latitude
hovplot.lon <- 
  ggplot(data.spatial.lai_long, aes(x = Time, y = lon_21Nov2018, fill = LAI)) +
  geom_tile() +
  scale_fill_gradient(low = "blue2", high = "red2") +
  labs(title = "Hovmöller Plot of Leaf Area Index",
       x = "Time",
       y = "Longitude",
       fill = "LAI") +
  # geom_text(aes(label = lon_21Nov2018), color = "black", size = 3, vjust = -0.5, hjust = 0.5) +
  theme_minimal()

hovplot.lon
ggsave("LAI_lon_Hovmöller_plot.jpeg", plot = hovplot.lon, width = 11.7, height = 8.3, units = "in", dpi = 500)

# Create individual plot assesment 
data.plot.lai <-data.frame(cbind(sf.object.7Jan2019$plot_id ,sf.object.21Nov2018$LAI, sf.object.12Dec2018$LAI, 
                            sf.object.7Jan2019$LAI, sf.object.25Jan2019$LAI))

# Set column names
colnames(data.plot.lai) <- c("plot_id" ,"LAI_21Nov2018", "LAI_12Dec2018", "LAI_7Jan2019", "LAI_25Jan2019")

summary(data.plot.lai)

data.plot.lai <- group_by(data.plot.lai)

df.plot.lai <- round(data.plot.lai %>% group_by(plot_id) %>% 
              summarise(across(c(LAI_21Nov2018,
                                 LAI_12Dec2018, 
                                 LAI_7Jan2019,
                                 LAI_25Jan2019), mean)),2)
# Set column names
colnames(df.plot.lai) <- c("plot_id" ,"21Nov2018", "12Dec2018", "7Jan2019", "25Jan2019")

# Melt the data for easier plotting
df_melted.lai <- melt(df.plot.lai, id.vars = "plot_id")



# Create the bar plot
bar_plot <- ggplot(df_melted.lai, aes(x = plot_id, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Plot for Individual Plot Performance",
       x = "Plot",
       y = "Leaf Area Index",
       fill = "Variable") +
  theme_minimal()
bar_plot



#===========================================================
# Get the plot labels on the map

sf.object <- sf.object.7Jan2019

# Check and fix invalid geometries
sf.object <- st_make_valid(sf.object)

# Grouping the data by plot_id
grouped_sf <- sf.object %>%
  group_by(plot_id) %>%
  summarise()

# Calculate centroids of each group for label placement
centroid <- st_centroid(grouped_sf)

plot <- ggplot() +
  geom_sf(data = sf.object, aes(fill = LAI_rn)) +
  geom_sf_text(data = centroid, aes(label = plot_id), size = 3, color = "black", fontface = "bold") +  # Add plot_id labels at centroids
  ggtitle('LAI of 7th Jan 2019 with Plot Ids') +
  theme_minimal() +
  coord_sf(
    xlim = c(78.3986, 78.39915),
    ylim = c(17.3241, 17.3245),
    expand = TRUE
  ) +
  annotation_scale(location = "bl", width_hint = 0.25) +  # Add scale bar
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in")
  ) +  # Add north arrow
  scale_fill_viridis_c(name = "Normalised LAI") # Apply viridis color palette and legend title
plot
ggsave("Plot_Ids.jpeg", plot = plot, width = 11.7, height = 8.3, units = "in", dpi = 500)


#-------------------------------------------------------------------------------
# Create individual plot assessment 
data.plot.lai <-data.frame(cbind(sf.object.7Jan2019$plot_id ,sf.object.21Nov2018$LAI, sf.object.12Dec2018$LAI, 
                                 sf.object.7Jan2019$LAI, sf.object.25Jan2019$LAI))

# Set column names
colnames(data.plot.lai) <- c("plot_id" ,"LAI_21Nov2018", "LAI_12Dec2018", "LAI_7Jan2019", "LAI_25Jan2019")

summary(data.plot.lai)

data.plot.lai <- group_by(data.plot.lai)

df.plot.lai <- round(data.plot.lai %>% group_by(plot_id) %>% 
                       summarise(across(c(LAI_21Nov2018,
                                          LAI_12Dec2018, 
                                          LAI_7Jan2019,
                                          LAI_25Jan2019), mean)),2)
# Set column names
colnames(df.plot.lai) <- c("plot_id" ,"21Nov2018", "12Dec2018", "7Jan2019", "25Jan2019")

# Melt the data for easier plotting
df_melted.lai <- melt(df.plot.lai, id.vars = "plot_id")

# Create the bar plot
bar_plot <- ggplot(df_melted.lai, aes(x = plot_id, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
  # geom_line(aes(color = variable, group = variable), stat = "identity") +
  labs(title = "Bar Plot for Individual Plot Performance",
       x = "Plot",
       y = "Leaf Area Index",
       fill = "Variable") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 27, by = 1)) +  # Custom x-axis ticks
  scale_y_continuous(breaks = seq(0, 8, by = 1))  # Custom y-axis ticks
# xlim = (0:8)

bar_plot

# Create the stacked bar plot
stacked_bar_plot <- ggplot(df_melted.lai, aes(x = plot_id, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Plot for Individual Plot Performance",
       x = "Plot",
       y = "Leaf Area Index",
       fill = "Date") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 27, by = 1)) +  # Custom x-axis ticks
  scale_y_continuous(breaks = seq(0, 20, by = 0.5))  # Custom y-axis ticks

# Print the plot
print(stacked_bar_plot)
ggsave("Stacked LAI.jpeg", plot = stacked_bar_plot, width = 11.7, height = 8.3, units = "in", dpi = 500)



#===============================================================================

data.plot.lai.overall <-data.frame(cbind(sf.object.7Jan2019$plot_id ,sf.object.21Nov2018$LAI, sf.object.12Dec2018$LAI, 
                                         sf.object.7Jan2019$LAI, sf.object.25Jan2019$LAI))

df.lai.plot.overall <- data.frame(cbind(sf.object.7Jan2019$plot_id,(data.plot.lai.overall$X2+
                                                                      data.plot.lai.overall$X3+
                                                                      data.plot.lai.overall$X4+
                                                                      data.plot.lai.overall$X5)/4))

colnames(df.lai.plot.overall) <- c('plot_id', 'lai_overall')
df.lai.plot.overall <- round(df.lai.plot.overall %>% group_by(plot_id) %>% 
                               summarise(across(c(lai_overall), sum)),2)

# Melt the data for easier plotting
df_melted.lai.overall <- melt(df.lai.plot.overall, id.vars = "plot_id")

ggplot(df_melted.lai.overall, aes(x = plot_id, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
  geom_line(aes(color = variable, group = variable), stat = "identity")


