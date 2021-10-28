# install necessary packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, kohonen, datasets, grid , tidyverse, ggplot2, lubridate, arules, plyr)
pacman::p_load(here, data.table, purrlyr, tidyverse , simplevis, gt, gtsummary, flextable, SmartEDA, DataExplorer, DT, expss, 
                ggcal, vtree, inspectdf, lubridate, janitor, forcats, fastDummies, units, tsibble, feasts, fable, 
                sf, raster, mapproj, tmap, tmaptools, mapdeck, leaflet, leafgl, rgeoda, osmplotr, osmdata, 
                exactextractr, geomerge, hereR, ggmap, 
                kableExtra, knitr, 
                colorValues, viridis, 
                readxl, rio, fst, 
                tictoc, beepr, 
                ggfortify, gganimate, 
                grateful)

# load dataset
load("airbnbListings.RData")

# class of dataset 
class(cpt_listings_in)

# Provided code chunks
# ref: https://rpubs.com/sebnemer/insideairbnbCapeTown
# clean and variable mutate
cpt_listings_in <- cpt_listings_in %>%
  mutate(price_clean = price, 
         price_clean = as.numeric(str_remove_all( price_clean, "\\$|")), 
         bathrooms_text = replace(bathrooms_text, bathrooms_text == "Half-bath", 0.5), 
         bathrooms_text = replace(bathrooms_text, bathrooms_text == "Shared half-bath", 0.5),
         bathrooms_text = replace(bathrooms_text, bathrooms_text == "private half-bath", 0.5),
         bathrooms_text = str_remove_all(bathrooms_text, "private |shared |s"), 
         bathrooms = as.numeric(str_remove_all(bathrooms_text, "bath")), 
         # clean price - remove dollar and comma
         haccrclean = host_acceptance_rate, 
         haccrclean = as.numeric(str_remove_all(host_response_rate, "\\%")),
         hresponserclean = host_response_rate, 
         hresponserclean = as.numeric(str_remove_all(host_response_rate, "\\%"))) %>%
  mutate(across(where(is.logical), as.character)) %>%
  separate(neighbourhood_cleansed, c("Ward ", "number")) %>%
  mutate(WARDNO=as.numeric(number)) %>%
  mutate(fullyBooked = availability_365==0)


cpt_listings_in <- cpt_listings_in %>%
  mutate(neighbourhood_clean = neighbourhood,
         neighbourhood_clean= sub("Cape Town, WC, South Africa", "Western Cape", neighbourhood_clean), 
         neighbourhood_clean = sub("Western Cape , South Africa , South Africa", "Western Cape", neighbourhood_clean), 
         neighbourhood_clean = sub("Cape Town, Western Cape, South Africa", "Western Cape", neighbourhood_clean), 
         neighbourhood_clean = sub("\nWestern Cape", "Western Cape", neighbourhood_clean), 
         neighbourhood_clean = sub("Cape Town, South Africa / Cape Town / Western Cape, South Africa", "Western Cape", neighbourhood_clean), 
         neighbourhood_clean = sub("cape town,, western cape, South Africa", "Western Cape", neighbourhood_clean), 
         neighbourhood_clean = sub("Cape Town, Western Province, South Africa", "Western Cape", neighbourhood_clean))

cbind(cpt_listings_in$neighbourhood_clean,"#", cpt_listings_in$neighbourhood)

# Generate Dummy variables
cpt_listings_in = cpt_listings_in%>%mutate(var = 1) %>%
  spread(room_type, var, fill = 0, sep = "_") %>%
  left_join(cpt_listings_in) %>%
  dplyr::select(everything())

cpt_listings_in = cpt_listings_in%>%mutate(var = 1) %>%
  spread(property_type, var, fill = 0, sep = "_") %>%
  left_join(cpt_listings_in) %>%
  dplyr::select(everything())

cpt_listings_in = cpt_listings_in%>%mutate(var = 1) %>%
  spread(fullyBooked, var, fill = 0, sep = "_") %>%
  left_join(cpt_listings_in) %>%
  dplyr::select(everything())

cpt_listings_in = cpt_listings_in%>%mutate(var = 1) %>%
  spread(host_is_superhost, var, fill = 0, sep = "_") %>%
  left_join(cpt_listings_in) %>%
  dplyr::select(everything())

# Data overview and Summary stats

names(cpt_listings_in)

cpt_listings_in %>% ggplot(mapping = aes(x = bedrooms, y = log(price_clean))) + geom_point()

cpt_listings_in %>% ggplot(mapping = aes (x = accommodates, y = price_clean)) + geom_point()


# ward level averages 
wardLevelAverages = cpt_listings_in %>%
  group_by(WARDNO) %>%
  dplyr::summarise(averageprice = mean(price_clean),
                   hsuperhostratio = mean(host_is_superhost_TRUE), 
                   avertype_enthome = mean(`room_type_Entire home/apt`), 
                   avertype_Hotelroom = mean(`room_type_Hotel room`), 
                   avertype_Privateroom = mean(`room_type_Private room`), 
                   avertype_Sharedroom = mean(`room_type_Shared room`), 
                   aveaccommodates = mean(accommodates), 
                   avebedrooms = mean(bedrooms), avebeds = mean(beds), 
                   avefbooked = mean(fullyBooked_TRUE))


# Part 1: Exploratory Data Analysis

# select variables that will be used for clustering
selected_data <- dplyr::select(cpt_listings_in, c(neighbourhood, property_type,
                                         room_type, accommodates,
                                         bathrooms, beds, price_clean, review_scores_rating,
                                         bedrooms, host_total_listings_count, hresponserclean))
  
# convert selected data to data frame 
selected_data.df <- as.data.frame(selected_data)
dim(selected_data.df) # dimension of data

# Type of Variables in the dataset
str(selected_data.df)

# visualise missing data 
# library(mice)
# md.pattern(selected_data.df)

library(VIM)
aggr_plot <- aggr(selected_data.df, col=c('navyblue','red'), numbers=TRUE, 
                  sortVars=TRUE, labels=names(selected_data.df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# 53.8% of the data are missing price value

# display missing data in a table
lapply(selected_data.df, function(x) {length(which(is.na(x)))})

# omit all missing values
cpt_data <- na.omit(selected_data.df)

cpt_data.df <- as.data.frame(cpt_data)
dim (cpt_data.df)
view(cpt_data.df)

# display number of missing values in a table
# lapply(cpt_data.df, function(x) {length(which(is.na(x)))})


# visualizing distributions 
# ggplot(data = cpt_data) + geom_bar(mapping = aes(x = room_type))
# ggplot(data = cpt_data) + geom_bar(mapping = aes(x = property_type))

ggplot(data=cpt_data.df) + 
  geom_histogram(mapping = aes(x=host_total_listings_count), binwidth=1)

cpt_data %>% 
  dplyr::count(cpt_data$room_type)

cpt_data %>% 
  dplyr::count(cpt_data$property_type)

cpt_data %>% 
  dplyr::count(cpt_data$neighbourhood)

ggplot(data=cpt_data) +
  geom_histogram(mapping = aes(x = accommodates), binwidth = 1)

ggplot(data=cpt_data) + 
  geom_histogram(mapping = aes(x = bathrooms), binwidth = 1)

ggplot(data=cpt_data) + 
  geom_histogram(mapping = aes(x = beds), binwidth = 1)

ggplot(data = cpt_data) + 
  geom_histogram(mapping = aes(x = price_clean), binwidth = 100)

ggplot(data = cpt_data, mapping = aes(x = room_type, y = price_clean)) + geom_boxplot()

# plotting a map 
cpt_listings_in_noNA = cpt_listings_in
cpt_listings_in_noNA %>% 
  drop_na(price_clean)

height <- max(cpt_listings_in_noNA$latitude) - min(cpt_listings_in_noNA$latitude)
width <- max(cpt_listings_in_noNA$longitude) - min(cpt_listings_in_noNA$longitude)
capeTown_borders <- c(bottom  = min(cpt_listings_in_noNA$latitude)  - 0.1 * height, 
                top     = max(cpt_listings_in_noNA$latitude)  + 0.1 * height,
                left    = min(cpt_listings_in_noNA$longitude) - 0.1 * width,
                right   = max(cpt_listings_in_noNA$longitude) + 0.1 * width)

map <- get_stamenmap(capeTown_borders, zoom = 10, maptype = "toner-lite")

ggmap(map) +
  geom_point(data = cpt_listings_in_noNA, mapping = aes(x = cpt_listings_in_noNA$longitude, y = cpt_listings_in_noNA$latitude, 
                                               col = cpt_listings_in_noNA$price_clean)) +
  scale_color_distiller(palette = "YlOrRd", direction = 1)


library(corrplot)
data_corre <- cor(data_cor)
corrplot(data_corre, method="circle")







# part 2: Cluster Analysis Hierarchical (HC) and non-hierarchical (NHC)
# Heirarchical (HC) analysis
cpt_data <- na.omit(cpt_data)
cpt_data.df <- as.data.frame(cpt_data[, 4:11])
cpt_data.df <- scale(cpt_data.df)

set.seed(123)
dist.out <- dist(cpt_data[, 4:11], method = "manhattan")

hc <- hclust(dist.out, method = "complete")
plot(hc, hang = -1, cex = 0.6)




# k - means (non-hierarchical)
km.out <- kmeans(cpt_data.df, 2, nstart = 25)
km.out$cluster

# elbow method for k-means clustering
k.max <- 15 # maximum number of clusters
cpt_data.df.out <-cpt_data.df 
wss <- sapply(1:k.max, 
              function(k){kmeans(cpt_data.df.out, k , nstart=25) $tot.withinss})
plot(1:k.max, wss, type="b", pch = 19, frame = FALSE, xlab = "Number of clusters k", 
     ylab="Total within-clusters sum of squares")
abline(v=2, lty=2)

fviz_cluster(km.out, data = cpt_data.df.out,
             palette = c("#2E9FDF", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


km.out$centers



# Part 3: Self Organising Map 
cpt_data_som <- cpt_data[, 4:11]
cpt_data_som = scale(cpt_data_som)

som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
set.seed(123)
som_model <- som(cpt_data_som, grid = som_grid, 
                 rlen = 1000, 
                 alpha = c(0.05, 0.01), 
                 keep.data = TRUE)
names(som_model)
plot(som_model, type="changes")
plot(som_model, type="counts")
plot(som_model, type="dist.neighbours")
plot(som_model, type="codes")

# codes = some_model$codes[[1]]
# par(mfrow = c(3, 3))
# for(i in 2:10) {
#   plot(som_model, type="property", property=codes[,i-1], main=names(cpt_data_som)[i])
# }

data <- som_model$codes[[1]]
wss <- (nrow(data)-1)*sum(apply(data, 2, var)) 
for(i in 2:10) {
  wss[i] <- sum(kmeans(data, centers = i)$withinnss)
}
plot(wss, type = "o")

som_cluster <- cutree(hclust(dist(som_model$codes [[1]])), 2)

plot(som_model, type="mapping", labels=cpt_data$neighbourhood, main = "clusters", cex = .5)
