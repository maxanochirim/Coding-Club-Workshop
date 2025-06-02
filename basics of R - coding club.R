# Coding Club Workshop 1 - Basics of R
# Learning how to import and explore data, and make graphs about Edinburgh's biodiversity
# Written by Maximus Anochirim 20/01/2024 University of Greifswald
library(dplyr)
# package for manipulating and formatting data
setwd("tC:/CC_course_stream1-maser")
getwd()
#codes for setting and getting your working directory - a folder where R works from (gets data, saves plots and files)
edidiv <- read.csv("edidiv.csv")
View(edidiv)
str(edidiv)
summary(edidiv)
dim(edidiv)
head(edidiv)
tail(edidiv)
head(edidiv$taxonGroup)
class(edidiv$taxonGroup)
edidiv$taxonGroup <- as.factor(edidiv$taxonGroup)
summary(edidiv$taxonGroup)
unique(edidiv$taxonGroup)
Beetle <- filter(edidiv, taxonGroup == "Beetle")
Bird <- filter(edidiv, taxonGroup == "Bird")
Butterfly <- filter(edidiv, taxonGroup == "Butterfly")
Dragonfly <- filter(edidiv, taxonGroup == "Dragonfly")
Flowering.Plants <- filter(edidiv, taxonGroup == "Flowering.Plants")
Fungus <- filter(edidiv, taxonGroup == "Fungus")
Hymenopteran <- filter(edidiv, taxonGroup == "Hymenopteran")
Lichen <- filter(edidiv, taxonGroup == "Lichen")
Liverwort <- filter(edidiv, taxonGroup == "Liverwort")
Mammal <- filter(edidiv, taxonGroup == "Mammal")
Mollusc <- filter(edidiv, taxonGroup == "Mollusc")
library(vegan)
# Package containing functions for calculating species richness, diversity and some other stats in vegetation ecology
#Calculating species richness:the number of different species in each group.
# For this, we will nest two functions together: unique(), which identifies different species, and length(), which counts them.
a <- length(unique(Beetle$taxonName))
b <- length(unique(Bird$taxonName))
c <- length(unique(Butterfly$taxonName))
d <- length(unique(Dragonfly$taxonName))
e <- length(unique(Flowering.Plants$taxonName))
f <- length(unique(Fungus$taxonName))
g <- length(unique(Hymenopteran$taxonName))
h <- length(unique(Lichen$taxonName))
i <- length(unique(Liverwort$taxonName))
j <- length(unique(Mammal$taxonName))
k <- length(unique(Mollusc$taxonName))

biodiv <- c(a,b,c,d,e,f,g,h,i,j,k)
names(biodiv) <- c("Beetle",
                   "Bird",
                   "Butterfly",
                   "Dragonfly",
                   "Flowering.Plants",
                   "Fungus",
                   "Hymenopteran",
                   "Lichen",
                   "Liverwort",
                   "Mammal",
                   "Mollusc")

data_frames_list <- list(Beetle, Bird, Butterfly, Dragonfly,
                         Flowering.Plants, Fungus, Hymenopteran,
                         Lichen, Liverwort, Mammal, Mollusc)
# Create a list of data frames
# Assuming your data frames are stored in a list named 'data_frames_list'
result <- data_frames_list %>%
  lapply(function(df) summarise(df, unique_count = n_distinct(taxonName))) %>%
  bind_cols()
# The code above uses the lapply function to apply the summarise function to each data frame in the list, calculating the number of distinct values in the "taxonName" column. 
# The bind_cols function is then used to combine the results into a single data frame.
names(result) <- c("Beetle",
                   "Bird",
                   "Butterfly",
                   "Dragonfly",
                   "Flowering.Plants",
                   "Fungus",
                   "Hymenopteran",
                   "Lichen",
                   "Liverwort",
                   "Mammal",
                   "Mollusc")
#barplot(result)
barplot(biodiv, xlab="Taxa", ylab="Number of species", ylim=c(0,600), cex.names= 1.5, cex.axis=1.5, cex.lab=1.5)

# fixing the plot (axis titles, making all column labels visible, and extending the y-axis value for plant species )
help(barplot)     # For help with the barplot() function
help(par)         # For help with plotting in general

#saving the plot using png and dev.off functions
png("barplot.png", width=1600, height=600)
barplot(biodiv, xlab="Taxa", ylab="Number of species", ylim=c(0,600), cex.names= 1.5, cex.axis=1.5, cex.lab=1.5)
dev.off()
# The cex code increases the font size when greater than one (and decreases it when less than one). 

#Creating a dataframe
# Creating an object called "taxa" that contains all the taxa names
taxa <- c("Beetle",
          "Bird",
          "Butterfly",
          "Dragonfly",
          "Flowering.Plants",
          "Fungus",
          "Hymenopteran",
          "Lichen",
          "Liverwort",
          "Mammal",
          "Mollusc")
# Turning this object into a factor, i.e. a categorical variable
taxa_f <- factor(taxa)

# Combining all the values for the number of species in an object called richness
richness <- c(a,b,c,d,e,f,g,h,i,j,k)

# Creating the data frame from the two vectors
biodata <- data.frame(taxa_f, richness)

# Saving the file
write.csv(biodata, file="biodata.csv")  # it will be saved in your working directory


#QUIZ
#producing a bar plot of the mean wingspan for each species

bird_sp <- c("sparrow",
             "kingfisher",
             "eagle",
             "hummingbird",
             "sparrow",
             "kingfisher",
             "eagle",
             "hummingbird",
             "sparrow",
             "kingfisher",
             "eagle",
             "hummingbird")
bird_sp_f <- factor(bird_sp)
wingspan <- c(22,26,195,8,24,23,201,9,21,25,185,9)
bird_values <- data.frame(bird_sp_f, wingspan)
str(bird_values)

eagle <- filter(bird_values, bird_sp_f == "eagle")
eagle_mean <- mean(eagle$wingspan)

hummingbird <- filter(bird_values, bird_sp_f == "hummingbird")
hummingbird_mean <- mean(hummingbird$wingspan)

kingfisher <- filter(bird_values, bird_sp_f == "kingfisher")
kingfisher_mean <- mean(kingfisher$wingspan)

sparrow <- filter(bird_values, bird_sp_f == "sparrow")
sparrow_mean <- mean(sparrow$wingspan)

total_mean <- c(eagle_mean, hummingbird_mean, kingfisher_mean, sparrow_mean)
names(total_mean) <- c("eagle",
                       "hummingbird",
                       "kingfisher",
                       "sparrow")
png("Quiz_wingspan_plot.png", width=800, height=600)
barplot(total_mean)
barplot(total_mean,
        xlab="Bird species",
        ylab="Mean wingspan (cm)",
        ylim=c(0,200),
        col = "gold")
dev.off()


#OR
wings <- list(eagle, hummingbird, kingfisher, sparrow)
wings_result <- wings %>%
  lapply(function(df) summarise(df, mean = mean(wingspan))) %>%
  bind_cols()
names(wings_result) <- c("eagle",
                         "hummingbird",
                         "kingfisher",
                         "sparrow")