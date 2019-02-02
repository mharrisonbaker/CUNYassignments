#' ---
#' title: "CUNY DATA 607 Assignment 1"
#' author: "Matthew Baker"
#' date: "Feb 2, 2019"
#' ---


#import data file into dataframe
mushdat = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", header = TRUE)

#check column names
names(mushdat)

#rename columns
names(mushdat) <- c("classes", "cap_shape", "cap_surface", "cap_color", "bruises", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")

#convert table to characters
mushdat[] <- lapply(mushdat, as.character)

#rename table values
mushdat$classes[mushdat$classes == "e"] <- "edible"
mushdat$classes[mushdat$classes == "p"] <- "poisonous"
mushdat$cap_shape[mushdat$cap_shape == "b"] <- "bell"
mushdat$cap_shape[mushdat$cap_shape == "c"] <- "conical"
mushdat$cap_shape[mushdat$cap_shape== "x"] <- "convex"
mushdat$cap_shape[mushdat$cap_shape== "f"] <- "flat"
mushdat$cap_shape[mushdat$cap_shape== "k"] <- "knobbed"
mushdat$cap_shape[mushdat$cap_shape== "s"] <- "sunken"
mushdat$cap_surface[mushdat$cap_surface== "f"] <- "fibrous"
mushdat$cap_surface[mushdat$cap_surface== "g"] <- "grooves"
mushdat$cap_surface[mushdat$cap_surface== "y"] <- "scaly"
mushdat$cap_surface[mushdat$cap_surface== "s"] <- "smooth"
mushdat$cap_color[mushdat$cap_color== "n"] <- "brown"
mushdat$cap_color[mushdat$cap_color== "b"] <- "buff"
mushdat$cap_color[mushdat$cap_color== "c"] <- "cinnamon"
mushdat$cap_color[mushdat$cap_color== "g"] <- "gray"
mushdat$cap_color[mushdat$cap_color== "r"] <- "green"
mushdat$cap_color[mushdat$cap_color== "p"] <- "pink"
mushdat$cap_color[mushdat$cap_color== "u"] <- "purple"
mushdat$cap_color[mushdat$cap_color== "e"] <- "red"
mushdat$cap_color[mushdat$cap_color== "w"] <- "white"
mushdat$cap_color[mushdat$cap_color== "y"] <- "yellow"
mushdat$bruises[mushdat$bruises== "t"] <- "bruises"
mushdat$bruises[mushdat$bruises== "f"] <- "no"

#show subset of columns
mushdat[1:5]






