# Make sure you've parsed the data and all the CSV files are in parsed_data folder
# java -jar sgbbookparser.jar jean-complete.dat parsed_data/part1_nodes.csv parsed_data/part1_edges.csv parsed_data/jean-complete-character-table.html parsed_data/jean-complete-chapter-table.html "1\.(.*)"

# Set working dir to the project's root
setwd("[PROJECT'S ROOT]")
library(igraph)
library(colorspace)

# Load helper functions or manually add them from R/utils.R
library(devtools)
devtools::source_gist('6e4bf332d95b46d6d068')

# Fire!
#detectLesMisCommunities( c("part0", "part1", "part2", "part3", "part4", "part5") )
detectLesMisCommunities("part0");