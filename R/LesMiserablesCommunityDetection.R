# Make sure you've parsed the data and all the CSV files are in parsed_data folder
java -jar sgbbookparser.jar jean-complete.dat parsed_data/part0_nodes.csv parsed_data/part0_edges.csv parsed_data/jean-complete-character-table.html parsed_data/jean-complete-chapter-table.html
java -jar sgbbookparser.jar jean-complete.dat parsed_data/part1_nodes.csv parsed_data/part1_edges.csv parsed_data/jean-complete-character-table.html parsed_data/jean-complete-chapter-table.html "1\.(.*)"
java -jar sgbbookparser.jar jean-complete.dat parsed_data/part2_nodes.csv parsed_data/part2_edges.csv parsed_data/jean-complete-character-table.html parsed_data/jean-complete-chapter-table.html "2\.(.*)"
java -jar sgbbookparser.jar jean-complete.dat parsed_data/part3_nodes.csv parsed_data/part3_edges.csv parsed_data/jean-complete-character-table.html parsed_data/jean-complete-chapter-table.html "3\.(.*)"
java -jar sgbbookparser.jar jean-complete.dat parsed_data/part4_nodes.csv parsed_data/part4_edges.csv parsed_data/jean-complete-character-table.html parsed_data/jean-complete-chapter-table.html "4\.(.*)"
java -jar sgbbookparser.jar jean-complete.dat parsed_data/part5_nodes.csv parsed_data/part5_edges.csv parsed_data/jean-complete-character-table.html parsed_data/jean-complete-chapter-table.html "5\.(.*)"

# Set working dir to the project's root
setwd("[PROJECT'S ROOT]")
library(igraph)
library(colorspace)

# Load helper functions or manually add them from R/utils/utils.R
library(devtools)
devtools::source_gist('6e4bf332d95b46d6d068')

# Fire one by one to see results
detectLesMisCommunities("part0");
detectLesMisCommunities("part1");
detectLesMisCommunities("part2");
detectLesMisCommunities("part3");
detectLesMisCommunities("part4");
detectLesMisCommunities("part5");