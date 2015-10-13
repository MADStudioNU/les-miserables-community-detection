# Make sure you've parsed the data and all the CSV files are in parsed_data folder
java -jar sgbbookparser.jar jean-completeO.dat parsed_data/part0o_nodes.csv parsed_data/part0o_edges.csv parsed_data/jean-complete-character-tableO.html parsed_data/jean-complete-chapter-tableO.html
java -jar sgbbookparser.jar jean-completeO.dat parsed_data/part1o_nodes.csv parsed_data/part1o_edges.csv parsed_data/jean-complete-character-tableO.html parsed_data/jean-complete-chapter-tableO.html "1\.(.*)"
java -jar sgbbookparser.jar jean-completeO.dat parsed_data/part2o_nodes.csv parsed_data/part2o_edges.csv parsed_data/jean-complete-character-tableO.html parsed_data/jean-complete-chapter-tableO.html "2\.(.*)"
java -jar sgbbookparser.jar jean-completeO.dat parsed_data/part3o_nodes.csv parsed_data/part3o_edges.csv parsed_data/jean-complete-character-tableO.html parsed_data/jean-complete-chapter-tableO.html "3\.(.*)"
java -jar sgbbookparser.jar jean-completeO.dat parsed_data/part4o_nodes.csv parsed_data/part4o_edges.csv parsed_data/jean-complete-character-tableO.html parsed_data/jean-complete-chapter-tableO.html "4\.(.*)"
java -jar sgbbookparser.jar jean-completeO.dat parsed_data/part5o_nodes.csv parsed_data/part5o_edges.csv parsed_data/jean-complete-character-tableO.html parsed_data/jean-complete-chapter-tableO.html "5\.(.*)"

# Set working dir to the project's root
#setwd('/Applications/MAMP/htdocs/LesMiserablesCommunityDetection/')
setwd("[PROJECT'S ROOT]")
library(igraph)
library(colorspace)

# Load helper functions or manually add them from R/utils/functions.R
library(devtools)
devtools::source_gist('6e4bf332d95b46d6d068')

# Fire one by one to see results
detectLesMisCommunities("part0");
detectLesMisCommunities("part1");
detectLesMisCommunities("part2");
detectLesMisCommunities("part3");
detectLesMisCommunities("part4");
detectLesMisCommunities("part5");