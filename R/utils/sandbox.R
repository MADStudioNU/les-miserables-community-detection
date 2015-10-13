### Start ###

## Setup ##

# Parse
java -jar sgbbookparser.jar jean-complete.dat parsed_data/part1_nodes.csv parsed_data/part1_edges.csv parsed_data/jean-complete-character-table.html parsed_data/jean-complete-chapter-table.html "1\.(.*)"

# Set up the workspace
setwd('/Applications/MAMP/htdocs/LesMiserablesCommunityDetection/')
library(igraph)
library("devtools", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
#devtools::source_gist('45b49da5e260a9fc1cd7')
#devtools::source_gist('0b602fe4f48f2772f07a')
devtools::source_gist('6e4bf332d95b46d6d068')

# Read edges table
# d0 = read.csv(paste(file, ".csv", sep=""))
d = read.csv(file.choose(), header = T)[ ,c("Source", "Target")]

# Read nodes table
a = read.csv(file.choose(), header = T)

# Cast matrix type
m = as.matrix(d);

# Create undirected graph
g = graph.data.frame(m, directed = F)

# Create adjacency matrix
M = get.adjacency(g, sparse = F)

# Create weighted igraph from adjacency matrix
g = graph.adjacency(M, mode = "undirected", weighted = T)

# Add missing attributes to vertices
V(g)$description = as.character(a$Description[match(V(g)$name,a$Id)])
V(g)$label = as.character(a$Label[match(V(g)$name,a$Id)])

## Let's go ##

# Export your graphs from Gephi #

# Load graphs into R
book0graph_u = read.graph("step3_prelim_gephi_gml_exports/book0.gml", format=("gml"));
book1graph_u = read.graph("step3_prelim_gephi_gml_exports/book1.gml", format=("gml"));
book2graph_u = read.graph("step3_prelim_gephi_gml_exports/book2.gml", format=("gml"));
book3graph_u = read.graph("step3_prelim_gephi_gml_exports/book3.gml", format=("gml"));
book4graph_u = read.graph("step3_prelim_gephi_gml_exports/book4.gml", format=("gml"));
book5graph_u = read.graph("step3_prelim_gephi_gml_exports/book5.gml", format=("gml"));

book0graph_w = read.graph("step3_prelim_gephi_gml_exports/book0.gml", format=("gml"));
book1graph_w = read.graph("step3_prelim_gephi_gml_exports/book1.gml", format=("gml"));
book2graph_w = read.graph("step3_prelim_gephi_gml_exports/book2.gml", format=("gml"));
book3graph_w = read.graph("step3_prelim_gephi_gml_exports/book3.gml", format=("gml"));
book4graph_w = read.graph("step3_prelim_gephi_gml_exports/book4.gml", format=("gml"));
book5graph_w = read.graph("step3_prelim_gephi_gml_exports/book5.gml", format=("gml"));

# Run Newman Girvan community detection algorithm and store results in ebc*(_w) variable #
ebc0_u <- edge.betweenness.community(allgraph,   directed = FALSE, weights = NULL);
ebc1_u <- edge.betweenness.community(book1graph, directed = FALSE, weights = NULL);
ebc2_u <- edge.betweenness.community(book2graph, directed = FALSE, weights = NULL);
ebc3_u <- edge.betweenness.community(book3graph, directed = FALSE, weights = NULL);
ebc4_u <- edge.betweenness.community(book4graph, directed = FALSE, weights = NULL);
ebc5_u <- edge.betweenness.community(book5graph, directed = FALSE, weights = NULL);

ebc0_w <- edge.betweenness.community(allgraph_w,   directed = FALSE, weights = E(allgraph_w)$value);
ebc1_w <- edge.betweenness.community(book1graph_w, directed = FALSE, weights = E(book1graph_w)$value);
ebc2_w <- edge.betweenness.community(book2graph_w, directed = FALSE, weights = E(book2graph_w)$value);
ebc3_w <- edge.betweenness.community(book3graph_w, directed = FALSE, weights = E(book3graph_w)$value);
ebc4_w <- edge.betweenness.community(book4graph_w, directed = FALSE, weights = E(book4graph_w)$value);
ebc5_w <- edge.betweenness.community(book5graph_w, directed = FALSE, weights = E(book5graph_w)$value);

# Figure out how many groups do we have
book0GroupsN_u = max(ebc0_u$membership);
book1GroupsN_u = max(ebc1_u$membership);
# todo: finish this

# Add new "group" attribute to each vertex that equals to the calculated modularity class value #
V(allgraph_u)$group <-   ebc0_u$membership;
V(book1graph_u)$group <- ebc1_u$membership;
V(book2graph_u)$group <- ebc2_u$membership;
V(book3graph_u)$group <- ebc3_u$membership;
V(book4graph_u)$group <- ebc4_u$membership;
V(book5graph_u)$group <- ebc5_u$membership;

V(allgraph_w)$group <-   ebc0_w$membership;
V(book1graph_w)$group <- ebc1_w$membership;
V(book2graph_w)$group <- ebc2_w$membership;
V(book3graph_w)$group <- ebc3_w$membership;
V(book4graph_w)$group <- ebc4_w$membership;
V(book5graph_w)$group <- ebc5_w$membership;

# Generate palette and apply it to clusters (we want most distinctive colors)
V(allgraph)$color <- iwanthue(max(edge.betweenness.community(allgraph, directed=F, weights=NULL)$membership), cmin=40, lmin=55)[V(allgraph)$group]

V(book2graph)$color <- iwanthue(max(edge.betweenness.community(book2graph, directed=F,weights=NULL)$membership), cmin=40, lmin=55)[V(book2graph)$group]
plot(book2graph, vertex.label=V(book2graph)$Abbr)
# Optional: two handy functions for color palette generating
# library("devtools", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
# devtools::source_gist('45b49da5e260a9fc1cd7');

# One-liner plotter
# plot(allgraph, vertex.color = iwanthue(max(ebc0$membership), cmin = 40, lmin = 55)[V(allgraph)$color], layout = layout.auto);



# Write back to new gml files #
write.graph(book0graph_u, "step4_community_detection_in_R/book0-NG_u.gml", format = ("gml"));
write.graph(book1graph_u, "step4_community_detection_in_R/book1-NG_u.gml", format = ("gml"));
write.graph(book2graph_u, "step4_community_detection_in_R/book2-NG_u.gml", format = ("gml"));
write.graph(book3graph_u, "step4_community_detection_in_R/book3-NG_u.gml", format = ("gml"));
write.graph(book4graph_u, "step4_community_detection_in_R/book4-NG_u.gml", format = ("gml"));
write.graph(book5graph_u, "step4_community_detection_in_R/book5-NG_u.gml", format = ("gml"));

write.graph(book0graph_w, "step4_community_detection_in_R/book0-NG_w.gml", format = ("gml"));
write.graph(book1graph_w, "step4_community_detection_in_R/book1-NG_w.gml", format = ("gml"));
write.graph(book2graph_w, "step4_community_detection_in_R/book2-NG_w.gml", format = ("gml"));
write.graph(book3graph_w, "step4_community_detection_in_R/book3-NG_w.gml", format = ("gml"));
write.graph(book4graph_w, "step4_community_detection_in_R/book4-NG_w.gml", format = ("gml"));
write.graph(book5graph_w, "step4_community_detection_in_R/book5-NG_w.gml", format = ("gml"));

# End #

#java -jar classes/artifacts/sgbbookparser_jar/sgbbookparser.jar /Applications/MAMP/htdocs/lesmiserables_DATA/jean-complete.dat /Applications/MAMP/htdocs/lesmiserables_DATA/parsed_data/part0_nodes.csv  /Applications/MAMP/htdocs/lesmiserables_DATA/parsed_data/part0_edges.csv /Applications/MAMP/htdocs/lesmiserables_DATA/parsed_data/jean-complete-character-table.html /Applications/MAMP/htdocs/lesmiserables_DATA/parsed_data/jean-complete-chapter-table.html
java -jar sgbbookparser.jar jean-complete.dat parsed_data/part0_nodes.csv parsed_data/part0_edges.csv parsed_data/jean-complete-character-table.html parsed_data/jean-complete-chapter-table.html

#java -jar classes/artifacts/sgbbookparser_jar/sgbbookparser.jar /Applications/MAMP/htdocs/lesmiserables_DATA/jean-complete.dat /Applications/MAMP/htdocs/lesmiserables_DATA/parsed_data/part1_nodes.csv /Applications/MAMP/htdocs/lesmiserables_DATA/parsed_data/part1_edges.csv /Applications/MAMP/htdocs/lesmiserables_DATA/parsed_data/jean-complete-character-table.html /Applications/MAMP/htdocs/lesmiserables_DATA/parsed_data/jean-complete-chapter-table.html "1\.(.*)"
java -jar sgbbookparser.jar jean-complete.dat parsed_data/part1_nodes.csv parsed_data/part1_edges.csv parsed_data/jean-complete-character-table.html parsed_data/jean-complete-chapter-table.html "1\.(.*)"