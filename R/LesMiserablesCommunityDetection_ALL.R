detectLesMisCommunitiesAllAlgorithms <- function(sourceFileName){

#    for(source in sources) {
        d <- read.csv(paste("parsed_data/", sourceFileName, "_edges.csv", sep=""), header = T, na.strings = "NaN")[ ,c("Source", "Target")]
        a <- read.csv(paste("parsed_data/", sourceFileName, "_nodes.csv", sep=""), header = T, na.strings = "NaN")

        m <- as.matrix(d)
        g <- graph.data.frame(m, directed = F)
        M <- get.adjacency(g, sparse = F)
        g <- graph.adjacency(M, mode = "undirected", weighted = T)

        V(g)$description <- as.character(a$Description[match(V(g)$name,a$Id)])
        V(g)$label <- as.character(a$Label[match(V(g)$name,a$Id)])

        G_u <- g
        G_w <- g

        ebc_u <- edge.betweenness.community(g, directed = FALSE, weights = NULL)
        ebc_w <- edge.betweenness.community(g, directed = FALSE, weights = E(g)$weight)
        #todo: add all other from igraph (one-liners)
        V(G_u)$ebc  <- cluster_edge_betweenness(g, directed = FALSE, weights = NULL)$membership
        V(G_u)$fgc  <-
        V(G_u)$wtc4 <-
        V(G_u)$wtc5 <-
        V(G_u)$wtc6 <-
        V(G_u)$sgc  <-



        n_u <- max(ebc_u$membership);
        n_w <- max(ebc_w$membership);

        V(G_u)$ebc <- ebc_u$membership;
        V(G_w)$ebc <- ebc_w$membership;

        V(G_u)$ebc_color <- iwanthue(n_u, cmin=40, lmin=55)[V(G_u)$ebc]
        V(G_w)$ebc_color <- iwanthue(n_w, cmin=40, lmin=55)[V(G_w)$ebc]


        write.graph(G_u, paste("detected_communities/", sourceFileName, "-EBC_u.gml", sep=""), format = ("gml"));
        write.graph(G_w, paste("detected_communities/", sourceFileName, "-EBC_w.gml", sep=""), format = ("gml"));

        write.graph(G_u, paste("detected_communities/", sourceFileName, "-ALL_u.gml", sep=""), format = ("gml"));
        write.graph(G_w, paste("detected_communities/", sourceFileName, "-ALL_w.gml", sep=""), format = ("gml"));

        print("Done!")
#        print(paste("For", sourceFileName, "I detected", n_u, "(unweighed method) and", n_w, "(weighted method) communities."))
#        print(paste("Here is the palette applied onto the unweighted graph:"))
#        print(iwanthue(n_u, cmin=40, lmin=55))
#        print(paste("Here is the palette applied onto the weighted graph:"))
#        print(iwanthue(n_w, cmin=40, lmin=55))
#    }

#    par(mfrow=c(1,2))
#    plot(G_u, vertex.label=V(G_u)$name, main = paste(sourceFileName, "(unweighted)"))
#    plot(G_w, vertex.label=V(G_w)$name, main = paste(sourceFileName, "(weighted)"))
}