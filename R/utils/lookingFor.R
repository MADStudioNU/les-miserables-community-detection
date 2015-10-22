library("igraph")
setwd("/Applications/MAMP/htdocs/LesMiserablesCommunityDetection/")

swatch <- function(x) {
  par(mai=c(0.2, max(strwidth(x, "inch") + 0.4, na.rm = TRUE), 0.2, 0.4))
  barplot(rep(1, length(x)), col=rev(x), space = 0.1, axes=FALSE,
          names.arg=rev(x), cex.names=0.8, horiz=T, las=1)
}

iwanthue <- function(n, hmin=0, hmax=360, cmin=0, cmax=180, lmin=0, lmax=100,
                     plot=FALSE, random=FALSE) {
  require(colorspace)
  stopifnot(hmin >= 0, cmin >= 0, lmin >= 0,
            hmax <= 360, cmax <= 180, lmax <= 100,
            hmin <= hmax, cmin <= cmax, lmin <= lmax,
            n > 0)
  if(!random) {
    if (exists(".Random.seed", .GlobalEnv)) {
      old_seed <- .GlobalEnv$.Random.seed
      on.exit(.GlobalEnv$.Random.seed <- old_seed)
    } else {
      on.exit(rm(".Random.seed", envir = .GlobalEnv))
    }
    set.seed(1)
  }
  lab <- LAB(as.matrix(expand.grid(seq(0, 100, 1),
                                   seq(-100, 100, 5),
                                   seq(-110, 100, 5))))
  if (any((hmin != 0 || cmin != 0 || lmin != 0 ||
           hmax != 360 || cmax != 180 || lmax != 100))) {
    hcl <- as(lab, 'polarLUV')
    hcl_coords <- coords(hcl)
    hcl <- hcl[which(hcl_coords[, 'H'] <= hmax & hcl_coords[, 'H'] >= hmin &
                       hcl_coords[, 'C'] <= cmax & hcl_coords[, 'C'] >= cmin &
                       hcl_coords[, 'L'] <= lmax & hcl_coords[, 'L'] >= lmin), ]
    lab <- as(hcl, 'LAB')
  }
  lab <- lab[which(!is.na(hex(lab))), ]
  clus <- kmeans(coords(lab), n, iter.max=50)
  if (isTRUE(plot)) {
    swatch(hex(LAB(clus$centers)))
  }
  hex(LAB(clus$centers))
}

lookingFor <- function(sourceFileName, postfix, writeFile = F){

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

    ###

#        ebc_u <- cluster_edge_betweenness(g, directed = FALSE, weights = NULL)
#        ebc_w <- cluster_edge_betweenness(g, directed = FALSE, weights = E(g)$weight)
#        sg_u <- cluster_spinglass(g, spins = 100, cool.fact = 0.99, gamma = 1.5, weights = NULL)
#        sg_w <- cluster_spinglass(g, spins = 100, cool.fact = 0.99, gamma = 1.5, weights = E(g)$weight)

    # Algorithms (PICK ONE)
#    ebc_u  <- cluster_edge_betweenness(g, directed = FALSE, weights = NULL)
#    fgc_u  <- cluster_fast_greedy(g, weights = NULL)
#    wtc4_u <- cluster_walktrap(g, weights = NULL, steps = 4)
#    wtc5_u <- cluster_walktrap(g, weights = NULL, steps = 5)
#    wtc6_u <- cluster_walktrap(g, weights = NULL, steps = 6)
#    wtc7_u <- cluster_walktrap(g, weights = NULL, steps = 7)
#    sgc_u  <- cluster_spinglass(g, spins = 100, cool.fact = 0.99, gamma = 1.5, weights = NULL)
#    inf_u  <- cluster_infomap(g, nb.trials = 100, e.weights = NULL)
#    lou_u  <- cluster_louvain(g, weights = NULL)
#    opt_u  <- cluster_optimal(g, weights = NULL)
#    eig_u  <- cluster_leading_eigen(g, weights = NULL)

#    ebc_w  <- cluster_edge_betweenness(g, directed = FALSE, weights = E(g)$weight)
#    fgc_w  <- cluster_fast_greedy(g, weights = E(g)$value)
#    wtc4_w <- cluster_walktrap(g, weights = E(g)$value, steps = 4)
#    wtc5_w <- cluster_walktrap(g, weights = E(g)$value, steps = 5)
#    wtc6_w <- cluster_walktrap(g, weights = E(g)$value, steps = 6)
#    wtc7_w <- cluster_walktrap(g, weights = E(g)$value, steps = 7)
#    sgc_w  <- cluster_spinglass(g, spins = 100, cool.fact = 0.99, gamma = 1.5, weights = E(g)$weight)
#    inf_w  <- cluster_infomap(g, nb.trials = 100, e.weights = E(g)$value)
#    lou_w  <- cluster_louvain(g, weights = E(g)$value)
#    opt_w  <- cluster_optimal(g, weights = E(g)$value)
#    eig_w  <- cluster_leading_eigen(g, weights = E(g)$value)

    groups_u <- cluster_spinglass(g, spins = 100, cool.fact = 0.99, gamma = 1.5, weights = NULL)
    groups_w <- cluster_spinglass(g, spins = 100, cool.fact = 0.99, gamma = 1.5, weights = E(g)$weight)

    n_u <- max(groups_u$membership);
    n_w <- max(groups_w$membership);

    V(G_u)$group <- groups_u$membership;
    V(G_w)$group <- groups_w$membership;

    V(G_u)$color <- iwanthue(n_u, cmin=40, lmin=55)[V(G_u)$group]
    V(G_w)$color <- iwanthue(n_w, cmin=40, lmin=55)[V(G_w)$group]

    if(writeFile)
    {
        write.graph(G_u, paste("detected_communities/looking/", sourceFileName, "-", postfix, "_u.gml", sep=""), format = ("gml"));
        write.graph(G_w, paste("detected_communities/looking/", sourceFileName, "-", postfix, "_w.gml", sep=""), format = ("gml"));
    }

    print("Done!")
    print(paste("For", sourceFileName, "I detected", n_u, "(unweighed method) and", n_w, "(weighted method) communities."))
    print(paste("Here is the palette applied onto the unweighted graph:"))
    print(iwanthue(n_u, cmin=40, lmin=55))
    print(paste("Here is the palette applied onto the weighted graph:"))
    print(iwanthue(n_w, cmin=40, lmin=55))

#    plot(G_u, vertex.label=V(G_u)$name, vertex.size = 5, vertex.label.cex = 0.75, main = paste(sourceFileName, postfix, "(unweighted)"))
#    plot(G_w, vertex.label=V(G_w)$name, vertex.size = 5, vertex.label.cex = 0.75, main = paste(sourceFileName, postfix, "(weighted)"))

    tkplot(G_u, vertex.label=V(G_u)$name, main = paste(sourceFileName, postfix, "(unweighted)"))
    tkplot(G_w, vertex.label=V(G_u)$name, main = paste(sourceFileName, postfix, "(weighted)"))
}