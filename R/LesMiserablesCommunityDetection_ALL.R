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

############################

detectLesMisCommunitiesAllAlgorithms <- function(sourceFileName){

    d <- read.csv(paste("parsed_data/", sourceFileName, "_edges.csv", sep=""), header = T, na.strings = "NaN")[ ,c("Source", "Target")]
    a <- read.csv(paste("parsed_data/", sourceFileName, "_nodes.csv", sep=""), header = T, na.strings = "NaN")

    m <- as.matrix(d)
    g <- graph.data.frame(m, directed = F)
    M <- get.adjacency(g, sparse = F)
    g <- graph.adjacency(M, mode = "undirected", weighted = T)

    V(g)$description <- as.character(a$Description[match(V(g)$name,a$Id)])
    V(g)$label <- as.character(a$Label[match(V(g)$name,a$Id)])

    G <- g

    ## Algos ##

    # U
    ebc_u  <- cluster_edge_betweenness(g, directed = FALSE, weights = NULL)$membership
    fgc_u  <- cluster_fast_greedy(g, weights = NULL)$membership
    wtc4_u <- cluster_walktrap(g, weights = NULL, steps = 4)$membership
#    wtc4_u <- 1
    wtc5_u <- cluster_walktrap(g, weights = NULL, steps = 5)$membership
#    wtc5_u <- 1
    wtc6_u <- cluster_walktrap(g, weights = NULL, steps = 6)$membership
#    wtc6_u <- 1
#    wtc7_u <- cluster_walktrap(g, weights = NULL, steps = 7)$membership
    sgc_u  <- cluster_spinglass(g, weights = NULL, spins = 200)$membership
#    sgc_u  <- 1
    inf_u  <- cluster_infomap(g, nb.trials = 100, e.weights = NULL)$membership
#    inf_u  <- 1
    lou_u  <- cluster_louvain(g, weights = NULL)$membership
#    lou_u  <- 1
    opt_u  <- cluster_optimal(g, weights = NULL)$membership
#    opt_u  <- 1
#    eig_u  <- cluster_leading_eigen(g, weights = NULL)$membership
    eig_u  <- 1

    # W
    ebc_w  <- cluster_edge_betweenness(g, directed = FALSE, weights = E(g)$weight)$membership
    fgc_w  <- cluster_fast_greedy(g, weights = E(g)$value)$membership
    wtc4_w <- cluster_walktrap(g, weights = E(g)$value, steps = 4)$membership
#    wtc4_w <- 1
    wtc5_w <- cluster_walktrap(g, weights = E(g)$value, steps = 5)$membership
#    wtc5_w <- 1
    wtc6_w <- cluster_walktrap(g, weights = E(g)$value, steps = 6)$membership
#    wtc6_w <- 1
#    wtc7_w <- cluster_walktrap(g, weights = E(g)$value, steps = 7)$membership
    sgc_w  <- cluster_spinglass(g, weights = E(g)$value, spins = 200)$membership
#    sgc_w  <- 1
    inf_w  <- cluster_infomap(g, nb.trials = 100, e.weights = E(g)$value)$membership
#    inf_w  <- 1
    lou_w  <- cluster_louvain(g, weights = E(g)$value)$membership
#    lou_w  <- 1
    opt_w  <- cluster_optimal(g, weights = E(g)$value)$membership
#    opt_w  <- 1
#    eig_w  <- cluster_leading_eigen(g, weights = E(g)$value)$membership
    eig_w  <- 1

    ## Assignment ##

    # U
    V(G)$ebcU  <- ebc_u
    V(G)$fgcU  <- fgc_u
    V(G)$wtc4U <- wtc4_u
    V(G)$wtc5U <- wtc5_u
    V(G)$wtc6U <- wtc6_u
#    V(G)$wtc7U <- wtc7_u
    V(G)$sgcU  <- sgc_u
    V(G)$infU  <- inf_u
    V(G)$louU  <- lou_u
    V(G)$optU  <- opt_u
    V(G)$eigU  <- eig_u

    # W
    V(G)$ebcW  <- ebc_w
    V(G)$fgcW  <- fgc_w
    V(G)$wtc4W <- wtc4_w
    V(G)$wtc5W <- wtc5_w
    V(G)$wtc6W <- wtc6_w
#    V(G)$wtc7W <- wtc7_w
    V(G)$sgcW  <- sgc_w
    V(G)$infW  <- inf_w
    V(G)$louW  <-lou_w
    V(G)$optW  <- opt_w
    V(G)$eigW  <- eig_w

    ## Coloring ##

    # U
    V(G)$ebcUcolor  <- iwanthue(max(ebc_u), cmin = 40, lmin = 55)[V(G)$ebcU]
    V(G)$fgcUcolor  <- iwanthue(max(fgc_u), cmin = 40, lmin = 55)[V(G)$fgcU]
    V(G)$wtc4Ucolor <- iwanthue(max(wtc4_u), cmin = 40, lmin = 55)[V(G)$wtc4U]
    V(G)$wtc5Ucolor <- iwanthue(max(wtc5_u), cmin = 40, lmin = 55)[V(G)$wtc5U]
    V(G)$wtc6Ucolor <- iwanthue(max(wtc6_u), cmin = 40, lmin = 55)[V(G)$wtc6U]
    V(G)$sgcUcolor  <- iwanthue(max(sgc_u), cmin = 40, lmin = 55)[V(G)$sgcU]
    V(G)$infUcolor  <- iwanthue(max(inf_u), cmin = 40, lmin = 55)[V(G)$infU]
    V(G)$louUcolor  <- iwanthue(max(lou_u), cmin = 40, lmin = 55)[V(G)$louU]
    V(G)$optUcolor  <- iwanthue(max(opt_u), cmin = 40, lmin = 55)[V(G)$optU]
    V(G)$eigUcolor  <- iwanthue(max(eig_u), cmin = 40, lmin = 55)[V(G)$eigU]

    # W
    V(G)$ebcWcolor  <- iwanthue(max(ebc_w), cmin = 40, lmin = 55)[V(G)$ebcW]
    V(G)$fgcWcolor  <- iwanthue(max(fgc_w), cmin = 40, lmin = 55)[V(G)$fgcW]
    V(G)$wtc4Wcolor <- iwanthue(max(wtc4_w), cmin = 40, lmin = 55)[V(G)$wtc4W]
    V(G)$wtc5Wcolor <- iwanthue(max(wtc5_w), cmin = 40, lmin = 55)[V(G)$wtc5W]
    V(G)$wtc6Wcolor <- iwanthue(max(wtc6_w), cmin = 40, lmin = 55)[V(G)$wtc6W]
    V(G)$sgcWcolor  <- iwanthue(max(sgc_w), cmin = 40, lmin = 55)[V(G)$sgcW]
    V(G)$infWcolor  <- iwanthue(max(inf_w), cmin = 40, lmin = 55)[V(G)$infW]
    V(G)$louWcolor  <- iwanthue(max(lou_w), cmin = 40, lmin = 55)[V(G)$louW]
    V(G)$optWcolor  <- iwanthue(max(opt_w), cmin = 40, lmin = 55)[V(G)$optW]
    V(G)$eigWcolor  <- iwanthue(max(eig_w), cmin = 40, lmin = 55)[V(G)$eigW]

    ## Write out ##

    write.graph(G, paste("detected_communities/", sourceFileName, "-GRAND.gml", sep=""), format = ("gml"));

    print("Done!")

    plot(
        c(max(ebc_u), max(ebc_w), max(fgc_u), max(fgc_w), max(wtc4_u), max(wtc4_w), max(wtc5_u), max(wtc5_w), max(wtc6_u), max(wtc6_w), max(sgc_u), max(sgc_w), max(inf_u), max(inf_w), max(lou_u), max(lou_w), max(opt_u), max(opt_w), max(eig_u), max(eig_w)),
        main = sourceFileName,
        xlab = "Algorithms",
        ylab = "N of detected communities",
        type = "p",
        xaxt="n"
    )

    axis(1, at = 1:20, labels = c("ebcU", "ebcW", "fgcU", "fgcW", "wtc4U", "wtc4W", "wtc5U", "wtc5W", "wtc6U", "wtc6W", "sgcU", "sgcW", "infU", "infW", "louU", "louW", "optU", "optW", "eigU", "eigW"))
}