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

detectLesMisCommunities <- function(sourceFileName){

#    for(source in sources) {
        d <- read.csv(paste("parsed_data/", sourceFileName, "_edges.csv", sep=""), header = T)[ ,c("Source", "Target")]
        a <- read.csv(paste("parsed_data/", sourceFileName, "_nodes.csv", sep=""), header = T)

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

        n_u <- max(ebc_u$membership);
        n_w <- max(ebc_w$membership);

        V(G_u)$group <- ebc_u$membership;
        V(G_w)$group <- ebc_w$membership;

        V(G_u)$color <- iwanthue(n_u, cmin=40, lmin=55)[V(G_u)$group]
        V(G_w)$color <- iwanthue(n_w, cmin=40, lmin=55)[V(G_w)$group]

        write.graph(G_u, paste("detected_communities/", sourceFileName, "-NG_u.gml", sep=""), format = ("gml"));
        write.graph(G_w, paste("detected_communities/", sourceFileName, "-NG_w.gml", sep=""), format = ("gml"));

        print("Done!")
        print(paste("For", sourceFileName, "I detected", n_u, "(unweighed method) and", n_w, "(weighted method) communities."))
        print(paste("Here is the palette applied onto the unweighted graph:"))
        print(iwanthue(n_u, cmin=40, lmin=55))
        print(paste("Here is the palette applied onto the weighted graph:"))
        print(iwanthue(n_w, cmin=40, lmin=55))
#    }

    par(mfrow=c(1,2))
    plot(G_u, vertex.label=V(G_u)$name, main = paste(sourceFileName, "(unweighted)"))
    plot(G_w, vertex.label=V(G_u)$name, main = paste(sourceFileName, "(weighted)"))
}