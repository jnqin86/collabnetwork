# genbank networks

# Power law alpha value, fit, and x-min. 
# Then we can calculate and visualize the change in power law alpha per year. 

library(igraph)
library(scales)
pub.dat.dir <- "C:/Users/jjhemsle/Dropbox/GenBank datasets/Analysis result files/publication/"
sub.dat.dir <- "C:/Users/jjhemsle/Dropbox/GenBank datasets/Analysis result files/datasubmissions/"
plot.dir <- "C:/Users/jjhemsle/Dropbox/GenBank datasets/Analysis result files/NetworkPlots-sub.pub/"
years <- 1992:2018

# how big do you want the pngs?
plot.width <- plot.height <- 2000

pub.dat.files <- paste(pub.dat.dir, "pub-network-graph", years, ".rda", sep = "")
sub.dat.files <- paste(sub.dat.dir, "sub-network-graph", years, ".rda", sep = "")

for (i in 1:length(years)) {
    # i <- 10
    print(paste0(" . . . STARTING ", years[i]))
    fname <- pub.dat.files[i]
    print(fname)
    load(fname)
    pub.g <- g
    E(pub.g)$color <- "red"
    V(pub.g)$color <- "red"
    rm(g)
    
    fname <- sub.dat.files[i]
    print(fname)
    load(fname)
    sub.g <- g
    E(sub.g)$color <- "green"
    V(sub.g)$color <- "green"
    rm(g)
    
    print(paste0(years[i], ":: Pub nodes: ", vcount(pub.g), ", sub nodes: ", vcount(sub.g)))
    
    g <- sub.g %u% pub.g
    g
    rm(pub.g, sub.g)
    
    # need a way to know who is in which network. Some are in both. No one is in neither. :-)
    pub.v.index <- which(!is.na(V(g)$color_2))
    pub.e.index <- which(!is.na(E(g)$color_2))
    sub.v.index <- which(!is.na(V(g)$color_1))
    sub.e.index <- which(!is.na(E(g)$color_1))
    
    in.both.v.index <- intersect(pub.v.index, sub.v.index)
    in.both.e.index <- intersect(pub.e.index, sub.e.index)
    print(paste0(years[i], ":: nodes in both networks: ", length(in.both.v.index)))
    
    V(g)$frame.color <- NA
    V(g)$label <- NA
    E(g)$curved <- .5
    
    V(g)$color <- "cornflowerblue"
    V(g)$color[sub.v.index] <- "brown1"
    V(g)$color[in.both.v.index] <- "darkorchid"
    V(g)$color <- alpha(V(g)$color, .4)
    
    E(g)$color <- "green"
    E(g)$color[sub.e.index] <- "red"
    E(g)$color[in.both.e.index] <- "brown4"
    E(g)$color <- alpha(E(g)$color, .2)
    
    
    #size <- 1 + log10(degree(g))
    #plot(size)
    #V(g)$size <- size
    V(g)$size <- 1
    
    plot.fname <- paste0(plot.dir, "FullNetwork_Degree_dist_", years[i], "-", plot.width, "x", plot.height, ".png")
    png(plot.fname, width = 1000, height = 1000)
    plot(jitter(degree(g)), col = V(g)$color, pch = 16, cex = .1, bty = "n", ylab = "degree", xlab = "no sort: order as in network")
    mtext(text = paste0(years[i], ": degree distribution"), side = 3, line = 2, adj = 0, cex = 1.4)
    mtext(text = "red: submission; blue: publication; purple: both", side = 3, line = .8, adj = 0, cex = .9)
    mtext(text = 
            paste0("sub only nodes: ", length(sub.v.index), ", pub only nodes: ", length(pub.v.index), ", both: ", length(in.both.v.index))
          , side = 3, line = -.2, adj = 0, cex = .9)
    dev.off()
    
    #g2 <- permute(g, sample(vcount(g)))    
    #plot(jitter(degree(g2)), col = V(g2)$color, pch = 16, cex = .1)
    g
    
    V(g)$comp <- components(g)$membership
    sort(table(V(g)$comp), decreasing = T)[1:10]
    # g <- induced_subgraph(g, V(g)[components(g)$membership == which(components(g)$csize == 57)])
    g <- induced_subgraph(g, V(g)[components(g)$membership == which.max(components(g)$csize)])
    
    
    ptm <- proc.time()
    l <- layout_with_drl(g, options = drl_defaults$final)
    how.long.did.this.take <- proc.time() - ptm
    print(paste0(years[i], ":: layout time ", round(how.long.did.this.take[3], 4), " seconds on ", vcount(g), " nodes"))
    
    
    plot.fname <- paste0(plot.dir, "FullNetwork-", years[i], "-", plot.width, "x", plot.height, ".png")
    ptm <- proc.time()
    
    png(filename = plot.fname, width = plot.width, height = plot.height)
    plot.igraph(g, layout = l, edge.arrow.size=0, edge.arrow.width=0)
    dev.off()
    
    how.long.did.this.take <- proc.time() - ptm
    print(paste0(" -- plotting took ", round(how.long.did.this.take[3], 4), " seconds (", round(how.long.did.this.take[3]/60, 2), " minutes)"))

    rm(g, l, pub.e.index, pub.v.index, sub.e.index, sub.v.index, in.both.e.index, in.both.v.index)
}














# extra code NOT run.
# stop execution here.
stop()

ptm <- proc.time()
l <- layout_with_lgl(g)
how.long.did.this.take <- proc.time() - ptm
print(paste0(years[i], ":: layout time ", round(how.long.did.this.take[3], 4), " seconds"))

ptm <- proc.time()
plot.igraph(g, layout = l, edge.arrow.size=0, edge.arrow.width=0)
how.long.did.this.take <- proc.time() - ptm
print(paste0(" -- plotting took ", round(how.long.did.this.take[3], 4), " seconds"))


