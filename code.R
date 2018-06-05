barplot(res$eig[,2], names.arg = 1:nrow(res$eig))
drawn <-
c("Myanmar", "Rwanda", "Burundi", "Somaliland Region", "New Zealand", 
"Lithuania", "Greece", "Denmark", "Switzerland")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'ind', invisible = 'quali', title = '', cex = cex)
wilks.p <-
structure(1.21717361534953e-38, .Names = "Region")
wilks.p
sample = sample(rownames(res$call$X), length(rownames(res$call$X)))
res$call$X = res$call$X[sample,]
res$ind$coord = res$ind$coord[sample[!sample %in% rownames(res$ind.sup$coord)],]
res$ind.sup$coord = res$ind.sup$coord[sample[sample %in% rownames(res$ind.sup$coord)],]
drawn <-
c("Myanmar", "Rwanda", "Burundi", "Somaliland Region", "New Zealand", 
"Lithuania", "Greece", "Denmark", "Switzerland")
hab <-
"Region"
plotellipses(res, axes = 1:2, invisible = 'quali', select = drawn, keepvar = hab, title = '', cex = cex)
drawn <-
c("Happiness.Score", "Happiness.Rank", "Economy", "Life.Expectancy", 
"Family", "Freedom", "Generosity", "Gov.Trust")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'var', title = '', cex = cex)
drawn <-
c("Western Europe", "Australia and New Zealand", "Sub-Saharan Africa", 
"North America", "Southern Asia", "Southeastern Asia", "Central and Eastern Europe", 
"Latin America and Caribbean")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'ind', invisible = c('ind', 'ind.sup'), title = '', cex = cex)
res.hcpc = HCPC(res, nb.clust = -1, graph = FALSE)
drawn <-
c("Myanmar", "Rwanda", "Burundi", "Somaliland Region", "New Zealand", 
"Lithuania", "Greece", "Denmark", "Switzerland")
plot.HCPC(res.hcpc, choice = 'map', draw.tree = FALSE, select = drawn, title = '')
dimdesc(res, axes = 1:2)
res.hcpc$desc.var
