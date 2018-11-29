plot_PCA = function(e, f, p, color,size, title = ''){

  sds = apply(e,1,sd,na.rm = TRUE)
  e = e[sds>0,]
  f = f[sds>0,]

  pca = prcomp(t(e), center = T, scale. = T)
  variance = pca$sdev^2/sum(pca$sdev^2)
  pca.data = data.frame(pca$x,color = color,order = 1:nrow(pca$x), size = size)

  gg = ggplot(pca.data, aes(PC1, PC2, color = color,size = size, order = order)) +
    geom_point(alpha = 3/4) +
    stat_ellipse( linetype = 2, size = 0.5) +
    labs(x = paste0("PC1: ",signif(variance[1]*100,3),"%"), y = paste0("PC2: ",signif(variance[2]*100,3),"%"),
         title = title)
  return(list(gg, pca))
}
