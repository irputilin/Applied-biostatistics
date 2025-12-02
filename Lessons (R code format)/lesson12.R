library(MASS)
library(factoextra)
library(vegan)
library(ggpubr)
library(FactoMineR)


data("biopsy")
biopsy
df <- na.omit(biopsy)


pca_res <- prcomp(df[,2:10], scale = T, center = T, retx = T)
names(pca_res)

summary(pca_res)
pca_res$rotation

pca_res$sdev


eigenvals(pca_res)
bstick(pca_res)



fviz_eig(pca_res, addlabels = T)

screeplot(pca_res, type = 'lines', bstick = T)

fviz_pca_biplot(pca_res, label = 'var', col.ind = df$class)

ggplot(pca_res$x, aes(x = PC1, y = PC2, col = df$class))+
  geom_point()+
  stat_ellipse(level = 0.95)


fviz_contrib(pca_res, choice = 'var', axes = c(1))
fviz_contrib(pca_res, choice = 'var', axes = c(2))
fviz_contrib(pca_res, choice = 'var', axes = c(1,2))






plot1 <- ggplot(pca_res$x, aes(x = PC1, y = PC2, col = df$class))+
  geom_point(alpha = .5)+
  stat_ellipse(level = 0.95)
plot2 <- fviz_contrib(pca_res, choice = 'var', axes = c(1))
plot3 <- fviz_contrib(pca_res, choice = 'var', axes = c(2))


ggarrange(plot1, ggarrange(plot2, plot3, nrow = 2, labels = c('B', 'C')), ncol = 2,
          labels = c('A'))


pca_res2 <- PCA(df[,2:10])

plot4 <- fviz_contrib(pca_res2, choice = "var", axes = 1)+
  geom_text(
    aes(y = round(pca_res2$var$contrib[, 1], 2), 
        label = paste0(round(pca_res2$var$contrib[, 1], 1), "%")),
    vjust = -0.5,
    size = 3
  )


plot5 <- fviz_contrib(pca_res2, choice = "var", axes = 2)+
  geom_text(
    aes(y = round(pca_res2$var$contrib[, 2], 2), 
        label = paste0(round(pca_res2$var$contrib[, 2], 1), "%")),
    vjust = -0.5,
    size = 3
  )



plot(bstick(pca_res)/sum(bstick(pca_res))*100, type = 'l')

ggarrange(plot1, ggarrange(plot4, plot5, nrow = 2, labels = c('B', 'C')), ncol = 2,
          labels = c('A'))


plot6 <- fviz_eig(pca_res, addlabels = T)+
  geom_line(aes(y = bstick(pca_res)/sum(bstick(pca_res))*100, col = 'red'),
            show.legend = F)+
  geom_point(aes(y = bstick(pca_res)/sum(bstick(pca_res))*100, col = 'red'),
            show.legend = F)+
  geom_text(label = c(paste(round(bstick(pca_res)/sum(bstick(pca_res))*100, 1), '%')),
            nudge_y = 7, nudge_x = 0.3, color = 'red')

ggarrange(plot1, plot6, plot4, plot5, ncol = 2,
          nrow = 2, labels = c('A', 'B', 'C', 'D'))


