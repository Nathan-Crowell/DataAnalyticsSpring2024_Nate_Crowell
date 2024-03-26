# PCA on Boston Dataset

install.packages('MASS')
data(Boston,package = 'MASS')

??Boston

# Principal Component Analysis 
# the prcomp() fucntion computes the principal components 
# and we have turned on scalling
help("prcomp")
pca_out = prcomp(Boston,scale. = T)

plot(pca_out)

help("biplot")
biplot(pca_out, scale = 0)

boston_pc = pca_out$x
head(boston_pc)

summary(boston_pc)
