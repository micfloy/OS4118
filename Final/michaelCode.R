library(ggfortify)   # help for plottng functions
library(GGally)      # some nice plot extensions to ggplot
library(cowplot)     # allows us to create matrix plots
library(varhandle)   # provides unfactor() function
library(cluster)     # provides bottom-up clustering support
library(factoextra)  # support for plotting of clusters
library(dbscan)      # support for the dbscan algorithm
library(gridExtra)
library(ggplot2)

frac = 0.8

rdf = read.csv (
  file = "2018-08-24_AGGREGATE_AAPL.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = ","
)

#rdf = read.csv (file = "2018-08-25_AGGREGATE_T.csv", header=TRUE, stringsAsFactors = FALSE, sep=",")

#rdf = read.csv (file = "2018-08-28_AGGREGATE_TSLA.csv", header=TRUE, stringsAsFactors = FALSE, sep=",")

dim(rdf)

sum(is.na(rdf))

#column names to delete

xcols = c(
  "Date",
  "open",
  "high",
  "low",
  "close",
  "adj.",
  "volume",
  "macd",
  "fastD.1",
  "fastK.1",
  "wikicnts",
  "d.hits",
  "wk.hits",
  "adj.d",
  "sd.month.d1",
  "sd.qrtr.d1",
  "sd.annual.d1",
  "Delt.1",
  "mrtg.d.hits",
  "trueHigh"
)

#xcols = list(NULL)

cols = colnames(rdf)
rem_cols = match(xcols, cols)
rem_cols
raw.predictors = rdf[, c(1:55)]
raw.predictors[, rem_cols] = list(NULL)
colnames(raw.predictors)

#split responses from predictors

raw.responses = rdf[, c(56:59)]

#raw.predictors = rdf[,c(1:55)]

#split test and training data

set.seed(7770777)

idx = seq(1, nrow(raw.predictors))
train_idx = sample(idx, round(frac * nrow(raw.predictors)), replace = FALSE)
length(train_idx)

train_predictors = raw.predictors[train_idx, ]
train_responses = raw.responses[train_idx, ]
train_all = cbind(train_predictors, train_responses)

test_predictors = raw.predictors[-train_idx, ]
test_responses = raw.responses[-train_idx, ]
test_all = cbind(test_predictors, test_responses)

head(test_predictors)


#################################################################
#### Old Code ###################################################
#################################################################

pca<- princomp(data.frame(scale(train_predictors)))
plot(pca)

plot(cumsum(pca$sdev^2/sum(pca$sdev^2)))

pca$loadings

pc <- prcomp(train_predictors, scale=TRUE)
plot(pc)

plot(cumsum(pc$sdev^2/sum(pc$sdev^2)), main = 'Cumulative Variance Explained by Components', 
     ylab="Explained", xlab="Component")

# First 10 principal components
comp <- data.frame(pc$x[,1:7])
# Plot
plot(comp, pch=9, col=rgb(0,0,0.5))

pc_plot1 <- autoplot(pc, data = train_all, main = "Plot of Resp.5d.2sd in Principle Component Space")
pc_plot2 <- autoplot(pc, data = train_all, colour = "Resp.5d.2sd", main = "Plot of Resp.5d.2sd in Principle Component Space") 

plot_grid(                                    # uses cowplot library to arrange grid
  pc_plot1, pc_plot2, 
  nrow = 1
)

autoplot(pc, data=train_all, colour = "Resp.5d.2sd")

autoplot(pc, data=train_all, colour = "Resp.10d.2sd")

pc_plot1 <- autoplot(pc, data = train_predictors, main = "Plot of Stock Data in Principal Components Space")
plot(pc_plot1)

#K-Means Clustering# Set a maxmum number of groups to explore
k.max <- 10

# Fit a kmeans cluster for each number of groups and calculate wthin sum of squares
wss <- sapply(1:k.max, function(k){kmeans(train_predictors, k)$tot.withinss})
wss

# Plot the results so we can find the "elbow"
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Apply silhoette method to determine clusters
fviz_nbclust(train_all, kmeans, method = "silhouette")

# Calculate the K-Means Clusters 
km.out <- kmeans(train_predictors, 2, nstart=20)
km.out

# Plot of assigned clusters
fviz_cluster(list(data = train_all, cluster = km.out$cluster))

# Comparing within sum of squares to between sum of squares
# Between Cluster Sum of Squares 
km.out$betweenss

# Within Cluster Sum of Squaeres
km.out$withinss

# The Ratio
km.out$withinss/km.out$betweenss

# Compare our results from km.out$cluster with the known values
results.compare<-data.frame(cbind(train_all$Resp.5d.2sd+1, km.out$cluster))
table(results.compare)

#Need to print accuracy
accuracy <- (241+47)/(581)
accuracy
# Get the Centerponts from K-Means Fit in 4D space
km.out$centers

# Map our centerponts in PC space
kmeans.pc_centerpoints <- data.frame(predict(pc, newdata = km.out$centers))

# Plot our centerpoints n 2D space without class labels
pc_plot3 <- ggplot(pc$x, aes(x=PC1, y=PC2)) + geom_point() + 
  ggtitle("K-Means Centerpoints in PC Space") + 
  geom_point(data =kmeans.pc_centerpoints, aes(x=PC1, y=PC2), colour = "red", shape=23, size=5, fill = "red")

plot(pc_plot3)

# Plot our centerpoints in 2D space with class labels
results_pc<- data.frame(cbind(pc$x[,1:7], train_all$Resp.5d.2sd))
colnames(results_pc)[8] <- "Resp.5d.2sd"
results_pc[,1:8] <- unfactor(results_pc[,1:8])

pc_plot4 <- ggplot(results_pc, aes(x=PC1, y=PC2, colour = Resp.5d.2sd), title = "K-Means Centerpoints in PC Space") + 
  geom_point() + ggtitle("K-Means Centerpoints in PC Space") + 
  geom_point(data =kmeans.pc_centerpoints, aes(x=PC1, y=PC2), colour = "black", shape=23, size=5, fill = "black")


plot_grid(                                    # uses cowplot library to arrange plot grid
  pc_plot3, pc_plot4, 
  nrow = 1
)



###########
####################
#############

# Dissimilarity matrix
d <- dist(train_predictors, method = "euclidean")   # can try different distance metrics

# Hierarchical clustering using Complete Linkage
# "complete", "average", "single", and "ward"
hc_agg <- agnes(d, method = "ward" )   # can try different methods here
hc_agg

# After trying all options, ward is best fit
hc_ward <- agnes(d, method = "ward" ) 
hc_ward

plot.new()
# Plot the obtained dendrogram
pltree(hc_ward, cex = 0.6, hang = -1, main="Ward Linkage")

# Cut tree into 2 groups
hc_sub_grp <- cutree(hc_ward, k = 2)  # gives us a vector of the cluster assigned for each observation

# Plot our clusters
fviz_cluster(list(data = train_all, cluster = hc_sub_grp))

# Compare our results from km.out$cluster with the known values
hcward_results.compare<-data.frame(cbind(train_all$Resp.5d.2sd, hc_sub_grp))
table(hcward_results.compare)

#Need to print accuracy
hc_ward_accuracy <- (207+62)/(581)
hc_ward_accuracy

######################################################

# Dissimilarity matrix
d <- dist(cyber.matrix, method = "euclidean")   # can try different distance metrics

# Hierarchical clustering using Complete Linkage
# "complete", "average", "single", and "ward"
hc_agg <- agnes(d, method = "ward" )   # can try different methods here
hc_agg

# After trying all options, ward is best fit
hc_ward <- agnes(d, method = "ward" ) 
hc_ward

plot.new()
# Plot the obtained dendrogram
pltree(hc_ward, cex = 0.6, hang = -1, main="Ward Linkage")

# Cut tree into 3 groups
hc_sub_grp <- cutree(hc_ward, k = 5)  # gives us a vector of the cluster assigned for each observation

# Plot our clusters
fviz_cluster(list(data = cyber.matrix, cluster = hc_sub_grp), main = "Hierarchical Cluster Plot")

#Diana
# Conduct divisive (top down) clustering
hc_divisive <- diana(cyber.matrix)
hc_divisive$dc                     # get the divisive coefficient - equivalent to ac



################################################
# DBSCAN #######################################
################################################

##### Calculate the epsilon neighborhood - i.e. find optimal eps
kNNdistplot(cyber.matrix, k=5)  

# Implement DBSCAN
db = dbscan(cyber.matrix, eps = 2, minPts = 5, borderPoints = FALSE)

# Visualize the results
fviz_cluster(list(data = cyber.matrix, cluster = db$cluster))

plot.new()
# Hullplot from dbscan package isn't confused by unassigned cases
hullplot(cyber.matrix, db$cluster)


###########################################################################################
##### Example code to use for future prediction ###########################################
# library("flexclust")
# data("Nclus")
# 
# set.seed(1)
# dat <- as.data.frame(Nclus)
# ind <- sample(nrow(dat), 50)
# 
# dat[["train"]] <- TRUE
# dat[["train"]][ind] <- FALSE
# 
# cl1 = kcca(dat[dat[["train"]]==TRUE, 1:2], k=4, kccaFamily("kmeans"))
# cl1    
# #
# # call:
# # kcca(x = dat[dat[["train"]] == TRUE, 1:2], k = 4)
# #
# # cluster sizes:
# #
# #  1   2   3   4 
# #130 181  98  91 
# 
# pred_train <- predict(cl1)
# pred_test <- predict(cl1, newdata=dat[dat[["train"]]==FALSE, 1:2])
# 
# image(cl1)
# points(dat[dat[["train"]]==TRUE, 1:2], col=pred_train, pch=19, cex=0.3)
# points(dat[dat[["train"]]==FALSE, 1:2], col=pred_test, pch=22, bg="orange")
