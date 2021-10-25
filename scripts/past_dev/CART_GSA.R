## CART analysis 19_1_12 
library(readr)
library(raster)
library(microbenchmark)
library(data.table)
library(purrr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(tidyr)
install.packages("randomForest")
install.packages("rpart.plot")
library(randomForest)
###########read in data 
dataset <- fread("processed_data/191014_PRW_MASTER_LIST", header=T)
# dataset <- as.data.table(dataset)

# sensitivity analysis ---------------------------------------------------

# three OFE slopes 
dataset_three <- dataset[grepl("t3|f3|h3|g3|s3", name)]

dataset_three[, g := .GRP, by=.(name)]

dataset_three[, c("climate", "soil","length","S1","S2","S3","rotation","tillage"):= tstrsplit(name, "_", fixed=TRUE)][
  ,managment := paste(rotation,"_",tillage)][
    , soil_n := .GRP, by=.(soil)][
      , slope_n := .GRP, by=.(S2)][
        , man_n := .GRP, by=.(managment)][
          , rot_n := .GRP, by=.(rotation)][
            , til_n := .GRP, by=.(tillage)][
              , climate:= as.numeric(climate)]

dataset_three <- dataset_three[til_n==4, til_n:=5][til_n==2, til_n:=4][til_n==5, til_n:=2]

#list by intensity 
#till 1 = na, 2 = nt, 3= mt, 4= ct 

tokeep <- which(sapply(dataset_three,is.numeric)) #grab the numeric columns and build a new data frame
test<-dataset_three[ , tokeep, with=FALSE][,c("OFE","erosion_kgm2_mean","erosion_kgm2_max","Q","UpStrmQ", "latqcc","climate","soil_n","slope_n","man_n","rot_n","til_n")] 

#Grab the 2nd OFE
dataset_ofe2<-dataset_three[, OFE %in% 2]
test_ofe2<-test[OFE %in% 2,]
#plot(test_ofe2)
unique(test_ofe2$til_n)
# 
# figs <- map(.x = c(unique(whitman_interest$iconic_taxon_name)),             .f = ~ whitman_interest %>%
#               filter(iconic_taxon_name == .x) %>%
#               ggplot(aes(x = month_obs, y = n)) +
#               geom_boxplot(outlier.shape = NA) +
#               geom_jitter(aes(color = factor(year_obs))) +
#               scale_color_viridis_d(name = "Year", drop = FALSE) +
#               scale_x_discrete(drop = FALSE) +
#               theme_bw() +
#               theme(axis.text.x = element_text(angle = 45)) +
#               ggtitle(label = .x) +
#               ylab(label = "Count") +
#               xlab(label = "Month observed"))
ggplot(test_ofe2, aes(x=rot_n, y=erosion_kgm2_mean))+
  geom_point()

#strsplit(as.character(dataset_three$name), "_")[[1]][1]
# hist(log10(dataset$erosion_kgm2_mean[dataset$OFE==2]))
# NROW(dataset)
test <- na.omit(test_ofe2)
datacor<- cor(test)
cor_str <- plot(test)

# fit <- rpart(erosion_kgm2_mean ~ climate + soil_n + slope_n + man_n, data = test_ofe2)
fit_1 <- rpart(erosion_kgm2_mean ~ climate + soil_n + slope_n + rot_n+til_n, data = test_ofe2)
# fit_2 <- rpart(erosion_kgm2_mean ~ soil_n + slope_n + man_n, data = test_ofe2)
# fit_3 <- rpart(erosion_kgm2_mean ~ climate + soil_n + slope_n , data = test_ofe2)
# fit_4 <- rpart(erosion_kgm2_mean ~ climate + slope_n + man_n, data = test_ofe2)
# 

  # 
# plot(fit)
# text(fit, use.n = TRUE)
par(mar = c(5, 2, 5, 2)) 
plot(fit_1)
text(fit_1,pretty=T, use.n = T, all=F)
rpart.plot(fit_1)
# plot(fit_2)
# text(fit_2, use.n = TRUE)
# 
# plot(fit_3)
# text(fit_3, use.n = TRUE)
# 
# plot(fit_4)
# text(fit_4, use.n = TRUE)

set.seed(71)
erosion__rf <- randomForest(erosion_kgm2_mean ~ climate + soil_n + slope_n + rot_n+til_n, data = test_ofe2, ntree= 2000,importance=TRUE,
                            proximity=TRUE)
getTree(iris.rf, k=2, labelVar=T)
print(erosion__rf)
plot(iris.rf)
?randomForest

varImpPlot(erosion__rf)
?varImpPlot


# We applied the GSA approach designed by Harper et al.
# (2011). This GSA approach uses Random Forest (RF) to
# rank factor and parameter importance and classification and regression tree (CART) to analyze and visualize the complex relationships among model factors. RF
# is an improved version of CART, since it is a forest (a
#                                                       collection of trees) where each tree is created by bootstrap sampling and where the factor and parameter at
# each node of the tree is randomly selected (Cutler et al.
#                                             2007). For every tree, 30% of the data (called the out-ofbag – OOB data) are randomly sampled and used to
# estimate model efficiency by cross validating results
# with the other 70% of the data (Cutler et al. 2007).
# Model efficiency is estimated as one minus the ratio
# between the mean squared error (MSE) and response
# variable variance (Pang et al. 2006). We used the R
# package randomForest 4.6–2 to estimate model efficiency (Breiman & Cutler 2011).
# The contribution of each factor to model predictions (or importance) was assessed by the node
# impurity metric, which measures changes in the residual sum of squared errors by splitting the factor at
# each node of the tree (Breiman & Cutler 2011). Node
# impurity values for each factor were normalized by
# the sum of the total node impurity and reflect the
# relative importance of each factor estimate using
# randomForest 4.6–2 R package (Breiman & Cutler
#                               2011). To visualize the higher order interactions
# between factors, we applied a CART analysis to
# each dataset. With CART, we were able to identify
# the specific factor combinations that generated lower
# and greater estimates of soil loss (R package rpart
#                                     4.1–9; Therneau & Atkinson 2011), indicating which
# factors create the most uncertainty in model
# predictions.



# NOT RUN {
## Classification:
##
data(iris)
set.seed(71)
iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)
## Look at variable importance:
round(importance(iris.rf), 2)
## Do MDS on 1 - proximity:
iris.mds <- cmdscale(1 - iris.rf$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(iris[,1:4], iris.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(iris$Species)],
      main="Iris Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(iris.mds$GOF)

## The `unsupervised' case:
set.seed(17)
iris.urf <- randomForest(iris[, -5])
MDSplot(iris.urf, iris$Species)

## stratified sampling: draw 20, 30, and 20 of the species to grow each tree.
(iris.rf2 <- randomForest(iris[1:4], iris$Species, 
                          sampsize=c(20, 30, 20)))

## Regression:
## data(airquality)
set.seed(131)
ozone.rf <- randomForest(Ozone ~ ., data=airquality, mtry=3,
                         importance=TRUE, na.action=na.omit)
print(ozone.rf)
## Show "importance" of variables: higher value mean more important:
round(importance(ozone.rf), 2)

## "x" can be a matrix instead of a data frame:
set.seed(17)
x <- matrix(runif(5e2), 100)
y <- gl(2, 50)
(myrf <- randomForest(x, y))
(predict(myrf, x))

## "complicated" formula:
(swiss.rf <- randomForest(sqrt(Fertility) ~ . - Catholic + I(Catholic < 50),
                          data=swiss))
(predict(swiss.rf, swiss))
## Test use of 32-level factor as a predictor:
set.seed(1)
x <- data.frame(x1=gl(53, 10), x2=runif(530), y=rnorm(530))
(rf1 <- randomForest(x[-3], x[[3]], ntree=10))

## Grow no more than 4 nodes per tree:
(treesize(randomForest(Species ~ ., data=iris, maxnodes=4, ntree=30)))

## test proximity in regression
iris.rrf <- randomForest(iris[-1], iris[[1]], ntree=101, proximity=TRUE, oob.prox=FALSE)
str(iris.rrf$proximity)
# }
