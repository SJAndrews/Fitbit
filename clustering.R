library(plyr)
library(missForest)
library(cluster)

bpm.ls2 <- split(bpm, as.Date(bpm$date, tz = "Australia/Sydney"))
out <- lapply(bpm.ls2, function(x){
  hr <- x$hr
  time <- strftime(x$time, format="%H:%M:%S")
  names(hr) <- time  
  as.data.frame(t(as.data.frame(hr)))
})

dat <- as.data.frame(do.call(rbind.fill, out))
dat$date <- dates
dat$month <- month(dat$date, label=TRUE) 
dat$day <- wday(dat$date, label=TRUE) 
dat <- merge(dat, weather[, c("date", "hightemp", 'lowtemp', "rain")], by = 'date')
missing <- apply(dat, 1, function(x){
  sum(is.na(x)) > 58
})
impute <- dat[missing == FALSE,]
demp.imp <- missForest(impute[,-1], verbose = TRUE)
dat <- demp.imp$ximp

fit <- agnes(dat[1:288], metric = 'euclidean', method = 'ward', stand = TRUE)
fit <- agnes(dat, metric = 'euclidean', method = 'ward', stand = TRUE)
plot(fit)
plot(fit, which.plot = 2)
rect.hclust(fit, k=2, border="green")
rect.hclust(fit, k=3, border="green")
rect.hclust(fit, k=4, border="red")
rect.hclust(fit, k=5, border="blue")
plot(fit, which.plot = 2); rect.hclust(fit, k=6, border="blue")

ddata <- dendro_data(dhc, type = 'rectangle')
ggplot(segment(ddata)) +  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + theme_bw() + ylab("height") + xlab("") + theme(axis.title.y=element_text(size=20), axis.text=element_text(size=16), axis.text.x = element_blank(), legend.position="none", axis.ticks = element_blank()) + theme_dendro()

p
groups <- cutree(fit, k = 6)
table(groups)
dat$groups <- groups
dat$date <- dates[missing == FALSE]
(prop.days <- as.data.frame(prop.table(table(dat$groups, dat$day), 1)*100))
(prop.months <- as.data.frame(prop.table(table(dat$groups, dat$month), 1)*100))
aggregate(dat[, "hightemp"], list(dat$groups), mean, na.rm = TRUE)
aggregate(dat[, "lowtemp"], list(dat$groups), mean, na.rm = TRUE)
aggregate(dat[, "rain"], list(dat$groups), mean, na.rm = TRUE)

dat.long <- reshape(dat, idvar = "date", varying = list(1:288), v.names = "hr", direction = "long")
dat.long <- dat.long[order(dat.long$date, dat.long$time),]
#time <- bpm.ls2[[1]]$time
dat.long$time <- time
dat.long$time <- as.POSIXct(rep(time, 178), format = "%H:%M:%S")

ggplot(dat.long, aes(x = time, y = hr, group = 1, colour = hr)) + geom_point() + geom_smooth(colour = 'black', size = 2) + theme_bw() + scale_colour_gradient(high = "red", low = "blue") + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour") + facet_grid(groups ~ .) + ylab("Beats per Minutes") + theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text=element_text(size=16), axis.text.x = element_text(angle = -45, hjust = -.05))

# pie charts for months
ggplot(prop.months, aes(x = factor(1), y = Freq, fill = as.factor(Var2))) + geom_bar(stat="identity") + facet_grid(Var1 ~ .) + coord_polar(theta = "y") +  scale_fill_manual(values=c("#CB181D", "#A50F15", "#9E9AC8", "#807DBA", "#6A51A3", "#4292C6", "#2171B5", "#08519C", "#41AB5D", "#238B45", "#006D2C", "#EF3B2C")) + ylab("Months (%)") + xlab("") + theme(legend.title=element_blank())


p <- lapply(1:6, function(no.){
  ggplot(prop.months[prop.months$Var1 == no.,], aes(x = factor(1), y = Freq, fill = as.factor(Var2))) + geom_bar(stat="identity") + coord_polar(theta = "y") +  scale_fill_manual(values=c("#CB181D", "#A50F15", "#9E9AC8", "#807DBA", "#6A51A3", "#4292C6", "#2171B5", "#08519C", "#41AB5D", "#238B45", "#006D2C", "#EF3B2C")) + ylab("") + xlab("") + theme(legend.position="none") + ggtitle(paste("Group", no.))
})

grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], nrow = 2, ncol = 3)

#pie charts for days
p <- lapply(1:6, function(no.){
  ggplot(prop.days[prop.days$Var1 == no.,], aes(x = factor(1), y = Freq, fill = as.factor(Var2))) + geom_bar(stat="identity") + coord_polar(theta = "y") +  scale_fill_manual(values=c("#006D2C", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#41AB5D", "#238B45")) + ylab("") + xlab("") + theme(legend.title=element_blank()) + theme(legend.position="none") + ggtitle(paste("Group", no.))
})
grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], nrow = 2, ncol = 3)


ggplot(prop.days, aes(x = factor(1), y = Freq, fill = as.factor(Var2))) + geom_bar(stat="identity") + facet_grid(Var1 ~ .) + coord_polar(theta = "y") +  scale_fill_manual(values=c("#006D2C", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#41AB5D", "#238B45")) + ylab("Days (%)") + xlab("") + theme(legend.title=element_blank())

ggplot(dat.long, aes(x = time, y = hr, group = 1, colour = hr)) + geom_point() + geom_smooth(colour = 'black', size = 2) + theme_bw() + scale_colour_gradient(high = "red", low = "blue") + scale_x_datetime(labels = date_format("%H:%M"), breaks = "1 hour") + facet_grid(groups ~ .)

test <- bpm.ls[[1]]
time <- test$time
test$time <- 1:288

Extract.data.frame()


princ <- prcomp(dat[1:288])
nComp <- 2
project <- predict(princ, newdata = dat[1:288])[,1:nComp]
project.plus <- cbind(as.data.frame(project), cluster = as.factor(groups))
ggplot(project.plus, aes(x = PC1, y = PC2, fill = groups)) + geom_point()


##-----------------------------------------##
#   Stochastic Gradient Boosting
##-----------------------------------------##

dat.wide <- dat
dat.sleep <- dat.long[,c("date", "time", "hr")] 

dat.sleep$date <- as.POSIXct(with(dat.sleep, paste(as.character(date), strftime(time, format="%H:%M:%S"))), format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Sydney")

dat.sleep$hr_minus5  <- c(NA, dat.sleep[1:nrow(dat.sleep)-1,]$hr)
dat.sleep$hr_minus10 <- c(rep(NA, 2), dat.sleep[1:(nrow(dat.sleep)-2),]$hr)
dat.sleep$hr_minus15 <- c(rep(NA, 3), dat.sleep[1:(nrow(dat.sleep)-3),]$hr)
dat.sleep$hr_minus20 <- c(rep(NA, 4), dat.sleep[1:(nrow(dat.sleep)-4),]$hr)
dat.sleep$hr_minus25 <- c(rep(NA, 5), dat.sleep[1:(nrow(dat.sleep)-5),]$hr)
dat.sleep$hr_minus30 <- c(rep(NA, 6), dat.sleep[1:(nrow(dat.sleep)-6),]$hr)
dat.sleep$hr_minus35 <- c(rep(NA, 7), dat.sleep[1:(nrow(dat.sleep)-7),]$hr)
dat.sleep$hr_minus40 <- c(rep(NA, 8), dat.sleep[1:(nrow(dat.sleep)-8),]$hr)
dat.sleep$hr_minus45 <- c(rep(NA, 9), dat.sleep[1:(nrow(dat.sleep)-9),]$hr)
dat.sleep$hr_minus50 <- c(rep(NA, 10), dat.sleep[1:(nrow(dat.sleep)-10),]$hr)
dat.sleep$hr_minus55 <- c(rep(NA, 11), dat.sleep[1:(nrow(dat.sleep)-11),]$hr)
dat.sleep$hr_minus60 <- c(rep(NA, 12), dat.sleep[1:(nrow(dat.sleep)-12),]$hr)

dat.sleep <- merge(dat.sleep, bpm.sleep[,c("date", 'asleep')])
dat.sleep <- dat.sleep[complete.cases(dat.sleep),]

dat.sleep$asleep <- factor(dat.sleep$asleep, labels = c("no", "yes"))
dat.sleep$asleep <- relevel(dat.sleep$asleep, ref = 'yes')

#grid of tuning parameters for tuning stage
gbm.grid <- expand.grid(interaction.depth = seq(1, 10, by = 1),
                        n.trees = seq(100, 2000, by = 100),
                        shrinkage = c(0.01),
                        n.minobsinnode = 10)

gbm.grid <- expand.grid(interaction.depth = seq(1, 10, by = 1),
                        n.trees = seq(100, 10000, by = 500),
                        shrinkage = c(0.01),
                        n.minobsinnode = 10)



set.seed(333)
tune.sleep <- train(asleep ~ ., data = dat.sleep[3:16],
                    method = 'gbm',
                    metric = "ROC",
                    trControl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary),
                    tuneGrid = gbm.grid,
                    verbose = FALSE)

# tune parameters
# rush = interaction.depth = 2, n.trees = 1100, shrinkage = 0.01, n.minobsinnode = 1

set.seed(333)
gbm.rushapoe <- train(asleep ~ ., data = dat.sleep[3:16],
                      method = 'gbm',
                      trControl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary, savePredictions = TRUE),
                      metric = "ROC",
                      tuneGrid = data.frame(interaction.depth = 10,
                                            n.trees = 10000,
                                            shrinkage = 0.01,
                                            n.minobsinnode = 10),
                      verbose = FALSE)
gbm.rushapoe
confusionMatrix(gbm.rushapoe, norm = "average")

roc.plot <- function(data){
  rocCurve <- roc(data$obs, data$no, levels = c('no', 'yes')) 
  rocdf <- as.data.frame(cbind(rocCurve$thresholds, rocCurve$specificities, rocCurve$sensitivities))
  colnames(rocdf) <- c("threshold", "specificity", "sensitivity")
  j <- coords(rocCurve, "b", ret=c("threshold", "specificity", "sensitivity"), best.method="youden")
  t <- coords(rocCurve, 0.50,  ret=c("threshold", "specificity", "sensitivity"))
  jt <- as.data.frame(rbind(j, t))
  lable.j <- paste0('(',round(jt[1,1],2),': ' ,round(jt[1,2],2),', ' ,round(jt[1,3],2),')')
  lable.t <- paste0('(',round(jt[2,1],2),': ' ,round(jt[2,2],2),', ' ,round(jt[2,3],2),')')  
  ggplot(rocdf, aes(x=specificity, y = sensitivity, )) + geom_line(colour = "#0072B2", size = 1.25) + scale_x_reverse() + theme_bw() + 
    geom_abline(slope=1, intercept= 1, linetype = 2) + 
    geom_point(data = jt[1,], colour = 'red', size = 3.5) + geom_text(data = jt[1,], label=lable.j, hjust=-0.1) + 
    geom_point(data = jt[2,], colour = 'red', size = 3.5) + geom_text(data = jt[2,], label=lable.t, hjust=-0.1) +
    theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text=element_text(size=16))
}

roc.plot(gbm.rushapoe$pred)


