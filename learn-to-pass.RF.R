library(randomForest)
library(FactoMineR)
library(kernlab)
library(ROCR)



set.seed(42)

#Read om games, combine, and add game feature
game1 <- read.csv('Mtl-Ott-game1.csv')
game1$game <- 1
game2 <- read.csv('Mtl-Ott-game2.csv')
game2$game <- 2
game3 <- read.csv('Mtl-Ott-game3.csv')
game3$game <- 3
game4 <- read.csv('Mtl-Ott-game4.csv')
game4$game <- 4
game5 <- read.csv('Mtl-Ott-game5.csv')
game5$game <- 5
game6 <- read.csv('Mtl-Ott-game6.csv')
game6$game <- 6
events.all <- rbind(game1, game2, game3, game4, game5, game6)

#The position of the net
netpos.X <- 90.0
netpos.Y <- 0.0


#
#Feature Engineering
#


d2net <- function(pos.X, pos.Y)
#Distance to net (90,0) from coordinates
{
    return( sqrt((abs(pos.X) - netpos.X)^2 + (abs(pos.Y) - netpos.Y)^2) )
}


d2event <- function(event1, event2)
#Physical distance between two events
{
    #Check that events are in the same game and period
    if(event1$game != event2$game & event1$period != event2$period)
        {
            print("warning: computing distance incorrectly")
            print(event1)
            print(event2)
        }
    return( sqrt((event1$xCoord - event2$xCoord)^2 + (event1$yCoord - event2$yCoord)^2) )
}

t2event <- function(event1, event2)
#Time separating two events, assumed to be in the same period
{
    
    fps <- 30.0 #approximate frames per second
    #Test that events are in the same game and period
    if(event1$game != event2$game & event1$period != event2$period
       & !(event2$id==1 | event2$type != "faceoff"))
        {
            print("warning: Computing time incorrectly")
            print(event1)
            print(event2)
        }
    #If the second event is a faceoff or has id=1
    if( event2$type == "faceoff" | event2$id == 1)
        {
            return( event2$frame/fps )
        }
    return( max(event2$frame - event1$frame,1)/fps )
}


#Prepare columns for 
events.all$passDist <- 0 #Physical distance of pass
events.all$passSpeed <- 0 #Speed of the pass
events.all$d2netNext <- 0 #Distance to the net of the next play
events.all$playDuration <- 0 #How long has the team controlled the puck
for (i in 1:nrow(events.all))
{
   if (events.all[i,"name"] == "pass")
       {
           ## Distance of pass using next play
           events.all[i,]$passDist <- d2event(events.all[i,],events.all[i+1,])
           ## Speed of pass using next play
           events.all[i,]$passSpeed <- events.all[i,]$passDist /
               t2event(events.all[i,], events.all[i+1,])
           #Check for negative speeds
           if (events.all[i,]$passSpeed < 0)
               {
                   print("warning: negative pass speed")
                   print(events.all[i,])
                   print(events.all[i+1,])
                   events.all[i,]$passSpeed <- -events.all[i,]$passSpeed
               }
           #Print debugging info for very large speeds
           else if (events.all[i,]$passSpeed > 10000)
               {
                   print("Warning: Unusual speed")
                   print(events.all[i,])
                   print(events.all[i+1,])
               }
           ## Position of next play, distance from the net
           events.all[i,]$d2netNext <- d2net(events.all[i+1,]$xAdjCoord, events.all[i+1,]$yAdjCoord)
           ## Time since previous play (how long player had the puck)
           events.all[i,]$playDuration <- t2event(events.all[i-1,], events.all[i,])
           #Assume that very long plays (longer than 1 minute) do not occur
           if (events.all[i,]$playDuration > 60)
               {
                   events.all[i,]$playDuration <- 0
               }
       }
}

#Extract out the passing features
events.passes <- subset(events.all, name=="pass")
events.passes$id <- as.factor(events.passes$id)
events.passes$period <- as.factor(events.passes$period)
events.passes$xPos <- abs(events.passes$xCoord)
events.passes$yPos <- abs(events.passes$yCoord)

#Simplify the shorthand column
events.passes$shorthand <- gsub("\\+", "", events.passes$shorthand)
events.passes$shorthand <- gsub("-", "", events.passes$shorthand)
events.passes$type <- factor(events.passes$type)

#Compute distances to the net
events.passes$netdist <- 0
events.passes$netdist <- d2net(events.passes$xPos, events.passes$yPos)


#Helper function to extract features for Random Forest   
extractFeatures <- function(data) {
      features <- c(#"period",
#                    "team",
                    "zone",
                    "type",
                    "xPos",
                    "yPos",
                    #"game",
                    "netdist",
                    #"currentPlayInPossession",
                    "passDist",
                    "passSpeed",
                    "d2netNext",
                    "playDuration")
#                    "playerPosition")
        return(data[,features])
}


#Divide data set into training and validation sets
trainEvents <- sample(1:dim(events.passes)[1], 2500)
valEvents <- setdiff(1:dim(events.passes)[1], trainEvents)

#Compute a randomForest model for the training set
rf <- randomForest(extractFeatures(events.passes[trainEvents,]),
                   events.passes[trainEvents,]$outcome, ntree=1000,
                   mtry=4, sampsize=1000, importance=TRUE)
#rf <- tuneRF(extractFeatures(events.passes[trainEvents,]),
#             events.passes[trainEvents,]$outcome, ntreeTry=1000, mtryStart=4,
#             stepFactor=1, doBest=TRUE)

#Feature importances
imp <- importance(rf, type=2)
print(imp)

#Plot of feature importances
png('varImp.png', width=1000)
varImpPlot(rf, main="")
dev.off()


#Predictions for validation and training sets
prediction.val <- predict(rf, events.passes[valEvents,-13], type="prob")
prediction.train <- predict(rf, type="prob")

#A classification error measure
dist <- function(prob, truth)
{
    if (truth == "successful") t <- 1
    else t <- 0
    return(abs(t - prob))
}


#Compute classification errors
distances.val <- mapply(dist, prediction.val[,2], events.passes[valEvents,]$outcome)
distances.train <- mapply(dist, prediction.train[,2], events.passes[trainEvents,]$outcome)

#Plot of classification error distributions
png('testValErrors.png')
hist(distances.train, col=rgb(1,0,0,0.5), probability=T, breaks=50,
     main="", xlab="Classification error")
hist(distances.val, col=rgb(0,0,1,0.5), probability=T,
     add=T, breaks=50, xlab="", main="")
legend("topright", c("Training", "Validation"),
              col=c("red", "blue"), lwd=10)
dev.off()

#Successful and failed subsets
pass.success <- subset(events.passes, outcome == "successful")
pass.fail <- subset(events.passes, outcome == "failed")

#Histograms of distance to net of next event for successful and failed passes
png('d2netNext.png')
hist(pass.success$d2netNext, col=rgb(0,1,0,0.5), probability=T, breaks=50,
     main="", xlab="Distance to net of pass destination (feet)")
hist(pass.fail$d2netNext, col=rgb(1,0,0,0.5), probability=T, breaks=50,
     main="", add=T, xlab="")
dev.off()

#Histograms of pass distance for successful and failed events
png('passDist.png')
hist(pass.success$passDist, col=rgb(0,1,0,0.5), probability=T, breaks=50,
     main="", xlab="Pass Distance (feet)")
hist(pass.fail$passDist, col=rgb(1,0,0,0.5), probability=T, breaks=50,
     main="", add=T, xlab="")
dev.off()

#Histograms of pass speed for successful and failed events
png('passSpeed.png')
hist(pass.success[pass.success$passSpeed <200,]$passSpeed, col=rgb(0,1,0,0.5), probability=T, breaks=50,
     main="", xlab="Pass Speed (feet / s)")
hist(pass.fail[pass.fail$passSpeed < 200,]$passSpeed, col=rgb(1,0,0,0.5), probability=T, breaks=50,
     main="", add=T, xlab="")
dev.off()

#Precision and Recall
pred.obj <- prediction(prediction.val[,2], events.passes[valEvents,]$outcome)

#Plot of false positive rate vs. true positive rate
png("ROC.png")
ROC.perf <- performance(pred.obj, "tpr","fpr")
plot(ROC.perf, xlab="False success prediction rate", ylab="True success prediction rate")
dev.off()

#Report Classification accuracy
print(sum(distances.val <0.5)/sum(distances.val >= 0.0))

#Player by player analysis
events.passes$fullName <- paste(events.passes$playerFirstName, events.passes$playerLastName)
successRate <- function(player)
{
    player.successes <- subset(events.passes, fullName == player & outcome == "successful")
    player.fails <- subset(events.passes, fullName == player & outcome == "failed")
    return(nrow(player.successes)/(nrow(player.successes) + nrow(player.fails)))
    
}

successRateErr <- function(player)
{
    player.successes <- subset(events.passes, fullName == player & outcome == "successful")
    nSucc <- nrow(player.successes)
    nTot <- sum(events.passes$fullName == player)
    return(sqrt(nSucc/nSucc^2 + nTot/nTot^2)*nSucc/nTot)
    
}

players <- data.frame()
for (player in unique(events.passes$fullName))
{
    players <- rbind(players, data.frame(name = player,
              position = events.passes[events.passes$fullName == player, "playerPosition"][1],
              passRate = successRate(player),
              passRateErr = successRateErr(player),
              attempts = nrow(subset(events.passes, fullName == player)))
                                         )
}

png('players.png', height=6, width=8.5, units='in', res=72)
par(mar=c(10,4,2,0))
bp <- barplot(players[order(players$passRate), ]$passRate,
#     names.arg=players[order(players$passRate),]$playerPosition,
     names.arg=paste(players[order(players$passRate),]$position, "-",
         players[order(players$passRate),]$name),
     cex.names=0.9,
     cex.axis=1.5, las=2)
arrows(bp, players[order(players$passRate), ]$passRate
       + players[order(players$passRate), ]$passRateErr,
       bp, players[order(players$passRate), ]$passRate
       -players[order(players$passRate), ]$passRateErr,
       length=0.02, angle=90, code=3)
polygon(c(0, 0, 53, 53), c(0.69,0.73, 0.73, 0.69), col='green', density=20)
dev.off()

deltas <- vector()
for (i in 1:(nrow(events.passes)-1))
    {
        deltas <- append(deltas, t2event(events.passes[i,], events.passes[i+1,]))
    }
