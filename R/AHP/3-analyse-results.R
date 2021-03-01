## https://rpubs.com/gluc/ahp

#install.packages("R6")
library(R6)
#vignette('Introduction', package = 'R6')

#install.packages("data.tree")
library(data.tree)
#vignette(package = 'data.tree')

#install.packages("ahp")
library(ahp)

#ahpFile <- system.file("extdata", "car.ahp", package="ahp")
#ahpFile <- system.file("extdata", "tom_dick_harry.ahp", package="ahp")
#ahpFile <- system.file("extdata", "vacation.ahp", package="ahp")

## Load all required packages
mainDir <- getwd()
ahpFile <- paste0(mainDir,"/data/car.ahp")
ahpFile <- paste0(mainDir,"/data/data.ahp")
processedAHP <- Load(ahpFile)


print(processedAHP, filterFun = isNotLeaf)

Calculate(processedAHP)
## Defaults option
Calculate(processedAHP,
          pairwiseFun = PrioritiesFromPairwiseMatrixEigenvalues,
          scoresFun = PrioritiesFromScoresDefault)

## Option 2
Calculate(processedAHP,
          pairwiseFun = PrioritiesFromPairwiseMatrixGeometricMean,
          scoresFun = PrioritiesFromScoresDefault)


## Option 3
Calculate(processedAHP,
          pairwiseFun = PrioritiesFromPairwiseMatrixMeanNormalization,
          scoresFun = PrioritiesFromScoresDefault)

print(processedAHP, priority = function(x) x$parent$priority["Total", x$name])

Visualize(processedAHP)

Analyze(processedAHP)
AnalyzeTable(processedAHP)

## install.packages("shinythemes")
RunGUI(port = getOption("shiny.port"))
