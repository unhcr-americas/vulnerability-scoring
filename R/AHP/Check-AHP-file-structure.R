### Just a few script to explore the structure of an AHP file

### Load libraries ####
library(yaml)
library(R6)
library(data.tree)
library(ahp)

### Get examples - copies were paste in the project repo - data folder -...###
mainDir <- getwd()
#ahpFile <- paste0(mainDir,"/data/car.ahp")
#ahpFile <- paste0(mainDir,"/data/tom_dick_harry.ahp")
ahpFile <- paste0(mainDir,"/data/vacation.ahp")
#processedAHP <- Load(ahpFile)

## Load YAMl files in list object
ahptree <- yaml.load_file(ahpFile)

### We have now a list object
str(ahptree)
class(ahptree)
summary(ahptree)

#############################
## Explore the list object
##### Check the version
summary(ahptree$Version)
str(ahptree[["Version"]])

#ahptree$Alternatives
#ahptree$Goal

ahptree.Goal <- ahptree$Goal
summary(ahptree.Goal)
ahptree.Goal.preferences <- ahptree.Goal$preferences
summary(ahptree.Goal.preferences)
str(ahptree.Goal.preferences)

## check how we can convert this back to YAML file..
as.yaml(ahptree.Goal.preferences)
write(as.yaml(ahptree.Goal.preferences), "data/datatest2.ahp")

#### This does not save a file with the right structure...

#ahptree.Goal.preferences.pairwise <- ahptree.Goal.preferences$pairwise
#ahptree.Goal.preferences.pairwise <- ahptree.Goal.preferences$Dad
#summary(ahptree.Goal.preferences.pairwise)
#str(ahptree.Goal.preferences.pairwise)
#ahptree.Goal.preferences.pairwise.df <- as.data.frame(ahptree.Goal.preferences.pairwise)
#ahptree.Goal.children <- ahptree.Goal$children
#summary(ahptree.Goal.children)


############################################################
## Now converting the list to a data tree ##################
ahpnode <- as.Node(ahptree)
## Name 'children' is a reserved word as defined in NODE_RESERVED_NAMES_CONST
## The conversion does not work properly..
print(ahpnode)
class(ahpnode)
str(ahpnode)
## "Node" "R6"
ahpnode$fieldsAll

##############################################################
## Use a function to get directly from YAML to data tree #####
ahpnode2 <- Load(ahpFile)

# look at the structure

print(ahpnode2)
class(ahpnode2)
str(ahpnode2)
## "Node" "R6"
ahpnode2$fieldsAll

### changing the structure
# look at some preference dfs
ahpnode2$preferences$Dad$pairwise$preferences

# manipulate preferences by code
newprefs <- structure(list(c1 = list("Costs", "Costs", "Fun"),
                           c2 = list("Fun", "Spa", "Spa"),
                           preference = c(7, 2, 3)),
                      .Names = c("c1","c2", "preference"),
                      row.names = c(NA, -3L), class = "data.frame")

ahpnode2$preferences$Dad$pairwise$preferences <- newprefs

# reset voting powers by code (power to the children!):
ahpnode2$`decision-makers` <- c(Dad = 0.1, Mom = 0.2, Kid = 0.7)

# ...except maybe for spa, that's still Mom's territory
ahpnode2$Spa$`decision-makers` <- c(Dad = 0.2, Mom = 0.7, Kid = 0.1)


## calculate AHP
Calculate(ahpnode2)

## Get the analysis...
ahp::Analyze(ahpnode2)

################################################################
## check how we can convert this back to YAML file.. ###########
as.yaml(ahpnode2)
write(as.yaml(ahpnode2), "data/0-test-node-to-yaml.ahp")
### don't know how to emit object of type: 'environment', class: Node R6

## Convert to list before saving as YAML
write(as.yaml(ToListExplicit(ahpnode2, unname = TRUE)), "data/0-test-node-to-yaml.ahp")



