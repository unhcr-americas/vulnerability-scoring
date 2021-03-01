## This will create the xlsfrom based on the list of vulenrability criteria to be compared

library(readr)

## survey intro and response can be predifined
survey <- read_csv("data/survey.csv")
choices <- read_csv("data/choices.csv")

## load criteria#Note that there is a list of reserved names you cannot use as Node fields (aka indicators)
# NODE_RESERVED_NAMES_CONST
## do not add more than 7 criteria to cope with natural cognitive limitations
criteria <- read_csv("data/criteria.csv")

if (nrow(criteria) > 7) {
 cat("Do not add more than 7 criteria to cope with natural cognitive limitations \n")
 cat("This will also bring too many pairwise comparison to complete \n")
}

## Creating setting for the xlsform
settings <- data.frame(c("Weight Vulnerability Criteria"))
names(settings)[1] <- "form_title"
settings$id_string <- "Pairwise comparison of criteria"
settings$style <- "theme-grid"


### Now building questions for pairwise comparison based on the criteria
for (i in 1:nrow(criteria)) {
  critname1 <- as.character(criteria[ i,1])
  critlabel1 <- as.character(criteria[ i,2])

  # uncomment for debugging
  # cat(paste("i =", i,"\n"))
  for (j in (i + 1):nrow(criteria)) {
    # uncomment for debugging
    # cat(paste("j =",j,"\n"))
    if ( j < nrow(criteria) + 1 & j != i) {
      critname2 <-  as.character(criteria[ j, 1])
      critlabel2 <-  as.character(criteria[ j, 2])
      compname <- paste(critname1, "-to-", critname2, sep = "" )
      compname2 <- paste("Comp_",compname, sep = "" )
      complabel <- paste("__Compare__ ",critlabel1, " __with__ ", critlabel2, sep = "")

      surveytemp1 <- data.frame(c("select_one order"))
      names(surveytemp1)[1] <- "type"
      surveytemp1$type <- as.character(surveytemp1$type)
      surveytemp1$name <- compname2
      surveytemp1$label <- complabel
      surveytemp1$hint <- "Pairwise comparison of criteria"
      surveytemp1$required <- "true"
      surveytemp1$relevant <- ""

      surveytemp1[2 ,1 ] <- "select_one importance"
      surveytemp1[2 ,2 ] <- paste0("imp_",compname)
      surveytemp1[2 ,3 ] <- "Scale of __relative__ importance"
      surveytemp1[2 ,4 ] <- "Scale of __relative__ importance"
      surveytemp1[2 ,5 ] <- "true"
      surveytemp1[2 ,6 ] <- paste0("selected(${",compname2,
                                   "},'firscriteria') or selected(${",
                                   compname2,"},'secondcriteria')")

      survey <- rbind(survey, surveytemp1)
      rm(surveytemp1)

      # uncomment for debugging
      #cat(paste(compname,"\n"))
      #cat(paste(complabel,"\n"))

    } else {
      # uncomment for debugging
      # cat("next\n")
      }
  }
}
rm(compname, complabel, critlabel1, critlabel2, critname1, critname2, i, j)
## Add a line to close the group
survey[ nrow(survey) + 1 , c("type")] <- "end_group"

#surveytemp <- surveytemp[ 2:nrow(surveytemp), ]
cat("Writing the final xlsform")

## write a list of frame
l <- list(   "survey"  = survey,
   "choices" = choices,
   "settings" = settings,
   "criteria" = criteria)

## define a style for column headers
hs <- openxlsx::createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                  halign = "center", valign = "center", textDecoration = "Bold",
                  border = "TopBottomLeftRight")

openxlsx::write.xlsx(l, file = "data/CriteriaComparison.xlsx", borders = "rows", headerStyle = hs)
rm(hs,l)

