# cleanup
closeAllConnections()

getToP <- function(TableOfProjectsFile = ToPs[2]) {
    if (length(TableOfProjectsFile) != 1 || typeof(TableOfProjectsFile) != "character" || 
        !file.exists(TableOfProjectsFile)) {
        stop(paste("ERROR: Project',TableOfProjectsFile,'is not good, breaking!"))  # more useful error message
    }
    readRDS(file = TableOfProjectsFile)
}


doProject <- function(ToP, ProjectName = ProjectNames[2]) {
    # make ProjectName a parameter
    if (typeof(ProjectName) != "character") {
        stop(paste("ERROR: Bad Project Name:", ProjectName))
    }
    TableOfProjects <- getToP(ToP)
    if (sum(ProjectName == rownames(TableOfProjects)) != 1) {
        # rownames are unique
        stop(paste("ERROR:',ProjectName,'is not present, breaking!"))  # not present is the only possible error
    }
    projectData <- TableOfProjects[ProjectName, ]
    # checks done, thing is doable
}

# Projects
ToPs <- c("rest_TableOfProjects.Rda", "failmefilename.Rda")
ProjectNames <- c("DevRunTest", "sdhsdsddssd", "AllCityCapture")

if(!requireNamespace('ultraCounter')){
	library(devtools)
	devtools::install_github('mm0hgw/R',subdir='ultraCounter')
}
library(ultraCounter)

projectCounter <- ultraCounter(ToPs, ProjectNames)
i <- 1
out <- list()
while( i <= length(projectCounter)){
	args <- projectCounter$index(i)
	append(out,try(do.call(doProject,args)))
	names(out)[i] <- paste(collapse='.',args)
	i<-i+1
}
