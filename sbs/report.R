testSuite <- c(mean,min,max)
names(testSuite) <- c('mean','min','max')

report <- function(FUNS,data){
	if(!all(sapply(FUNS,is.function)))
		stop()
	sapply(FUNS,function(FUN,data)FUN(data),data)
}

report(testSuite,seq(10)) #labels transferred from tests to results
