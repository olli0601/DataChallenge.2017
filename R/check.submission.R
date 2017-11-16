#' @export
#' @title Check if a submission to the DataChallenge has the right format.
#' @param ev data.table with specific columns, see below. and predictions in the PREDICTION rows and  
#' @return TRUE or FALSE
#' @description Your submission should have the following columns 
#' \itemize{
#'  \item{"TEAM_ID"}{a unique team ID (character string)}
#'  \item{"SUBMISSION_ID"}{a unique ID of this subission (character string).}
#'  \item{"ISO"}{country code of the country for which you are making a submission. You need to make a prediction for 66 countries.}
#'  \item{"YEAR"}{year for which you are making a submission. This should be either 2015 or 2016 (integer).}
#'  \item{"PREDICTION"}{Predicted number of epidemic outbreaks in this country and this year. This can be real value.}
#' }
#' This function will print a warning if there are any issues with the format of your submission.
dc.check.submission<- function(ev)
{
	#	check submission
	ans		<- 1
	tmp		<- setdiff(c('ISO','YEAR','TEAM_ID','SUBMISSION_ID','PREDICTION'), colnames(ev))
	if(length(tmp))
	{
		warning('Found missing column', paste(tmp, collapse=', '))
		ans	<- 0
	}		
	tmp		<- length(unique(ev$TEAM_ID))
	if(length(tmp)!=1)
	{
		warning('Found multipe TEAM_IDs')
		ans	<- 0
	}		
	tmp		<- length(unique(ev$SUBMISSION_ID))
	if(length(tmp)!=1)
	{
		warning('Found multipe SUBMISSION_IDs')
		ans	<- 0
	}		
	if(nrow(ev)!=66*2)
	{
		warning('There should be 66 predictions, one for each country and 2015 - 2016.')
		ans	<- 0
	}
	if(any(is.na(ev$PREDICTION)))
	{
		warning('There are missing values in your prediction.')
		ans	<- 0
	}
	ans
}
