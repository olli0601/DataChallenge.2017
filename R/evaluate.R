#' @export
#' @import data.table
#' @title Evaluate the MSE of submitted predictions to the Data Challenge
#' @param save boolean if to save MSE ranking to file  
#' @return data.table with columns F, T, TEAM_ID, SUBMISSION_ID, MSE
dc.evaluate<- function(save=FALSE)
{		
	#
	#	find submissions	
	indir.submissions	<- "/Users/Oliver/git/DataChallenge.2017/submissions"
	infile.test.data	<- "/Users/Oliver/git/DataChallenge.2017/data_private/emdat_test.RData"
	infile.baseline		<- "/Users/Oliver/git/DataChallenge.2017/data_private/DataChallenge_olli0601_baseline_predictions.csv"
	
	#infile.results		<- "~/Box Sync/OR_Work/teaching/2017_DataChallenge"
	infile.results		<- list.files(indir.submissions, pattern='_predictions.csv$',full.name=TRUE)	
	infile.results		<- c(infile.baseline, infile.results)
	infile.results		<- data.table(FILE=infile.results)
	#	add time stamp
	tmp					<- infile.results[, list(TIME=gsub(' ','_',as.character(file.mtime(FILE)))), by='FILE']
	infile.results		<- merge(infile.results, tmp, by='FILE')	
	#infile.results		<- subset(infile.results, grepl('EMD',FILE))
	#	check if submissions are valid
	infile.results		<- infile.results[, {
				ev	<- as.data.table(read.csv(FILE, stringsAsFactors=FALSE))				
				list(PASSED_CHECK=dc.check.submission(ev))
			}, by=c('FILE','TIME')]	
	#	exclude those that don t pass check
	tmp				<- subset(infile.results, PASSED_CHECK!=1)
	if(nrow(tmp))
		warning('The following submissions did not pass the check and will be excluded\n',tmp[, paste(basename(FILE), collapse=', ')])
	infile.results	<- subset(infile.results, PASSED_CHECK==1)
	#	read submissions
	tmp			<- lapply(seq_len(nrow(infile.results)), function(i){
				ev	<- as.data.table(read.csv(infile.results[i,FILE], stringsAsFactors=FALSE))
				ev[, FILE:= infile.results[i,FILE]]
				ev[, TIME:= infile.results[i,TIME]]
				ev
			})	
	ev			<- do.call('rbind',tmp)	
	#	read test data and merge
	load(infile.test.data)
	setnames(test, 'DISASTERS_N','ACTUAL')
	test		<- unique(subset(test, YEAR>=2015, select=c(ISO, YEAR, ACTUAL)))
	ev			<- merge(test, ev, by=c('ISO','YEAR'))
	#	calculate MSE
	evs			<- ev[, list( MSE= mean((ACTUAL-PREDICTION)*(ACTUAL-PREDICTION)) ), by=c('FILE','TIME','TEAM_ID','SUBMISSION_ID')]	
	#	return MSE
	if(save)		
		write.table(evs, row.names=FALSE, sep='\t', file=file.path(indir.submissions, "MSE_latest_submissions.txt"))
	setkey(evs, MSE)
	return(evs)	
}

#dc.evaluate.plot<- function()
#{
	#tmp			<- rbind( subset(ev, select=c(ISO,COUNTRY,YEAR,PR_METHOD,PR_VALUE)), melt(ev, id.vars=c('ISO', 'COUNTRY', 'YEAR'), measure.vars=c('ACTUAL')))
	#set(tmp, NULL, 'variable', tmp[, factor(variable, levels=c('EMS','value'), labels=c('obs in 2016','predicted for 2016'))])
	#ggplot(tmp, aes(y=COUNTRY, x=value, colour=variable)) + 
	#		geom_point() +			
	#		theme_bw() 
#ggsave(file=paste0(outfile.base,'randomforestv2_obs_pred.pdf'), h=25, w=8)
#}
