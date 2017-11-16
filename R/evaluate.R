dc.evaluate<- function(save=FALSE)
{
	require(data.table)
	require(ggplot2)
	require(DataChallenge.2017)
	
	#
	#	find submissions
	infile.results		<- system.file(package='DataChallenge.2017', "submissions")
	#infile.results		<- "~/Box Sync/OR_Work/teaching/2017_DataChallenge"
	infile.results		<- data.table(F=list.files(infile.results, pattern='_predictions.csv',full.name=TRUE))
	tmp					<- infile.results[, list(T=gsub(' ','_',as.character(file.mtime(F)))), by='F']
	infile.results		<- merge(infile.results, tmp, by='F')
	#infile.results		<- subset(infile.results, grepl('EMD',F))
	#	check if submissions are valid
	infile.results		<- infile.results[, {
				ev	<- as.data.table(read.csv(F, stringsAsFactors=FALSE))				
				list(PASSED_CHECK=dc.check.submission(ev))
			}, by=c('F','T')]
	
	#	read submissions
	tmp			<- lapply(seq_len(nrow(infile.results)), function(i){
				ev	<- as.data.table(read.csv(infile.results[i,F], stringsAsFactors=FALSE))
				ev[, F:= infile.results[i,F]]
				ev[, T:= infile.results[i,T]]
				ev
			})	
	ev			<- do.call('rbind',tmp)
	
	#	read test data and merge
	infile.test.data	<- system.file(package='DataChallenge.2017', "data_private", "emdat_test.RData")
	#infile.test.data	<- "~/Box Sync/OR_Work/teaching/2017_DataChallenge/MSc_stats_ICL_MSc_stats_ICL_EMDatSummary_Test.rda"
	load(infile.test.data)
	setnames(test, 'DISASTERS_N','ACTUAL')
	test		<- unique(subset(test, YEAR>=2015, select=c(ISO, YEAR, ACTUAL)))
		
	ev			<- merge(test, ev, by=c('ISO','YEAR'))
	
	#	calculate MSE
	evs			<- ev[, list( MSE= mean((ACTUAL-PREDICTION)*(ACTUAL-PREDICTION)) ), by=c('F','T','TEAM_ID','SUBMISSION_ID')]
	
	#	return MSE
	if(save)		
		write.table(evs, row.names=FALSE, sep='\t', file=file.path(system.file(package='DataChallenge.2017', "submissions"), "MSE_latest_submissions.txt"))
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
