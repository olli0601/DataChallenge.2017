dc.read.emdat.data<- function()
{
	require(data.table)
	
	indir		<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge'	
	infile.emd	<- file.path(indir,'EMDAT_Epidemics2000-2017_2017.09.14.csv')
	outfile.base<- file.path(indir,'MSc_stats_ICL')
	
	#	get default ISO values that we are only going to consider
	#	no need to anonymise
	infile.iso	<- file.path(indir,'MSc_stats_ICL_Data_AnonymizedCountryCode.rda')
	load(infile.iso)	#loads "iso"  		
	iso[, ISO_RND:=NULL]
	#setnames(iso, 'ISO_RND', 'ISO')
	
	
	#
	#	read detailed EMS data
	#	
	emd	<- as.data.table(read.csv(infile.emd, stringsAsFactors=FALSE))
	setnames(emd, colnames(emd), gsub('\\.+','_',toupper(colnames(emd))))
	#	remove footer
	emd	<- emd[1:778, ]		
	#	extract start years, end years 
	regex	<- '(.*)/(.*)/(.*)'
	emd[, START_YR:= as.integer(gsub(regex,'\\3',START_DATE))]
	emd[, START_MO:= gsub(regex,'\\2',START_DATE)]
	emd[, START_DY:= gsub(regex,'\\1',START_DATE)]
	set(emd, emd[, which(!grepl('^[0-9]+$',START_MO))], 'START_MO', NA_character_)
	set(emd, emd[, which(!grepl('^[0-9]+$',START_DY))], 'START_DY', NA_character_)
	set(emd, NULL, 'START_MO', emd[, as.integer(START_MO)])
	set(emd, NULL, 'START_DY', emd[, as.integer(START_DY)])
	emd[, END_YR:= as.integer(gsub(regex,'\\3',END_DATE))]
	emd[, END_MO:= gsub(regex,'\\2',END_DATE)]
	emd[, END_DY:= gsub(regex,'\\1',END_DATE)]
	set(emd, emd[, which(!grepl('^[0-9]+$',END_MO))], 'END_MO', NA_character_)
	set(emd, emd[, which(!grepl('^[0-9]+$',END_DY))], 'END_DY', NA_character_)
	set(emd, NULL, 'END_MO', emd[, as.integer(END_MO)])
	set(emd, NULL, 'END_DY', emd[, as.integer(END_DY)])
	set(emd, NULL, c('START_DATE','END_DATE'), NULL)
	#	restrict to 2010-2016
	emd	<- subset(emd, START_YR>2009 & START_YR<2017)
	set(emd, emd[, which(END_YR>2016)], 'END_YR', 2016)
	#	restrict to ISOs in iso
	tmp	<- setdiff(emd$ISO, iso$ISO)
	emd	<- subset(emd, !ISO%in%tmp)	
	#	clean up disease names
	set(emd, emd[, which(DISASTER_NAME=='' | DISASTER_NAME=='un-identified diseases' | DISASTER_NAME=='unknown' )], 'DISASTER_NAME', NA_character_)
	set(emd, NULL, 'DISASTER_NAME', emd[, tolower(DISASTER_NAME)])
	set(emd, emd[, which(grepl('acute',DISASTER_NAME) & grepl('syndrome',DISASTER_NAME) & grepl('diar',DISASTER_NAME))], 'DISASTER_NAME', 'acute diarroheal syndrome')
	set(emd, emd[, which(grepl('acute',DISASTER_NAME) & grepl('watery',DISASTER_NAME) & grepl('diar',DISASTER_NAME))], 'DISASTER_NAME', 'acute diarroheal syndrome')
	set(emd, emd[, which(grepl('acute',DISASTER_NAME) & grepl('respiratory',DISASTER_NAME) & grepl('syndrome',DISASTER_NAME))], 'DISASTER_NAME', 'SARS')	
	set(emd, emd[, which(grepl('hepatit',DISASTER_NAME) & grepl(' a',DISASTER_NAME))], 'DISASTER_NAME', 'Hepatitis_A')
	set(emd, emd[, which(grepl('hepatit',DISASTER_NAME) & grepl(' e',DISASTER_NAME))], 'DISASTER_NAME', 'Hepatitis_E')
	set(emd, emd[, which(grepl('acute jaundice syndrome',DISASTER_NAME))], 'DISASTER_NAME', 'Hepatitis_E')	
	set(emd, emd[, which(grepl('polio',DISASTER_NAME))], 'DISASTER_NAME', 'polio')
	set(emd, emd[, which(grepl('cholera',DISASTER_NAME))], 'DISASTER_NAME', 'cholera')
	set(emd, emd[, which(grepl('meni',DISASTER_NAME) & grepl('gitis',DISASTER_NAME))], 'DISASTER_NAME', 'meningitis')
	set(emd, emd[, which(grepl('meni',DISASTER_NAME) & grepl('occal',DISASTER_NAME))], 'DISASTER_NAME', 'meningitis')
	set(emd, emd[, which(grepl('yellow',DISASTER_NAME) & grepl('fever',DISASTER_NAME))], 'DISASTER_NAME', 'yellow fever')
	set(emd, emd[, which(grepl('meales',DISASTER_NAME) | grepl('measles',DISASTER_NAME))], 'DISASTER_NAME', 'measles')
	set(emd, emd[, which(grepl('ebola',DISASTER_NAME))], 'DISASTER_NAME', 'haemorrhagic fever')
	set(emd, emd[, which(grepl('haemor',DISASTER_NAME))], 'DISASTER_NAME', 'haemorrhagic fever')
	set(emd, emd[, which(grepl('yellow',DISASTER_NAME) & grepl('fever',DISASTER_NAME))], 'DISASTER_NAME', 'haemorrhagic fever')
	set(emd, emd[, which(grepl('lassa',DISASTER_NAME) & grepl('fever',DISASTER_NAME))], 'DISASTER_NAME', 'haemorrhagic fever')
	set(emd, emd[, which(grepl('dengue',DISASTER_NAME) & grepl('fever',DISASTER_NAME))], 'DISASTER_NAME', 'dengue fever')
	set(emd, emd[, which(grepl('dengue',DISASTER_NAME))], 'DISASTER_NAME', 'dengue fever')
	set(emd, emd[, which(grepl('chikungunya',DISASTER_NAME) | grepl('chukungunya',DISASTER_NAME))], 'DISASTER_NAME', 'chikungunya')
	set(emd, emd[, which(grepl('hand',DISASTER_NAME) & grepl('foot',DISASTER_NAME) & grepl('mouth',DISASTER_NAME))], 'DISASTER_NAME', 'hand foot and mouth disease')
	set(emd, emd[, which(grepl('disentery',DISASTER_NAME) | grepl('dysentery',DISASTER_NAME) | grepl('dysentry',DISASTER_NAME))], 'DISASTER_NAME', 'dysentery')
	set(emd, emd[, which(grepl('japanese',DISASTER_NAME) & grepl('encephalitis',DISASTER_NAME))], 'DISASTER_NAME', 'japanese encephalitis')
	set(emd, emd[, which(grepl('marburd virus',DISASTER_NAME) | grepl('marburg virus',DISASTER_NAME))], 'DISASTER_NAME', 'ebola')
	set(emd, emd[, which(grepl('diar',DISASTER_NAME) & grepl('hoea',DISASTER_NAME))], 'DISASTER_NAME', 'diarrhoea outbreak')
	set(emd, emd[, which(grepl('diar',DISASTER_NAME) & grepl('heal',DISASTER_NAME))], 'DISASTER_NAME', 'diarrhoea outbreak')
	set(emd, emd[, which(grepl('diar',DISASTER_NAME) & grepl('heic',DISASTER_NAME))], 'DISASTER_NAME', 'diarrhoea outbreak')
	set(emd, emd[, which(grepl('dysentery',DISASTER_NAME))], 'DISASTER_NAME', 'diarrhoea outbreak')
	set(emd, emd[, which(grepl('enteric',DISASTER_NAME))], 'DISASTER_NAME', 'diarrhoea outbreak')
	set(emd, emd[, which(grepl('gastroenteritis',DISASTER_NAME))], 'DISASTER_NAME', 'diarrhoea outbreak')
	set(emd, emd[, which(grepl('cryptosporidiosis',DISASTER_NAME))], 'DISASTER_NAME', 'diarrhoea outbreak')	
	set(emd, emd[, which(grepl('plague',DISASTER_NAME))], 'DISASTER_NAME', 'pneumonic plague')
	set(emd, emd[, which(grepl('penumon',DISASTER_NAME))], 'DISASTER_NAME', 'pneumonic plague')
	set(emd, emd[, which(grepl('pneumonia',DISASTER_NAME))], 'DISASTER_NAME', 'pneumonic plague')
	set(emd, emd[, which(grepl('leishmaniasis',DISASTER_NAME))], 'DISASTER_NAME', 'visceral leishmaniasis')
	set(emd, emd[, which(grepl('shigel',DISASTER_NAME))], 'DISASTER_NAME', 'diarrhoea outbreak')
	set(emd, emd[, which(grepl('typh',DISASTER_NAME))], 'DISASTER_NAME', 'typhoid fever')
	set(emd, emd[, which(grepl('^ari$',DISASTER_NAME) | grepl('influenza',DISASTER_NAME) | grepl('infuenza',DISASTER_NAME))], 'DISASTER_NAME', 'influenza')
	set(emd, emd[, which(grepl('rift',DISASTER_NAME) & grepl('valley',DISASTER_NAME))], 'DISASTER_NAME', 'rift valley fever')
	#	remove some columns
	set(emd,NULL, c('COUNTRY','MAGNITUDE_SCALE','DISASTER_TYPE','INSURED_LOSSES','MAGNITUDE_VALUE','ASSOCIATED_DISASTER','ASSOCIATED_DISASTER2','TOTAL_DAMAGE_000_US_','START_MO','START_DY','END_MO','END_DY'), NULL)
	
	#	merge iso
	emd		<- merge(emd, iso, by=c('ISO'))	
	save(emd, file=paste0(outfile.base,'_MSc_stats_ICL_EMDat_clean.rda'))
	
	#	expand if disasters span multiple years
	emd		<- emd[, {	
				z				<- START_YR:END_YR
				TOTAL_DEATHS	<- round(TOTAL_DEATHS/length(z))
				TOTAL_AFFECTED	<- round(TOTAL_AFFECTED/length(z))
				list(YEAR=z, TOTAL_DEATHS=TOTAL_DEATHS, TOTAL_AFFECTED=TOTAL_AFFECTED)	
			}, by=c('DISASTER_NO_','ISO','LOCATION','DISASTER_SUBTYPE','DISASTER_NAME')]
	emds	<- emd[, list(	DISASTERS_N=length(DISASTER_NO_), 
					DISASTER_NO_=paste0(DISASTER_NO_,collapse=','),
					TOTAL_DEATHS=sum(TOTAL_DEATHS), 
					TOTAL_AFFECTED=sum(TOTAL_AFFECTED)
			), by=c('ISO','YEAR')]
	tmp		<- as.data.table(expand.grid(ISO=unique(emds$ISO), YEAR=unique(emds$YEAR), stringsAsFactors=FALSE))	
	emds	<- merge(tmp, emds, by=c('ISO','YEAR'), all.x=TRUE)
	set(emds, emds[, which(is.na(DISASTERS_N))], 'DISASTERS_N', 0L)
	set(emds, emds[, which(is.na(TOTAL_DEATHS))], 'TOTAL_DEATHS', 0L)
	set(emds, emds[, which(is.na(TOTAL_AFFECTED))], 'TOTAL_AFFECTED', 0L)
	save(emds, file=paste0(outfile.base,'_MSc_stats_ICL_EMDatSummary.rda'))	
	
	test	<- subset(emds, YEAR>=2015)	
	train	<- copy(test)
	set(train, NULL, 'DISASTERS_N', NA_integer_)
	set(train, NULL, 'DISASTER_NO_', NA_character_)
	set(train, NULL, 'TOTAL_DEATHS', NA_real_)
	set(train, NULL, 'TOTAL_AFFECTED', NA_real_)	
	train	<- rbind(subset(emds, YEAR<2015), train)
	
	save(train, file=paste0(outfile.base,'_MSc_stats_ICL_EMDatSummary_Train.rda'))
	save(test, file=paste0(outfile.base,'_MSc_stats_ICL_EMDatSummary_Test.rda'))
}




dc.read.all.data<- function()
{
	dc.read.WorldBank.Capacity.data()
	dc.read.WorldBank.DevIndicators.data()
	dc.read.WorldBank.GDP.data()
	dc.read.WorldBank.Nutrition.data()
	dc.read.WorldBank.Poverty.data()	
	#dc.read.Cholera.data()
	dc.read.EMS.data()
	dc.read.IHR.data()	
}

dc.read.WorldBank.Capacity.data<- function()
{	
	require(data.table)		
	indir		<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge'
	outfile.base<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge/'
	
	#	get default ISO values that we are only going to consider
	infile.iso	<- file.path(indir,'MSc_stats_ICL_Data_AnonymizedCountryCode.rda')
	load(infile.iso)	#loads "iso"  		
	iso[, ISO_RND:=NULL]
	
	
	#
	#	read Data_Extract_From_Statistical_Capacity_Indicators.csv
	#	source: http://databank.worldbank.org/data	
	infile.cap	<- file.path(indir,'Data_Extract_From_Statistical_Capacity_Indicators.csv')	
	cap			<- as.data.table(read.csv(infile.cap, stringsAsFactors=FALSE))
	cap			<- cap[1:1085,]
	setnames(cap, colnames(cap), gsub('\\.+','_',toupper(colnames(cap))))
	setnames(cap, c('TIME','COUNTRY_CODE','COUNTRY_NAME'), c('YEAR','ISO','COUNTRY'))
	set(cap, NULL, c('TIME_CODE'), NULL)
	for(x in colnames(cap))
		set(cap, which(cap[[x]]=='..'), x, NA_character_)
	for(x in setdiff(colnames(cap), c('ISO','COUNTRY')))
		set(cap, NULL, x, as.numeric(cap[[x]]))
	set(cap, NULL, 'ISO', NULL)	
	setnames(cap, setdiff(colnames(cap), c('ISO','YEAR','COUNTRY')), paste0('CAP_',setdiff(colnames(cap), c('ISO','YEAR','COUNTRY'))))
	#	
	#	merge iso + cap	
	#	some failing, just ignore...
	sort(unique(setdiff(iso$COUNTRY, cap$COUNTRY)))
	sort(unique(setdiff(cap$COUNTRY, iso$COUNTRY)))	
	cap	<- merge(iso, cap, by='COUNTRY')
	set(cap, NULL, 'COUNTRY',NULL)	
	
	
	save(cap, file=paste0(outfile.base,'cap.RData'))
}


dc.read.WorldBank.Nutrition.data<- function()
{	
	require(data.table)		
	indir		<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge'
	outfile.base<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge/'
	
	#	get default ISO values that we are only going to consider
	infile.iso	<- file.path(indir,'MSc_stats_ICL_Data_AnonymizedCountryCode.rda')
	load(infile.iso)	#loads "iso"  		
	iso[, ISO_RND:=NULL]	
	
	#
	#	read Data_Extract_From_Health_Nutrition_and_Population_Statistics.csv
	#	source: http://databank.worldbank.org/data	
	infile.nut	<- file.path(indir,'Data_Extract_From_Health_Nutrition_and_Population_Statistics.csv')		
	nut			<- as.data.table(read.csv(infile.nut, stringsAsFactors=FALSE))
	nut			<- nut[1:1813,]
	setnames(nut, colnames(nut), gsub('\\.+','_',toupper(colnames(nut))))
	setnames(nut, c('YEAR','COUNTRY_CODE','COUNTRY_NAME'), c('YEAR','ISO','COUNTRY'))
	set(nut, NULL, c('YEAR_CODE'), NULL)
	for(x in colnames(nut))
		set(nut, which(nut[[x]]=='..'), x, NA_character_)
	for(x in setdiff(colnames(nut), c('ISO','COUNTRY')))
		set(nut, NULL, x, as.numeric(nut[[x]]))
	set(nut, NULL, 'ISO', NULL)
	set(nut, nut[, which(COUNTRY=="Korea, Dem. People’s Rep.")], 'COUNTRY', "Korea, Dem. Rep.")
	set(nut, nut[, which(COUNTRY=="St. Martin (French part)")], 'COUNTRY', "St. Martin")	
	setnames(nut, setdiff(colnames(nut), c('ISO','YEAR','COUNTRY')), paste0('NUT_',setdiff(colnames(nut), c('ISO','YEAR','COUNTRY'))))
	#	
	#	merge iso + nut
	#	some failing, just ignore...
	sort(unique(setdiff(iso$COUNTRY, nut$COUNTRY)))
	sort(unique(setdiff(nut$COUNTRY, iso$COUNTRY)))	
	nut	<- merge(iso, nut, by='COUNTRY')
	set(nut, NULL, 'COUNTRY',NULL)	
	save(nut, file=paste0(outfile.base,'nut.RData'))
}

dc.read.anonymize.country.code<- function()
{
	require(data.table)		
	outfile.base<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge/MSc_stats_ICL'			
	#
	#	read GDP data
	#	source: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD?view=map	
	infile.gdp	<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge/Data_Extract_From_GDP.csv'
	gdp			<- as.data.table(read.csv(infile.gdp, stringsAsFactors=FALSE, skip=4))
	setnames(gdp, colnames(gdp), gsub('\\.','_',toupper(colnames(gdp))))
	setnames(gdp, c('COUNTRY_NAME','COUNTRY_CODE'), c('COUNTRY','ISO'))
	set(gdp, NULL, 'X', NULL)
	gdp			<- melt(gdp, id.vars=c('COUNTRY','ISO','INDICATOR_NAME','INDICATOR_CODE'), variable.name='START_YR', value.name='GDP')
	iso			<- unique(subset(gdp, select=c(COUNTRY, ISO)))
	
	set.seed(43L)
	tmp			<- paste0( letters[round(runif(nrow(iso), min=1, max=24), d=0)], letters[round(runif(nrow(iso), min=1, max=24), d=0)], letters[round(runif(nrow(iso), min=1, max=24), d=0)], letters[round(runif(nrow(iso), min=1, max=24), d=0)])
	iso[, ISO_RND:=tmp]
	save(iso, file=paste0(outfile.base,'_Data_AnonymizedCountryCode.rda'))
}

dc.read.WorldBank.Poverty.data<- function()
{	
	require(data.table)		
	indir		<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge'
	outfile.base<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge/'
	
	#	get default ISO values that we are only going to consider
	infile.iso	<- file.path(indir,'MSc_stats_ICL_Data_AnonymizedCountryCode.rda')
	load(infile.iso)	#loads "iso"  		
	iso[, ISO_RND:=NULL]
	
	#
	#	read Data_Extract_From_Poverty_and_Equity.csv
	#	source: http://databank.worldbank.org/data
	infile.pov	<- file.path(indir,'Data_Extract_From_Poverty_and_Equity.csv')	
	pov			<- as.data.table(read.csv(infile.pov, stringsAsFactors=FALSE))
	pov			<- pov[1:1288,]
	setnames(pov, colnames(pov), gsub('\\.+','_',toupper(colnames(pov))))
	setnames(pov, 'COUNTRY_CODE', 'ISO')
	set(pov, NULL, c('YEAR_CODE'), NULL)
	for(x in colnames(pov))
		set(pov, which(pov[[x]]=='..'), x, NA_character_)
	for(x in setdiff(colnames(pov), c('ISO','COUNTRY')))
		set(pov, NULL, x, as.numeric(pov[[x]]))
	set(pov, NULL, 'ISO', NULL)
	setnames(pov, setdiff(colnames(pov), c('ISO','YEAR','COUNTRY')), paste0('POV_',setdiff(colnames(pov), c('ISO','YEAR','COUNTRY'))))
	#	
	#	merge iso + pov
	#	some failing, just ignore...
	sort(unique(setdiff(iso$COUNTRY, pov$COUNTRY)))
	sort(unique(setdiff(pov$COUNTRY, iso$COUNTRY)))	
	pov	<- merge(iso, pov, by='COUNTRY')
	set(pov, NULL, 'COUNTRY',NULL)	
	
	save(pov, file=paste0(outfile.base,'pov.RData'))
}

dc.read.WorldBank.GDP.data<- function()
{
	require(data.table)		
	outfile.base<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge/'
	
	infile.iso	<- file.path(indir,'MSc_stats_ICL_Data_AnonymizedCountryCode.rda')
	load(infile.iso)	#loads "iso"  		
	iso[, ISO_RND:=NULL]	
	
	#
	#	read GDP data
	#	source: https://data.worldbank.org/indicator/NY.GDP.MKTP.KD?view=map	
	infile.gdp	<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge/Data_Extract_From_GDP.csv'
	gdp			<- as.data.table(read.csv(infile.gdp, stringsAsFactors=FALSE, skip=4))
	setnames(gdp, colnames(gdp), gsub('\\.','_',toupper(colnames(gdp))))
	setnames(gdp, c('COUNTRY_NAME','COUNTRY_CODE'), c('COUNTRY','ISO'))
	set(gdp, NULL, 'X', NULL)
	gdp			<- melt(gdp, id.vars=c('COUNTRY','ISO','INDICATOR_NAME','INDICATOR_CODE'), variable.name='START_YR', value.name='GDP')
	set(gdp, NULL, 'START_YR', gdp[, as.integer(gsub('^X','',as.character(START_YR)))])
	gdp			<- subset(gdp, !is.na(GDP), c(COUNTRY, START_YR, GDP))
	setnames(gdp, 'START_YR','YEAR')
	gdp			<- merge(iso, gdp, by='COUNTRY')
	gdp[, COUNTRY:=NULL]
	save(gdp, file=paste0(outfile.base,'gdp.RData'))
}


dc.read.WorldBank.DevIndicators.data<- function()
{
	require(data.table)		
	indir		<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge'
	outfile.base<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge/'
	
	#	get default ISO values that we are only going to consider
	infile.iso	<- file.path(indir,'MSc_stats_ICL_Data_AnonymizedCountryCode.rda')
	load(infile.iso)	#loads "iso"  		
	iso[, ISO_RND:=NULL]			
	
	#
	#	read world-development-indicators
	#	source: http://databank.worldbank.org/data
	infile.dev	<- file.path(indir,'Data_Extract_From_World_Development_Indicators.csv')
	devi		<- as.data.table(read.csv(infile.dev, stringsAsFactors=FALSE))	
	setnames(devi, colnames(devi), gsub('\\.+','_',toupper(colnames(devi))))
	setnames(devi, c('TIME','COUNTRY_CODE','COUNTRY_NAME'), c('YEAR','ISO','COUNTRY'))
	set(devi, NULL, c('TIME_CODE'), NULL)
	for(x in colnames(devi))
		set(devi, which(devi[[x]]=='..'), x, NA_character_)
	for(x in setdiff(colnames(devi), c('ISO','COUNTRY')))
		set(devi, NULL, x, as.numeric(devi[[x]]))
	set(devi, NULL, 'ISO', NULL)
	set(devi, devi[, which(COUNTRY=="Korea, Dem. People\xd5s Rep.")], 'COUNTRY', "Korea, Dem. Rep.")	 	
	setnames(devi, setdiff(colnames(devi), c('ISO','YEAR','COUNTRY')), paste0('DEV_',setdiff(colnames(devi), c('ISO','YEAR','COUNTRY'))))
	#	
	#	merge iso + devi
	#	some failing, just ignore...
	sort(unique(setdiff(iso$COUNTRY, devi$COUNTRY)))
	sort(unique(setdiff(devi$COUNTRY, iso$COUNTRY)))		
	devi	<- merge(iso, devi, by='COUNTRY')
	set(devi, NULL, 'COUNTRY',NULL)			
	save(devi, file=paste0(outfile.base,'devi.RData'))
}

dc.read.IHR.data<- function()
{
	require(data.table)		
	indir		<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge'
	outfile.base<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge/'
	
	#	get default ISO values that we are only going to consider
	infile.iso	<- file.path(indir,'MSc_stats_ICL_Data_AnonymizedCountryCode.rda')
	load(infile.iso)	#loads "iso"  		
	iso[, ISO_RND:=NULL]	
	
	#
	#	read IHR
	#	source: Victor @ WHO
	infile.ihr	<- file.path(indir,'MSc_stats_ICL_IHR.csv')
	ihr	<- as.data.table(read.csv(infile.ihr, stringsAsFactors=FALSE))	
	setnames(ihr, colnames(ihr), gsub('\\.','_',toupper(colnames(ihr))))
	set(ihr, c(223L, 605L, 755L), 'COUNTRY', 'Cote D Ivoire')
	setnames(ihr, c('ISO3','YEAR'), c('ISO','START_YR'))
	set(ihr, NULL, c('PUBLISH_STATES','TOTAL_IHR'), NULL)
	for(x in colnames(ihr))
		set(ihr, which(ihr[[x]]=='.'), x, NA_character_)
	for(x in setdiff(colnames(ihr),c('START_YR','COUNTRY','ISO','TOTAL_IHR','WHO_REGION')))
		set(ihr, NULL, x, as.numeric(ihr[[x]]))	
	
	set(ihr, ihr[, which(COUNTRY=='Bahamas')], 'COUNTRY', "Bahamas, The")
	set(ihr, ihr[, which(COUNTRY=='Bolivia (Plurinational State of)')], 'COUNTRY', "Bolivia")
	set(ihr, ihr[, which(COUNTRY=="Congo")], 'COUNTRY', "Congo, Rep.")
	set(ihr, ihr[, which(COUNTRY=="Cote D Ivoire")], 'COUNTRY', "Cote d'Ivoire")
	set(ihr, ihr[, which(COUNTRY=="Czechia")], 'COUNTRY', "Czech Republic")
	set(ihr, ihr[, which(COUNTRY=="Democratic Republic of the Congo")], 'COUNTRY', "Congo, Dem. Rep.")
	set(ihr, ihr[, which(COUNTRY=="Egypt")], 'COUNTRY', "Egypt, Arab Rep.")
	set(ihr, ihr[, which(COUNTRY=="Gambia")], 'COUNTRY', "Gambia, The")
	set(ihr, ihr[, which(COUNTRY=="Iran (Islamic Republic of)")], 'COUNTRY', "Iran, Islamic Rep.")
	set(ihr, ihr[, which(COUNTRY=="Kyrgyzstan")], 'COUNTRY', "Kyrgyz Republic")
	set(ihr, ihr[, which(COUNTRY=="Lao People's Democratic Republic")], 'COUNTRY', "Lao PDR")
	set(ihr, ihr[, which(COUNTRY=="Micronesia (Federated States of)")], 'COUNTRY', "Micronesia, Fed. Sts.")
	set(ihr, ihr[, which(COUNTRY=="Republic of Korea")], 'COUNTRY', "Korea, Rep.")
	set(ihr, ihr[, which(COUNTRY=="Saint Kitts and Nevis")], 'COUNTRY', "St. Kitts and Nevis")
	set(ihr, ihr[, which(COUNTRY=="Saint Lucia")], 'COUNTRY', "St. Lucia")
	set(ihr, ihr[, which(COUNTRY=="The former Yugoslav republic of Macedonia")], 'COUNTRY', "Macedonia, FYR")
	set(ihr, ihr[, which(COUNTRY=="United Kingdom of Great Britain and Northern Ireland")], 'COUNTRY', "United Kingdom")	
	set(ihr, ihr[, which(COUNTRY=="United Republic of Tanzania")], 'COUNTRY', "Tanzania")
	set(ihr, ihr[, which(COUNTRY=="United States of America")], 'COUNTRY', "United States")
	set(ihr, ihr[, which(COUNTRY=="Venezuela (Bolivarian Republic of)")], 'COUNTRY', "Venezuela, RB")
	set(ihr, ihr[, which(COUNTRY=="Viet Nam")], 'COUNTRY', "Vietnam")
	set(ihr, ihr[, which(COUNTRY=="West Bank and Gaza Strip")], 'COUNTRY', "West Bank and Gaza")
	set(ihr, ihr[, which(COUNTRY=="Yemen")], 'COUNTRY', "Yemen, Rep.")	
	set(ihr, ihr[, which(COUNTRY=="Slovakia")], 'COUNTRY', "Slovak Republic")
	set(ihr, ihr[, which(COUNTRY=="Cape Verde")], 'COUNTRY', "Cabo Verde")
	set(ihr, ihr[, which(COUNTRY=="United States Virgin Islands")], 'COUNTRY', "Virgin Islands (U.S.)")
	set(ihr, ihr[, which(COUNTRY=="Saint Vincent and the Grenadines")], 'COUNTRY', "St. Vincent and the Grenadines")
	set(ihr, ihr[, which(COUNTRY=="Republic of Moldova")], 'COUNTRY', "Moldova")
	set(ihr, ihr[, which(COUNTRY=="Democratic People's Republic of Korea")], 'COUNTRY', "Korea, Dem. Rep.")
	
	whoreg	<- unique(subset(ihr, !is.na(WHO_REGION), select=c(COUNTRY, WHO_REGION)))
	#	merge whoreg + ISO
	sort(unique(setdiff(iso$COUNTRY, whoreg$COUNTRY)))
	sort(unique(setdiff(whoreg$COUNTRY, iso$COUNTRY)))		
	whoreg	<- merge(iso, whoreg, by='COUNTRY')
	set(whoreg, NULL, 'COUNTRY',NULL)
	save(whoreg, file=paste0(outfile.base,'whoreg.RData'))
	
	#	
	#	merge ihr + ISO	
	#	some failing, just ignore...
	set(ihr, NULL, c('ISO','WHO_REGION'), NULL)
	sort(unique(setdiff(iso$COUNTRY, ihr$COUNTRY)))
	sort(unique(setdiff(ihr$COUNTRY, iso$COUNTRY)))	
	ihr	<- merge(iso, ihr, by='COUNTRY')
	set(ihr, NULL, 'COUNTRY',NULL)		
	
	setnames(ihr, 'START_YR','YEAR')
	setnames(ihr, setdiff(colnames(ihr), c('ISO','YEAR','COUNTRY')), paste0('IHR_',setdiff(colnames(ihr), c('ISO','YEAR','COUNTRY'))))	
	save(ihr, file=paste0(outfile.base,'ihr.RData'))
}

dc.merge.data.by.countrycode<- function()
{
	require(DataChallenge.2017)
	data(train)
	data(cap)
	data(ihr)
	data(gdp)
	data(nut)
	data(pov)
	data(devi)
	data(whoreg)
	data(pop13)
	
	outdir		<- '~/Box Sync/OR_Work/teaching/2017_DataChallenge'
	outfile.base<- file.path(outdir,'MSc_stats_ICL_')	
	#
	# merge emd + others by iso and year	
	emd	<- merge(emd, whoreg, by=c('ISO'), all.x=TRUE)
	emd	<- merge(emd, pop13, by=c('ISO'), all.x=TRUE)
	emd	<- merge(emd, ihr, by=c('ISO','YEAR'), all.x=TRUE)	
	emd	<- merge(emd, gdp, by=c('ISO','YEAR'), all.x=TRUE)
	emd	<- merge(emd, devi, by=c('ISO','YEAR'), all.x=TRUE)
	emd	<- merge(emd, pov, by=c('ISO','YEAR'), all.x=TRUE)
	emd	<- merge(emd, cap, by=c('ISO','YEAR'), all.x=TRUE)
	emd	<- merge(emd, nut, by=c('ISO','YEAR'), all.x=TRUE)
	emd	<- subset(emd, !is.na(WHO_REGION))		# drop from test "Aruba"                 "Albania"               "Puerto Rico"           "West Bank and Gaza"    "Virgin Islands (U.S.)" "Kosovo"
	#
	write.csv(emd, file=paste0(outfile.base,'MergedData_EMD_plus_Covariates.csv'), row.names=FALSE)	
}
