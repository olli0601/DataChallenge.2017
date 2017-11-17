dc.make.help<- function()
{
	require(roxygen2)
	roxygenize('/Users/Oliver/git/DataChallenge.2017')
	require(DataChallenge.2017)
	
	require(devtools)
	setwd('~/git/DataChallenge.2017')
	devtools::document()
	devtools::use_github_links()
	use_travis()
	use_cran_badge() 
	knit(input="README.rmd", output = "README.md")
	
	use_vignette("DataChallenge.2017")
}

dc.make.web<- function()
{
	require(pkgdown)
	setwd('~/git/DataChallenge.2017')	
	pkgdown::build_site()
}
