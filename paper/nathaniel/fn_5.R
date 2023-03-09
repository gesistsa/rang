                                        #Before starting we route all output to a file, fn_5.txt
sink("fn_5.txt")
                                        #First we load library foreign and library bife
                                        #Note the library command is hardcoded to the standard file for libraries
                                        #If your libraries are different, or they fail to load,  look at the commented out lib.loc subcommand
#These are the only R packages needed that are not in baseR
library("foreign")
#lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("bife")
#lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
                                        # now we read in the same Stata data as used in Table_1, saved as an early version Stata dta file, besley.dta which is put into data_b
                                        #For timing
start.time <- Sys.time()
 data_b<-read.dta("besley.dta")
glm_naive<-glm(graduate~Democracy+logGDPcapita+factor(country),data=data_b,family=binomial(link=logit))
 summary.glm(glm_naive)
 #now we run bife to both get correct LogitFE results and to get the data set corresponding to NotAllZero groups) - we run "vanilla" bife without cluster robust standard errors
 bife_all<-bife(graduate~Democracy+logGDPcapita|country,data=data_b,bias_corr="no")
 summary(bife_all,fixed="TRUE")
                                        #Now we create the NotAllZero data set for glm to show this works
y_notall0<-bife_all$model_info$y
x_notall0<-bife_all$model_info$X
country_notall0<-bife_all$model_info$id
glm_notall0 <- glm(y_notall0~x_notall0+factor(country_notall0),family=binomial(link=logit))
summary.glm(glm_notall0)
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("Execution time:",  time.taken)
closeAllConnections()
