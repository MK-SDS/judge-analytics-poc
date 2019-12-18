#Author:
#Mkhuseli Mthukwane

#PDF scraper method


#In case you have issues with Rjava:

  if (Sys.getenv("JAVA_HOME")!="")
    +     Sys.setenv(JAVA_HOME="")

library(pdftools)
library(stringr)
library(tidyverse)

#row vector:

values.list<<-list()
all.cases.final<<-data.frame(Judge=character(0),Date=character(0),Name=character(0),Case_No=character(0),stringsAsFactors=FALSE)

#This is the file path that goes into the loop. The location of the cases:

cse.dir<<-dir("C:/Users/8460p/Documents/UCT Case")

#set working directory to where the folder is:
setwd("C:/Users\8460p/Documents/UCT Case")

pdf.mine<<-function(x){


for(ipath in cse.dir){

tstv<<-pdf_text(ipath )%>%
readr::read_lines()


##########Extract judge name:

#add boundary for grep for safety:

jvaris<<-paste("\\b","HONOURABLE","\\b",sep="")

fnd.jdge<<-grep(jvaris,tstv)

#cases begin at cut-off, case name index +2:

cse.cut<<-fnd.jdge+2

#split just in case theres more than 1 judge:

fnd.jdgesplt<<-strsplit(tstv[fnd.jdge],"AND") 


fnd.jdge<<-word(unlist(fnd.jdgesplt)[!unlist(fnd.jdgesplt) %in% ""], 2, sep="HONOURABLE")

#Remove punctuation & remove extra white space:
fnd.jdge<<-gsub("[^A-Za-z0-9]", " ", str_squish(fnd.jdge))

#add "and" if more than 1 judge:

if(length(fnd.jdge)>1){

fnd.jdge<<-paste(fnd.jdge,collapse=" AND ")

}



raw.cases<<-tstv[cse.cut:length(tstv)]

#get cases only:

raw.casefnd<<-grep(paste("\\b","vs","\\b",sep=""),raw.cases)

if(length(raw.casefnd)==0){

#raw.casefnd<<-raw.cases

#remove leading numbers & extra white space:
#cases<<-substring(raw.casefnd, 2)


cases<<-str_squish(raw.cases[!raw.cases %in% ""])
cases<<-cases[!cases %in% "ADMISSIONS"]

}else if(length(raw.casefnd)>0){

cases<<-raw.cases[raw.casefnd]

#remove leading numbers & extra white space:
cases<<-substring(cases, 3)


cases<<-str_squish(cases)

}



#get dates:

#There are spelling mistakes in some of the PDF's, "OCTOER" for example, so i add it explicitly. 
#We may miss the ones that are unsupervised. if you come across one, just add it manually to the below grep:

dtes<<-grep(paste(tolower(c(format(ISOdate(2004,1:12,1),"%B"),"OCTOER")),collapse="|"),tolower(tstv),value=TRUE)[1]

#bit of a risk splitting at the semi colon but what the heck:

dte<<-last(unlist(strsplit(dtes,":")))






#extract case names and case numbers:

for(cs in cases){

values.v<<-vector()

#extract case number:

case.no<<-last(unlist(strsplit(cs," "))) 

#define line vector:

ln.v<<-unlist(strsplit(cs," "))

case.name<<-paste(ln.v[1:length(ln.v)-1],collapse=" ")


tmp.df<<-data.frame(Judge=fnd.jdge,Date=dte,Name=case.name,Case_No=case.no,stringsAsFactors=FALSE)

tmp.df<<-data.frame(Judge=fnd.jdge,Date=dte,Name=case.name,Case_No=case.no,stringsAsFactors=FALSE)

#add to global list:

values.list[[cs]] <-tmp.df

}

tmp.global.cases<<-do.call(rbind,values.list )

tmp.global.cases<<-na.omit(tmp.global.cases)

all.cases.final<<-rbind(all.cases.final,tmp.global.cases)

#remove duplicate rows:

all.cases.final<<-distinct(all.cases.final)

}
}

#Run function:

pdf.mine(cse.dir)

write.xlsx(all.cases.final,"WC Court Roll.xlsx")


