excel2msatBEAST <- function(x,file="msat.txt",ds.name="data"){
	ni <- dim(x)[1]
	nl <- (dim(x)[2]-3)/2
	#a1 <- seq(1,nl*2,2)
	#a2 <- seq(2,nl*2,2)
	he <- sort(c(paste("Locus",1:nl,sep=""),paste("Locus",1:nl,sep="")))
	he <- c("id",he)
	cat("#microsat","\n",sep="",file=file,append=T)
	cat("#name\t",ds.name,"\n",sep="",file=file,append=T)
	cat(he,"\n",sep="\t",file=file,append=T)
	for (i in 1:ni){
		lab <- as.character(x[i,1])
		ms <- as.character(x[i,4:dim(x)[2]])
		#ms1 <- as.character(x[i,a1+3])
		#ms2 <- as.character(x[i,a2+3])
		if (length(which(ms == "0")) != 0)
			ms[which(ms == "0")] = "?"
		#if (length(which(ms2 == "0")) != 0)
		#	ms2[which(ms2 == "0")] = "?"
		#cat(paste(lab),ms1,"\n",sep="\t",file=file,append=T)
		cat(paste(lab),ms,"\n",sep="\t",file=file,append=T)
	}
	print("Done")
}