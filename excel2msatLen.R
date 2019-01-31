excel2msatLen <- function(x,nuc){
ni <- dim(x)[1]
nl <- (dim(x)[2]-3)/2
lc <- seq(1,nl*2,2)+3
res <- x
if (length(lc) != length(nuc))
stop("nuc needs to be the same length as lc (number of loci)")
for (i in 1:length(lc)){
	dipl <- c( x[,lc[i]],x[,lc[i]+1] )
	allsorted <- sort(dipl[ dipl != 0 ])
	alleles <- allsorted[ !duplicated(allsorted) ]
	allen <- vector()
	for (j in 1:length(alleles))
		allen[j] <- ((alleles[j] - min(alleles))/nuc[i]) + 1
	names(allen) <- alleles
	res[ res[,lc[i]] != 0, lc[i] ] <- as.numeric(na.omit(allen[ as.character(x[,lc[i]]) ]))
	res[ res[,lc[i]+1] != 0, lc[i]+1 ] <- as.numeric(na.omit(allen[ as.character(x[,lc[i]+1]) ]))
	}
return(res)
}