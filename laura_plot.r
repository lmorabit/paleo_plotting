## un-split
## save as .csv file

A <- read.csv('data2.csv', header=TRUE, stringsAsFactors=FALSE)

my_data <- A[ , 4:dim(A)[2] ]

#corrected_heights <- A$Corrected.height..m.
corrected_heights <- A$Corrected.height

## map to x and y coordinates

blankx <- seq(1,dim(my_data)[2]) 
blanky <- blankx
blanky[length(blanky)] <- length(corrected_heights)

## Y-AXIS (HEIGHT)
tick_spacing <- 20.
##tmp <- strsplit(as.character(max(corrected_heights)),".", fixed=TRUE)[[1]][1]
##topval <- max(corrected_heights) / (nchar(tmp)-1)
topval <- 160.
labelvec <- seq(0., topval,tick_spacing)
atvec <- ( labelvec - min(labelvec)) / max(labelvec - min(labelvec)) * ( length(corrected_heights) - 1 ) + 1

## X-AXIS (NAMES)
names <- colnames( my_data )
for (i in seq(1,length(names))){
	names[i] <- sub("..","& ",names[i], fixed=TRUE)
	names[i] <- sub("."," ", names[i], fixed=TRUE)
	names[i] <- sub("&",".",names[i], fixed=TRUE)
}

tiff("laura_plot2.tiff", width=960, height=960)
par(mar=c(2,6,22,2))
nsteps <- length(blankx) - 1
stepsize <- (max(labelvec)-min(labelvec))/nsteps
blanky <- seq(min(labelvec),max(labelvec),stepsize)
print(blankx)
print(blanky)
plot( blankx, blanky, type='n', axes=FALSE, ann=FALSE,cex.lab=1.5)
print(par("yaxp"))
axis( side=2, at=labelvec,  labels=labelvec, las=2, cex.axis=1.5 )
mtext( side=2, text="Corrected height (m)", line=4, cex=1.5)
axis( side=3, at=seq(1,length(names)),labels=names, font=3, las=2, cex.axis=1.5)

## loop through the species
for (i in seq(1,dim(my_data)[2])){
	## loop through the data for that species
	for (j in seq(1,dim(my_data)[1])){
		if (my_data[j,i] == "x"){
			points( i, corrected_heights[j], pch=16, cex=1.2 )
		}
	}
}
dev.off()


pdf("laura_plot2.pdf")
par(mar=c(2,6,16,2))
nsteps <- length(blankx) - 1
stepsize <- (max(labelvec)-min(labelvec))/nsteps
blanky <- seq(min(labelvec),max(labelvec),stepsize)
plot( blankx, blanky, type='n', axes=FALSE, ann=FALSE )
print(par("yaxp"))
axis( side=2, at=labelvec,  labels=labelvec, las=2 )
mtext( side=2, text="Corrected height (m)", line=3.5 )
axis( side=3, at=seq(1,length(names)),labels=names, font=3, las=2, cex.axis=0.7 )

## loop through the species
for (i in seq(1,dim(my_data)[2])){
	## loop through the data for that species
	for (j in seq(1,dim(my_data)[1])){
		if (my_data[j,i] == "x"){
			points( i, corrected_heights[j], pch=16, cex=0.5)
		}
	}
}
dev.off()

