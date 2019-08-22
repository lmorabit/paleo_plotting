#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

## test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

## process the arguments
infile = args[1]
mcd_col = as.numeric(args[2])
if ( length(args) == 3 ){
	outfile = args[3]
} else {
	outfile = gsub( '.csv', '.pdf', infile )
}

## define a function to get the data columns
find_data_cols <- function( mydf, nc=1 ){

	# loop through data columns and find where the values are longer than one character
	data_cols <- c()
	for ( xx in 1:length(colnames(mydf)) ){
		check_col <- 0
		dcol <- mydf[,xx]
		for ( dd in dcol ){
			if( nchar( trimws(dd) ) > nc ) check_col <- check_col + 1
		}
		if( check_col == 0 ) data_cols <- c( data_cols, xx )
	}
	return( data_cols ) 
}

## define a function to define lines with endpoints
find_endpoints <- function( datacol ){

	p1 <- c()
	p2 <- c()
	xx <- 1
	while ( xx <= length(datacol) ){
		## first check if there is a value
		if ( datacol[xx] != "" ){
			## there is a value, try the next in the sequence
			yy <- xx + 1
			## but also handle when there is a value at the end
			if ( yy < length( datacol ) ){
				if ( datacol[yy] == "" ){
					p1 <- c( p1, xx )
					p2 <- c( p2, xx )
					xx <- yy 
				} else {
					while( datacol[yy] != "" && yy <= length(datacol) ){
						yy <- yy + 1
					}
					p1 <- c( p1, xx )
					p2 <- c( p2, yy )
					xx <- yy 
				}
			} else {
				p1 <- c( p1, xx )
				p2 <- c( p2, xx )
				xx <- xx + 1
			}		
		} else {
			xx <- xx + 1 
		}
	}


	return( list( p1=p1, p2=p2 ) )
}

## read in data and get column names
A <- read.csv( infile, header=TRUE, stringsAsFactors=FALSE )
A_col_names <- colnames( A )

## find the right column for the Metres Composite Depth
mcd_depths <- A[,mcd_col]

## select out the data
data_idx <- find_data_cols( A )
my_data <- A[,data_idx]

## if there are missing depths, remove those rows
not_missing_idx <- which( is.finite( mcd_depths ) )
A <- A[not_missing_idx,]
mcd_depths <- mcd_depths[not_missing_idx]


## map to x and y coordinates
blankx <- seq(1,dim(my_data)[2])
blanky <- blankx
blanky[length(blanky)] <- length(mcd_depths)

## Y-AXIS (HEIGHT)
tick_spacing <- 20.
##tmp <- strsplit(as.character(max(mcd_depths)),".", fixed=TRUE)[[1]][1]
##topval <- max(mcd_depths) / (nchar(tmp)-1)
minval <- 10 * floor( min( mcd_depths )/10 )
maxval <- 10 * ceiling( max( mcd_depths )/10 )
if ( (maxval-minval) %% 20 > 0 ) maxval <- maxval + 10. 
labelvec <- seq(minval, maxval,tick_spacing)
atvec <- ( labelvec - min(labelvec)) / max(labelvec - min(labelvec)) * ( length(mcd_depths) - 1 ) + 1

## X-AXIS (NAMES)
names <- colnames( my_data )
for (i in seq(1,length(names))){
        names[i] <- sub("..","& ",names[i], fixed=TRUE)
        names[i] <- sub("."," ", names[i], fixed=TRUE)
        names[i] <- sub("&",".",names[i], fixed=TRUE)
}

pdf(outfile, width=15, height=5)
par(mar=c(1,6,10,0))
nsteps <- length(blankx) - 1
stepsize <- (max(labelvec)-min(labelvec))/nsteps
blanky <- seq(max(labelvec),min(labelvec),-stepsize)
plot( blankx, blanky, type='n', axes=FALSE, ann=FALSE,cex.lab=1.5, ylim=c(max(blanky),min(blanky)))
axis( side=2, at=labelvec,  labels=labelvec, las=2, cex.axis=1.0 )
mtext( side=2, text="Metres Composite Depth (mcd)", line=4)
axis( side=3, at=seq(1,length(names)),labels=names, font=3, las=2, cex.axis=0.7)

## set the point size
my_cex <- 0.8

## loop through the species
for (ii in seq(1,dim(my_data)[2])){
	species_data <- my_data[,ii]
	species_points <- find_endpoints( species_data )
	for ( jj in 1:length(species_points$p1) ){
		if ( species_points$p1[jj] == species_points$p2[jj] ){
			## plot a point
                        points( ii, mcd_depths[species_points$p1[jj]], pch=16, cex=my_cex )
		} else {
			## plot a line with endpoints
			points( c( ii, ii ), mcd_depths[c(species_points$p1[jj],species_points$p2[jj])], pch=16, cex=my_cex )
			lines( c( ii, ii ), mcd_depths[c(species_points$p1[jj],species_points$p2[jj])], lwd=1.5 )
		}
	}
}
dev.off()

