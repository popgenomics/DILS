#!/shared/software/miniconda/envs/r-3.5.1/bin/Rscript
library(FactoMineR)
for(i in commandArgs()){
	tmp = strsplit(i, '=')
	if(tmp[[1]][1] == 'timeStamp'){ timeStamp = tmp[[1]][2] }
}


input = read.table( paste( timeStamp, '/distribution_PCA.txt', sep=''), h=T, sep='\t')

toRemove = c(1)
for(i in 1:(ncol(input)-1)){
	if(sd(input[,i]) < 0.00001){
		toRemove = c(toRemove, i)
	}
}

toRemove = unique(toRemove)
input = input[, -toRemove]

res.pca <- PCA(input[, -ncol(input)], graph = FALSE, ncp=3)

output_coord = data.frame(res.pca$ind$coord, origin = input$origin)
output_contrib = data.frame(res.pca$var$contrib)

# write the output file
write.table(output_coord, paste( timeStamp, '/table_coord_PCA_SS.txt', sep=''), col.names=T, row.names=F, quote=F, sep='\t') # coordinates to plot the 3D PCA
write.table(output_contrib, paste( timeStamp, '/table_contrib_PCA_SS.txt', sep=''), col.names=T, row.names=T, quote=F, sep='\t') # coordinates to plot the table of contributions
write.table(res.pca$eig, paste( timeStamp, '/table_eigenvalues_PCA_SS.txt', sep=''), col.names=T, row.names=T, quote=F, sep='\t') # coordinates to plot the table of contributions

