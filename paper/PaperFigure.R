library('Sushi')
pdfname = "vignettes/Figure_1.pdf"
Sushi_data = data(package = 'Sushi')
data(list = Sushi_data$results[,3]) 
makepdf = TRUE

###
### CODE
###

if (makepdf == TRUE)
{
  pdf(pdfname, height=10, width=12)
}

# make a layout for all of the plots
layout(matrix(c(1,1,1,1,
                1,1,1,1,
                2,2,8,8,
                2,2,9,9,
                3,3,10,10,
                3,3,10,10,
                4,4,11,11,
                4,4,11,11,
                5,5,12,12,
                5,5,12,12,
                6,7,13,13,
                6,7,14,14
), 12, 4, byrow=TRUE))
par(mgp=c(3,.3,0))


###
### (A) manhattan plot
###

# set the margins
par(mar=c(3,4,3,2))

# set the genomic regions
chrom1            = "chr11"
chromstart1       = 500000
chromend1         = 5050000

chrom2            = "chr15"
chromstart2       = 73000000
chromend2         = 89500000

# make the manhattan plot
plotManhattan(bedfile=Sushi_GWAS.bed, pvalues=Sushi_GWAS.bed[,5], genome=Sushi_hg18_genome, cex=0.75)

# add zoom 1
zoomsregion(region=c(chromstart1,chromend1), chrom=chrom1, genome=Sushi_hg18_genome, extend=c(0.07,0.2), wideextend=0.2, offsets=c(0,.535))

# add zoom 2
zoomsregion(region=c(chromstart2,chromend2), chrom=chrom2, genome=Sushi_hg18_genome, extend=c(0.07,0.2), wideextend=0.2, offsets=c(.535,0))

# add labels
labelgenome(genome=Sushi_hg18_genome, n=4, scale="Mb", edgeblankfraction=0.20)

# add y-axis
axis(side=2, las=2, tcl=.2)
mtext("log10(P)", side=2, line=1.75, cex=.75, font=2)

# Add plot label
labelplot("A)", " GWAS", letteradj=-.025)


###
### (B) Hi-C
###

# set the margins
par(mar=c(3,4,2,2))

# set the genomic regions
chrom            = "chr11"
chromstart       = 500000
chromend         = 5050000
zoomregion       = c(1700000,2350000)

# plot the HiC data
phic = plotHic(Sushi_HiC.matrix, chrom, chromstart, chromend, max_y = 20, zrange=c(0,28))

# add labels
labelgenome(chrom, chromstart, chromend, n=4, scale="Mb", edgeblankfraction=0.20)

# add the legend
addlegend(phic[[1]], palette=phic[[2]], title="score", side="right", bottominset=0.4, topinset=0, xoffset=-.035, labelside="left", width=0.025, title.offset=0.035)

# add zoom
zoomsregion(region=zoomregion, extend=c(0.05,0.25))

# add zoombox
zoombox(zoomregion=zoomregion)

# Add plot label
labelplot("B)", " HiC")


###
### (C) 5C
###

# set the margins
par(mar=c(3,4,2,2))

# set the genomic regions
chrom            = "chr11"
chromstart       = 1650000
chromend         = 2350000

# plot the loops
pbpe = plotBedpe(Sushi_5C.bedpe, chrom, chromstart, chromend, heights=Sushi_5C.bedpe$score, offset=0, flip=FALSE, bty='n', lwd=1, plottype="loops", colorby=Sushi_5C.bedpe$samplenumber, colorbycol=SushiColors(3))

# add zoombox
zoombox(passthrough=TRUE)

# add the genome labels
labelgenome(chrom, chromstart, chromend, n=3, scale="Mb")

# add the legend
legend("topright", inset=0.01, legend=c("K562","HeLa","GM12878"), col=SushiColors(3)(3), pch=19, bty='n', text.font=2)

# add y-axis
axis(side=2, las=2, tcl=.2)
mtext("Z-score", side=2, line=1.75, cex=.75, font=2)

# Add plot label
labelplot("C)", " 5C")


###
### (D) ChIA PET (PolII)
###

# set the margins
par(mar=c(3,4,2,2))

# set the genomic regions
chrom            = "chr11"
chromstart       = 1650000
chromend         = 2350000

# plot the loops
pbpe = plotBedpe(Sushi_ChIAPET_pol2.bedpe, chrom, chromstart, chromend, flip=TRUE, bty='n', lwd=1, plottype="lines", colorby=abs(Sushi_ChIAPET_pol2.bedpe$start1-Sushi_ChIAPET_pol2.bedpe$start2), colorbycol=SushiColors(5))

# add the genome labels
labelgenome(chrom, chromstart, chromend, n=4, scale="Mb")

# add the legend
addlegend(pbpe[[1]], palette=pbpe[[2]], title="distance (bp)", side="right", bottominset=0.05, topinset=0.35, xoffset=-.035, labelside="left", width=0.025, title.offset=0.08, labels.digits=0)

# add zoombox
zoombox(passthrough=TRUE)

# Add plot label
labelplot("D)", " ChIA-PET (Pol2)")


###
### (E) DNaseI
###

# set the margins
par(mar=c(3,4,2,2))

# set the genomic regions
chrom            = "chr11"
chromstart       = 1650000
chromend         = 2350000
zoomregion1      = c(1860000,1861000)
zoomregion2      = c(2281000,2282400)

# overlapping, transparent, and rescaled
plotBedgraph(Sushi_DNaseI.bedgraph, chrom, chromstart, chromend, colorbycol=SushiColors(5))

# add zoom 1
zoomsregion(zoomregion1, extend=c(-0.8,0.18), wideextend=0.10, offsets=c(0,.577))

# add the genome labels
labelgenome(chrom, chromstart, chromend, n=4, scale="Mb")

# add zoombox
zoombox(zoomregion=zoomregion)

# add zoom 2
zoomsregion(zoomregion2, extend=c(0.01,0.18), wideextend=0.10, offsets=c(.577,0))

# add y-axis
axis(side=2, las=2, tcl=.2)
mtext("Read Depth", side=2, line=1.75, cex=.75, font=2)

# Add plot label
labelplot("E)", " DnaseI")


###
### (F) ChIP-Seq ChIP Exo
###

# set the genomic regions
chrom            = "chr11"
chromstart       = 1650000
chromend         = 2350000
zoomregion1      = c(1860000,1861000)
zoomregion2      = c(2281000,2282400)

# plot chip-seq data
plotBedgraph(Sushi_ChIPSeq_CTCF.bedgraph, chrom, zoomregion1[1], zoomregion1[2], transparency=.50, color=SushiColors(2)(2)[1])

# plot chip-seq data
plotBedgraph(Sushi_ChIPExo_CTCF.bedgraph, chrom, zoomregion1[1], zoomregion1[2], transparency=.50, color=SushiColors(2)(2)[2], overlay=TRUE, rescaleoverlay=TRUE)

# Add plot label
labelplot("F)", " ChIP-Seq / ChIP-Exo", letteradj=-.125)

# add the genome labels
labelgenome(chrom, zoomregion1[1], zoomregion1[2], n=3, line=.5, scale="Mb", edgeblankfraction=0.2)

# add zoombox
zoombox()

# add legend
legend("topright", inset=0.025, legend=c("ChIP-seq (CTCF)","ChIP-exo (CTCF)"), fill=opaque(SushiColors(2)(2),0.5), border=SushiColors(2)(2), text.font=2, cex=0.75)


###
### (G) Bed Pile up
###

# set the genomic regions
chrom            = "chr11"
chromstart       = 1650000
chromend         = 2350000
zoomregion1      = c(1955000,1965000)
zoomregion2      = c(2281000,2282400)

# plt the chip-seq data as a pile-up
plotBed(beddata=Sushi_ChIPSeq_pol2.bed, chrom=chrom, chromstart=zoomregion2[1], chromend=zoomregion2[2], colorby=Sushi_ChIPSeq_pol2.bed$strand, colorbycol=SushiColors(2), wiggle=0.001, height=0.25)

# add the genome labels
labelgenome(chrom, zoomregion2[1], zoomregion2[2], n=2, scale="Mb")

# add zoombox
zoombox()

# add legend
legend("topright", inset=0.025, legend=c("reverse","forward"), fill=SushiColors(2)(2), border=SushiColors(2)(2), text.font=2, cex=0.75)

# Add plot label
labelplot("G)", " ChIP-Seq", letteradj=-.125)


###
### (H) manhattan plot zoomed
###

# set the margins
par(mar=c(0.1,4,2,2))

# set the genomic regions
chrom            = "chr15"
chromstart       = 60000000
chromend         = 80000000
chromstart2      = 72000000
chromend2        = 74000000

# make the manhattan plot
plotManhattan(bedfile=Sushi_GWAS.bed, chrom=chrom2, chromstart=chromstart, chromend=chromend, pvalues=Sushi_GWAS.bed$pval.GC.DBP, col=SushiColors(6)(nrow(Sushi_hg18_genome))[15], cex=0.75)

# add zoom in
zoomsregion(region=c(chromstart2,chromend2), chrom=chrom2, genome=NULL, extend=c(0.075,1), offsets=c(0.0,0))

# add zoom box
zoombox(passthrough=TRUE, topextend=5)

# add y-axis
axis(side=2, las=2, tcl=.2)
mtext("Z-score", side=2, line=1.75, cex=.75, font=2)

# Add plot label
labelplot("H)", " GWAS")


###
### (I) Gene density
###

# set the margins
par(mar=c(3,4,1.8,2))

# set the genomic regions
chrom            = "chr15"
chromstart       = 60000000
chromend         = 80000000
chrom_biomart    = gsub("chr","",chrom)

# set the mart (since we want hg18 coordinates)
mart=useMart(host='may2009.archive.ensembl.org', biomart='ENSEMBL_MART_ENSEMBL', dataset='hsapiens_gene_ensembl')

# get just gene info
geneinfobed = getBM(attributes=c("chromosome_name","start_position","end_position"), filters=c("chromosome_name","start","end"), values=list(chrom_biomart,chromstart,chromend), mart=mart)

# add "chr" to the chrom column
geneinfobed[,1] = paste("chr",geneinfobed[,1],sep="")

# plot gene density
plotBed(beddata=geneinfobed[!duplicated(geneinfobed),], chrom=chrom, chromstart=chromstart, row='supplied', chromend=chromend, palettes=list(SushiColors(7)), type="density")

#label genome
labelgenome(chrom=chrom, chromstart, chromend, n=4, scale="Mb", edgeblankfraction=0.10)

# add zoom in
zoomsregion(region=c(chromstart2,chromend2), chrom=chrom2, genome=NULL, extend=c(2,1.0), wideextend=.75, offsets=c(0.0,0))

# add zoombox
zoombox(zoomregion=c(chromstart2,chromend2), topextend=5)

# Add plot label
labelplot("I)", " Gene Density")


###
### (J) RNA seq
###

# set the margins
par(mar=c(3,4,2,2))

# set the genomic regions
chrom2            = "chr15"
chromstart2       = 72800000
chromend2         = 73100000
zoomregion        = c(72998000,73020000)
chrom2_biomart    = 15

# plot transcripts
pg = plotGenes(Sushi_transcripts.bed, chrom2, chromstart2, chromend2, types=Sushi_transcripts.bed$type, colorby=log10(Sushi_transcripts.bed$score+0.001), colorbycol=SushiColors(5), labeltext=FALSE, maxrows=50, height=0.4, plotgenetype="box")

# label genome
labelgenome(chrom2, chromstart2, chromend2, n=3, scale="Mb")

# add the legend
addlegend(pg[[1]], palette=pg[[2]], title="log10(FPKM)", side="right", bottominset=0.4, topinset=0, xoffset=-.035, labelside="left", width=0.025, title.offset=0.055)

# add zoombox
zoombox(passthrough=TRUE)

# add zoom
zoomsregion(region=zoomregion, extend=c(-.025,1))

# Add plot label
labelplot("J)", " RNA-seq")


###
### (K) ChIP Seq peaks
###

# set the margins
par(mar=c(3,4,2,2))

# set the genomic regions
chrom            = "chr15"
chromstart       = 72800000
chromend         = 73100000
zoomregion       = c(72998000,73020000)

Sushi_ChIPSeq_severalfactors.bed$color = maptocolors(Sushi_ChIPSeq_severalfactors.bed$row, col=SushiColors(6))

# plot it
plotBed(beddata=Sushi_ChIPSeq_severalfactors.bed, chrom=chrom, chromstart=chromstart, chromend=chromend, rownumber=Sushi_ChIPSeq_severalfactors.bed$row, type="circles", color=Sushi_ChIPSeq_severalfactors.bed$color, row="given", plotbg="grey95", rowlabels=unique(Sushi_ChIPSeq_severalfactors.bed$name), rowlabelcol=unique(Sushi_ChIPSeq_severalfactors.bed$color), rowlabelcex=0.75)

# label genome
labelgenome(chrom, chromstart, chromend, n=3, scale="Mb")

# add zoom
zoombox(zoomregion = zoomregion)

# add zoom in
zoomsregion(region=zoomregion, chrom=chrom, extend=c(0.5,.22), wideextend=0.15, offsets=c(0.0,0))

# Add plot label
labelplot("K)", " ChIP-seq")


###
### (L) Pol2 bedgrpah
###

# set the margins
par(mar=c(3,4,2,2))

# set the genomic regions
chrom            = "chr15"
chromstart       = 72998000
chromend         = 73020000

# plot the Pol2 bedgraph data
plotBedgraph(Sushi_ChIPSeq_pol2.bedgraph, chrom, chromstart, chromend, colorbycol=SushiColors(5))

# label genome
labelgenome(chrom, chromstart, chromend, n=3, scale="Mb")

# add zoombox
zoombox(passthrough=TRUE)

# add y-axis
axis(side=2, las=2, tcl=.2)
mtext("Read Depth", side=2, line=1.75, cex=.75, font=2)

# Add plot label
labelplot("L)", " Chip-Seq (Pol2)")


###
### (M) RNA-seq bedgraph
###

# set the margins
par(mar=c(2,4,.5,2))

# set the genomic regions
chrom            = "chr15"
chromstart       = 72998000
chromend         = 73020000

# plot the K562 RNAseq bedgraph data
plotBedgraph(Sushi_RNASeq_K562.bedgraph, chrom, chromstart, chromend, colorbycol=SushiColors(5))

# label genome
labelgenome(chrom, chromstart, chromend, n=3, scale="Mb")

# add zoombox
zoombox(passthrough=TRUE)

# Add plot label
labelplot("M)", " RNA-seq")


###
### (N) Gene Structures
###

# set the margins
par(mar=c(3,4,.5,2))

# set the genomic region
chrom            = "chr15"
chromstart       = 72998000
chromend         = 73020000

# plot gene structures
plotGenes(Sushi_genes.bed, chrom, chromstart, chromend, maxrows=1, bheight=0.15, plotgenetype="arrow", bentline=FALSE, labeloffset=1, fontsize=1.2,arrowlength = 0.01)

# label genome
labelgenome(chrom, chromstart, chromend, n=3, scale="Mb")

# add zoombox
zoombox()

# Add plot label
labelplot("N)", " Gene Structures", letterline=-0.4, titleline=-0.4)

if (makepdf == TRUE)
{
  dev.off()
}


