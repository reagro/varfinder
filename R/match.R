

get_match <- function(x, cutoff=0.9){
  # x: a data.frame contains IBS results
  # cutoff: IBS cutoff for a matching 
	sub <- x[x$IBS > cutoff, ]
	out <- sub[, c("FID2", "FID1", "IBS")]
	names(out) <- c("field_id", "ref_id", "IBS")
	out <- out[with(out, order(field_id, -IBS)),]
	out2 <- out[!duplicated(out$field_id),]  
    list(out, out2)
}

# rescale x from 0 to 1
rescale01 <- function(x) {                              
  # Create user-defined function
	(x - min(x)) / (max(x) - min(x))
}


het_rate <- function(x){
	a <- as.data.frame(table(x))
	b <- merge(data.frame(x=c(0,1,2,3), val=c(0,0,0,0)), a, by="x", all.x=TRUE)
	if(nrow(a) < 4){
		b$Freq[is.na(b$Freq)] <- 0
	}
	b[b$x==1,]$Freq / (b[b$x==0,]$Freq + b[b$x==1,]$Freq + b[b$x==2,]$Freq)
}


combine_rf <- function(r, f) {
  
	if (nrow(r) != nrow(f)) {
		stop(sprintf("ref has %s rows and field has %s rows",  nrow(r), ncol(f)))
	}
	names(r)[1:3] <- c("SNPID", "Chr", "Pos")
	names(r)[-c(1:3)] <- paste0("REF_", names(r)[-c(1:3)])

	f <- f[, -c(2:3)]
	names(f)[1] <- "SNPID"
	names(f)[-1] <- paste0("FLD_", names(f)[-1])
  
	i <- c(f[,1] %in% r[,1], r[,1] %in% f[,1])
	if (!all(i)) {
		warning("not a perfect match between field and reference markers") 
		meta1 <- data.frame(Metric=c("Reference markers", "Sample markers",  "Common markers"), 
                      Value=c(nrow(r), nrow(f), nrow(d)))
		print(meta1)
	}

  	merge(r, f, by="SNPID")
}


recode_rf <- function(d, biallelic=TRUE, missflags="-") {

	### recode SNP to be the number of A alleles
	# There are four possible values stored in the variable genotype: 0, 1, 2 and 3. 
	# For bi-allelic SNP sites, 
		#0 = two B alleles, 
		#1 = one A and one B allele, 
		#2 = two A alleles
		#3 = missing
		
	# For multi-allelic sites, it is a count of the reference allele (3 meaning no call).
	# “Bit2” indicates that each byte encodes up to four SNP genotypes 
	# since one byte consists of eight bits.

	tmp <- as.matrix(d[, -c(1:3)])
	u <- unique(as.vector(tmp))
	unexpected <- u[!(u %in% c("0", "1", "2", missflags, NA))]
	if (length(unexpected) > 0) {
		stop(paste("Unexpected allelic value(s) found:", unexpected))
	}
	
	# missing
	tmp[is.na(tmp)] <- 3
	tmp[tmp %in% missflags] <- 3
	
	tmp <- apply(tmp, 2, as.numeric)
	if (biallelic) {
		tmp[tmp == 2] <- 9 # 2 het to 9 tmp
		tmp[tmp == 1] <- 2 # 1 homo alt to 2
		tmp[tmp == 9] <- 1 # 9 het to 1
	}
	
	cbind(d[, 1:3], tmp)

}

describe_rf <- function(df0, d0, genofile, sm, MAF_cutoff, SNP_Missing_Rate, Sample_Missing_Rate, Inb_method) {

	meta1 <- data.frame(Metric="Markers", Value=nrow(df0))

	df <- as.matrix(df0[,-c(1:3)])

	field.id <- grep("^FLD_", names(df0), value=TRUE)
	ref.id <- grep("^REF_", names(df0), value=TRUE)
  
  # calculate sample missing rate
	s <- as.data.frame(sm)
	s0 <- subset(s, sm <= Sample_Missing_Rate)
	refcnt <- length(ref.id)
	fldcnt <- length(field.id)
	meta2 <- data.frame(Metric=c("Marker MAF Cutoff", "Marker Missing Cutoff", "Marker Final", "Marker Final Coverage", "Ref Entries Total", "Field Sample Total", "Sample Missing Cutoff", "Sample Final"), 
    Value=c(MAF_cutoff, SNP_Missing_Rate, nrow(d0), ".", refcnt, fldcnt, Sample_Missing_Rate, nrow(s0)))
  
  # calculate inbreeding coefficient and then heterozygocity
	inb <- snpgdsIndInb(genofile, sample.id=row.names(s0), snp.id=d0$snp.id, autosome.only=FALSE,remove.monosnp=TRUE, maf=NaN,missing.rate=NaN, method=Inb_method, allele.freq=NULL, out.num.iter=TRUE,verbose=TRUE)
  
  # normalize the range between 0 and 1
	ic <- data.frame(sid=inb$sample.id, inb=inb$inbreeding)
	inb$inb[inb$inb < 0] <- 0
	inb$inb[inb$inb > 1] <- 1
	ic$het <- 1 - ic$inb
	#range(ic$inb)

	h <- apply(df, 2, het_rate)
	h1 <- as.data.frame(h)
	h1$sid <- row.names(h1)
	ic <- merge(ic, h1, by="sid")
	
	meta3 <- data.frame(Metric=c("Sample Het Avg", "Sample Het SD", "Sample Het Max", "Sample Het Min"), 
                      Value=c(mean(ic$h), sd(ic$h), max(ic$h), min(ic$h) ))
  	rbind(meta1, meta2, meta3)
}

match_rf <- function(d, MAF_cutoff, SNP_mr, sample_mr, 
			IBS_cutoff, inb_method = "mom.visscher", cpus=1){
  
	inb_method <- match.arg(inb_method, c("mom.weir", "mom.visscher", "mle", "gcta1", "gcta2", "gcta3"))

	df <- as.matrix(d[,-c(1:3)])

	field.id <- grep("^FLD_", names(d), value=TRUE)
	ref.id <- grep("^REF_", names(d), value=TRUE)

##  create a gds file
	obj_gds <- paste0(tempfile(), "_geno.gds")
	SNPRelate::snpgdsCreateGeno(obj_gds, genmat = df,
                   sample.id = names(d)[-c(1:3)], snp.id = d$SNPID,
                   snp.chromosome = d$Chr,
                   snp.position = d$Pos,
                   snp.allele = NULL, snpfirstdim=TRUE)
 
  # open the gbs obj
	genofile <- SNPRelate::snpgdsOpen(obj_gds)
	on.exit(SNPRelate::snpgdsClose(genofile))

  # calculate SNP maf and missing rate
	frq <- SNPRelate::snpgdsSNPRateFreq(genofile, sample.id=NULL, snp.id=NULL, with.id=TRUE)
	snpmr <- data.frame(snpid=frq$snp.id, maf=frq$MinorFreq, mr=frq$MissingRate)
	d0 <- subset(snpmr, maf >= MAF_cutoff & mr <= SNP_mr)
	sm <- SNPRelate::snpgdsSampMissRate(genofile, sample.id=field.id, snp.id=d0$snpid, with.id=TRUE)

	meta123 <- describe_rf(d, d0, genofile, sm, MAF_cutoff, SNP_mr, sample_mr, inb_method)
    
  # calculate pair-wise IBS
	ibs <- SNPRelate::snpgdsIBS(genofile, num.thread=cpus, autosome.only=FALSE, maf= MAF_cutoff, missing.rate=SNP_mr)
	out <- ibs[[3]]
	dimnames(out) <- dimnames(out) <- list(names(d)[-c(1:3)], names(d)[-c(1:3)])
	xy <- t(combn(colnames(out), 2))
  
  # convert to data.frame
	D <- data.frame(xy, dist=out[xy])
	names(D) <- c("ref", "field", "IBS")
  
  # subset ref vs. field
	D$type1 <- "field"
	D$type2 <- "ref"
	D[D$ref %in% ref.id, ]$type1 <- "ref"
	D[D$field %in% field.id, ]$type2 <- "field"
  
	D <- subset(D, type1 == "ref" & type2 =="field")
  
	names(D)[1:3] <- c("FID1", "FID2", "IBS")
  
	outlist <- list()
	outlist[["metadata"]] <- meta123

	for(i in IBS_cutoff){
		lout <- get_match(D, cutoff=i)

		### best match
		lout2 <- merge(lout[[2]], sm, by.x="field_id", by.y="row.names", all.y=TRUE)
		names(lout2)[4] <- "Sample_SNP_mr"
		
		lout2$field_id <- gsub("^FLD_", "", lout2$field_id)
		lout2$ref_id <- gsub("^REF_", "", lout2$ref_id)

		name <- paste0("IBS_cutoff_", i, "_best_match")
		outlist[[name]] <- lout2
		
		name2 <- paste0("IBS_cutoff_", i, "_all_match")
		lout[[1]]$field_id <- gsub("^FLD_", "", lout[[1]]$field_id)
		lout[[1]]$ref_id <- gsub("^REF_", "", lout[[1]]$ref_id)
		outlist[[name2]] <- lout[[1]]

		
		rp <- aggregate(lout[[1]]$IBS, lout[[1]][, "field_id", drop=FALSE], 
			function(i) c(mean(i, na.rm=TRUE), sd(i, na.rm=TRUE)))
		rp <- apply(rp[,2], 2, mean, na.rm=TRUE)
								
		nr <- as.data.frame(table(lout[[1]]$field_id))
		
		meta4 <- data.frame(Metric=c(paste0("Samples with match using IBS=",i), paste0("Avg number matches per sample using IBS=",i), paste0("Avg of IBS value with IBS=", i), paste0("SD of IBS value with IBS=", i)), Value=c(nrow(nr), mean(nr$Freq), rp[1], rp[2]) )
		
		meta <- rbind(meta123, meta4)
	}
	outlist[["metadata"]] <- meta
	outlist[["snpinfo"]] <- snpmr
  
	outlist
}



workflow_rf  <- function(ref_file, fld_file, country, crop, year, outdir=".", MAF_cutoff=0.05, SNP_mr=0.2, sample_mr=0.2, IBS_cutoff=.8, biallelic=TRUE, missflags="-") {
  
   ref <- data.table::fread(ref_file)
   fld <- data.table::fread(fld_file)
   crf <- varfinder::combine_rf(ref, fld)
   rec <- varfinder::recode_rf(crf, biallelic=biallelic, missflags=missflags)
   out <- varfinder::match_rf(rec, MAF_cutoff=MAF_cutoff, SNP_mr=SNP_mr, sample_mr=sample_mr, IBS_cutoff=IBS_cutoff)
   fxl <- file.path(outdir, paste0(c(country, crop, year, "matches.xlsx"), collapse="_"))
   writexl::write_xlsx(out, fxl)
   fxl
}
