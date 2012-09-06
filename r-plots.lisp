(in-package :vtools)

(defun start-r ()
  (rcl:r-init))

(defvar *r-quad-plot-function-string*
  "(function(infile,outfile,title){
  tab <- read.table(infile,header=F);
  pdf(outfile);
  plot(tab[,1],tab[,2], col='black',xlab='Volume (A^3)',ylab='Energy (eV)',cex=2.5);
  polyfit <- coef(lm(tab[,2] ~ poly(tab[,1],2,raw=T)));
  points(tab[,1],polyfit[1] + tab[,1]*polyfit[2] + (tab[,1])^2*polyfit[3],col='red',cex=2.5,ylim=c(-22.31,-22.3));
  legend('bottomleft',  cex=1.2, c('energy curve','quadratic fit'), col=c('black','red'),pch=21);
  title(main=title);
  bm <- 2 * polyfit[3] *  tab[which.min(tab[,2]), 1];
  dev.off();
  print(c('a0=',polyfit[1]));
  print(c('a1=',polyfit[2]));
  print(c('a2=',polyfit[3]));
  print(c('bulk modulus= ',round(160.218*bm,digits=3)))})")

(defun quadfit (data-in-pathname pdf-out-pathname title)
  (let* ((in (delimit-string #\' (namestring data-in-pathname)))
	 (out (delimit-string #\' (namestring pdf-out-pathname)))
	 (title (delimit-string #\' title))
	 (args (conc "(" (conc-with-char #\, in out title) ")"))
	 (r-command (conc *r-quad-plot-function-string* args)))
    (rcl::r-parse-eval r-command)))
