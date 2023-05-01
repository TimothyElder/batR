package:
# cp /Users/timothyelder/Documents/shape_of_stories/code/functions.R 
	R -e 'library(devtools);document()'
	cd ".."; R CMD build batR; R CMD INSTALL batR

documentation:
	 cd ".."; R -e 'library(devtools); build_manual(pkg = "batR", path = "/Users/timothyelder/Documents/batR")'

#documentation:
#  cd ".."; tar zxvf batR_0.0.1.tar.gz; R CMD Rd2pdf batR