.LOCAL_PKG_NAME <- packageName()

roctave <- function(end=TRUE, load=TRUE){
	
	if( load )
		devtools::load_all(.LOCAL_PKG_NAME)
	if( end ) 
		oend()
	
	# compile source files
	compile_src()
	
	#ostart()	
}
