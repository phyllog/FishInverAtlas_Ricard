DFO-Maritimes-atlas.pdf: DFO-Maritimes-atlas.ps 
	ps2pdf DFO-Maritimes-atlas.ps 
	
DFO-Maritimes-atlas.ps: DFO-Maritimes-atlas.dvi DFO-Maritimes-atlas.tex ../Figure/Atlas_summary_table1.tex Atlas_speciessummary_table1.tex SS-summaries.tex
	latex DFO-Maritimes-atlas.tex
	makeindex DFO-Maritimes-atlas
	latex DFO-Maritimes-atlas.tex
	latex DFO-Maritimes-atlas.tex
	dvips DFO-Maritimes-atlas.dvi

../Figure/Atlas_summary_table1.tex: 
	sed -i -e 's/y\.//g' ../Figure/Atlas_summary_table1.tex 
	sed -i -e 's/y\.//g' ../Figure/Atlas_summary_table2.tex 
	sed -i -e 's/y\.//g' ../Figure/Atlas_summary_table3.tex 
	
	sed -i -e 's/\& Area \& /\& Area ($km^{2}$) \& /g' ../Figure/Atlas_summary_table1.tex 
	sed -i -e 's/\& Area \& /\& Area ($km^{2}$) \& /g' ../Figure/Atlas_summary_table2.tex 
	sed -i -e 's/\& Area \& /\& Area ($km^{2}$) \& /g' ../Figure/Atlas_summary_table3.tex 	
	
	sed -i -e 's/Freq/Freq./g' ../Data/*_alldist.tex
	sed -i -e 's/Depth/Depth\/Prof../g' ../Data/*_alldist.tex
	sed -i -e 's/Temp/Temp./g' ../Data/*_alldist.tex
	sed -i -e 's/Sal/Sal./g' ../Data/*_alldist.tex
	
	sed -i -e 's/F50/$F_{50}$/g' ../Data/*_alldist.tex
	sed -i -e 's/F5/$F_{5}$/g' ../Data/*_alldist.tex
	sed -i -e 's/F25/$F_{25}$/g' ../Data/*_alldist.tex
	sed -i -e 's/F75/$F_{75}$/g' ../Data/*_alldist.tex
	sed -i -e 's/F95/$F_{95}$/g' ../Data/*_alldist.tex
	
#	& Area &
#	& Area ($km^{2}$) &
	
#SS-summaries.tex: species-list.csv#
#	./create-summary-tex-specnum.pl

#../Figure/SS10_Stratified-abundance.eps:
	#/cygdrive/c/Program\ Files/R/R-2.15.1/bin/R.exe CMD BATCH --no-save Atlas.R 
	
	
# rsync.exe -av --no-p --no-g --inplace /cygdrive/c/ATLAS_poissons_SS/ /cygdrive/r/Shared/Shackell_Nancy/ATLAS_poissons_SS 
