guide.pdf: guide.ps
	ps2pdf guide.ps

guide.ps: guide.dvi
	dvips -t letter -o guide.ps guide.dvi

guide.dvi: guide.tex
	rubber guide.tex
