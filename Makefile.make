file = cp2021t


pdf: $(file).tex
	pdflatex $(file).tex

bib: 
	bibtex $(file)

all: $(file).tex
	lhs2TeX $(file).lhs > $(file).tex
	pdflatex $(file).tex
	#bibtex $(file)
	#pdflatex $(file).tex
	#pdflatex $(file).tex

.PHONY : rubber
rubber :
	@rubber --pdf -f $(file)

clean:
	rm -rf *.aux *.log *.bbl *.bak *.ptb *.blg *.out *.spl 

cleanall : clean
	rm $(file).pdf