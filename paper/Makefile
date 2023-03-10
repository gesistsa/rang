render: bib
	quarto render paper.qmd
	sed -e '259,280d' paper.tex > paper2.tex
	pdflatex paper2.tex
	bibtex paper2
	pdflatex paper2.tex
	pdflatex paper2.tex
	mv paper2.pdf paper.pdf
	rm paper2.*
bib:
	bibcon -b ~/dev/dotfiles/bib.bib -o paper.bib paper.qmd
