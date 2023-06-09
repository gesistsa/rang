render:
	bash clean.sh ## clean up the plos theme
	quarto render paper.qmd
bib:
	bibcon -b ~/dev/dotfiles/bib.bib -o paper.bib paper.qmd
diff: render
	cp paper_r0.tex temp1.tex
	-latex temp1
	-bibtex temp1
	pdflatex temp1
	pdflatex temp1
	perl latexpand --expand-bbl temp1.bbl temp1.tex > flat1.tex
	cp paper.tex temp2.tex
	-latex temp2
	-bibtex temp2
	pdflatex temp2
	pdflatex temp2
	perl latexpand --expand-bbl temp2.bbl temp2.tex > flat2.tex
	cp flat2.tex paper_flat.tex
	latexdiff flat1.tex flat2.tex > diff.tex
	-pdflatex diff.tex
	-pdflatex diff.tex
	rm temp*
	rm flat*
	rm diff.aux
	rm diff.log
	rm diff.tex
