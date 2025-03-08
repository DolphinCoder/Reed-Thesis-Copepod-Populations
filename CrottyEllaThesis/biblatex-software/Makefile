TEXS = $(wildcard *.tex)
MAIN = sample-use-sty

.SILENT: distrib all


all: $(MAIN).pdf software-biblatex.pdf

biblatex-styles: stublist mkbiblatexstubs.sh
	bash mkbiblatexstubs.sh < stublist

biblio.bib: ../swentry.org
	../bin/extract-bibtex.pl < $< > $@

swentries.tex: ../swentry.org
	../bin/export-org-to-latex $<
	../bin/extract-entries.pl < ../swentry.tex > $@

software-biblatex.pdf: software-biblatex.tex swentries.tex history.tex biblio.bib manual.bib
	pdflatex software-biblatex.tex > /dev/null
	bibtex software-biblatex
	pdflatex software-biblatex.tex > /dev/null
	pdflatex software-biblatex.tex > /dev/null

sample-use-sty.pdf: biblio.bib sample-content.tex sample-use-sty.tex

sample-use-sty.nohyperref.tex: sample-use-sty.tex
	grep -v hyperref $< > $@

sample-use-sty.nohyperref.pdf: biblio.bib sample-content.tex sample-use-sty.nohyperref.tex

sample-use-sty.backref.pdf: biblio.bib sample-content.tex
	# test that backrefs work
	pdflatex -jobname sample-use-sty.backref "\PassOptionsToPackage{backref=true}{biblatex} \input{sample-use-sty.tex}" > /dev/null
	biber sample-use-sty.backref > /dev/null
	pdflatex -jobname sample-use-sty.backref "\PassOptionsToPackage{backref=true}{biblatex} \input{sample-use-sty.tex}" > /dev/null
	pdflatex -jobname sample-use-sty.backref "\PassOptionsToPackage{backref=true}{biblatex} \input{sample-use-sty.tex}" > /dev/null

history.tex:
	git for-each-ref --format="\begin{release}{%(refname:short)}{%(taggerdate:short)} \item %(subject) \end{release}" refs/tags | grep bltx | tac > history.tex

sample.tex : biblio.bib biblatex-styles

%.pdf: %.tex
ifeq (, $(shell PATH=$(PATH) which rubber))
	echo "No rubber in $(PATH), falling back to basic pdflatex calls"
	pdflatex $< > /dev/null
	biber $* > /dev/null
	pdflatex $< > /dev/null
	pdflatex $< > /dev/null
else
	rubber -m pdftex $<
endif

%.txt: %.pdf
	pdftotext $<

test:	sample-use-sty.txt sample-use-sty.nohyperref.txt sample-use-sty.backref.txt
	# check that we get the same results with and without hyperref
	echo -n "Testing hyperref vs nohyperref: "
	(diff sample-use-sty.txt sample-use-sty.nohyperref.txt > /dev/null; if [ $$? -ne 0 ]; then echo "FAIL"; else echo "SUCCESS"; fi)
	# check that backrefs work
	echo -n "Testing presence of backrefs: "
	(grep "cited on page" sample-use-sty.backref.txt > /dev/null; if [ $$? -ne 0 ]; then echo "FAIL"; else echo "SUCCESS"; fi)

distrib:
	# remind maintainer to update version number in the documentation
	echo -n "Updated version number in software-biblatex.tex, date strings in software.* ? [y/N]"
	read ans; if [ ".$$ans" != ".y" ]; then echo "Please update this information and run make distrib again"; exit 1; fi
	# update history
	rm history.tex
	make software-biblatex.pdf
	rm -rf biblatex-software
	mkdir biblatex-software
	cp -a `git ls-files` biblatex-software
#	tar --transform 's,^\(.\),biblatex-software/\1,' -czvf biblatex-software.tar.gz `git ls-files`
	tar -czvf biblatex-software.tar.gz biblatex-software

.PHONY: preserve restore clean distclean

clean:
	# preserve PDF files under version control
	mkdir -p .save
	mv `git ls-files | grep pdf` .save
	# clean up the rest
	make cleaninner
	# restore PDF files under version control
	mv .save/*.pdf .
	rmdir .save

cleaninner: $(patsubst %,%/clean,$(TEXS))
	rm -f *+sw.bbx *+sw.dbx *+sw.cbx *+sw.lbx *~ *.txt sample-use-sty.backref.* sample-use-sty.nohyperref.*

%/clean:
	if [ -f $* ]; then rubber -m pdftex --clean $* ; fi

distclean: clean
