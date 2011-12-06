guide.html:
accessHTML.pdf:

TEX_FILES=accessHTML
PDF_FILES=$(TEX_FILES:%=%.pdf)

clean:
	-rm $(TEX_FILES:%=%.log) $(TEX_FILES:%=%.aux) $(TEX_FILES:%=%.bbl) $(TEX_FILES:%=%.blg) $(TEX_FILES:%=%.out)

squeekyClean: clean
	-rm $(PDF_FILES)      


%.pdf: %.tex
	pdflatex $<
	bibtex $*
	pdflatex $<
	pdflatex $<

%.ps: %.dvi
	dvips -o $@ $<

%.dvi: %.tex
	latex $<
	bibtex $*
	latex $<
	latex $<



XSLTPROC=xsltproc
XSL_FILE="http://www.omegahat.org/XSL/html/Rhtml.xsl"
#myarticle.xsl

XSLT_PARAMS=-param "html.stylesheet" '"http://www.omegahat.org/OmegaTech.css"' 
XSLT_OPTS=--nonet --xinclude 



%.html: %.xml GNUmakefile # $(XSL_FILE)
	$(XSLTPROC) -o $@ $(XSLT_OPTS) $(XSLT_PARAMS) $(XSL_FILE) $<




