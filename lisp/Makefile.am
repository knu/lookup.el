SOURCES = evi.el evi-mule.el lookup.el lookup-utils.el lookup-types.el \
	  lookup-vars.el lookup-gaiji.el lookup-cache.el lookup-record.el \
	  lookup-search.el lookup-select.el lookup-history.el \
	  lookup-glyph.el ndeb-image.el \
	  ndtut.el ndic.el ndeb.el ndtp.el ndict.el ndsrd.el ndkks.el \
	  ndspell.el ndcookie.el sdicf.el stem-english.el
EXTRA_DIST = lookup-vars.el.in
DISTCLEANFILES = lookup-vars.el
MAINTAINERCLEANFILES = Makefile.in

all: lookup.elc lookup-autoloads.el 

lookup.elc: lookup-vars.el lookup-compile.el $(SOURCES)
	$(EMACS) -batch -l lookup-compile.el -f lookup-bytecomp $(SOURCES)

lookup-autoloads.el: lookup-vars.el lookup-compile.el $(SOURCES)
	rm -f lookup-autoloads.el
	$(EMACS) -batch -l lookup-compile.el -f lookup-autoload $(SOURCES)

lookup-vars.el: $(top_builddir)/config.status $(srcdir)/lookup-vars.el.in
	@rm -f lookup-vars.el
	sed -e "s/\@VERSION\@/$(VERSION)/" \
	     -e "s!\@pkgemacsdir\@!$(emacsdir)/lookup!" \
	     $(srcdir)/lookup-vars.el.in > lookup-vars.el
