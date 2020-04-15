ASCIIDOCTOR := asciidoctor

.PHONY: all
all: index.html purebred.1

.PHONY: clean
clean:
	rm -f index.html keybindings.adoc

keybindings.adoc:
	purebred-dump-keybinding > keybindings.adoc

index.html: index.adoc keybindings.adoc
	${ASCIIDOCTOR} index.adoc

purebred.1: index.adoc keybindings.adoc
	${ASCIIDOCTOR} -b manpage index.adoc
