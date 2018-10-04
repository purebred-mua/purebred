%.html: %.md
	pandoc -V theme=solarized --fail-if-warnings --slide-level 2 --self-contained -t revealjs -s $< -o $@

PHONY: all

all: %.html
