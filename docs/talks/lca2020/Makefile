%.html: %.md
	pandoc --fail-if-warnings --slide-level 2 --self-contained -t revealjs -s $< -o $@

PHONY: all

all: roman_joost_tasty_tmux.html
