TARGETS			:= $(patsubst %.tex, %.png, $(wildcard *.tex))
LATEX			:= latex
DVIPNG			:= dvipng
DVIPNGFLAGS		:= -x 1200 -T tight z 9

.PHONY: all clean
all: $(TARGETS)

$(TARGETS): %.png: %.dvi
	$(DVIPNG) $(DVIPNGFLAGS) -o $@ $^

%.dvi: %.tex
	$(LATEX) $^

clean:
	rm -f $(TARGETS) $(TARGETS:.png=.aux) $(TARGETS:.png=.dvi) \
		$(TARGETS:.png=.log)
