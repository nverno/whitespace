emacs ?= emacs
ruby ?= ruby

html= "http://compsoc.dur.ac.uk/whitespace/tutorial.html"
json= tables.json
dat= $(json:.json=.dat)

.PHONY: clean
all: data tutorial

data: $(dat)
$(dat) : $(json)
	$(emacs) -batch -l build.el -f batch-convert $<

$(json) : $(tutorial)
	$(ruby) parse.rb

tutorial : tutorial.html
	wget $(html)

clean:
	$(RM) *~ tables.json
