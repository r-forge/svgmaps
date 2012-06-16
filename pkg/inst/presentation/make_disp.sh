R CMD BATCH to-presentation.R
~/.cabal/bin/pandoc -s -S  -t slidy --mathjax --self-contained presentation.md -o presentation.html