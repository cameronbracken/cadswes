#!/usr/bin/env Rscript

f <- readLines('crss-powell-rules.txt')
f <- gsub('> ','',f)
f <- unique(f)
ln <- grep('ASSIGN',f,invert=T); f <- f[ln]
ln <- grep('FOREACH',f,invert=T); f <- f[ln]
ln <- grep('#',f,invert=T); f <- f[ln]
f <- sort(f)
f <- f[f != ""]

cat(f,file='powellFunctionsSorted.txt',sep='\n')
