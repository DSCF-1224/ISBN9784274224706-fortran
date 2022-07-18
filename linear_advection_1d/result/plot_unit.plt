set terminal svg
set output graph_title . ".svg"
set title graph_title

plot \
for [ itr_step = 0 : 6 : 2 ] \
file_name \
every 1:1:0:itr_step::itr_step \
with linespoints \
title sprintf( "n=%d", itr_step )
