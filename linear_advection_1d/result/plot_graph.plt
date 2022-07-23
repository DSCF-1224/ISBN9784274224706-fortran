set terminal svg
set output folder_name . '/' . graph_title . ".svg"
set title graph_title

plot \
for [ itr_step = 0 : 6 : 2 ] \
folder_name . '/' . file_name \
every 1:1:0:itr_step::itr_step \
with linespoints \
title sprintf( "n=%d", itr_step )

set output
