set xrange [0:2]
set yrange [*:*]

set format x '%2.1f'
set format y '%2.1f'

set grid

set xzeroaxis linetype -1

set key reverse
set key invert

graph_title = 'FTCS'
file_name   = 'ftcs.dat'
load 'plot_unit.plt'

graph_title = 'Lax'
file_name   = 'lax.dat'
load 'plot_unit.plt'

graph_title = 'Lax-Wendroff'
file_name   = 'lax_wendroff.dat'
load 'plot_unit.plt'

graph_title = 'upwind1'
file_name   = 'upwind1.dat'
load 'plot_unit.plt'

graph_title = 'upwind2'
file_name   = 'upwind2.dat'
load 'plot_unit.plt'
