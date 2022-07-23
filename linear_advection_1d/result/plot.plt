set xrange [ 0.0:2.0]
set yrange [-1.0:2.0]

set format x '%2.1f'
set format y '%2.1f'

set grid

set xzeroaxis linetype -1

set key reverse
set key noinvert

folder_name = 'sample1'; load 'plot_sample.plt'
folder_name = 'sample2'; load 'plot_sample.plt'
folder_name = 'sample3'; load 'plot_sample.plt'
folder_name = 'sample4'; load 'plot_sample.plt'
