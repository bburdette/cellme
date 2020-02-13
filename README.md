# cellme

Schelme cells, which are schelme programs that can refer to each other by id.
Suitable for making schelme spreadsheets and such.

Cellme.Cellme is an abstraction for cells that can refer to each other by some 'id', and are 
contained in a container of some sort, 'cc'.

ArrayCellme implements Cellme where the cells are contained in a 2D array and have (Int,Int) ids.

DictCellme implements Cellme where the cells are contained in a Dict and have String ids.

Check out the live demos of both - demo source is in the [examples](https://github.com/bburdette/cellme/tree/master/examples) dir.

[2D array (spreadsheet) demo](https://bburdette.github.io/cellme/arraycelldemo.html).

[Markdown demo](https://bburdette.github.io/cellme/mdcelldemo.html).
