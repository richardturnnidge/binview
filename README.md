### Binary File Viewer

`Binview` is a simple MOSlet that allows you to view the contants of a binary file of any type.

Nano is great for viewing an editing text files, but if you want to quickly check a binary file, you need something which will display the byte information, even when not in the visibale ascii character set.

To use, first download the binary file and put it in the MOS directory on your SD card.

It is a commnad line tool, so requires some parameters to function.

The format is:

`binview filename [columns] [display format]`

eg.

`binview samplefile.mod 12 h`

To show the file with 12 columns in hexadecimal.

The filename is needed, obviously, but the last two are optional. The order, however, is important. You can have columns without format, but not the other way round. 

The number of columns can be any number practical for the display resolution.

The display format options are:
`h = hexadecimal
d = decimal
b = binary`


`binview samplefile.mod` will use a default of 8 columns and show hexadecimal.

`binview samplefile.mod 4` will use 4 columns and show hexadecimal.

`binview samplefile.mod 4 d` will use 4 columns and show decimal.




The columns argument is simply how many rows of vertical columns of data should be displayed.

The display format is whether to show Hexadecimal, Decimal, or Binary format.

An ascii representation will also be dsiplayed, where possible.

