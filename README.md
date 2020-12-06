# Gardener of Chaos

A tool used to generate the Garden of Chaos website, using (currently) hardcoded formatting methods and CSV data stored under "editable/csv". Note: I have butchered the CSV format, using lines with different numbers of elements, including totally blank lines. It's okay, I parsed it manually, and the CSV files are designed for use with a text editor anyway!

Also contains all the files used to generate the current version of the Garden of Chaos site, which is hopefully a sufficient demonstration of how it works!

"pages.csv" specification:
Markdown: \[ output HTML file ]|\[ page title ]|\[ input MD file ]|markdown
Non-Markdown: \[ output HTML file ]|\[ page title ]|\[ input CSS file ]|\[ input HTML file ]|\[ input JavaScript file ]|script

"navbar.csv" specification:
Entry with an icon: icon|\[ text ]|\[ hovertext ]|\[ positioning in the various sizes of navbar ]|\[ url ]|\[ image id ]|\[ image file ]|\[ image alt text ]
Entry without: text|\[ text ]|\[ hovertext ]|\[ positioning in the various sizes of navbar ]|\[ url ]

Any line in an input file of the form "!!!\[ CSV file, excluding .csv ]|\[ formatter name ]" will insert every line of that CSV file, formatted according to the formatter defined in Printer.hs. If it instead has a double pipe, it will format each of them simultaneously into a single line. I refuse to apologise for this horrific parsing method.

After running this, simply drag all the created HTML files (including any in subfolders, such as the "thoughts" subfolder that will be created in the current configuration) into the website directory.

Note: There is no CSS in this repository other than input files, nor are there images, so the pages will look broken! Use the CSS files from the Garden of Chaos repository to see how those are *supposed* to look, or just open the [actual page](https://gardenofchaos.xyz).
