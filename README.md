# IMprint

<img align="right" src="https://raw.github.com/wiki/emmanueltouzery/imprint/imprint-128.png" alt="app icon"/>

![Main view detail](https://raw.github.com/wiki/emmanueltouzery/imprint/main_window.png)

IMprint can add text labels on your pictures, like the date on which a picture was taken. But besides this, it can add many more types of data, like exposition time, ISO.. And you can format the date to your liking.

You can pick the text style from a collection:

![Style list](https://raw.github.com/wiki/emmanueltouzery/imprint/styles_list.png)

Or customize the style yourself:

![Style editor](https://raw.github.com/wiki/emmanueltouzery/imprint/style_editor.png)

And it is possible to place several display items on a single picture, top-left, top-center, top-right, bottom-left, bottom-center, and bottom-right.

![Style editor](https://raw.github.com/wiki/emmanueltouzery/imprint/items_editor.png)

Besides English, Imprint is also translated in French and Slovenian languages.

To install the application on linux, install the haskell platform, get this git repository and run:

	cabal install

The application will register in the menus on linux, however for it to actually run, you must add ~/.cabal/bin to your path.

You can download a windows build in the release section.

[a gtk2hs bug]: https://github.com/gtk2hs/gtk2hs/issues/33
