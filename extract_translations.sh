xgettext --sort-output --keyword=translatable -o po/imprint_glade.pot imprint.ui
hgettext -k __ -o po/imprint_source.pot src/*.hs
sed --in-place po/imprint_glade.pot --expression=s/CHARSET/UTF-8/
sed --in-place po/imprint_source.pot --expression=s/CHARSET/UTF-8/
msgcat po/imprint_source.pot po/imprint_glade.pot > po/imprint.pot
rm po/imprint_source.pot po/imprint_glade.pot
