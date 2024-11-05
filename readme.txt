HAC - HAC Ada Compiler
======================

HAC is perhaps the first open-source (albeit very
partial) Ada compiler fully programmed in Ada itself.

====
More complete description in the file:  doc/hac.txt
====

If you are impatient: in Alire (https://alire.ada.dev/),
do "alr get hac", then "alr run" from the hac* directory.

Alternatively: have GNAT installed (https://www.adacore.com/download),
then, on your preferred command-line interpreter:

  gnatmake -P hac

  cd exm
  ../hac gallery.adb

(if `gnatmake` doesn't work, try `gprbuild`;
 for Windows, '\' is meant in place of '/')

Or, if you don't want to touch the command-line at all for playing
with HAC, you can use LEA (http://l-e-a.sf.net/).

Enjoy!
