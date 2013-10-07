Introduction
============

Translates the file positions.out, output from PIMAIM, into a movie file at format XYZ.

Author
======

pimaim2XFS is written by Maximilien Levesque, while in postdoc at UPMC, in the group of Mathieu Salanne.

Installation
============

You need SCONS to compile pimaim2xsf. SCONS is a modern alternative to CONFIGURE + MAKE.
You will certainly find it in the default repositories of your linux distribution.
SCONS works on Mac but I have no feedback about it.
Once SCONS is installed:
1/ Download the source files of pimaim2XSF from github. Do not use any other (=older) versions.
2/ Unzip (or untar) it in a folder we will later call /home/levesque/MYFOLDER
3/ Open a terminal in this folder: $ cd /home/levesque/MYFOLDER
4/ $ scons
5/ ./pimaim2xsf

Use
===

A file named positions.out is needed. This file is an output of pimaim

Simply answer the questions in the terminal
1/ How many ionic species ? (for instance 3 for Li,F,Be)
2/ Symbol of species 1 (for instance F)
3/ How many of species 1 in the supercell?
Same for other species.
To visualize the output file with VMD, type:
$ vmd -xyz positions.xyz

Output Files
============

pimaim2XSF output a single file. It is a movie of all steps found in positions.out.
The format is XYZ, which can be surprising considering the name of the executable.

Why not XSF?
Because to my knownledge XSF format does not support movies. Or it is difficult to read it with VMD.

Warranty
========

This program comes with no warranty.
Please send me bug reports or ideas of improvements. If you have in mind a file format that would contain periodicity informations,
I'm eager to hear from you.
You're very welcome to improve or debug the program by fork and pull to my github.

Licence
=======

You're not allowed to distribute this code without my explicit agreement. Email me.
