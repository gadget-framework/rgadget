rgadget
=======

Rgadget is a set of useful utilities for gadget, a statistical
multi-species multi-area marine ecosystem modelling toolbox.

This package aids in the developement of Gadget models in a number of
ways. It can interact with Gadget, by manipulating input files, digest
output and rudimentary plots.

Prerequisites
-------------
Gadget obviously, can be obtained from github.com/hafro/gadget

Installing
----------
You can use devtools to install this directly:

    # install.packages("devtools")
    devtools::install_github("bthe/rgadget")

Using
-----
Simple example (more to come):

	# read fit data:
	fit <- gadget.fit()
	plot(fit)

Acknowledgements
----------------

This project has received funding from the European Union’s Seventh Framework
Programme for research, technological development and demonstration under grant
agreement no.613571.
