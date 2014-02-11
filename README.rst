====================================================================
 code and data for "Simulations of delta check rule performance..."
====================================================================

This repository contains all code and data necessary to reproduce our
study investigating the usefulness of delta checks for detecting
specimen mislabeling in the clinical laboratory, published as:

**Simulations of delta check rule performance to detect specimen mislabeling using historical laboratory data.**
Strathmann FG, Baird GS, Hoffman NG.
*Clin Chim Acta.* 2011 Oct 9;412(21-22):1973-7.
doi: http://dx.doi.org/10.1016/j.cca.2011.07.007
PMID: 21782806_

.. _21782806: http://www.ncbi.nlm.nih.gov/pubmed/21782806

Requirements
============

 * R (last tested with version 3.0.2 but developed using 2.13-ish)
 * R packages: ROCR, ggplot2, lattice, multicore, xtable
 * Python 2.7
 * scons

Execution
=========

Assuming all dependencies are installed, it should be possible to
perform the simulation and generate figures, tables, and the
manuscript using the build script (SConstruct) with a single command::

  git clone https://github.com/nhoffman/deltas.git
  cd deltas
  scons
