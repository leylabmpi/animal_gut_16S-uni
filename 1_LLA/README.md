# Description 

Running Ley Lab Amplicon pipeline (LLA) on Georg Animal Microbiome dataset.

This pipeline is just a snakemake wrapper for running QIIME2 commands.
The pipeline consists of:

* Sequence variant (SV) construction with DADA2
* Classification of SVs with SILVA (or GreenGenes)
* Creating a 16S phylogeny for all SVs

