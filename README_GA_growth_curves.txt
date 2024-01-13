This folder contains the plate reader data used to plot growth of strains expressing sfGFP under the control of YTK promoters. 

Every '.xlsx' file contiains the raw data, with the timepoint of each reading indicated in the file name. 
The layout of the 96-well plate can be found in the '...layout.csv' file. Within this file, the promoter for each strain is indicated below:

897 = pCCW12
898 = pPGK1
899 = pHHF2
902 = pHHF1
903 = pHTB2
905 = pALD6
908 = pRNR1
914 = pREV1
BP = wildtype with pHLUM minichromosome (-ve control)

sm1, sm3 and sm4 were treated with alpha-factor.

In the platecurver script, the 'plater' package is used to match sample names in the layout.csv file and OD600 readings in the .xlsx files. Then a series of functions are used to plot the blank-corrected values. 

'tidy_new.csv' contains the non blank-corrected OD600 readings for all of the samples at different time points. It can be generated from the platecurver script.

'230323_raw.svg' contains all the growth curves. 



