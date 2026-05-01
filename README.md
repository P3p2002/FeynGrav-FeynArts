# FeynGrav-FeynArts
The file labelled "FG-to-FA" generates two documents, which will be the models that should be inserted into FeynArts.

The two files are called "Generic_Couplings_QG.txt" and "Coupling_Matrices.txt". Once they are created, one should follow the route in their computer:
"C:\User\AppData\Roaming\Wolfram\Applications\FeynArts\Models"
In such directory one needs to create two files, with extensions ".gen" and ".mod". The contents of the file "Generic_Couplings_QG.txt" should be copy pasted to the ".gen" file, while the contents of the other file should be copy pasted to the ".mod" file.

This will generate the Feynman rules in FeynArts for some given number of gravitons and scalars (which can be made distinguishable if one needs to). 

If one wishes to include Feynman rules for more than one process, i.e., the Feynman Rules with 2 gravitons and with 1 and 2 scalars, then she/he/they will need to run the "FG-to-FA" for each case, and keep adding the terms. One example of ".gen" and ".mod" files are given and named "QG.gen" and "QG.mod". For more information on how these works, the reader is encouraged to look at the manual of FeynArts.

The interested reader can also find uploaded a very short example.
