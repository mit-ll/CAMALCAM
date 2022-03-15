# Overview

## Stardust-Chicken
This is an open research platform to tie together TCAM and string matching research into an extensible algorithmic research algorithm reference.
The primary use case is to generate state machines for string matching algorithms and to translate those state machines into TCAM based state machines.
This enables researchers developing hardware implementations to develop test suites to measure performance improvements from a stable baseline.
This also enables the development and testing of algorithms that tie into hardware developers' processes cleanly. 


### Example usage

#### building project
dotnet build -c Release

#### Getting help 
dotnet run

#### Generate a 4-Variable TCAM table from a list of strings
dotnet .\bin\Release\net6.0\Stardust-Chicken.dll gen_tcam_tables 4 .\dataset-Nov-darpa98\top.10.txt -f .\top.10.tcam.csv

#### Simulate a 4-Variable TCAM table from a list of strings on a file.
dotnet .\bin\Release\net6.0\Stardust-Chicken.dll print tcam simulate .\dataset-Nov-darpa98\top.10.txt 4 .\darpa-1998-pcap\00.pcap

# Citation

Please use this DOI number reference, published on Zenodo, when citing the software:

DOI: TODO once released to github

# Disclaimer
DISTRIBUTION STATEMENT A. Approved for public release. Distribution is unlimited.

This material is based upon work supported by the Under Secretary of Defense for
Research and Engineering under Air Force Contract No. FA8702-15-D-0001. Any
opinions, findings, conclusions or recommendations expressed in this material
are those of the author(s) and do not necessarily reflect the views of the Under
Secretary of Defense for Research and Engineering.

© 2022 Massachusetts Institute of Technology.

The software/firmware is provided to you on an As-Is basis

Delivered to the U.S. Government with Unlimited Rights, as defined in DFARS Part
252.227-7013 or 7014 (Feb 2014). Notwithstanding any copyright notice, U.S.
Government rights in this work are defined by DFARS 252.227-7013 or DFARS
252.227-7014 as detailed above. Use of this work other than as specifically
authorized by the U.S. Government may violate any copyrights that exist in this
work.

