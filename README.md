# mimhao_smb
This repository contains the code to calculate tables
of ice-shelf mass balances for Antarctica

Input data is located in `data`

The calculations are performed in R, first
by loading the relevant datasets and then
by performing calculations:
```
source("load_data.r")
source("calculations.r")
```
Running these commands will process the input data, prepare various masks,
calculate quantities of interest and output some data tables and plots.

-------------------------------------------------------------------------------------------------

> Help for work with github:

	git clone https://github.com/alex-robinson/mimhao_smb.git

or with ssh

	git clone git@github.com/alex-robinson/mimhao_smb.git

> Update local repository (inside mimhao_smb)

	git fetch origin

> Commands for update modifications in local repository to github (exist more variants, per file...)

	git add .
	or
	gid add <FILENAME>

	git commit -m ""

	git push origin master
	or
	git push git@github.com:alex-robinson/mimhao_smb.git
