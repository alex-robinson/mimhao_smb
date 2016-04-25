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

---------------------------------------------

# Initial local repository setup via github
1. Clone the repository to your local computer.
2. Create a branch to work on.
3. Push your local branch to repository.

```
git clone https://github.com/alex-robinson/mimhao_smb.git
git checkout -b your-branch
git push -u origin your-branch
```

You're now ready to work locally, `add` changes, `commit` them to the
local repository, and `push` them back to the central repository.
