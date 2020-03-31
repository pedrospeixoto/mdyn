# mdyn
Mobile dynamics

Scripts to analyse mobile data and model population dynamics

Requires:
- Python
- Anaconda/conda enviroment manager

Installation:
- After donwloading or cloning the repository
-- create enviroment with conda using 
$ conda create --name mdyn --file spec-file.txt 
-- load enviroment
# conda activate mdyn
-- Create e link to the dataset
$ ln -s /tmp/inloco/data/ data

Running example:
$ python mdyn_run.py -f mdyn_params_SP_mesoreg.txt -o 0
-- pip install missing packages
$ pip install pkg_name
-- create/edit parameter files mdyn_param_xxxx.py for different runs
