# mdyn
Mobile dynamics

Scripts to analyse mobile data and model population dynamics

Main developers:
- Pedro Peixoto (ppeixoto@usp.br)
- Diego Marcondes (dmarcondes@ime.usp.br)

DISCLAIMER: This study is under constant change, therefore algorithms and data have frequently changing and being improved. Please contact the authors before using any information available here.

Requires:
- Python
- Anaconda/conda enviroment manager
- R

Installation:
- After donwloading or cloning the repository
- create enviroment with conda using 
> conda create --name mdyn --file spec-file.txt 
- load enviroment
> conda activate mdyn
- Create e link to the dataset
> ln -s /tmp/inloco/data/ data

Running example:
> python mdyn_run.py -f mdyn_params_SP_mesoreg.txt -o 0
- pip install missing packages
> pip install pkg_name
- create/edit parameter files mdyn_param_xxxx.py for different runs
