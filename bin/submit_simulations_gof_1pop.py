#!/usr/bin/python
import sys
import os
import time

if len(sys.argv) != 12:
	print("\n\tsubmit_simulations_gof_1pop.py [outgroup] [nmultilocus] [iteration] [model: Constant_x; Discrete_x; Expo_x] [nameA] [sub_dir_sim] [sub_dir_model] [config_yaml] [project's directory name, i.e, timeStamp] [modePrior in [joint; disjoint; randomBeta] [binpath]")
	print("\n\tex: submit_simulations_gof_1pop.py 1 1000 2 Constant_1N flo sim_Constant_1N Constant_1N posterior_Constant_1N.txt Ng4PymB1dy randomBeta\n\tto simulate 1000 multilocus simulations at the second iteration, in the folder sim_Constant_1N, with outgroup") 
	sys.exit(0)

outgroup = int(sys.argv[1])
nmultilocus = int(sys.argv[2]) # 10000
iteration = int(sys.argv[3]) # 2
model = sys.argv[4]
nameA = sys.argv[5] # name of the species A
sub_dir_sim = sys.argv[6] # name of the subdir where the simulations will be run
sub_dir_model = sys.argv[7] # name of the sub_sub_dir containing ABCstat.txt
posterior_file = sys.argv[8]
timeStamp = sys.argv[9]
modePrior = sys.argv[10]
binpath = sys.argv[11]

print(model)
path = os.getcwd() + '/{0}'.format(timeStamp)

test_bpfile = os.path.isfile('{0}/bpfile'.format(path))
if test_bpfile == False:
	sys.exit('\n\tERROR in submit_simulations_1pop.py : the file {0}/bpfile is not found\n'.format(path))
else:
	infile = open('{0}/bpfile'.format(path), 'r')
	tmp = infile.readline()
	tmp = infile.readline().strip().split('\t')
	nlocus = len(tmp)
	infile.close()

mscommand = ""
if "Constant" in model:
	mscommand = "-t tbs -r tbs tbs"
if "Discrete" in model:
	mscommand = "-t tbs -r tbs tbs -eN tbs tbs"
if "Expo" in model:
	mscommand = "-t tbs -r tbs tbs -G tbs -eG tbs 0.0 -eN tbs tbs"
if "Contraction" in model:
	mscommand = "-t tbs -r tbs tbs -eN tbs tbs"
if "Expansion" in model:
	mscommand = "-t tbs -r tbs tbs -eN tbs tbs"

if mscommand == "":
	print("You specified a wrong model: Constant_x, Discrete_x or Expo_x\n")
	sys.exit()

#tmp = "mkdir {0}/{1}; ".format(path, sub_dir_sim)
#tmp += "mkdir {0}/{1}/{2}_{3}; ".format(path, sub_dir_sim, sub_dir_model, iteration)
tmp = "cp {0}/bpfile {0}/{1}/{2}_{3}/; ".format(path, sub_dir_sim, sub_dir_model, iteration)
tmp += "cd {0}/{1}/{2}_{3}; ".format(path, sub_dir_sim, sub_dir_model, iteration)

tmp += "{7}/priorgen_gof_1pop.py {0} {1} {2} {6} | {7}/msnsam tbs {3} {4} | {7}/mscalc_1pop_SFS.py {5}".format(model, nmultilocus, posterior_file, nmultilocus*nlocus, mscommand, outgroup, modePrior, binpath)
print(tmp)
os.system(tmp) # to submit the job using slurm

