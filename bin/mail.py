#!/usr/bin/python
import sys
import os
from os import path

timeStamp = sys.argv[1] # timeStamp of the analysis (for instance: byWQxjtdnp)
mail_address = sys.argv[2] # address to which the DILS results are send (for instance: user@gmail.com)
binpath = sys.argv[3] # directory where all DILS binaries are located
datapath = sys.argv[4] # directory where all DILS analysis are performed
send_mail = str(sys.argv[5]) # True: send mail; False: do not send mail
statu = str(sys.argv[6]) # 'start' or 'end'

def mail(timeStamp, mail_address, binpath, statu):
	if statu == 'end':
		results_file = '{0}/{1}/{1}.tar.gz'.format(datapath, timeStamp)
		test_file = path.isfile(results_file)
		if test_file == True: # if the DILS analysis produced the final tar.gz
			commande = 'mailx -a {0}/{1}/{1}.tar.gz -s "DILS analysis {1} completed" {2} < {3}/message_end.txt'.format(datapath, timeStamp, mail_address, binpath)
		else: # if something happened to this analysis
			commande = 'mailx -s "DILS analysis {1} is crashed" {2} < {3}/message_crash.txt'.format(timeStamp, mail_address, binpath)

	else:
		commande = 'mailx -a {0}/{1}/{1}.yaml -s "DILS analysis {1} is started" {2} < {3}/message_start.txt'.format(datapath, timeStamp, mail_address, binpath)
	os.system(commande)
	return(0)

if send_mail == 'True':
	mail(timeStamp, mail_address, binpath, statu)

