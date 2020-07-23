#!/usr/bin/python
import sys
import os

timeStamp = sys.argv[1] # timeStamp of the analysis (for instance: byWQxjtdnp)
mail_address = sys.argv[2] # address to which the DILS results are send (for instance: user@gmail.com)
binpath = sys.argv[3] # directory where all DILS binaries are located
send_mail = sys.argv[4] # True: send mail; False: do not send mail

def mail(timeStamp, mail_address, binpath):
	commande = 'mailx -a {0}.tar.gz -s "DILS analysis {0} completed" {1} < {2}/message.txt'.format(timeStamp, mail_address, binpath)
	os.system(commande)
	return(0)

if send_mail == True:
	mail(timeStamp, mail_address, binpath)

