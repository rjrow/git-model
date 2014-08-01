from rpy2.robjects.packages import SignatureTranslatedAnonymousPackage as STAP
from rpy2.robjects.functions import SignatureTranslatedFunction as STF
import rpy2.robjects as robjects
import rpy2.rinterface as ri
from pprint import pprint
import os
import sys
import json
import subprocess
from os import path, getcwd, chdir


def main():
	""" Main function to be ran, all subsequent calls are for modeling and data manipulation and transfer/storage.
	Initially we take in an input from Synapse, this input will dictate everything about the model run """

	dir = getcwd()



	# Standard JSON setup to simulate model input
	json = {"name": "p_techProgFac",
	"slots" : {
		"r" : "ROW",
		"j" : "gas",
		"f" : "",
		"s" : "",
		"i" : ""
		},
	"value" : "1.2"
	}

	
	data = json
	# pprint(data)
	# json.close()

	json_parameter = data['name']
	json_value     = data['value']
	json_slots     = data['slots']
	json_slots = dict((k, v) for k, v in json_slots.iteritems() if v)
	json_slots_list = []
	for key, value in json_slots.iteritems():
		temp = [key,value]
		json_slots_list.append(temp)
	print json_parameter, json_value, json_slots_list


	""" Here we need to parse the data structure Synapse gives Insert dummy data instead """
	# Add in slot values, parameters need to be generated into a list to pass to be written	
	file_name = robjects.StrVector("main_data.gdx")
	slots = json_slots_list
	value = robjects.FloatVector((json_value,0)) # the zero represents an unfortunate hack, serves no purpose but circumvention
	prm_name = robjects.StrVector(json_parameter)


	#main_filename = os.path.join(dir, '/CGE/main.R')

	main_filename     	 = dir + '/CGE/main.R'
	parse_db_filename 	 = dir + '/CGE/databasing.R'
	regional_io_filename = dir + '/CGE/regional_io.R'



	with open(main_filename, 'r') as f:
		string_main_cge = ''.join(f.readlines())


	""" Here we are going to load in functions to send out data to DB """
	with open(parse_db_filename, 'r') as f:
		string_db = ''.join(f.readlines())	


	""" Here we switch to regional modeling, this is conditonal upon successful CGE run """	
	with open(regional_io_filename, 'r') as f:
		string_regional = ''.join(f.readlines())


	cge_functions		 = STAP(string_main_cge, "cge_functions")
	db_functions		 = STAP(string_db, "db_functions")
	regional_functions   = STAP(string_regional, "regioanl_functions")
	

	#####  perturb_data (file.name, slots, value, prm.name) #####
	# This function takes in particular slots, value, and file name of gdx to read in and outputs
	# "main_data_out.gdx" which then is fed into model run

	#####  run()  #### 
	# This function runs the main cge model "mrtmcp.gms" which in turn runs a report.gms
	# which outputs a shock_out_prod.gdx file which is used subsequently. It contains the result of the 
	# model run from the perturbed data

	# regional_functions = STAP(string_regional, "regional_functions")

	#####  regional_main_run()  ######
	# This function runs the mexico regional model it in turn runs empl_change()
	# which is internal for regional_main_run() and calculates the percentages changes
	# in the cge model and uses that to calculate regional allocation
	

if __name__ == "__main__":
		main()
