#!/usr/bin/python

'''
 - Filtrando os dados para obter apenas 1 mes de cada casa da base de dados REFIT
 - Apenas um mes de 2015 (2015-03)
 - Usar:
	$ python filter_data_1month.py
'''

import sys

def main():
	print 'Filtering data ... \n'

	for x in range(1,22):
		if x != 14:
			inputFile = 'House_' + str(x) + '.csv'
			outputFile = 'house_' + str(x) + '_month.csv'

			outFile = open(outputFile, 'w') # Output file
			firstLine = 'Time,Unix,Aggregate,Appliance1,Appliance2,Appliance3,Appliance4,Appliance5,Appliance6,Appliance7,Appliance8,Appliance9\n'
			outFile.write(firstLine)
			with open(inputFile, "r") as arq:
			    for line in arq:
				if line.find('2015-03-') > -1:
				    outFile.write(line)
			outFile.close()
			print outputFile

	print '\nDone.'


main()


