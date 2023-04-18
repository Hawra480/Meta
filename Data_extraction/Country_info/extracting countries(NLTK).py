from __future__ import division
import sys 
sys.path.append('/Users/hawraal-ghafli/anaconda3/lib/python3.7/site-packages')
import csv
from csv import writer
from csv import reader
from csv import DictReader
from csv import DictWriter
from geograpy import extraction
import nltk
from montylingua import MontyExtractor
import montylingua
import pprint
import re

input_file = open ("/Users/hawraal-ghafli/Desktop/sys.review/abstracts.csv", 'r')
output_file = open ("/Users/hawraal-ghafli/Desktop/sys.review/output.countries.csv", 'w')
            
abstractsh=[]
with open ("/Users/hawraal-ghafli/Desktop/sys.review/abstracts.csv") as file1:
    reader = csv.reader(file1, delimiter="\t")
    for row in reader:
        abstractsh= [row for row in file1] 
print (abstractsh[0])
# since no package could do what we whant excatly, I ended up copaming the list of chunked words from each study with the below list of countries. 
# that means if there is a match in items -it will be added to a sapert list. 
countries=[]
with open("/Users/hawraal-ghafli/Desktop/sys.review/countries.csv", "r") as f:
    reader = csv.reader(f, delimiter= "\t" )
    for row in reader:
       countries= [row for row in f] 
countries2 = [x.replace('\n', '') for x in countries]        
print (countries2.index("Iran"))

personsh=[]
with open("/Users/hawraal-ghafli/Desktop/sys.review/na_cou.csv", "r") as n:
    reader = csv.reader(n, delimiter= "\t" )
    for colom in reader:
       personsh= [colom for colom in n] 
print(personsh[7])
personsh2 = [x.replace('\n', '') for x in personsh]

countries_up=[]
with open("/Users/hawraal-ghafli/Desktop/sys.review/country_list.csv", "r") as g:
    reader = csv.reader(g, delimiter= "\t" )
    for colom in reader:
       countries_up= [colom for colom in g] 
print(countries_up[7])


#testing the code in a single study first 
from geograpy import extraction
e = extraction.Extractor (text = abstractsh[3])
e.find_entities()
print (e.places)
nf = [x for x in countries2 if x in e.places]
print (nf)

# it worked, so now I will make a function. 

# now trying to do it for all studies in a list
countires_extratced=[]
def country_extractor(text, country):
    for i in text:
        e = extraction.Extractor (i)
        e.find_entities()
        e.places
        ne=[x for x in e.places if x in country] 
        he=[]
        # to get rid of duplications in idenitifed countries (sometimes a country is mentioned twice)
        for x in ne:
            if x not in he:
                he.append(x)
        # if he has only one item , add it to final list of countires_extratced
        if len(he)<2:
                    for x in he:
                        countires_extratced.append(x)
        # if he has more than one item , add them as an inner list to the final list of countires_extratced so that when exported we know its from the same study
        if len(he)>=2:
            countires_extratced.append(he)                  
        if he == []:
            countires_extratced.append('NA')
                        
  
country_extractor (abstractsh[0:1023], countries2)

print (len(countires_extratced))

import csv

# export list of extracted countries as column

import xlrd
from xlrd import open_workbook
import xlwt

book = xlwt.Workbook('output.countries1.xls')
sheet1 = book.add_sheet('sheet1')
for i,e in enumerate(countires_extratced):
    sheet1.write(i,1,e)
name = "random.xls"
book.save(name)
# extracted countries are correct (as planned) but the export method is not optimum (errors in separating items in a list to diff cells:must be corrected manually).
# I should look for another approach to export list as colum in excel.
    

