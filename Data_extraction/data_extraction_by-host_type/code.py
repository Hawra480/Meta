from __future__ import division
import sys 
sys.path.append('/Users/hawraal-ghafli/anaconda3/lib/python3.7/site-packages')
import csv
from csv import writer
from csv import reader
from csv import DictReader
from csv import DictWriter

input_file = open ("/Users/hawraal-ghafli/Desktop/sys.review/abstracts.csv", 'r')
output_file = open ("/Users/hawraal-ghafli/Desktop/sys.review/output.abstracts.csv", 'w')
            
abstracts=[]
with open ("/Users/hawraal-ghafli/Desktop/sys.review/abstracts.csv") as file1:
    reader = csv.reader(file1, delimiter="\t")
    for row in reader:
        abstracts= [row for row in file1] 
print (abstracts[0])
abstracts2 = [x.replace('\n', '') for x in abstracts]


hosts=[]
with open("/Users/hawraal-ghafli/Desktop/sys.review/hosts.csv", "r") as h:
    reader = csv.reader(h, delimiter= "\t" )
    for row in reader:
       hosts= [row for row in h] 
hosts1=  [x.replace('canine', '') for x in hosts]
hosts2 = [x.replace('\n', '') for x in hosts1]
print (hosts2.index("dogs"))

import nltk
from montylingua import MontyExtractor
import montylingua
import pprint
import re
from nltk import RegexpParser
from nltk.corpus import conll2000
from nltk.corpus import conll2002
from nltk.corpus import stopwords
from nltk import word_tokenize

#this is to test the code for one study
#cleaning the text from font styles and pancutation 
no_panc = re.sub('[!#?,.:";]', '', abstracts2[0])
print(no_panc)
# default sentence segmenter  
process11 = nltk.word_tokenize (str(no_panc))
# tokenizing unstructred test 
process11 = [nltk.word_tokenize(abst) for abst in process11]
# part of speach tagging 
process11 = [nltk.pos_tag(abst) for abst in process11]
print (process11[1])
#trying to idenitify the level of inner lists 
example=[('Visceral', 'JJ')]
example1= ('Visceral', 'JJ')
#testing the extraction
print (example1[0])
hostsm=[]
for x in process11:
    for i in x:
        for h in i:
            if h in hosts2:
                hostsm.append(h)     
print(hostsm)        

#writing the function to extract
hosts_extratced= []
def host_extractor(text, host):
    for x in text:
        no_panc = re.sub('[!#?,.:";]', '', x)
        process1 = nltk.word_tokenize (str(no_panc))
        process1 = [nltk.word_tokenize(abst) for abst in process1]
        process1 = [nltk.pos_tag(abst) for abst in process1]
        ne=[] 
        for x in process1:
            for i in x:
                for h in i:
                    if h in hosts2:
                        ne.append(h)
        he=[]
        for x in ne:
            if x not in he:
                he.append(x)
        # if he has only one item , add it to final list of countires_extratced
        if len(he)<2:
                    for x in he:
                        hosts_extratced.append(x)
        # if [he] has more than one item , add them as an inner list to the final list of countires_extratced
        if len(he)>=2:
            hosts_extratced.append(he)                  
        if he == []:
            hosts_extratced.append('NA')

host_extractor(abstracts[0:1024], hosts2)
print (hosts_extratced)



import csv

# export list of extracted countries as column

import xlrd
from xlrd import open_workbook
import xlwt

book = xlwt.Workbook('output.countries3.xls')
sheet1 = book.add_sheet('sheet1')
for i,e in enumerate(hosts_extratced):
    sheet1.write(i,1,e)
name = "random_hosts.xls"
book.save(name)
# good, its working so far but the export method is not optimum.
# I should look for another approach to export list as colum in excel as the above does not put each item in the listin a list

    

        

