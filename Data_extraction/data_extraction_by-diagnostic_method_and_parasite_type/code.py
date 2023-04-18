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

parasites=[]
with open("/Users/hawraal-ghafli/Desktop/sys.review/parasite_type.csv", "r") as f:
    reader = csv.reader(f, delimiter= "\t" )
    for row in reader:
       parasites= [row for row in f] 
parasites2 = [x.replace('\n', '') for x in parasites]        


parasitesm=[]
with open("/Users/hawraal-ghafli/Desktop/sys.review/parasitem.csv", "r") as d:
    reader = csv.reader(d, delimiter= "\t" )
    for row in reader:
       parasitesm= [row for row in d] 
parasitesm2 = [x.replace('\n', '') for x in parasitesm]        

dignostic=[]
with open("/Users/hawraal-ghafli/Desktop/sys.review/dignostic_tests.csv", "r") as h:
    reader = csv.reader(h, delimiter= "\t" )
    for row in reader:
       dignostic= [row for row in h] 
dignostic2 = [x.replace('\n', '') for x in dignostic]

dignosticm=[]
with open("/Users/hawraal-ghafli/Desktop/sys.review/dignosticm.csv", "r") as s:
    reader = csv.reader(s, delimiter= "\t" )
    for row in reader:
       dignosticm= [row for row in s] 
dignosticm2 = [x.replace('\n', '') for x in dignosticm]

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
from textblob import TextBlob
from nltk import RegexpParser



process1 = nltk.word_tokenize (str(abstracts2[0]))
process1 = [nltk.word_tokenize(abst) for abst in process1]
process1 = [nltk.pos_tag(abst) for abst in process1]
        

# below is a test_code1 that turns out not ideal
#testing test_code 1 on one study    
gfg1 = TextBlob(str(abstracts2[4]))
gfg1 = gfg1.noun_phrases
print(gfg1)

par=  []
par2=[]
for x in gfg1:
    if x in parasites2:
        par.append(x)
if par != []:
    for x in par:
        par2.append(x)    
if par ==[]:
    par2.append('NA')

print (par)

for x in parasites2:
    ne= re.search(str(x), abstracts2[4])
print (ne)
#testing test_Code_1 for all by writing a function
dig_extracted=[]
def dignostic_finder(text, dignostic_list):
    for x in text:
        gfg = TextBlob(x)
        gfg= gfg.noun_phrases
        par=[]
        for x in gfg:
            if x in dignostic_list:
                par.append(x)
        par2=[]
        for x in par:
            if x not in par2:
                par2.append(x)
        if len(par2)<2:
            for x in par2:
                dig_extracted.append(x)
        if len(par2)>=2:
            dig_extracted.append(par2)
        if par2 == []:
            dig_extracted.append('NA')


      
dignostic_finder(abstracts2[0:1024], dignostic2)                
print (dig_extracted)

#creating functions to help me write test_code_2 approach (parasite_extraction and digno_extraction)
def statment_print(text):
    for y in text:
        print ("        ne" + str(text.index(y))+ "=" + "re.findall(" + "'" + y + "'" +  ",X)")

#creating functions to help me write the functions (parasite_extraction and digno_extraction)

def st_print_2(text):
    for y in text:
        print ("ne"+str(text.index(y)) + "+")

#statment_print(dignosticm2)
# writing functions for test_code2 
parasite_extracted=[]
def parasite_extraction(text):
    for X in text:
        ne0=re.findall('Leishmania infantum',X)
        ne1=re.findall('L. infantum',X)
        ne2=re.findall('Leishmania spp.',X)
        ne3=re.findall('L. tropica',X)
        ne4=re.findall('L. mexicana',X)
        ne5=re.findall('L. denovani',X)
        ne6=re.findall('L. aethiopica',X)
        ne7=re.findall('L. waltoni',X)
        ne8=re.findall('L. amazonesis',X)
        ne9=re.findall('L. major',X)
        ne10=re.findall('Leishmania major',X)
        ne11=re.findall('Leishmania  amazonesis',X)
        ne12=re.findall('Leishmania denovani',X)
        ne13=re.findall('Leishmania tropica',X)
        ne14=re.findall('Leishmania waltoni',X)
        ne15=re.findall('Leishmania aethiopica',X)
        ne16=re.findall('Leishmania brazilliensis',X)
        ne17=re.findall('L. brazilliensis',X)
        ne18=re.findall('Leishmania peruviana',X)
        ne19=re.findall('Leishmania guyanesis',X)
        ne20=re.findall('Leishmania panamensis',X)
        ne21=re.findall('Leishmania shawi',X)
        ne22=re.findall('Leishmania lindenberrgi',X)
        ne23=re.findall('Leishmania naiffi',X)
        ne24=re.findall('L. peruviana',X)
        ne25=re.findall('L. guyanesis',X)
        ne26=re.findall('L. panamensis',X)
        ne27=re.findall('L. shawi',X)
        ne28=re.findall('L. lindenberrgi',X)
        ne29=re.findall('L. naiffi',X)
        ne30=re.findall('T. evansi',X)
        ne31=re.findall('T. equperdum',X)
        ne32=re.findall('T. brucei brucei',X)
        ne33=re.findall('T.b. rhodensiense',X)
        ne34=re.findall('T.b. gambiense',X)
        ne35=re.findall('T. brucei',X)
        ne36=re.findall('T. suis',X)
        ne37=re.findall('T. congolense',X)
        ne38=re.findall('T. godfreyi',X)
        ne39=re.findall('T. vivax',X)
        ne40=re.findall('T. cruzi',X)
        ne41=re.findall('Trypanosoma congolense',X)
        ne42=re.findall('Trypanosoma evansi',X)
        ne43=re.findall('Trypanosoma vivax',X)
        ne44=re.findall('Trypanosoma godfreyi',X)
        ne45=re.findall('Trypanosoma brucei',X)
        ne46=re.findall('Trypanosoma brucei gambiense',X)
        ne47=re.findall('Trypanosoma brucei rhodensiense',X)
        ne48=re.findall('Trypanosoma brucei brucei',X)
        ne49=re.findall('Trypansoma suis',X)
        ne50=re.findall('Trypansoma cruzi',X)
        ne51=re.findall('bovine trypanosomosis',X)
        ne52=re.findall('C. bombi ',X)
        ne53=re.findall('Crithidia bombi',X)
        ne54=re.findall('Crithidia mellificae',X)
        ne55=re.findall('Lotmaria passim',X)
        ne56=re.findall('L. passim',X)
        ne57=re.findall('C. mellificae',X)
        ne58=re.findall('Leptotmonas',X)
        ne59=re.findall('Crithidia fasciculata.',X)
        ne60=re.findall('C. fasciculata.',X)
        ne61=re.findall('Trypanosomosis',X)
        ne62=re.findall('L. turanica',X)
        ne63=re.findall('Leishmania turanica',X)
        ne64=re.findall('T. simiae',X)
        ne65=re.findall('Trypanosoma simiae',X)
        ne66=re.findall('bovine',X)
        ne67=re.findall('Leishmania',X)
        ne68=re.findall('Trypanosoma',X)

        nall= ne0+ne1+ne2+ne3+ne4+ne5+ne6+ne7+ne8+ne9+ne10+\
              ne11+ne12+ne13+ne14+ne15+ne16+ne17+ne18+ne19+\
              ne20+ne21+ne22+ne23+ne24+ne25+ne26+ne27+ne28+ne29+\
              ne30+ne31+ne32+ne33+ne34+ne35+ne36+ne37+ne38+ne39+\
              ne40+ne41+ne42+ne43+ne44+ne45+ne46+ne47+ne48+ne49+\
              ne50+ne51+ne52+ne53+ne54+ne55+ne56+ne57+ne58+ne59+\
              ne60+ne61+ne62+ne63+ne64+ne65+ne66+ne67+ne68
        par=[]
        for x in nall:
            if x not in par:
                par.append(x)

        if len(par)<2:
            for x in par:
                parasite_extracted.append(x)
        if len(par)>=2:
            parasite_extracted.append(par)
        if par == []:
            parasite_extracted.append('NA')
        
parasite_extraction(abstracts2[0:1024])                
print (parasite_extracted)      
    
digno_extracted=[]
def digno_extract(text):
    for X in text:
        ne0=re.findall('buffy coat ',X)
        ne1=re.findall('direct agglutination test',X)
        ne2=re.findall('(DAT)',X)
        ne3=re.findall('enzyme-linked immunosorbent assay',X)
        ne4=re.findall('(ELISA)',X)
        ne5=re.findall('microscopic',X)
        ne6=re.findall('discrete typing units',X)
        ne7=re.findall('DTUs',X)
        ne8=re.findall('microscope',X)
        ne9=re.findall('PCR-RFLP',X)
        ne10=re.findall('qPCR',X)
        ne11=re.findall('indirect fluorescent antibody test',X)
        ne12=re.findall('(IFAT)',X)
        ne13=re.findall('particle gel immuno-assay',X)
        ne14=re.findall('(PaGIA) ',X)
        ne15=re.findall('modified agglutination test',X)
        ne16=re.findall('(MAT)',X)
        ne17=re.findall('ITS1-PCR-RFLP',X)
        ne18=re.findall('PCR-RFLP.',X)
        ne19=re.findall('nested PCR',X)
        ne20=re.findall('smears',X)
        ne21=re.findall('cultures',X)
        ne22=re.findall('blood film',X)
        ne23=re.findall('western blot',X)
        ne24=re.findall('western blotting',X)
        ne25=re.findall('Sabin-Feldman dye test',X)
        ne26=re.findall('SFDT',X)
        ne27=re.findall('card agglutination test ',X)
        ne28=re.findall('(CATT)',X)
        ne29=re.findall('AB-ELISA',X)
        ne30=re.findall('immunochomatography',X)
        ne31=re.findall('RT-PCR',X)
        ne32=re.findall('qRT-PCR',X)
        ne33=re.findall('(LAMP)',X)
        ne34=re.findall('Loop-mediated isothermal amplification',X)
        ne35=re.findall('xenodiagnosis',X)
        ne36=re.findall('(IFA)',X)
        ne37=re.findall('parasitological culture',X)
        ne38=re.findall('polymerase chain reaction',X)
        ne39=re.findall('microscopy',X)
        ne40=re.findall('immunoenzymatic assay',X)
        ne41=re.findall('(IEA)',X)
        ne42=re.findall('rapid immunochromatographic assay',X)
        ne43=re.findall('dual path platform',X)
        ne44=re.findall('(DPP)',X)
        ne45=re.findall('PCR',X)
        ne46=re.findall('(BCT)',X)
        ne47=re.findall('microscopic dignosis',X)
        ne48=re.findall('Giemsa-stained',X)
        ne49=re.findall('superoxide dismutase-enzyme-linked immunosorbent assay',X)
        ne50=re.findall('(SOD-ELISA)',X)
        ne51=re.findall('CATT/T.evansi',X)
        ne52=re.findall('RoTat 1.2 immune trypanolysis',X)
        ne53=re.findall('ITS1',X)
        ne54=re.findall('ITS2',X)
        ne55=re.findall('microhaematocrit centrifugation technique',X)
        ne56=re.findall('MHCT',X)
        ne57=re.findall('Giemsa stained',X)
        ne58=re.findall('sequencing',X)
        ne58=re.findall('chromatographic immunoassay', X)
        nall= ne0+ne1+ne2+ne3+ne4+ne5+ne6+ne7+ne8+ne9+ne10+\
              ne11+ne12+ne13+ne14+ne15+ne16+ne17+ne18+ne19+\
              ne20+ne21+ne22+ne23+ne24+ne25+ne26+ne27+ne28+ne29+\
              ne30+ne31+ne32+ne33+ne34+ne35+ne36+ne37+ne38+ne39+\
              ne40+ne41+ne42+ne43+ne44+ne45+ne46+ne47+ne48+ne49+\
              ne50+ne51+ne52+ne53+ne54+ne55+ne56+ne57+ne58
        par=[]
        for x in nall:
            if x not in par:
                par.append(x)

        if len(par)<2:
            for x in par:
                digno_extracted.append([x])
        if len(par)>=2:
            digno_extracted.append(par)
        if par == []:
            digno_extracted.append(['NA'])

digno_extract(abstracts2[106:1024])                
print (digno_extracted) 
# i know there is a nother why with re package but this is working as well , I can work with the other way another time. 
import csv

# export list of extracted parasites/dignostic as columns

import xlrd
from xlrd import open_workbook
import xlwt

book = xlwt.Workbook('output.parasites.xls')
sheet1 = book.add_sheet('sheet1')
for i,e in enumerate(parasite_extracted):
    sheet1.write(i,1,e)
name = "extratced_parasites.xls"
book.save(name)


book = xlwt.Workbook('output.dignostic.xls')
sheet1 = book.add_sheet('sheet1')
for i,e in enumerate(digno_extracted):
    sheet1.write(i,0,e)
name = "extratced_dignostic_test.xls"
book.save(name)



#Exporting: Assuming res is a list of lists
with open("/Users/hawraal-ghafli/Desktop/sys.review/working codes/extratced_dignostic_test2.csv", "w") as output:
   writer = csv.writer(output, lineterminator='\t')
   for x in digno_extracted:
       writer.writerow(x)
        
with open("/Users/hawraal-ghafli/Desktop/sys.review/working codes/extratced_dignostic_test3.csv", "w") as output:
   writer = csv.writer(output, lineterminator='\t')
   writer.writerows(digno_extracted)
           
