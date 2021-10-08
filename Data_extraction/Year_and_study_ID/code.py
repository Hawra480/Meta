import sys
sys.path.append('/Users/hawraal-ghafli/anaconda3/lib/python3.7/site-packages')
import pandas as pd
import re

mydata = pd.read_csv("/Users/hawraal-ghafli/Desktop/sys.review/PE_extract draft.csv")
yeares=pd.read_csv("/Users/hawraal-ghafli/Desktop/sys.review/Book12.csv")

Titles=mydata['title']
Abstract=mydata['abstract']
Year=mydata['year']
Authors=mydata['authors']
Tilte2=yeares['title']
Tilte2= [x.replace('\xa0 ', '') for x in Tilte2]        
ye=yeares['year']
ye=[str(x).replace('\xa0', '') for x in ye]
h=[]
j=[]
def fe_year(l1,l2):
    for x in l1:
        if x in l2:
            if x not in h:
                index1=l2.index(x)
                h.append(ye[index1])
        if x not in l2:
            h.append('Na')
            
    if len(h)>=2:
        j.append(h)
    if len(h)<2:
        for x in j:
            j.append(x)

h1=[]
def fe_year1(l1,l2):
        if l1 in l2:
            if l1 not in h:
                index1=l2.index(l1)
                h.append(ye[index1])
        if l1 not in l2:
            h1.append('Na')
            
    
            
fe_year(Titles,Tilte2)

uyears=pd.DataFrame( h)

uyears.to_csv(r'/Users/hawraal-ghafli/Desktop/sys.review/working codes/updated_year.csv', index = False)

authors = pd.read_csv("/Users/hawraal-ghafli/Desktop/sys.review/authors_and_years.csv")

year_author=authors['Year']
Full_author=authors['Authors'].values.tolist()
First_author=authors['First']

r=[]
r1=[]
def study_ID(fullauthors, year, f_author ):
    for x in fullauthors:
        index2=fullauthors.index(x)
        r=re.findall('and', x)
        if len(r)>1:
            r1.append("("+ f_author[index2] + " et al., "+ year[index2]+")")
        if len(r)==1:
            keyword= 'and'
            before_keyword, keyword, after_keyword = fullauthors[index2].partition(keyword)
            result = re.findall(r'^[^,]+(?=,)', after_keyword)
            r1.append("("+ f_author[index2] + " &"+ str(result[0]) + ", " + str(year[index2])+")")
        if len(r)==0:
            r1.append("("+ f_author[index2] + ", " + year[index2]+")")
        
             
study_ID(Full_author, year_author, First_author)
print(r1[0:10])

study_id=pd.DataFrame(r1)
study_id.to_csv(r'/Users/hawraal-ghafli/Desktop/sys.review/working codes/updated_study_id.csv', index = False)
