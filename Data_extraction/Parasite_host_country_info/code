iimport sys
sys.path.append('/Users/hawraal-ghafli/anaconda3/lib/python3.7/site-packages')
import pandas as pd
from geograpy import extraction
import nltk
from montylingua import MontyExtractor
import montylingua
import pprint
import re
from csv import writer
from csv import reader
from csv import DictReader
from csv import DictWriter
import csv
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

# importing data for extraction
mydata = pd.read_csv("/Users/hawraal-ghafli/Desktop/sys.review/PE_extract draft.csv")

Ttiles=mydata['title']
Abstract=mydata['abstract']
Year=mydata['year']
Authors=mydata['authors']
ta = ('' if mydata['title'].empty else mydata['title'].map(str)) + ('' if mydata['abstract'].empty else mydata['abstract'].map(str)) 
tad=pd.DataFrame(ta)
combined=ta.values.tolist()

countries=[]
with open("/Users/hawraal-ghafli/Desktop/sys.review/countries.csv", "r") as f:
    reader = csv.reader(f, delimiter= "\t" )
    for row in reader:
       countries= [row for row in f] 
countries2 = [x.replace('\n', '') for x in countries]        
hosts=[]
with open("/Users/hawraal-ghafli/Desktop/sys.review/hosts.csv", "r") as h:
    reader = csv.reader(h, delimiter= "\t" )
    for row in reader:
       hosts= [row for row in h] 
hosts1=  [x.replace('canine', '') for x in hosts]
hosts2 = [x.replace('\n', '') for x in hosts1]


def statment_print(text):
    for y in text:
        print ("        ne" + str(text.index(y))+ "=" + "re.findall(" + "'" + y + "'" +  ",X)")


def st_print_2(text):
    for y in text:
        print ("ne"+str(text.index(y)) + "+")

#statment_print(dignosticm2)

countries_extracted=[]
def con_extraction(text):
    for X in text:
        ne0=re.findall('Afghanistan',X)
        ne1=re.findall('Albania',X)
        ne2=re.findall('Algeria',X)
        ne3=re.findall('Andorra',X)
        ne4=re.findall('Angola',X)
        ne5=re.findall('Antigua',X)
        ne6=re.findall('Barbuda',X)
        ne7=re.findall('Argentina',X)
        ne8=re.findall('Armenia',X)
        ne9=re.findall('Australia',X)
        ne10=re.findall('Austria',X)
        ne11=re.findall('Azerbaijan',X)
        ne12=re.findall('Bahamas',X)
        ne13=re.findall('Bahrain',X)
        ne14=re.findall('Bangladesh',X)
        ne15=re.findall('Barbados',X)
        ne16=re.findall('Belarus',X)
        ne17=re.findall('Belgium',X)
        ne18=re.findall('Belize',X)
        ne19=re.findall('Benin',X)
        ne20=re.findall('Bhutan',X)
        ne21=re.findall('Bolivia',X)
        ne22=re.findall('Bosnia and Herzegovina',X)
        ne23=re.findall('Botswana',X)
        ne24=re.findall('Brazil',X)
        ne25=re.findall('Brunei',X)
        ne26=re.findall('Bulgaria',X)
        ne27=re.findall('Burkina Faso',X)
        ne28=re.findall('Burundi',X)
        ne29=re.findall('Cabo Verde',X)
        ne30=re.findall('Cambodia',X)
        ne31=re.findall('Cameroon',X)
        ne32=re.findall('Canada',X)
        ne33=re.findall('Central African Republic (CAR)',X)
        ne34=re.findall('Chad',X)
        ne35=re.findall('Chile',X)
        ne36=re.findall('China',X)
        ne37=re.findall('Colombia',X)
        ne38=re.findall('Comoros',X)
        ne39=re.findall('"Congo, Democratic Republic of the"',X)
        ne40=re.findall('"Congo, Republic of the"',X)
        ne41=re.findall('Costa Rica',X)
        ne42=re.findall('Cote d Ivoire',X)
        ne43=re.findall('Croatia',X)
        ne44=re.findall('Cuba',X)
        ne45=re.findall('Cyprus',X)
        ne46=re.findall('Czechia',X)
        ne47=re.findall('Denmark',X)
        ne48=re.findall('Djibouti',X)
        ne49=re.findall('Dominica',X)
        ne50=re.findall('Dominican Republic',X)
        ne51=re.findall('Ecuador',X)
        ne52=re.findall('Egypt',X)
        ne53=re.findall('El Salvador',X)
        ne54=re.findall('Equatorial Guinea',X)
        ne55=re.findall('Eritrea',X)
        ne56=re.findall('Estonia',X)
        ne57=re.findall('Eswatini (formerly Swaziland)',X)
        ne58=re.findall('Ethiopia',X)
        ne59=re.findall('Fiji',X)
        ne60=re.findall('Finland',X)
        ne61=re.findall('France',X)
        ne62=re.findall('Gabon',X)
        ne63=re.findall('Gambia',X)
        ne64=re.findall('Georgia',X)
        ne65=re.findall('Germany',X)
        ne66=re.findall('Ghana',X)
        ne67=re.findall('Greece',X)
        ne68=re.findall('Grenada',X)
        ne69=re.findall('Guatemala',X)
        ne70=re.findall('Guinea',X)
        ne71=re.findall('Guinea-Bissau',X)
        ne72=re.findall('Guyana',X)
        ne73=re.findall('Haiti',X)
        ne74=re.findall('Honduras',X)
        ne75=re.findall('Hungary',X)
        ne76=re.findall('Iceland',X)
        ne77=re.findall('India',X)
        ne78=re.findall('Indonesia',X)
        ne79=re.findall('Iran',X)
        ne80=re.findall('Iraq',X)
        ne81=re.findall('Ireland',X)
        ne82=re.findall('Israel',X)
        ne83=re.findall('Italy',X)
        ne84=re.findall('Jamaica',X)
        ne85=re.findall('Japan',X)
        ne86=re.findall('Jordan',X)
        ne87=re.findall('Kazakhstan',X)
        ne88=re.findall('Kenya',X)
        ne89=re.findall('Kiribati',X)
        ne90=re.findall('Kosovo',X)
        ne91=re.findall('Kuwait',X)
        ne92=re.findall('Kyrgyzstan',X)
        ne93=re.findall('Laos',X)
        ne94=re.findall('Latvia',X)
        ne95=re.findall('Lebanon',X)
        ne96=re.findall('Lesotho',X)
        ne97=re.findall('Liberia',X)
        ne98=re.findall('Libya',X)
        ne99=re.findall('Liechtenstein',X)
        ne100=re.findall('Lithuania',X)
        ne101=re.findall('Luxembourg',X)
        ne102=re.findall('Madagascar',X)
        ne103=re.findall('Malawi',X)
        ne104=re.findall('Malaysia',X)
        ne105=re.findall('Maldives',X)
        ne106=re.findall('Mali',X)
        ne107=re.findall('Malta',X)
        ne108=re.findall('Marshall Islands',X)
        ne109=re.findall('Mauritania',X)
        ne110=re.findall('Mauritius',X)
        ne111=re.findall('Mexico',X)
        ne112=re.findall('Micronesia',X)
        ne113=re.findall('Moldova',X)
        ne114=re.findall('Monaco',X)
        ne115=re.findall('Mongolia',X)
        ne116=re.findall('Montenegro',X)
        ne117=re.findall('Morocco',X)
        ne118=re.findall('Mozambique',X)
        ne119=re.findall('Myanmar (formerly Burma)',X)
        ne120=re.findall('Namibia',X)
        ne121=re.findall('Nauru',X)
        ne122=re.findall('Nepal',X)
        ne123=re.findall('Netherlands',X)
        ne124=re.findall('New Zealand',X)
        ne125=re.findall('Nicaragua',X)
        ne126=re.findall('Niger',X)
        ne127=re.findall('Nigeria',X)
        ne128=re.findall('North Korea',X)
        ne129=re.findall('North Macedonia (formerly Macedonia)',X)
        ne130=re.findall('Norway',X)
        ne131=re.findall('Oman',X)
        ne132=re.findall('Pakistan',X)
        ne133=re.findall('Palau',X)
        ne134=re.findall('Palestine',X)
        ne135=re.findall('Panama',X)
        ne136=re.findall('Papua New Guinea',X)
        ne137=re.findall('Paraguay',X)
        ne138=re.findall('Peru',X)
        ne139=re.findall('Philippines',X)
        ne140=re.findall('Poland',X)
        ne141=re.findall('Portugal',X)
        ne142=re.findall('Qatar',X)
        ne143=re.findall('Romania',X)
        ne144=re.findall('Russia',X)
        ne145=re.findall('Rwanda',X)
        ne146=re.findall('Saint Kitts and Nevis',X)
        ne147=re.findall('Saint Lucia',X)
        ne148=re.findall('Saint Vincent and the Grenadines',X)
        ne149=re.findall('Samoa',X)
        ne150=re.findall('San Marino',X)
        ne151=re.findall('Sao Tome and Principe',X)
        ne152=re.findall('Saudi Arabia',X)
        ne153=re.findall('Senegal',X)
        ne154=re.findall('Serbia',X)
        ne155=re.findall('Seychelles',X)
        ne156=re.findall('Sierra Leone',X)
        ne157=re.findall('Singapore',X)
        ne158=re.findall('Slovakia',X)
        ne159=re.findall('Slovenia',X)
        ne160=re.findall('Solomon Islands',X)
        ne161=re.findall('Somalia',X)
        ne162=re.findall('South Africa',X)
        ne163=re.findall('South Korea',X)
        ne164=re.findall('South Sudan',X)
        ne165=re.findall('Spain',X)
        ne166=re.findall('Sri Lanka',X)
        ne167=re.findall('Sudan',X)
        ne168=re.findall('Suriname',X)
        ne169=re.findall('Sweden',X)
        ne170=re.findall('Switzerland',X)
        ne171=re.findall('Syria',X)
        ne172=re.findall('Taiwan',X)
        ne173=re.findall('Tajikistan',X)
        ne174=re.findall('Tanzania',X)
        ne175=re.findall('Thailand',X)
        ne176=re.findall('Timor-Leste',X)
        ne177=re.findall('Togo',X)
        ne178=re.findall('Tonga',X)
        ne179=re.findall('Trinidad and Tobago',X)
        ne180=re.findall('Tunisia',X)
        ne181=re.findall('Turkey',X)
        ne182=re.findall('Turkmenistan',X)
        ne183=re.findall('Tuvalu',X)
        ne184=re.findall('Uganda',X)
        ne185=re.findall('Ukraine',X)
        ne186=re.findall('United Arab Emirates (UAE)',X)
        ne187=re.findall('United Kingdom (UK)',X)
        ne188=re.findall('United States of America (USA)',X)
        ne189=re.findall('Uruguay',X)
        ne190=re.findall('Uzbekistan',X)
        ne191=re.findall('Vanuatu',X)
        ne192=re.findall('Vatican City (Holy See)',X)
        ne193=re.findall('Venezuela',X)
        ne194=re.findall('Vietnam',X)
        ne195=re.findall('Yemen',X)
        ne196=re.findall('Zambia',X)
        ne197=re.findall('Zimbabwe',X)
                        
        nall= ne0+ne1+ne2+ne3+ne4+ne5+ne6+ne7+ne8+ne9+ne10+\
              ne11+ne12+ne13+ne14+ne15+ne16+ne17+ne18+ne19+\
              ne20+ne21+ne22+ne23+ne24+ne25+ne26+ne27+ne28+ne29+\
              ne30+ne31+ne32+ne33+ne34+ne35+ne36+ne37+ne38+ne39+\
              ne40+ne41+ne42+ne43+ne44+ne45+ne46+ne47+ne48+ne49+\
              ne50+ne51+ne52+ne53+ne54+ne55+ne56+ne57+ne58+ne59+\
              ne60+ne61+ne62+ne63+ne64+ne65+ne66+ne67+ne68+ne69+\
              ne70+ne71+ne72+ne73+ne74+ne75+ne76+ne77+ne78+ne79+\
              ne80+ne81+ne82+ne83+ne84+ne85+ne86+ne87+ne88+ne89+\
              ne90+ne91+ne92+ne93+ne94+ne95+ne96+ne97+ne98+ne99+\
              ne100+ne101+ne102+ne103+ne104+ne105+ne106+ne107+ne108+\
              ne109+ne110+ne111+ne112+ne113+ne114+ne115+ne116+ne117+\
              ne118+ne119+ne120+ne121+ne122+ne123+ne124+ne125+ne126+\
              ne127+ne128+ne129+ne130+ne131+ne132+ne133+ne134+ne135+\
              ne136+ne137+ne138+ne139+ne140+ne141+ne142+ne143+ne144+\
              ne145+ne146+ne147+ne148+ne149+ne150+ne151+ne152+ne153+\
              ne154+ne155+ne156+ne157+ne158+ne159+ne16+ne161+ne162+\
              ne163+ne164+ne165+ne166+ne167+ne168+ne169+ne170+ne171+ne172+\
              ne173+ne174+ne175+ne176+ne177+ne178+ne179+ne180+ne181+ne182+ne183+\
              ne184+ne185+ne186+ne187+ne188+ne189+ne190+ne191+ne192+ne193+ne194+ne195+ne196+ne197
        con=[]
        for x in nall:
            if x not in con:
                con.append(x)

        if len(con)<2:
            for x in con:
                countries_extracted.append([x])
        if len(con)>=2:
             countries_extracted.append(con)
        if con == []:
            countries_extracted.append(['NA'])
        

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
                    if h in host:
                        ne.append(h)
        he=[]
        for x in ne:
            if x not in he:
                he.append(x)
        # if he has only one item , add it to final list of countires_extratced
        if len(he)<2:
                    for x in he:
                        hosts_extratced.append(x)
        # if he has more than one item , add them as an inner list to the final list of countires_extratced
        if len(he)>=2:
            hosts_extratced.append(he)                  
        if he == []:
            hosts_extratced.append('NA')
            
con_extraction (combined[0:946])
digno_extract(combined[0:946])
parasite_extraction(combined[0:946])
host_extractor(combined[0:946],hosts2)

Extraction=pd.DataFrame()
Extraction['studies']=Ttiles
Extraction=Extraction.assign(country=countries_extracted)
Extraction=Extraction.assign(Dignostic_method=digno_extracted)
Extraction=Extraction.assign(Tryp_type=parasite_extracted)
Extraction=Extraction.assign(host_type=hosts_extratced)



Extraction.to_csv(r'/Users/hawraal-ghafli/Desktop/sys.review/working codes/Extraction.csv', index = False)
