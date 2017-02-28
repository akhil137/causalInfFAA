#We used the mozilla firebug extension to capture curl, post , and headers of 
#a saved report (e.g. 'minSetFlights') for a particular 
#aspm module (e.g. indiv flights).

#Various files were use to extract/verify the request; curl is easiest
#Files; Description
#postParamsIndivFlights.txt; Copied from Firebug
#postHeadersIndivFlights.txt; has cookie
#curlInivFlights.txt; can be used to create requests.post
#					 with http://curl.trillworks.com/
#
#Note: copied curl only has post parameters that are non-empty (postParams has all)

#From above curl to requests website by trillworks

import datetime
import requests
from bs4 import BeautifulSoup

#we should keep cookies private

headers = {
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
    'Accept-Encoding': 'gzip, deflate, br',
    'Accept-Language': 'en-US,en;q=0.5',
    'Connection': 'keep-alive',
    'Host': 'aspm.faa.gov',
    'Referer': 'https://aspm.faa.gov/apm/sys/FlightLevel.asp',
    'Upgrade-Insecure-Requests': '1',
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:49.0) Gecko/20100101 Firefox/49.0',
    'Content-Type': 'application/x-www-form-urlencoded',
}


#data = 'dfldA=d_yyyymmdd&dlistA=20140701%2C20140702%2C20140703%2C20140704%2C20140705%2C20140706%2C20140707%2C20140708%2C20140709%2C20140710%2C20140711%2C20140712%2C20140713%2C20140714%2C20140715&dfldB=a_yyyymmdd&dlistB=20140701%2C20140702%2C20140703%2C20140704%2C20140705%2C20140706%2C20140707%2C20140708%2C20140709%2C20140710%2C20140711%2C20140712%2C20140713%2C20140714%2C20140715&deplist=&arrlist=%27+JFK%27&clist=&elist=&fldlist=DEP_LOCID%2CARR_LOCID%2CD_YYYYMMDD%2CA_YYYYMMDD%2CDEP_HR_LOCAL%2CARR_HR_LOCAL%2CFAACARRIER%2CFLTNO&rptlist=T_WHLS_OFF%2CT_EDCT_OFF%2CEDCT_HOLD%2CT_WHLS_ON%2CFILED_ETE%2CEDCT_ETE%2CF_TO_DIFF%2CF_TI_DIFF&line=SELECT+DEP_LOCID%2CARR_LOCID%2CD_YYYYMMDD%2CA_YYYYMMDD%2CDEP_HR_LOCAL%2CARR_HR_LOCAL%2CFAACARRIER%2CFLTNO%2CT_WHLS_OFF%2CT_EDCT_OFF%2CEDCT_HOLD%2CT_WHLS_ON%2CFILED_ETE%2CEDCT_ETE%2CF_TO_DIFF%2CF_TI_DIFF+FROM+CODASFLT++WHERE+D_YYYYMMDD+IN+%2820140701%2C20140702%2C20140703%2C20140704%2C20140705%2C20140706%2C20140707%2C20140708%2C20140709%2C20140710%2C20140711%2C20140712%2C20140713%2C20140714%2C20140715%29+AND+A_YYYYMMDD+IN+%2820140701%2C20140702%2C20140703%2C20140704%2C20140705%2C20140706%2C20140707%2C20140708%2C20140709%2C20140710%2C20140711%2C20140712%2C20140713%2C20140714%2C20140715%29+AND+ARR_LOCID+IN+%28%27+JFK%27%29+ORDER+BY+DEP_LOCID%2CARR_LOCID%2CD_YYYYMMDD%2CA_YYYYMMDD%2CDEP_HR_LOCAL%2CARR_HR_LOCAL%2CFAACARRIER%2CFLTNO&cmd=fl1&nopage=y&nost=&defs=&avgdays=&oktosave=y&sys=fl&DepInput_param=&depQuick_param=&ArrInput_param=&arrQuick_param=&eqptInput_param=&eqptQuick_param=&carrInput_param=&carrQuick_param=&fltype=%3F&phclass=%3F&usclass=%3F&oceanic=%3F&oooi=%3F&oag=%3F&etms=%3F&rawacid=&tailno=&fltno=&hrfrom=%3F%3F&hrto=%3F%3F&ahrfrom=%3F%3F&ahrto=%3F%3F&min_gd1=&max_gd1=&min_gd2=&max_gd2=&min_edctd=&max_edctd=&min_tot=&max_tot=&min_tod=&max_tod=&min_add1=&max_add1=&min_add2=&max_add2=&min_ab=&max_ab=&min_tit=&max_tit=&min_tid=&max_tid=&min_edcta=&max_edcta=&min_bd=&max_bd=&min_ad1=&max_ad1=&min_ad2=&max_ad2=&min_dz=&max_dz=&min_az=&max_az=&min_ete=&max_ete=&min_edctete=&max_edctete=&min_d1=&max_d1=&min_d2=&max_d2=&min_d3=&max_d3=&min_d4=&max_d4=&min_d5=&max_d5=&min_d6=&max_d6=&min_d7=&max_d7=&min_nas=&max_nas=&min_car=&max_car=&min_late=&max_late=&min_wx=&max_wx=&min_sec=&max_sec=&min_ttl=&max_ttl=&min_wthr=&max_wthr=&min_vol=&max_vol=&min_eq=&max_eq=&min_rwy=&max_rwy=&min_oth=&max_oth=&min_nm=&max_nm=&reptype=r1&reportformat=asp&MyReportName=minSetFlights'

#for later easy modification of date fields in data
#extract as dictionary (which requests module accepts)
#code to do this
#-------------------------
# import urlparse

# su=urlparse.urlsplit(url)
# q=dict(urlparse.parse_qsl(su.query))
# #just some garbage up front so parser knows where to begin
# url='http://doesntmatter.org/?'+data
#-------------------------
#Here's the result
q={'MyReportName': 'minSetFlights', 'ahrfrom': '??', 'ahrto': '??', 
 'arrlist': "' JFK'", 'cmd': 'fl1', 
 'dfldA': 'd_yyyymmdd', 'dfldB': 'a_yyyymmdd', 
 'dlistA': '20140701,20140702,20140703,20140704,20140705,20140706,20140707,20140708,20140709,20140710,20140711,20140712,20140713,20140714,20140715', 
 'dlistB': '20140701,20140702,20140703,20140704,20140705,20140706,20140707,20140708,20140709,20140710,20140711,20140712,20140713,20140714,20140715', 
 'etms': '?', 
 'fldlist': 'DEP_LOCID,ARR_LOCID,D_YYYYMMDD,A_YYYYMMDD,DEP_HR_LOCAL,ARR_HR_LOCAL,FAACARRIER,FLTNO', 
 'fltype': '?', 'hrfrom': '??', 'hrto': '??', 
 'line': "SELECT DEP_LOCID,ARR_LOCID,D_YYYYMMDD,A_YYYYMMDD,DEP_HR_LOCAL,ARR_HR_LOCAL,FAACARRIER,FLTNO,T_WHLS_OFF,T_EDCT_OFF,EDCT_HOLD,T_WHLS_ON,FILED_ETE,EDCT_ETE,F_TO_DIFF,F_TI_DIFF,F_AIR_DIFF,AIRBORNE,ACT_ON_EDCT_ON_DIF,ACT_OFF_EDCT_OFF_DIF FROM CODASFLT  WHERE D_YYYYMMDD IN (20140701,20140702,20140703,20140704,20140705,20140706,20140707,20140708,20140709,20140710,20140711,20140712,20140713,20140714,20140715) AND A_YYYYMMDD IN (20140701,20140702,20140703,20140704,20140705,20140706,20140707,20140708,20140709,20140710,20140711,20140712,20140713,20140714,20140715) AND ARR_LOCID IN (' JFK') ORDER BY DEP_LOCID,ARR_LOCID,D_YYYYMMDD,A_YYYYMMDD,DEP_HR_LOCAL,ARR_HR_LOCAL,FAACARRIER,FLTNO",
 'nopage': 'y',
 'oag': '?',
 'oceanic': '?',
 'oktosave': 'y',
 'oooi': '?',
 'phclass': '?',
 'reportformat': 'asp',
 'reptype': 'r1',
 'rptlist': 'T_WHLS_OFF,T_EDCT_OFF,EDCT_HOLD,T_WHLS_ON,FILED_ETE,EDCT_ETE,F_TO_DIFF,F_TI_DIFF,F_AIR_DIFF,AIRBORNE,ACT_ON_EDCT_ON_DIF,ACT_OFF_EDCT_OFF_DIF',
 'sys': 'fl',
 'usclass': '?'}

 #let's generate dates string

def gen_dates_string(base, numdays=14, fmt='%Y%m%d'):
    "return comma seperated string of dates"
    dates_list = [base + datetime.timedelta(days=x) for x in range(0, numdays)]
    datesString = ','.join([a.strftime(fmt) for a in dates_list])

    return datesString







#airport for 'line' value
ap='JFK'

#database query string that will make up 'line' value in above dict
# 'line': 
# "SELECT DEP_LOCID,ARR_LOCID,D_YYYYMMDD,A_YYYYMMDD,DEP_HR_LOCAL,ARR_HR_LOCAL,FAACARRIER,FLTNO,T_WHLS_OFF,T_EDCT_OFF,EDCT_HOLD,T_WHLS_ON,FILED_ETE,EDCT_ETE,F_TO_DIFF,F_TI_DIFF 
# FROM CODASFLT  
# WHERE D_YYYYMMDD IN (20140701,20140702,20140703,20140704,20140705,20140706,20140707,20140708,20140709,20140710,20140711,20140712,20140713,20140714,20140715) 
# AND A_YYYYMMDD IN (20140701,20140702,20140703,20140704,20140705,20140706,20140707,20140708,20140709,20140710,20140711,20140712,20140713,20140714,20140715) 
# AND ARR_LOCID IN (' JFK') ORDER BY DEP_LOCID,ARR_LOCID,D_YYYYMMDD,A_YYYYMMDD,DEP_HR_LOCAL,ARR_HR_LOCAL,FAACARRIER,FLTNO"

# #line_vals
# #
sel='SELECT DEP_LOCID,ARR_LOCID,D_YYYYMMDD,A_YYYYMMDD,DEP_HR_LOCAL,ARR_HR_LOCAL,FAACARRIER,FLTNO,T_WHLS_OFF,T_EDCT_OFF,EDCT_HOLD,T_WHLS_ON,FILED_ETE,EDCT_ETE,F_TO_DIFF,F_TI_DIFF,F_AIR_DIFF,AIRBORNE,ACT_ON_EDCT_ON_DIF,ACT_OFF_EDCT_OFF_DIF'

frm = 'FROM CODASFLT'
depDate = 'WHERE D_YYYYMMDD IN'
arrvDate = 'AND A_YYYYMMDD IN'
arrvLoc = 'AND ARR_LOCID IN'
order = 'ORDER BY DEP_LOCID,ARR_LOCID,D_YYYYMMDD,A_YYYYMMDD,DEP_HR_LOCAL,ARR_HR_LOCAL,FAACARRIER,FLTNO'

def gen_line_val(datesString,ap='JFK'):
    line_val = (sel + ' ' + frm  + '  ' + depDate + ' '+ '(' + datesString + ')' + 
    ' '+ arrvDate + ' '+ '(' + datesString + ')' + ' ' + arrvLoc + ' ' + 
    '(' + '\' ' + ap + '\'' + ')' + ' ' + order)

    return line_val



#now modify query dict
#add these to query dict
datesString=gen_dates_string(base=datetime.date(2014,7,1),numdays=1)
line_val=gen_line_val(datesString)

#for troubleshooting exact string used in above q from firebug vs line_val 
#gives locations where strings differ
#[i for i in xrange(len(line_val)) if line_val[i] != q['line'][i]]


q['dlistA']=datesString
q['dlistB']=datesString
q['line']=line_val

#write html file for easy vieweing
f=open('test.html','w')
f.write(r.content)
f.close()


#finally
r = requests.post('https://aspm.faa.gov/apm/sys/apm-server-x.asp', 
	headers=headers, cookies=cookies, data=q)


#parsing with bs

#s=BeautifulSoup(r.content)
s=BeautifulSoup(open("test.html"))
t=s.find('table')
rows=t.findAll('tr')
#zeroth row is title, first column headers
rows.pop(0)
f=open('out.csv','wb')
w=csv.writer(f)
for tr in rows:
    txt = [str(' '.join(a.findAll(text=True))).strip() for a in tr.findAll('td')]
    w.writerow(txt)

f.close()



