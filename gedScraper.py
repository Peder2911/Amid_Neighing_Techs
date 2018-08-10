#!../bin/python
import requests
import sys
import csv
import time

# Script to send a stream of requests to the UCDP database

#######################################

# Get arguments
# should be script country (startdate enddate)?
if len(sys.argv) == 4:
    _,country,startdate,enddate = sys.argv
elif len(sys.argv) == 2:
    _,country = sys.argv
    startdate = '1950-01-01'
    enddate = '2018-05-18'
else:
    print(['Invalid number of arguments supplied',
    'must be 3 or 1, was %i'%(len(sys.argv)-1)])

baseUrl = 'http://ucdpapi.pcr.uu.se/api/gedevents/17.2?pagesize=10'
delayTime = 0.01

#######################################

def gwCodes(path):
    # Make a dict of gwCodes to convert country name into code
    with open(path,'r',errors='ignore') as file:
        reader = csv.reader(file)
        dict = {name:code for code,short,name in reader}
    return(dict)

gwCodebook = gwCodes('./data/gledWard.csv')

def makeUrl(base,country,startdate,enddate):
    # make the base request url
    url = base + '&country=%s'%(gwCodebook[country])
    url += '&StartDate=%s'%(startdate)
    url += '&enddate=%s'%(enddate)
    return(url)

URL=makeUrl(baseUrl,country,startdate,enddate)

#######################################

def defineRemoteData(url):
    # Get the page number
    initQuery=requests.get(url).json()

    pages = initQuery['TotalPages']
#    count = initQuery['TotalCount']
    return(pages)

# Returns empty list if no results
def makeUrlStack(url,noOfPages):
    # Make a stack of urls, with different page numbers
    stack = [url + '&page=%i'%(page) for page in range(1,noOfPages)]
    return(stack)

def delayedGetJson(url):
    # requests.get-function with delay (could be a decorator?)
    pageResultJson = requests.get(url).json()['Result']
    print('Getting...%s'%(url))
    time.sleep(delayTime)
    return(pageResultJson)

def getResults(stack):
    # The "get-loop", gets all urls in the stack
    # sums pages into a single "stream"
    pages = [delayedGetJson(url) for url in stack]
    results = sum(pages,[])
    return(results)

#######################################

def makeCsv(results):
    # Write the results-stream into a csv
    keys = results[0].keys()
    with open('data/reqOut.csv','w') as outputFile:
        dictWriter = csv.DictWriter(outputFile,keys)
        dictWriter.writeheader()
        dictWriter.writerows(results)

#######################################

if __name__ == '__main__':
    stack=makeUrlStack(URL,defineRemoteData(URL))
    if len(stack) != 0:
      results = getResults(stack)
      makeCsv(results)
    else:
      sys.exit(1)
      
