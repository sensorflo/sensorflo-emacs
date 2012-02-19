#!/usr/local/bin/python
#
'''\
HTML Table Parser
htmlTableParse.py
jjk  01/13/98  001  from CTtableParse.py 002b
jjk  01/14/98  002  use UserList
jjk  02/03/98  003  rename (was CThtmlTableParse), split out tests
jjk  10/10/98  004  add test() back in, add some utillities and improve docs
jjk  01/05/99  005  fix/improve RemoveTags() add ConvertSpecialCharacters()
str  10/08/01  006  Use module re instead of regex

The ParsedDocument class breaks up an HTML string by tables, rows, and columns.
(the HTML code must have openening and closing table TR and TD tags)

The parsed data is retreived as a list of lists of lists, i.e.
    table4 = aParsedDocument[3]
    table4Row1 = aParsedDocument[3][0]
    table4Row1Col3 = aParsedDocument[3][0][2]

example usage:
    import htmlTableParse
    pd = htmlTableParse.ParsedDocument(htmlSourceString)
    numberOfTables = len(pd)
    contentsOfFithColumnOfThirdRowOfSecondTable = pd[1][2][4]

some useful functions:
    ParseOpenFile(fileStream)   #answer a ParsedDocument instance for contents of open file
    ParseFile(fileName) #answer a ParsedDocument instance for contents of a file
    ParseURL(url)       #answer a ParsedDocument instance for contents of a url
    RemoveTags(htmlString)  #remove all html tags from a string
    ConvertSpecialCharacters(htmlString)  #remove all special ampersand characters from a string

see also test() function

*** use this code at your own risk ***
*** some code may have been borrowed from other python modules ***
*** the programmer is a re newbie - this may not be the optimal solution :-) ***

'''

import re, string, sys, UserList

TraceFlag = 0

Re1 = '[Tt][Aa][Bb][Ll][Ee]'
TableStart=re.compile('<[ \t]*'+Re1+'[^<]*>')
TableEnd=re.compile('<[ \t]*/'+Re1+'[ \t]*>')
Re2 = '[Tt][Rr]'
RowStart=re.compile('<[ \t]*'+Re2+'[^<]*>')
RowEnd=re.compile('<[ \t]*/'+Re2+'[ \t]*>')
Re3 = '[Tt][Dd]'
ColStart=re.compile('<[ \t]*'+Re3+'[^<]*>')
ColEnd=re.compile('<[ \t]*/'+Re3+'[ \t]*>')
TagRe=re.compile('<[^<]*>')

AmpChars = [
    (re.compile('&[Nn][Bb][Ss][Pp];'),' ')
    ,(re.compile('&[Aa][Mm][Pp];'),'&')
    ]


class ParsedDocument(UserList.UserList):
    '''jjk  01/14/98'''

    def __init__(self, htmlSrc=''):
        '''jjk  01/14/98'''
        UserList.UserList.__init__(self)
        self._parseContents(htmlSrc)

    def report(self, outs, prefix=''):
        '''jjk  01/13/98'''
        i1 = 0
        for item in self.data:
            item.report(outs, prefix+str(i1)+'>')
            i1 = i1 + 1

    def reportStructure(self, outs, prefix=''):
        '''jjk  10/10/98'''
        outs.write('ParsedDocument: %d tables\n'%len(self))
        for i1 in range(len(self)):
            table = self[i1]
            outs.write('\tTable #%d: %d rows\n'%(i1,len(table)))
            for i2 in range(len(table)):
                row = table[i2]
                outs.write('\t\tRow #%d: %d columns\n'%(i2,len(row)))

    def _parseParams(self):
        '''jjk  01/13/98'''
        return(TableStart, TableEnd, ParsedTable)

    def _parseContents(self, htmlSrc):
        '''jjk  01/13/98'''
        startRegex, endRegex, contentClass = self._parseParams()
        hs = htmlSrc
        while (1):
            p1 = startRegex.search(hs)
            #print dir(p1),p1.start(),p1.end()
            if (p1==None): break
            p1=p1.start()
            hs = hs[p1:]
            p2 = endRegex.search(hs)
            #if (p2<0): p2 = len(hs)+1
            if (p2==None):
                p2 = len(hs)+1
            else:
                p2 = p2.start()
            self.append(contentClass(hs[:p2]))
            hs = hs[p2:]

    def tables(self):
        '''jjk  01/13/98'''
        return(self.data)

class ParsedTable(ParsedDocument):
    '''jjk  01/13/98'''

    def _parseParams(self):
        '''jjk  01/13/98'''
        return(RowStart, RowEnd, ParsedRow)

    def rows(self):
        '''jjk  01/13/98'''
        return(self.data)

class ParsedRow(ParsedTable):
    '''jjk  01/13/98'''

    def _parseParams(self):
        '''jjk  01/13/98'''
        return(ColStart, ColEnd, ParsedColumn)

    def columns(self):
        '''jjk  01/13/98'''
        return(self.data)

class ParsedColumn:
    '''jjk  01/13/98'''

    def __init__(self, htmlSrc=''):
        '''jjk  01/13/98'''
        self.contents = ''
        self._parseContents(htmlSrc)

    def __repr__(self):
        '''jjk  01/13/98'''
        return(self.contents)

    def _parseParams(self):
        '''jjk  01/13/98'''
        return(TableStart, ColEnd, ParsedTable)

    def _parseContents(self, htmlSrc):
        '''jjk  01/13/98'''
        hs = htmlSrc
        p1a = ColStart.search(hs)
        if (p1a==None): return
        p1a=p1a.start()
        p1b = string.find(hs,'>',p1a)
        #p1b=p1b.start()
        hs = hs[p1b+1:]
        p2 = ColEnd.search(hs)
        #if (p2<0): p2 = len(hs)+1
        if (p2==None):
            p2 = len(hs)+1
        else:
            p2 = ps.start()
        self.contents = string.strip(hs[:p2])

    def report(self, outs, prefix=''):
        '''jjk  01/13/98'''
        outs.write(prefix+self.contents+'\n')

def RemoveTags(htmlString):
    '''remove all html tags from a string
    jjk  01/05/99'''
    hs = htmlString
    while(1):
        if TraceFlag:
            print '~', hs
        p1 = TagRegex.search(hs)
        if (p1<0): break
        p2 = p1 + TagRegex.match(hs[p1:])
        if TraceFlag:
            print '~~',p1, p2, hs[p1:p2]
        hs = hs[:p1] + ' ' + hs[p2:]
    if TraceFlag:
        raw_input('z')
    return(hs)

def ConvertSpecialCharacters(htmlString):
    '''remove all special ampersand characters from a string
    jjk  01/05/99'''
    hs = htmlString
    for ac in AmpChars:
        while(1):
            if TraceFlag:
                print '~', hs
            p1 = ac[0].search(hs)
            if (p1<0): break
            p2 = p1 + ac[0].match(hs[p1:])
            if TraceFlag:
                print '~~',p1, p2, hs[p1:p2]
            hs = hs[:p1] + ac[1] + hs[p2:]
        if TraceFlag:
            raw_input('z')
    return(hs)

def ParseOpenFile(fileStream=sys.stdin):
    '''public: answer a ParsedDocument instance for contents of open file
    jjk  10/10/98'''
    fileData = fileStream.read()
    parsedDocument = ParsedDocument(fileData)
    return(parsedDocument)

def ParseFile(fileName):
    '''public: answer a ParsedDocument instance for contents of a file
    jjk  10/10/98'''
    fileStream = open(fileName)
    parsedDocument = ParseOpenFile(fileStream)
    fileStream.close()
    return(parsedDocument)

def ParseURL(url,proxy=None):
    '''public: answer a ParsedDocument instance for contents of a url
    jjk  10/10/98'''
    import urllib
    if proxy:
        import os
        os.environ['http_proxy'] = proxy
    fileName, msg = urllib.urlretrieve(url)
    return(ParseFile(fileName))

def test():
    '''jjk  10/10/98'''
    print 'testing htmlTableParse.py'
    url = raw_input('Enter a URL: ')
    pd = ParseURL(url)
    pd.reportStructure(sys.stdout)
    raw_input('press "Enter" for full report of contents')
    pd.report(sys.stdout)
    return(pd)

if (__name__=='__main__'):
    import pdb
    test()

# arch-tag: 31a4b67c-793d-4b54-9b14-89ace8db2439