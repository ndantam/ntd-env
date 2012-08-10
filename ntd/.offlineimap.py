import re

def makeimaptrans(prefix):
    def imaptrans(foldername):
        if( "INBOX" == foldername ):
            retval = "." + prefix
        else:
            retval = "." + prefix + "." + foldername
        print "imap trans: " + foldername + " -> " + retval
        return retval
    return imaptrans

def makelocalstrans(prefix):
    def localtrans(foldername):
        if(foldername == "." + prefix):
            retval = "INBOX"
        else:
            # remove leading '.acc1'
            retval = re.sub("^\." + prefix + "\.", "", foldername)
        print "local trans: " + foldername + " -> " + retval
        return retval
    return localtrans
