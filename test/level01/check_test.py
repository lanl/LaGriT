#! /usr/bin/env python
#------------------------------------------------------------------------------
#  Name: check_test.py
#  Last Modified: Jan 2008 by TAM 
#
#  writes differences to diffout_$OS.txt
#  and copies file to directory result_files 
#
#  Uses difflib.py providing diffs in four formats:
#  Current version uses unified
#
# ndiff:    lists every line and highlights interline changes.
# context:  highlights clusters of changes in a before/after format.
# unified:  highlights clusters of changes in an inline format.
# html:     generates side by side comparison with change highlights.
#
#------------------------------------------------------------------------------

import fileinput, string, os, difflib, sys, datetime, time

def fail(msg):
   out = sys.stderr.write
   out(msg + "\n\n")
   out(__doc__)
   return 0

def rstrip(line, JUNK='\n \t '):
   i = len(line)
   while i > 0 and line[i-1] in JUNK:
       i -= 1
   return line[:i]

##############################################################################
# MAIN begin
#
#------------------------------------------------------------------------------

if __name__ == "__main__":

  JUNK='\n \t'
  errList=[]
  errmess=[]
  nlines = 1
  rmspace = 0
  ierr=0
  itest=0
  ntest=0
  ndirs=0
  result_dir = 0

# get platform
  osname="unknown"
  osname= string.lower(sys.platform)
  if osname.find("linux") >= 0 :
     ostag="_lin" 
  elif osname.find("sun")>=0 or osname.find("sol")>=0 :
     ostag="_sun" 
  elif osname.find("darwin")>= 0 :
     ostag="_mac" 
  else :
     ostag="" 


# define top directory as current directory
  dtop = os.curdir
  dtop_path = os.getcwd()
  fout = dtop_path+"/diffout"+ostag+".txt"

  date = time.ctime()
  buff = "Start in directory: "+dtop_path+" at "+date
  print buff
  wfile = open(fout,'w')
  wfile.write(buff+"\n")
  wfile.close()

  fref = "/reference/outx3dgen"
  ftest = "/outx3dgen"

# for each test directory
# main loop
  for name in os.listdir(dtop) :
    dwork = os.path.join(dtop, name)

#---skip results directory until end 
    if (dwork == "./result_files") : 
        result_dir = 1 

#---check output for each directory and reference 
    elif os.path.isdir(dwork) : 
        wfile = open(fout,'a')
        ndirs=ndirs+1
        buff= repr(ndirs)+" Check Directory  "+dwork+" -----------------------"
        print buff
        wfile.write(buff+"\n")

        fromfile=dwork+fref
        tofile=dwork+ftest

#-------compare the output files
        if (os.path.exists(tofile)) : 

          fromdate = time.ctime(os.stat(fromfile).st_mtime)
          todate = time.ctime(os.stat(tofile).st_mtime)
          fromlines = open(fromfile, 'U').readlines()
          tolines = open(tofile, 'U').readlines()

          icomment=0
          ijunk=0
          itest=0
          print "Compare test +++ "+tofile+"   "+todate
          print "To reference --- "+fromfile+" "+fromdate

          diff = difflib.unified_diff(fromlines, tolines, fromfile, tofile, fromdate, todate, nlines)

#         remove commented and junk lines
          for line in diff :
            wfile.write(line)
            words = line.split(None)
            if (len(words) > 0) :
               if words[0].find("*") > -1 :
                  icomment=icomment+1
               elif words[0].find("+++") > -1 :
                  ijunk=ijunk+1
               elif words[0].find("---") > -1 :
                  ijunk=ijunk+1
               elif words[0].find("finish") > -1 :
                  ijunk=ijunk+1
               elif len(line) < 4 :
                  ijunk=ijunk+1
               else :

#              print line that differs if not comment or junk

#                 check to see if this is a new set of lines
                  if words[0].find("@") > -1 :
                     i = len(line)
                     tmp="\n"+"lines "+line[2:i-3]+" differ >>"
                     line=tmp

                  i = len(line)
                  if words[0]=="+" :
                     itest=itest+1

                  while i > 0 and line[i-1] in JUNK:
                     i -= 1
                  print "%s" % (line[:i])

          print " "
          if icomment : 
             buff= repr(icomment)+" comment lines."
             wfile.write(buff+"\n")
             print "Removed "+buff
          if ijunk : 
             buff = repr(ijunk)+" junk lines."
             wfile.write(buff+"\n")
             print "Removed "+buff
          if itest : 
            buff = repr(itest)+" lines failed."
            wfile.write(buff+"\n")
            print buff
            errList.append(dwork)
            errmess.append(repr(itest)+" lines failed.")
            ierr = ierr+1
          else :
            buff = "No lines differ."
            wfile.write(buff+"\n")
            print buff
          buff= repr(ndirs)+" Done with Directory "+dwork+" -----------------------"
          print buff
          wfile.write(buff+"\n")
          print " "



        else :
          print "File missing: outx3dgen" 
          errList.append(dwork)
          errmess.append("Missing LaGriT outx3dgen file.")
          ierr = ierr+1

#-------done with compare 
        if itest > 0 : ntest=ntest+1

#---done with work in lower directory
           
# end main loop
  if ntest : 
    buff = "All checks complete, "+repr(ntest)+" directories failed out of "+repr(ndirs)
    wfile.write(buff+"\n")
    print buff
  else :
    buff =  "All "+repr(ndirs)+" successful!"
    wfile.write(buff+"\n")
    print buff
  if (ierr > 0) :
    i = 0
    print "--------------------------------------"
    for d in errList :
      buff =  "  "+errList[i]+" Error: "+errmess[i]
      wfile.write(buff+"\n")
      print buff
      print "--------------------------------------"
      i=i+1
  wfile.write("Check done."+"\n")
  wfile.close()

  if result_dir :
    b = os.system('cp -p '+fout+' ./result_files')
    print "Check done."+"\n"+"Full result written to: "+"\n"
    print fout+"\n"
    print "and copied to ./result_files "+"\n"

#    fromfile = "./result_files/diffout"+ostag+".txt" 
#    tofile="./result_files/reference/diffout"+ostag+".txt" 
#    print  "Compare "+fromfile+" to "+tofile 
#    fromlines = open(fromfile, 'U').readlines()
#    tolines = open(tofile, 'U').readlines()
#    fdiff = difflib.ndiff(fromlines, tolines)
#    sys.stdout.writelines(fdiff) 

  else :
    print "Check done."+"\n"+"Full result written to "+fout+"\n"
      
# end Main 
#------------------------------------------------------------------------------

