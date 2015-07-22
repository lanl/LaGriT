#! /n/local_linux/epd/bin/python2.7
#
# This script will not run with /usr/local/bin/python -> python3.2  
# for linux /n/local_linux/python2.5.1/bin/python
#
# default is now vers 3 in  /usr/bin/python 
# for linux /n/local_linux/python2.5.1/bin/python
# for mac /usr/bin/env python 
# for lanl machines /usr/bin/env python
# for sgi /usr/lanl/bin/python
#------------------------------------------------------------------------------
#  Name: check_test.py
#  Usage: check_test.py (all dirs) or check_test.py dir1 (list dirs)
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

import fileinput, array, string, os, difflib, sys, datetime, time, copy

__all__ = ["fail", "rstrip", "diff_chunk", "Check"]

#------------------------------------------------------------------------------
def fail(msg):
   out = sys.stderr.write
   out(msg + "\n\n")
   out(__doc__)
   return 0

#------------------------------------------------------------------------------
def rstrip(line, JUNK='\n \t '):
   i = len(line)
   while i > 0 and line[i-1] in JUNK:
      i -= 1
   return line[:i]

#------------------------------------------------------------------------------
# compare using additional checks to diff

def diff_chunk(rlines,tlines,rcnt,tcnt, wfile) :

# need large epsilon to compare numbers  1.73469E+02 to 1.734693909E+02
  epsval = 0.001 
  tmp=[]
  tmp1=[]
  tmp2=[]
  ibad=0
  ifa = 0
  ico = 0
  iju = 0
  count = 0
  debug = 1

#-Loop over each pair of lines 0 to tcnt
  count = max(tcnt,rcnt)
  for idx in range(count) :
     rcount = 0
     tmp1="-"
     if (idx < rcnt) :
        rwords = rlines[idx].split(None)
        rcount = len(rwords)
        tmp="-"+rlines[idx] 
        tmp1=tmp.rstrip()
     tcount = 0
     tmp2="+"
     if (idx < tcnt) :
        twords = tlines[idx].split(None)
        tcount = len(twords)
        tmp="+"+tlines[idx] 
        tmp2=tmp.rstrip()

#    for lines with same number of words, compare values word by word 
     if (tcount == rcount and tcount>0 ) :

        ibad=0
#       check to see if these are lines to skip 
        if twords[0].find("*") > -1 :
           ico=ico+1
           if debug : print "comment line "+repr(idx)+tlines[idx]
        elif twords[0].find("#") > -1 :
           ico=ico+1
           if debug : print "comment line "+repr(idx)+tlines[idx]
        elif len(twords) < 2 :
           iju=iju+1
           if debug : print "junk line "+repr(idx)+tlines[idx]

        else :

#          Loop through word by word
           if debug : print "epsilon compare: %16.9f \n" % epsval
           for i in range(len(rwords)) :

              if (rwords[i].isalpha() and twords[i].isalpha()) :
                if (rwords[i] == twords[i]) : 
                   if debug : print "alpha same "+rwords[i]+" and "+twords[i] 

                else :
                   if debug : print "alpha differ "+rwords[i]+" and "+twords[i]
                   ibad = 1

              elif (rwords[i].isdigit() and twords[i].isdigit)  :
                xval=abs(float(rwords[i])-float(twords[i]))       
                if (xval > epsval) :
                    ibad = 1
                    if debug :
                        print "digits differ by %16.9f , %s %s " % (xval,rwords[i],twords[i])
                elif debug : 
                    print "digits same by %16.9f , %s %s " % (xval,rwords[i],twords[i])


              elif (rwords[i].find("E+")>0 and twords[i].find("E+")>0)  :
                xval=abs(float(rwords[i])-float(twords[i]))       
                if (xval > epsval) :
                    ibad = 1
                    if debug : 
                        print "numbers E+ differ by %16.9f , %s %s " % (xval,rwords[i],twords[i])
                elif debug : 
                    print "numbers E+ same by %16.9f , %s %s " % (xval,rwords[i],twords[i])


              elif (rwords[i].find("E-")>0 and twords[i].find("E-")>0)  :
                xval=abs(float(rwords[i])-float(twords[i]))       
                if (xval > epsval) :                         
                    ibad = 1
                    if debug : 
                        print "numbers E- differ by %16.9f , %s %s " % (xval,rwords[i],twords[i])
                elif debug : 
                    print "numbers E- same by %16.9f , %s %s " % (xval,rwords[i],twords[i])


              elif (rwords[i].find("e+")>0 and twords[i].find("e+")>0)  :
                xval=abs(float(rwords[i])-float(twords[i]))       
                if (xval > epsval) :                         
                    ibad = 1
                    if debug : 
                        print "numbers e+ differ by %16.9f , %s %s " % (xval,rwords[i],twords[i])
                elif debug : 
                    print "numbers e+ same by %16.9f , %s %s " % (xval,rwords[i],twords[i])

              elif (rwords[i].find("e-")>0 and twords[i].find("e-")>0)  :
                xval=abs(float(rwords[i])-float(twords[i]))       
                if (xval > epsval) :                         
                    ibad = 1
                    if debug : 
                        print "numbers e- differ by %16.9f , %s %s " % (xval,rwords[i],twords[i])
                elif debug : 
                    print "numbers e- same by %16.9f , %s %s " % (xval,rwords[i],twords[i])
     
              else :
                if (rwords[i] != twords[i]) :
                   if debug : print "words differ "+rwords[i]+" and "+twords[i] 
# check for -0 and 0 
                   ibad = 1
                else :
                   if debug : print "words same "+rwords[i]+" and "+twords[i] 

#          Done Loop through word by word

#    for lines with different number of words, assume difference 
     else :
        if debug : print "Lines differ in length" 
        ibad = 1

#    compare for test and reference pair done 
     ifa = ifa + ibad
     if debug or ibad : 
        print tmp1+"\n"+tmp2+"\n"
        wfile.write(tmp1+"\n"+tmp2+"\n")

  if debug : print "end routine tcnt,ico,iju,ifa: ",tcnt,ico,iju,ifa  
#-Done Loop over each pair of lines 0 to max(rcnt, tcnt)


  return ifa, ico, iju
# 
#    alternate method for checking, save in case needed
#    squish all together to see if match
#    rline=string.join(rwords,"")
#    tline=string.join(twords,"")
#    if (rline == tline) :  print "Match found: "+rline

     

#------------------------------------------------------------------------------

##############################################################################
# MAIN begin
#
#------------------------------------------------------------------------------
# if __name__ == "__main__":
def Check(**args):

  debug = 0
  JUNK='\n \t'
  errList=[]
  errmess=[]
  rlines=[]
  tlines=[]
  nlines = 1
  rmspace = 0
  ierr=0
  ifail=0
  nfail=0
  ndirs=0
  result_dir = 0
  target=args['target']

# get platform
  osname="unknown"
  osname= string.lower(sys.platform)
  if osname.find("linux") >= 0 :
     ostag="_lin" 
  elif osname.find("sun")>=0 or osname.find("sol")>=0 :
     ostag="_sun" 
  elif osname.find("darwin")>= 0 :
     ostag="_mac" 
  elif osname.find("irix")>= 0 :
     ostag="_sgi" 
  else :
     ostag="" 


# define top directory as current directory
  dtop = os.curdir
  dtop_path = os.getcwd()
  fout = dtop_path+"/diffout"+ostag+".txt"

  date = time.ctime()
  buff = "Start in directory: "+dtop_path+" at "+date
  print buff

  fref = "/reference/outx3dgen"
  ftest = "/outx3dgen"

# for each test directory
# main loop

  """if len(sys.argv) > 1 :
    dirnames = sys.argv[1:]
    fout=dtop_path+"/diffout"+ostag+"_select.txt"
  else :
    dirnames = os.listdir(dtop)"""

  print target
  print dtop
  if target == dtop:
  	dirnames = os.listdir(dtop)
  else:
  	dirnames = [target]
  	fout = dtop_path + "/diffout" + ostag + "_select.txt"

  wfile = open(fout,'w')
  wfile.write(buff+"\n")
  wfile.close()

  for name in dirnames :
    dwork = os.path.join(dtop, name)

#---skip results directory until end 
    if (dwork == "./test_results") : 
        result_dir = 1 

#---check output for each directory and reference 
#   header --- old reference file +++ new test file 
    elif os.path.isdir(dwork) : 
        wfile = open(fout,'a')
        wfile.write("\n"+"Diff Summary ==================================================================="+"\n\n")
        ndirs=ndirs+1
        buff= repr(ndirs)+" Check Directory "+dwork+" --------------------------"
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
          ifail=0
          iprintatt=0

          nlines=0
          diff = difflib.unified_diff(fromlines, tolines, fromfile, tofile, fromdate, todate, nlines)
          diffcopy = difflib.unified_diff(fromlines, tolines, fromfile, tofile, fromdate, todate, nlines)

#--------       
#         Loop through lines in diff result 
#         write full result to diffout_*.txt, summary to screen
#         Check the chunk from diff formatted result
#         start with line @@ -lineno,chunksize +lineno,chunksize @@ 
#         followed by chunksize number of lines
#         later versions of diff allow a single number
#         ' 'for common line -in reference file  +in test file

          chunk = 0
          for line in diff :
            words = line.split(None)

#           check to see if this is a new set of lines
#           if chunksize are equal, compare line pairs
#           otherwise print the whole chunk
            if words[0].find("@") > -1 :
               oldlins = string.split(words[1], ',' )
               newlins = string.split(words[2], ',' )
               tlineno= int(newlins[0][1:])

#          versions of python greater than 2.5 allow single value instead of pairs
#          avoid indexing number that may not exist
               if (len(newlins) > 1) :
                  tnum= int(newlins[1])
               else :
                  tnum = 0
               if (len(oldlins) > 1) :
                  rnum= int(oldlins[1])
               else :
                  rnum = 0

               if (tnum == rnum ) : 
                  chdr="\n"+"Test has "+repr(tnum)+" diffs at line "+repr(tlineno)+" >>"
               elif (tnum > rnum) :
                  ifail=ifail+(tnum-rnum)
                  chdr="\n"+"Test has "+repr(tnum)+" diffs at line "+repr(tlineno)+" >>"
                  chdr=chdr+"\n"+"Test has "+repr(tnum-rnum)+" extra lines in this chunk."
               elif (rnum > tnum) :
                  ifail=ifail+(rnum-tnum)
                  chdr="\n"+"Test has "+repr(tnum)+" diffs at line "+repr(tlineno)+" >>"
                  chdr=chdr+"\n"+"Test has "+repr(rnum-tnum)+" missing lines in this chunk."
               chunk = 1
               nread = tnum+rnum
               iread = 0
               rcnt = 0
               tcnt = 0

            if (chunk == 1) :
#              compare a chunk of line pairs output from diff
#              tlines are from test, rlines are from reference
               if line[0] =="-" :
                  rlines.append(line[1:])
                  rcnt= rcnt+1
                  iread=iread+1 
               elif line[0] =="+" :
                  tlines.append(line[1:])
                  tcnt=tcnt+1
                  iread=iread+1 
               else :
                  if (iread > 0) :
                     tmp=line.rstrip()
                     print tmp
                     wfile.write(tmp+"\n")

#              chunk has been read  
               if (chunk ==  1 and iread == nread ) : 
                  if debug : print "Compare chunk ==============================================="
                  print chdr 
                  wfile.write(chdr+"\n")
                  ifa, ico, iju = diff_chunk(rlines,tlines,rcnt,tcnt, wfile)
                  ifail = ifail + ifa
                  icomment = icomment + ico
                  ijunk = ijunk + iju
                  rlines=[]
                  tlines=[]
                  chunk = 0
                  buff= "Lines Essentially the Same: "+repr(tcnt-ifa)+" out of "+repr(tnum) 
                  print buff
                  wfile.write(buff+"\n")


#            else : 
##              check single lines
##              skip commented and junk lines
#               if (len(words) > 0) :
#                  if words[0].find("*") > -1 :
#                     icomment=icomment+1
#                  elif words[0].find("#") > -1 :
#                     icomment=icomment+1
#                  elif words[0].find("+++") > -1 :
#                     ijunk=ijunk+1
#                  elif words[0].find("---") > -1 :
#                     ijunk=ijunk+1
#                  elif words[0].find("finish") > -1 :
#                     ijunk=ijunk+1
#                  elif len(line) < 4 :
#                     ijunk=ijunk+1
#                  else :
#                     if words[0]=="+" : ifail=ifail+1
#
##                 write a line 
#                  tmp=line.rstrip()
#                  print tmp
#                  wfile.write(tmp+"\n")

#--------       
#         Done with loop through lines in diff result
          print " "
          if icomment : 
             buff= repr(icomment)+" comment lines."
             wfile.write(buff+"\n")
             print "Removed "+buff
          if ijunk : 
             buff = repr(ijunk)+" junk lines."
             wfile.write(buff+"\n")
             print "Removed "+buff
          if ifail : 
            buff = repr(ifail)+" lines failed."
            wfile.write(buff+"\n")
            print buff
            errList.append(dwork)
            errmess.append(repr(ifail)+" lines failed.")
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
        if ifail > 0 : nfail=nfail+1
        wfile.write("Check done."+"\n\n")
        wfile.write("Full Report from diff ========================================================="+"\n\n")
        wfile.writelines(diffcopy)
        wfile.write("\n"+"End Report from diff ========================================================="+"\n")
        wfile.close()

#---done with work in lower directory
           
# end main loop
  wfile = open(fout,'a')
  if nfail : 
    buff = "All checks complete, "+repr(nfail)+" directories failed out of "+repr(ndirs)
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

  wfile.write("\n")
  wfile.close()
  if result_dir :
    b = os.system('cp -p '+fout+' ./test_results')
    print "Check done."+"\n"+"Full result written to: "+"\n"
    print fout+"\n"
    print "and copied to ./test_results "+"\n"

#    fromfile = "./test_results/diffout"+ostag+".txt" 
#    tofile="./test_results/reference/diffout"+ostag+".txt" 
#    print  "Compare "+fromfile+" to "+tofile 
#    fromlines = open(fromfile, 'U').readlines()
#    tolines = open(tofile, 'U').readlines()
#    fdiff = difflib.ndiff(fromlines, tolines)
#    sys.stdout.writelines(fdiff) 

  else :
    print "Check done."+"\n"+"Full result written to "+fout+"\n"
      
# end Main 
#------------------------------------------------------------------------------

