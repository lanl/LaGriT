#! /n/local_linux/epd/bin/python2.7
#
# /usr/local/bin/python -> python3.2  
# NOTE: this uses python before vers 3
#        newer versions use print as function not statement
#
# for vers 3 lanl machines /usr/bin/env python
# for sgi /usr/lanl/bin/python
#------------------------------------------------------------------------------
#  Name: run_test.py
#  Last Modified: Jan 2008 by TAM tamiller@lanl.gov 
#
#  Need to:
#  add debug runs
#  test error captures
#
#------------------------------------------------------------------------------

import fileinput, string, os, sys, datetime, time, shutil
__all__ = ["directoryList", "RunTest"]

##############################################################################
# Routines listed here, main at bottom
#
#------------------------------------------------------------------------------
# Routine: directoryWalker()
# recursively walks through all directories 
#------------------------------------------------------------------------------
def directoryList( unused, dirName, fileList ):
    
    icount = 0
    for entry in fileList:
        icount = icount+1 
        
    print dirName+" has "+repr(icount)+" files."

##############################################################################
# MAIN begin
#
# call lagrit with driver
# starting programs from the command line 
# os.system(cmd) or fo1 = os.popen(cmd) fo1.close()
#
#------------------------------------------------------------------------------

# executes the tests in directories
def RunTest(**args):

  # dirList = []
  errList = []
  errmess = []
  errors = {'general': []} # USE THIS EVENTUALLY
  ierr = 0
  itest = 0
  osname="unknown"
  # ostag=""
  result_dir = 0
  tag = args['tag'] # output file name tag
  dtop = os.curdir # getting top level directory
  dtop_path = os.getcwd() # getting filepath to top directory
  osname = sys.platform # getting platform
  directories = os.listdir(dtop)

  # define executable
  #xlagrit = "/n/swdev/LAGRIT/bin/lagrit_ulin_g_gf_V3.107.so"
  # xlagrit = "/scratch/sft/yanki/lagrit/src/mylagrit"
  # xlagrit = "Y:/yanki/lagrit/src/mylagrit"
  xlagrit = args["executable"]

  print("=======")

  for name in directories:
    # if directory exists, add to dict of errors
    if os.path.isdir(name) : 
        errmess.append("empty")
        errors[name] = []

# define top directory as current directory
  fscreen = dtop_path + "/stdout_" + tag + ".txt"
  outfile = "stdout_" + tag + ".txt"
  date = time.ctime()
  # wfile = open(fscreen, 'w')
  wfile = open(outfile, 'w')
  line = ("Operating System:\t" + osname + "\n" +
        "Executable:\t\t" + xlagrit + "\n" +
        "Top directory:\t\t" + dtop_path + "\n" +
        "Out file:\t\tstdout_" + tag + ".txt\n\tOn " + date)
  print(line)
  print("=======")
  wfile.write(line + "\n")
  wfile.close()

# for each test directory
# main loop
  # for index, name in enumerate(os.listdir(dtop)):
  for name in directories:
    # if index > 2:
    #     result_dir = 1
    #     continue
    dwork = os.path.join(dtop, name)

#---skip results directory until end
    if name == "test_results":
        result_dir = 1
#---go into each directory and do some work
    elif os.path.isdir(name):

        errmess.append("empty")
        os.chdir(name)

        # itest = itest + 1
        itest += 1
        line = (" " + str(itest) + "  Test Directory " + name + " -----------------------")
        print line
        wfile = open(fscreen, 'a')
        wfile.write(line + "\n")
        wfile.close()

        if os.path.exists("outx3dgen"):
            shutil.copyfile("outx3dgen", "prev_outx3dgen")
            # for f in [ f for f in os.listdir('.') if f.startswith("out")]:
            #     os.remove(f)
            os.remove("outx3dgen")

        if (os.path.exists("input.lgi")) : 
          cmd = xlagrit + " < input.lgi >> " + fscreen
          print cmd
          fo1 = os.system(cmd)
          if fo1 != 0:
            print("System exit: %s" % fo1)
            errList.append(repr(itest) + " " + dwork)
            errmess[ierr] = "Exit code: " + repr(fo1)
            ierr = ierr + 1
            errors[name].append(str(itest) + " ERROR: Cannot execute input.\nExit code: " + str(fo1))
        else:
          print("ERROR: File missing: input.lgi") 
          errList.append(repr(itest) + " " + dwork)
          errmess[ierr] = "Missing LaGriT input file."
          ierr = ierr + 1
          errors[name].append(str(itest) + " ERROR: input.lgi file does not exist.")
        
        os.chdir(dtop_path)
#---done with work in lower directory
           
# end main loop
  wfile.close()
  print("Testing Done.")

# search outx3dgen files for key start and end phrases
  progstr = "Program header not found. "
  sustr = "Program not completed. "
  nfind = 0
  rfile = open(fscreen,'r')
  # outx3dgen = rfile.readlines()
  for line in rfile.readlines():
      
      dirno = line.find("Test Directory")
      progno = line.find("Program")
      suno = line.find("successfully")
      # print dirno, progno, suno
      if dirno >= 0:
          dirstr="Check outx3dgen "+line[:50]
      if progno >= 0 :
          progstr="   "+line[20:55] 
      if suno >= 0 :
          sustr=line[:29] 
          print dirstr
          print progstr + " : " + sustr
          nfind = nfind+1
  rfile.close()

# attempt to pass error conditions if found
  if (ierr > 0) :
    i = 0
    print
    print  "LAGRIT EXIT ERROR: "+repr(ierr)+" directories failed:"+"/n"
    for d in errList :
      print "    "+errList[i]+" Error: "+errmess[i]
      print "---- tail outx3dgen ------------------"
      cmd="tail "+errList[i]+"/outx3dgen"
      fo1 = os.system(cmd)
      print "--------------------------------------"
      print " "
      i = i + 1

  print("\nSummary:\t\t%s completed outx3dgen files out of %s test directories" % (repr(nfind), repr(itest)))
  if result_dir:
    shutil.copyfile(outfile, "./test_results/" + outfile)
    print("Output written to:\t%s\nAnd moved to:\t\t./test_results\n" % outfile)
  else:
    errors['general'].append("Warning: No test_results directory.")
    print("LaGriT outx3dgen and screen output written to: %s\n" % outfile)
      
# end Main 
#------------------------------------------------------------------------------
