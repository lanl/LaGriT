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
# ------------------------------------------------------------------------------
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
# ------------------------------------------------------------------------------

import fileinput, array, string, os, difflib, sys, datetime, time, copy
from typing import List
from .logger import log

__all__ = ["fail", "rstrip", "diff_chunk", "Check"]


class WFileProp(object):
    """
    In the original LaGriT diff, there
    was explicit file writing.
    This is a dummy class to mimic file writing
    while retaining the benefits of not having
    to modify any existing syntax.
    """
    def __init__(self):
        pass
    
    def write(self, *args):
        log.info(' '.join([str(x) for x in args]))

def remove_junk_from_filestream(
    lines, banner="-----oOo-----", comment_chars=["#", "!", "*"]
):
    """
    Removes comments, headers, and whitespace from a list of strings,
    parsed from a lagrit.out file.
    """
    newlines = []
    in_banner = False

    for line in lines:
        stripped = line.strip()

        if stripped == "":
            continue
        elif stripped[0] in comment_chars:
            continue
        elif banner in stripped:
            in_banner = not in_banner
        elif "log file: " in stripped:
            continue

        if not in_banner:
            newlines.append(stripped)

    return newlines[1:]

# ------------------------------------------------------------------------------
def rstrip(line, JUNK="\n \t "):
    i = len(line)
    while i > 0 and line[i - 1] in JUNK:
        i -= 1
    return line[:i]


# ------------------------------------------------------------------------------
# compare using additional checks to diff


def diff_chunk(rlines, tlines, rcnt, tcnt, wfile):

    # need large epsilon to compare numbers  1.73469E+02 to 1.734693909E+02
    epsval = 0.001
    tmp = []
    tmp1 = []
    tmp2 = []
    ibad = 0
    ifa = 0
    ico = 0
    iju = 0
    count = 0
    debug = 0

    # -Loop over each pair of lines 0 to tcnt
    count = max(tcnt, rcnt)
    for idx in range(count):
        rcount = 0
        tmp1 = "-"
        if idx < rcnt:
            rwords = rlines[idx].split(None)
            rcount = len(rwords)
            tmp = "-" + rlines[idx]
            tmp1 = tmp.rstrip()
        tcount = 0
        tmp2 = "+"
        if idx < tcnt:
            twords = tlines[idx].split(None)
            tcount = len(twords)
            tmp = "+" + tlines[idx]
            tmp2 = tmp.rstrip()

        #    for lines with same number of words, compare values word by word
        if tcount == rcount and tcount > 0:

            ibad = 0
            #       check to see if these are lines to skip
            if twords[0].find("*") > -1:
                ico = ico + 1
                if debug:
                    print("comment line " + repr(idx) + tlines[idx])
            elif twords[0].find("#") > -1:
                ico = ico + 1
                if debug:
                    print("comment line " + repr(idx) + tlines[idx])
            elif len(twords) < 2:
                iju = iju + 1
                if debug:
                    print("junk line " + repr(idx) + tlines[idx])

            else:

                #          Loop through word by word
                if debug:
                    print("epsilon compare: %16.9f \n" % epsval)
                for i in range(len(rwords)):

                    if rwords[i].isalpha() and twords[i].isalpha():
                        if rwords[i] == twords[i]:
                            if debug:
                                print("alpha same " + rwords[i] + " and " + twords[i])

                        else:
                            if debug:
                                print("alpha differ " + rwords[i] + " and " + twords[i])
                            ibad = 1

                    elif rwords[i].isdigit() and twords[i].isdigit:
                        xval = abs(float(rwords[i]) - float(twords[i]))
                        if xval > epsval:
                            ibad = 1
                            if debug:
                                print(
                                    "digits differ by %16.9f , %s %s "
                                    % (xval, rwords[i], twords[i])
                                )
                        elif debug:
                            print(
                                "digits same by %16.9f , %s %s "
                                % (xval, rwords[i], twords[i])
                            )

                    elif rwords[i].find("E+") > 0 and twords[i].find("E+") > 0:
                        xval = abs(float(rwords[i]) - float(twords[i]))
                        if xval > epsval:
                            ibad = 1
                            if debug:
                                print(
                                    "numbers E+ differ by %16.9f , %s %s "
                                    % (xval, rwords[i], twords[i])
                                )
                        elif debug:
                            print(
                                "numbers E+ same by %16.9f , %s %s "
                                % (xval, rwords[i], twords[i])
                            )

                    elif rwords[i].find("E-") > 0 and twords[i].find("E-") > 0:
                        xval = abs(float(rwords[i]) - float(twords[i]))
                        if xval > epsval:
                            ibad = 1
                            if debug:
                                print(
                                    "numbers E- differ by %16.9f , %s %s "
                                    % (xval, rwords[i], twords[i])
                                )
                        elif debug:
                            print(
                                "numbers E- same by %16.9f , %s %s "
                                % (xval, rwords[i], twords[i])
                            )

                    elif rwords[i].find("e+") > 0 and twords[i].find("e+") > 0:
                        xval = abs(float(rwords[i]) - float(twords[i]))
                        if xval > epsval:
                            ibad = 1
                            if debug:
                                print(
                                    "numbers e+ differ by %16.9f , %s %s "
                                    % (xval, rwords[i], twords[i])
                                )
                        elif debug:
                            print(
                                "numbers e+ same by %16.9f , %s %s "
                                % (xval, rwords[i], twords[i])
                            )

                    elif rwords[i].find("e-") > 0 and twords[i].find("e-") > 0:
                        xval = abs(float(rwords[i]) - float(twords[i]))
                        if xval > epsval:
                            ibad = 1
                            if debug:
                                print(
                                    "numbers e- differ by %16.9f , %s %s "
                                    % (xval, rwords[i], twords[i])
                                )
                        elif debug:
                            print(
                                "numbers e- same by %16.9f , %s %s "
                                % (xval, rwords[i], twords[i])
                            )

                    else:
                        if rwords[i] != twords[i]:
                            if debug:
                                print("words differ " + rwords[i] + " and " + twords[i])
                            # check for -0 and 0
                            ibad = 1
                        else:
                            if debug:
                                print("words same " + rwords[i] + " and " + twords[i])

        #          Done Loop through word by word

        #    for lines with different number of words, assume difference
        elif tcount != rcount and tcount > 0:
            if twords[0].find("*") > -1 or twords[0].find("#") > -1:
                ico = ico + 1
                if debug:
                    print("Comment lines differs in length")
        else:
            if debug:
                print("Lines differ in length")
            ibad = 1

        #    compare for test and reference pair done
        ifa = ifa + ibad
        if debug or ibad:
            print(tmp1 + "\n" + tmp2 + "\n")
            wfile.write(tmp1 + "\n" + tmp2 + "\n")

    if debug:
        print("end routine tcnt,ico,iju,ifa: ", tcnt, ico, iju, ifa)
    # -Done Loop over each pair of lines 0 to max(rcnt, tcnt)

    return ifa, ico, iju

def diff(output_test: str, output_reference: str):
    """Computes diff between files.

    Args:
        output_test (List[str]): Output file to be tested.
        output_reference (List[str]): Reference output file.
    """

    wfile = WFileProp()

    fromfile = output_reference
    tofile = output_test

    fromlines = tolines = None
    with open(fromfile, 'r', newline=None) as f:
        fromlines = f.readlines()
    with open(tofile, 'r', newline=None) as f:
        tolines = f.readlines()

    fromdate = time.ctime(os.stat(fromfile).st_mtime)
    todate = time.ctime(os.stat(tofile).st_mtime)

    # Remove comments + banner
    fromlines = remove_junk_from_filestream(fromlines)
    tolines = remove_junk_from_filestream(tolines)

    icomment = 0
    ijunk = 0
    ifail = 0
    iprintatt = 0
    debug = 0
    JUNK = "\n \t"
    errList = []
    errmess = []
    rlines = []
    tlines = []
    nlines = 1
    rmspace = 0
    ierr = 0
    ifail = 0
    nfail = 0
    ndirs = 0
    result_dir = 0
    diffcopy = ""
    nlines = 0

    diff = difflib.unified_diff(
        fromlines, tolines, fromfile, tofile, fromdate, todate, nlines
    )
    diffcopy = difflib.unified_diff(
        fromlines, tolines, fromfile, tofile, fromdate, todate, nlines
    )

    # --------
    #         Loop through lines in diff result
    #         write full result to diffout_*.txt, summary to screen
    #         Check the chunk from diff formatted result
    #         start with line @@ -lineno,chunksize +lineno,chunksize @@
    #         followed by chunksize number of lines
    #         later versions of diff allow a single number
    #         ' 'for common line -in reference file  +in test file

    chunk = 0
    for line in diff:
        words = line.split(None)

        #           check to see if this is a new set of lines
        #           if chunksize are equal, compare line pairs
        #           otherwise print the whole chunk
        if words[0].find("@") > -1:
            oldlins = words[1].split(",")
            newlins = words[2].split(",")
            tlineno = int(newlins[0][1:])

            #          versions of python greater than 2.5 allow single value instead of pairs
            #          avoid indexing number that may not exist
            if len(newlins) > 1:
                tnum = int(newlins[1])
            else:
                tnum = 0
            if len(oldlins) > 1:
                rnum = int(oldlins[1])
            else:
                rnum = 0

            if tnum == rnum:
                chdr = (
                    "\n"
                    + "Test has "
                    + repr(tnum)
                    + " diffs at line "
                    + repr(tlineno)
                    + " >>"
                )
            elif tnum > rnum:
                ifail = ifail + (tnum - rnum)
                chdr = (
                    "\n"
                    + "Test has "
                    + repr(tnum)
                    + " diffs at line "
                    + repr(tlineno)
                    + " >>"
                )
                chdr = (
                    chdr
                    + "\n"
                    + "Test has "
                    + repr(tnum - rnum)
                    + " extra lines in this chunk."
                )
            elif rnum > tnum:
                ifail = ifail + (rnum - tnum)
                chdr = (
                    "\n"
                    + "Test has "
                    + repr(tnum)
                    + " diffs at line "
                    + repr(tlineno)
                    + " >>"
                )
                chdr = (
                    chdr
                    + "\n"
                    + "Test has "
                    + repr(rnum - tnum)
                    + " missing lines in this chunk."
                )
            chunk = 1
            nread = tnum + rnum
            iread = 0
            rcnt = 0
            tcnt = 0

        if chunk == 1:
            #              compare a chunk of line pairs output from diff
            #              tlines are from test, rlines are from reference
            if line[0] == "-":
                rlines.append(line[1:])
                rcnt = rcnt + 1
                iread = iread + 1
            elif line[0] == "+":
                tlines.append(line[1:])
                tcnt = tcnt + 1
                iread = iread + 1
            else:
                if iread > 0:
                    tmp = line.rstrip()
                    wfile.write(tmp + "\n")

            #              chunk has been read
            if chunk == 1 and iread == nread:
                if debug:
                    print(
                        "Compare chunk ==============================================="
                    )
                wfile.write(chdr + "\n")
                ifa, ico, iju = diff_chunk(
                    rlines, tlines, rcnt, tcnt, wfile
                )
                ifail = ifail + ifa
                icomment = icomment + ico
                ijunk = ijunk + iju
                rlines = []
                tlines = []
                chunk = 0

                count_1 = abs(int(repr(tcnt - ifa)))
                count_2 = abs(int(repr(tnum)))

                buff = "Lines Essentially the Same: {0} out of {1}".format(
                    min(count_1, count_2), max(count_1, count_2)
                )
                wfile.write(buff + "\n")

    # --------
    #         Done with loop through lines in diff result
    if icomment:
        buff = repr(icomment) + " comment lines."
        wfile.write(buff + "\n")
    if ijunk:
        buff = repr(ijunk) + " junk lines."
        wfile.write(buff + "\n")
    if ifail:
        buff = repr(ifail) + " lines failed."
        wfile.write(buff + "\n")
        wfile.write(repr(ifail) + " lines failed.")
        ierr += 1
        return False
    else:
        buff = "No lines differ."
        wfile.write(buff + "\n")
    
    return True