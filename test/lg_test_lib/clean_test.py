import os

def Clean(tag:str=None):
    '''
    Removes all output files from a directory.
    '''

    # cd dir?
    dirs = os.listdir(os.curdir)
    
    for directory in dirs:
        try:
            if os.path.isdir(directory):
                if directory == "test_results":
                    print("Skipping directory: %s\n" % directory)
                else:
                    print("Directory: %s" % directory)
                    for the_file in os.listdir(directory):
                        file_path = os.path.join(directory,the_file)
                        try:
                            if os.path.isfile(file_path) \
                               and (the_file.startswith("out") \
                               or the_file.endswith("gmvF") \
                               or the_file.endswith("x3dgen")):
                                os.remove(file_path)
                        except Exception as e:
                            print(e)
                    print("\tClean.")
            elif os.path.isfile(directory) and not directory.endswith(".old.txt"):
                if directory.startswith("stdout_" + tag):
                    print("FOUND STDOUT")
                    os.rename(directory, directory[:-4] + ".old" + directory[-4:])
                elif directory.startswith("diffout_" + tag):
                    print("FOUND DIFFOUT_")
                    os.rename(directory, directory[:-4] + ".old" + directory[-4:])
        except Exception as e:
            print(e)
    print("Done. All output files removed from directories.\n")