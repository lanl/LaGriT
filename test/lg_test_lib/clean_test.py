import os


def CleanSingleDir(directory: str):
    """
    Cleans a single directory.

    Arguments
    ---------
       directory (str): directory path to clean
    """
    if directory == "test_results":
        print("Skipping directory: %s\n" % directory)
    elif directory == "reference":
        return
    else:
        print("Directory: %s" % directory)
        for the_file in os.listdir(directory):
            file_path = os.path.join(directory, the_file)
            try:
                if os.path.isfile(file_path) and (
                    the_file.startswith("out")
                    or the_file.endswith("gmvF")
                    or the_file.endswith("x3dgen")
                ):
                    os.remove(file_path)
            except Exception as e:
                print(e)
        print("\tClean.")


def Clean(tag: str = None):
    """
    Removes all output files from all subdirectories within the
    current working directory.
    """

    # cd dir?
    dirs = os.listdir(os.curdir)

    for directory in dirs:
        try:
            if os.path.isdir(directory):
                CleanSingleDir(directory)
            elif os.path.isfile(directory) and not directory.endswith(".old.txt"):
                if directory.startswith("stdout_" + tag):
                    os.rename(directory, directory[:-4] + ".old" + directory[-4:])
                elif directory.startswith("diffout_" + tag):
                    os.rename(directory, directory[:-4] + ".old" + directory[-4:])
        except Exception as e:
            print(e)
    print("Done. All output files removed from directories.\n")
