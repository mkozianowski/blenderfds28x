import os
import sys
import bpy
import difflib
import subprocess
import tempfile

from pprint import pprint
from xml.dom import minidom
from datetime import datetime
from xml.sax.saxutils import escape

#Routine to execute FDS on two files
#Returns: TRUE/FALSE []
#         string     []
def fds_run( file_fds ):

   #Modifying FDS input file as follows
   #   -> if not present add &MESH /
   #   -> set T_END to 0 ( &TIME T_END=0.0 / )

   addMesh = True

   with open( file_fds) as myfile:
      if '&MESH' in myfile.read():
         addMesh = False

   with open( file_fds, "r") as f:
      lines = f.readlines()

   with open( file_fds, "w") as f:
      for line in lines:

         #removing tail (we add all the new strings at the end of file)
         if line.strip("\n") != "&TAIL /" and line.find("&TIME"):
            f.write(line)

      if ( addMesh ):
          f.write( "&MESH /\n\n")

      f.write( "&TIME T_END=0.0 /\n\n" )

      f.write( "&TAIL /")


   #TODO set ulimit -s unlimited to allow RAM usage
   myCmd = 'fds ' + file_fds

   #Process execution
   print( myCmd)
   p = subprocess.Popen( myCmd, cwd='/tmp/', \
                 stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True )

   out,err = p.communicate()

   print ( 'OUT = ')
   print (  out)
   print ( 'ERR = ')
   print (  err)

#Routine to compare two files .FDS
#Returns: TRUE/FALSE [are the two files equals?]
#         string     [diff of the two files]
def compare_fds_files( filea, fileb ):

   if ( os.path.isfile( filea ) ):
      text1 = open( filea ).readlines()
   else:
      return [ False, "Missing file: " + filea ]

   if ( os.path.isfile( fileb ) ):
      text2 = open( fileb ).readlines()
   else:
      return [ False, "Missing file: " + fileb ]

   text1.pop( 0)
   text1.pop( 0)
   text2.pop( 0)
   text2.pop( 0)

   fds_equals = True
   string     = ""

   for line in difflib.unified_diff(text1, text2, n=0):
       if ( line[:3] == "---" ):
          continue

       if ( line[:3] == "+++" ):
          continue

       if ( line[:3] == "@@ " ):
          continue

       #Removing old comment lines
       if ( line[:3] == "-! " ):
          continue

       #Removing new comment lines
       if ( line[:3] == "+! " ):
          continue

       if ( not line.endswith("\n") ):
          line = line + " NEW_LINE_MISSING\n"

       string = string + line 
       fds_equals = False

   return [fds_equals, string]

#Routine to execute the check of a .blend or .fds file
#Input  : dirpath    [path to the directory with "Blender_Input_Files" and "FDS_Input_Files" folders within]
#         filename   [name of the .blend or .fds file to be tested]
#Returns: string     [the test result]
#         string     [the note of the test]
def do_check(dirpath, filename):

    result = None
    note   = None

    with tempfile.NamedTemporaryFile(suffix=".fds", delete=True) as temporaryFile:

        try:
            filepath_fds = None

            # .blend input
            if filename.endswith(".blend"):
                filepath_bln = os.path.join(dirpath, DIR_NAME_BLN2FDS, filename)
                filepath_fds = os.path.join(dirpath, DIR_NAME_FDS2FDS, filename.replace(".blend", ".fds"))
                bpy.ops.wm.open_mainfile(filepath=filepath_bln)
                
            # .fds input
            elif filename.endswith(".fds"):
                filepath_fds = os.path.join(dirpath, DIR_NAME_FDS2FDS, filename)
                bpy.ops.import_scene.fds(filepath=filepath_fds)
            
            else:
                raise ValueError("Invalid check type")

            # test
            bpy.ops.export_scene.fds(filepath=temporaryFile.name)
            compare = compare_fds_files(filepath_fds, temporaryFile.name)
            result = "OK" if compare[0] else "ERROR"
            note   = compare[1]

            #new FDS execution
            print ( "@@@@@ FDS RUN @@@@")
            fds_run( temporaryFile.name )
        
        except Exception as e:
            result = "EXCEPTION"
            note   = str(e)
    
    return [result, note]

#Routine to append a new Case tag with a test results
#Input  : xml             [results.xml root tag]
#         results         [Results tag to be modified]
#         contentName     [Name tag content]
#         contentType     [Type tag content]
#         contentInput    [Input tag content]
#         contentResult   [Result tag content]
#         contentNote     [Note tag content]
def append_case(xml, results, contentName, contentType, contentInput, contentResult, contentNote):

    def escape_text(text):
        return escape(text.encode("unicode_escape").decode("utf-8"))
    
    nodeName = xml.createElement("Name")
    nodeText = xml.createTextNode(escape_text(contentName))
    nodeName.appendChild(nodeText)

    nodeType = xml.createElement("Type")
    nodeText = xml.createTextNode(escape_text(contentType))
    nodeType.appendChild(nodeText)

    nodeInput = xml.createElement("Input")
    nodeText  = xml.createTextNode(escape_text(contentInput))
    nodeInput.appendChild(nodeText)

    nodeResult = xml.createElement("Result")
    nodeText   = xml.createTextNode(escape_text(contentResult))
    nodeResult.appendChild(nodeText)

    nodeNote = xml.createElement("Note")
    nodeText = xml.createTextNode(escape_text(contentNote))
    nodeNote.appendChild(nodeText)

    nodeCase = xml.createElement("Case")
    nodeCase.appendChild(nodeName)
    nodeCase.appendChild(nodeType)
    nodeCase.appendChild(nodeInput)
    nodeCase.appendChild(nodeResult)
    nodeCase.appendChild(nodeNote)
    results.appendChild(nodeCase)

#==================================================================

# parameters
PATH_TO_VALIDATION = os.path.dirname(os.path.realpath(__file__))
PATH_TO_RESULTS    = os.path.join(PATH_TO_VALIDATION, "results.xml")

DIR_NAME_BLN2FDS = "Blender_Input_Files"
DIR_NAME_FDS2FDS = "FDS_Input_Files"

RESULT_EMPTY = """<?xml version="1.0"?><testResults></testResults>"""
DATE_FORMAT = "%d-%m-%Y %H.%M"


# start of the verification script
xml = None

try:
    print("\n\n")
    print("####################################")
    print("UNIT TEST START")
    print("####################################")

    # opening the file results.xml if it exists, otherwise its initialization
    try:
        xml = minidom.parse(PATH_TO_RESULTS)
    except:
        xml = minidom.parseString(RESULT_EMPTY)
    
    root = xml.getElementsByTagName("testResults")[0]

    # removing any Results tags
    while xml.getElementsByTagName("Results").length >= 10:
        elements = xml.getElementsByTagName("Results")
        element_to_remove = None
        for element in elements:
            element_date = datetime.strptime(element.getAttribute("date"), DATE_FORMAT)
            element_to_remove = element if element_to_remove == None or element_date < datetime.strptime(element_to_remove.getAttribute("date"), DATE_FORMAT) else element_to_remove
        root.removeChild(element_to_remove)
    
    results = xml.createElement("Results")
    results.setAttribute("date", datetime.today().strftime(DATE_FORMAT))
    root.appendChild(results)

    # looping through the folders to be tested
    for dirname in next(os.walk(PATH_TO_VALIDATION))[1]:

        try:
            print("\n\n> Directory: " + dirname)
            print("----------------------------")

            dirpath = os.path.join(PATH_TO_VALIDATION, dirname)
            testXml = minidom.parse(os.path.join(dirpath, "test.xml"))
            blnfds = testXml.getElementsByTagName("blnfds")[0].firstChild.nodeValue == "true"
            fdsfds = testXml.getElementsByTagName("fdsfds")[0].firstChild.nodeValue == "true"

            # blend to fds test
            if blnfds:
                for filename in os.listdir(os.path.join(dirpath, DIR_NAME_BLN2FDS)):
                    if filename.endswith(".blend"):
                        print("> Test: blend to fds")
                        print("> Input: " + filename)
                        check = do_check(dirpath, filename)
                        append_case(xml, results, dirname, "blnfds", filename, check[0], check[1])
                        print("")

            # fds to fds test
            if fdsfds:
                for filename in os.listdir(os.path.join(dirpath, DIR_NAME_FDS2FDS)):
                    if filename.endswith(".fds"):
                        print("> Test: fds to fds")
                        print("> Input: " + filename)
                        check = do_check(dirpath, filename)
                        append_case(xml, results, dirname, "fdsfds", filename, check[0], check[1])
                        print("")
        
        except Exception as e:
            contentType = ""
            contentResult = "EXCEPTION"
            contentNote = str(e)
            append_case(xml, results, dirname, "", "", "EXCEPTION", str(e))

finally:
    if xml != None:

        # editing the results.xml file
        with open(PATH_TO_RESULTS, "w") as xmlFile:
            reparsed = minidom.parseString(xml.toprettyxml())
            string   = "\n".join([line for line in reparsed.toprettyxml(indent="\t").split("\n") if line.strip()])
            xmlFile.write(string)
            xml.unlink()
    
    print("")
    print("####################################")
    print("UNIT TEST END")
    print("####################################")
    print("\n\n")

