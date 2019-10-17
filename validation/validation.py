import os
import sys
import bpy
import difflib

from pprint import pprint
from xml.dom import minidom
from datetime import datetime
from xml.sax.saxutils import escape


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

       if ( not line.endswith('\n') ):
          line = line + ' NEW_LINE_MISSING\n'

       string = string + line 
       fds_equals = False

   return [fds_equals, string]

def do_check(checkType, dirpath, pathToExport):

    def get_filepath(path, ext):
        for filename in next(os.walk(path))[2]:
            if filename.endswith('.' + ext):
                return os.path.join(path, filename)

    result = None
    note   = None

    try:
        filepath_fds = get_filepath(os.path.join(dirpath, 'FDS_Input_Files'), 'fds')

        if checkType == "blnfds":
            filepath_blend = get_filepath(os.path.join(dirpath, 'Blender_Input_Files'), 'blend')
            bpy.ops.wm.open_mainfile(filepath=filepath_blend)
            
        elif checkType == "fdsfds":
            bpy.ops.import_scene.fds(filepath=filepath_fds)
        
        else:
            raise ValueError("Invalid check type")

        bpy.ops.export_scene.fds(filepath=pathToExport)
        compare = compare_fds_files(filepath_fds, pathToExport)
        result = "OK" if compare[0] else "ERROR"
        note   = compare[1]
    
    except Exception as e:
        result = "EXCEPTION"
        note   = str(e)
    
    return [result, note]

def append_case(xml, results, contentName, contentType, contentResult, contentNote):

    print (contentNote)

    def escape_text(text):
        return escape(text.encode("unicode_escape").decode("utf-8"))
    
    nodeName = xml.createElement("Name")
    nodeText = xml.createTextNode(escape_text(contentName))
    nodeName.appendChild(nodeText)

    nodeType = xml.createElement("Name")
    nodeText = xml.createTextNode(escape_text(contentType))
    nodeType.appendChild(nodeText)

    nodeResult = xml.createElement("Result")
    nodeText   = xml.createTextNode(escape_text(contentResult))
    nodeResult.appendChild(nodeText)

    nodeNote = xml.createElement("Note")
    nodeText = xml.createTextNode(escape_text(contentNote))
    nodeNote.appendChild(nodeText)

    nodeCase = xml.createElement('Case')
    nodeCase.appendChild(nodeName)
    nodeCase.appendChild(nodeType)
    nodeCase.appendChild(nodeResult)
    nodeCase.appendChild(nodeNote)
    results.appendChild(nodeCase)

#==================================================================

PATH_TO_VALIDATION = os.path.dirname(os.path.realpath(__file__))
PATH_TO_EXPORT     = os.path.join(PATH_TO_VALIDATION, 'export.fds')
PATH_TO_RESULTS    = os.path.join(PATH_TO_VALIDATION, 'results.xml')

RESULT_EMPTY = """<?xml version="1.0"?><testResults></testResults>"""

try:
    print("\n\n")
    print("####################################")
    print("UNIT TEST START")
    print("####################################")
    print("\n\n")

    try:
        xml = minidom.parse(PATH_TO_RESULTS)
    except:
        xml = minidom.parseString(RESULT_EMPTY)
    
    root = xml.getElementsByTagName('testResults')[0]

    results = xml.createElement('Results')
    results.setAttribute('date', datetime.today().strftime('%d-%m-%Y %H.%M'))
    root.appendChild(results)

    for dirname in next(os.walk(PATH_TO_VALIDATION))[1]:

        print("\n\n" + dirname)
        print("----------------------------")

        dirpath = os.path.join(PATH_TO_VALIDATION, dirname)
        testXml = minidom.parse(os.path.join(dirpath, 'test.xml'))
        blnfds = testXml.getElementsByTagName("blnfds")[0].firstChild.nodeValue == 'true'
        fdsfds = testXml.getElementsByTagName("fdsfds")[0].firstChild.nodeValue == 'true'

        # blnfds
        if blnfds:
            print("> Test: blend to fds")
            check = do_check("blnfds", dirpath, PATH_TO_EXPORT)
            append_case(xml, results, dirname, "blnfds", check[0], check[1])

        # fdsfds
        if fdsfds:
            print("> Test: fds to fds")
            check = do_check("fdsfds", dirpath, PATH_TO_EXPORT)
            append_case(xml, results, dirname, "fdsfds", check[0], check[1])
        
finally:
    if xml != None:
        with open(PATH_TO_RESULTS, "w") as xmlFile:
            reparsed = minidom.parseString(xml.toprettyxml())
            string   = '\n'.join([line for line in reparsed.toprettyxml(indent='\t').split('\n') if line.strip()])
            xmlFile.write(string)
            xml.unlink()
    
    print("\n\n")
    print("####################################")
    print("UNIT TEST END")
    print("####################################")
    print("\n\n")

