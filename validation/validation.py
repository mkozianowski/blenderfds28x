import os
import sys
import bpy
import difflib
import tempfile

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

def do_check(checkType, dirpath):

    def get_filepath(path, ext):
        for filename in next(os.walk(path))[2]:
            if filename.endswith('.' + ext):
                return os.path.join(path, filename)

    result = None
    note   = None

    with tempfile.NamedTemporaryFile(suffix='.fds', delete=True) as temporaryFile:
        try:
            filepath_fds = get_filepath(os.path.join(dirpath, 'FDS_Input_Files'), 'fds')

            if checkType == "blnfds":
                filepath_blend = get_filepath(os.path.join(dirpath, 'Blender_Input_Files'), 'blend')
                bpy.ops.wm.open_mainfile(filepath=filepath_blend)
                
            elif checkType == "fdsfds":
                bpy.ops.import_scene.fds(filepath=filepath_fds)
            
            else:
                raise ValueError("Invalid check type")

            bpy.ops.export_scene.fds(filepath=temporaryFile.name)
            compare = compare_fds_files(filepath_fds, temporaryFile.name)
            result = "OK" if compare[0] else "ERROR"
            note   = compare[1]
        
        except Exception as e:
            result = "EXCEPTION"
            note   = str(e)
        
        finally:
            print("-------------------------------------------------------------------------")
            print(temporaryFile.name + " - " + result + " - " + note)
            print("-------------------------------------------------------------------------")
            #temporaryFile.close()
    
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
PATH_TO_RESULTS    = os.path.join(PATH_TO_VALIDATION, 'results.xml')

RESULT_EMPTY = """<?xml version="1.0"?><testResults></testResults>"""
DATE_FORMAT = "%d-%m-%Y %H.%M"

xml = None

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

    while xml.getElementsByTagName('Results').length >= 10:
        elements = xml.getElementsByTagName('Results')
        element_to_remove = None
        for element in elements:
            element_date = datetime.strptime(element.getAttribute("date"), DATE_FORMAT)
            element_to_remove = element if element_to_remove == None or element_date < datetime.strptime(element_to_remove.getAttribute("date"), DATE_FORMAT) else element_to_remove
        root.removeChild(element_to_remove)
    
    results = xml.createElement('Results')
    results.setAttribute('date', datetime.today().strftime(DATE_FORMAT))
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
            check = do_check("blnfds", dirpath)
            append_case(xml, results, dirname, "blnfds", check[0], check[1])

        # fdsfds
        if fdsfds:
            print("> Test: fds to fds")
            check = do_check("fdsfds", dirpath)
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

