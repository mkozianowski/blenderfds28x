import os
import sys
import bpy
import difflib

from pprint import pprint
from xml.dom import minidom   #for xml parsing


def load_blend(filepath):
    #print(">> Import")
    bpy.ops.wm.open_mainfile(filepath=filepath)
    
def load_fds(filepath):
    #print(">> Import")
    bpy.ops.import_scene.fds(filepath=filepath)

def export_fds(filepath):
    #print(">> Export")
    bpy.ops.export_scene.fds(filepath=filepath)

def get_filepath(path, ext):
    for filename in next(os.walk(path))[2]:
        if filename.endswith('.' + ext):
            return os.path.join(path, filename)

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

       if ( not line.endswith('\n') ):
          line = line + ' NEW_LINE_MISSING\n'

       string = string + line 
       fds_equals = False

   return [fds_equals, string]

#==================================================================

try:
    print("\n\n")
    print("####################################")
    print("UNIT TEST START")
    print("####################################")
    print("\n\n")

    #TODO Path deve diventare parametrico
    #NB: Da modificare!!!!!!!
    PATH_TO_PROJECT = '/home/carlo/.config/blender/2.80/scripts/addons/blenderfds28x/'
    PATH_TO_EXPORT  = os.path.join(PATH_TO_PROJECT, 'validation/export.fds')

    for dirname in next(os.walk(os.path.join(PATH_TO_PROJECT, 'validation')))[1]:

        print("\n\n" + dirname)
        print("----------------------------")
        dirpath = os.path.join(PATH_TO_PROJECT, 'validation', dirname)

        testXml = minidom.parse(os.path.join(dirpath, 'test.xml'))
        blnfds = testXml.getElementsByTagName("blnfds")[0].firstChild.nodeValue == 'true'
        fdsfds = testXml.getElementsByTagName("fdsfds")[0].firstChild.nodeValue == 'true'

        #TODO Dentro ogni singola cartella del caso c'è un file text.xml
        #   se blnfds = TRUE ==> blend To Fds 
        #                             file blend nella cartella Blender_Input_Files
        #                             file fds   nella cartella FDS_Input_Files
        #   se fdsfds = TRUE ==> Fds To Fds 
        #                             file fds   nella cartella FDS_Input_Files

        # N.B. La funzione  compare_fds_files ritorna già se il test è OK o meno e le righe delle note

        # BLEND TO FDS
        #-----------------------------------
        if blnfds:
            print("> Test: blend to fds")
            filepath_blend = get_filepath(os.path.join(dirpath, 'Blender_Input_Files'), 'blend')
            filepath_fds   = get_filepath(os.path.join(dirpath, 'FDS_Input_Files'), 'fds')
            if filepath_blend != None and filepath_fds != None:
                try:
                    load_blend(filepath_blend)
                    export_fds(PATH_TO_EXPORT)
                    print(compare_fds_files(filepath_fds, PATH_TO_EXPORT))
                except Exception as e:
                    print("> Error!!")

        # FDS TO FDS
        #-----------------------------------
        if fdsfds:
            print("> Test: fds to fds")
            filepath_fds = get_filepath(os.path.join(dirpath, 'FDS_Input_Files'), 'fds')
            if filepath_fds != None:
                try:
                    load_fds(filepath_fds)
                    export_fds(PATH_TO_EXPORT)
                    print(compare_fds_files(filepath_fds, PATH_TO_EXPORT))
                except Exception as e:
                    print("> Error!!")
        
finally:
    print("\n\n")
    print("####################################")
    print("UNIT TEST END")
    print("####################################")
    print("\n\n")
        

    #Export dei risultati
    # Aggiungere al file results.xml un nuovo <Results>. Per ogni test bisogna indicare:
    #     <Name> Nome della cartella
    #     <Type> Tipo di test (blnfds oppure fdsfds)
    #     <Result> OK, ERROR, EXCEPTION a seconda se:
    #               OK -> tutto apposto e file uguali (ritorna TRUE la funzione compare_fds_files)
    #               ERROR -> se i file FDS sono diversi (ritorna FALSE la funzione compare_fds_files)
    #               EXCEPTION -> eccezione che va catturata
    #     <Note> Vi è la stringa che ritorna la funzione compare_fds_files o il testo dell'eccezione


# METTO QUI SUGGERIMENTI PER IL PARSER XML
#################################################
# PARSER FILE XML
#from xml.dom import minidom   #for xml parsing
#
#
#mydoc = minidom.parse( path + 'test.xml')
#
##cattura di un elemento
#element = mydoc.getElementsByTagName("n_cycle")[0];
#
#
##se ci sono più elementi allora ritorna un array
#array_element = mydoc.getElementsByTagName("steel")
#
#for element in array_element
#   example = element.getElementsByTagName("property")[0]
