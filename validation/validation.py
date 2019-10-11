import os
import bpy
from pprint import pprint


def load_blend(filepath):
    #print(">> Import")
    bpy.ops.wm.open_mainfile(filepath=filepath)
    
def load_fds(filepath):
    #print(">> Import")
    bpy.ops.import_scene.fds(filepath=filepath)

def export_fds(path, file_name):
    #print(">> Export")
    context = bpy.context
    sc = context.scene
    #pprint(type(context))
    #pprint(dir(context.scene))
    #pprint(dir(bpy.ops.export_scene.fds))
    bpy.ops.export_scene.fds(filepath=os.path.join(path, 'validation', file_name))

def get_filepath(path, ext):
    for filename in next(os.walk(path))[2]:
        if filename.endswith('.' + ext):
            return os.path.join(path, filename)

#==================================================================

#TODO Path deve diventare parametrico
PATH_TO_PROJECT = '/home/carlo/.config/blender/2.80/scripts/addons/blenderfds28x/' # NB: Da modificare!!!!!!!

for dirname in next(os.walk(os.path.join(PATH_TO_PROJECT, 'validation')))[1]:

    print("\n\n" + dirname)
    print("----------------------------")
    dirpath = os.path.join(PATH_TO_PROJECT, 'validation', dirname)

    #TODO Dentro ogni singola cartella del caso c'è un file text.xml
    #   se blnfds = TRUE ==> blend To Fds 
    #                             file blend nella cartella Blender_Input_Files
    #                             file fds   nella cartella FDS_Input_Files
    #   se fdsfds = TRUE ==> Fds To Fds 
    #                             file fds   nella cartella FDS_Input_Files

    # N.B. La funzione  compare_fds_files ritorna già se il test è OK o meno e le righe delle note

    # BLEND TO FDS
    #-----------------------------------
    if dirname.startswith('BLN2FDS'):
        filepath_blend = get_filepath(dirpath, 'blend')
        filepath_fds   = get_filepath(dirpath, 'fds')
        if filepath_blend != None and filepath_fds != None:
            try:
                load_blend(filepath_blend)
                #export_fds(PATH_TO_PROJECT, 'test1.fds')
            except Exception as e:
                print(e)

    # FDS TO FDS
    #-----------------------------------
    elif dirname.startswith('FDS2FDS'):
        filepath_fds   = get_filepath(dirpath, 'fds')
        if filepath_fds != None:
            try:
                load_fds(filepath_fds)
                #export_fds(PATH_TO_PROJECT, 'test1.fds')
            except Exception as e:
                print(e)

    #Export dei risultati
    # Aggiungere al file results.xml un nuovo <Results>. Per ogni test bisogna indicare:
    #     <Name> Nome della cartella
    #     <Type> Tipo di test (blnfds oppure fdsfds)
    #     <Result> OK, ERROR, EXCEPTION a seconda se:
    #               OK -> tutto apposto e file uguali (ritorna TRUE la funzione compare_fds_files)
    #               ERROR -> se i file FDS sono diversi (ritorna FALSE la funzione compare_fds_files)
    #               EXCEPTION -> eccezione che va catturata
    #     <Note> Vi è la stringa che ritorna la funzione compare_fds_files o il testo dell'eccezione
