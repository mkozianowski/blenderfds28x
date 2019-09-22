import os
import bpy
from pprint import pprint


def load_blend(path):
    print(">> Import")
    bpy.ops.wm.open_mainfile(filepath=os.path.join(path, 'validation\\cube\\cube.blend'))
    
def load_fds(path):
    print(">> Import")
    bpy.ops.import_scene.fds(filepath=os.path.join(path, 'validation\\cube\\cube.fds'))

def export_fds(path, file_name):
    print(">> Export")
    context = bpy.context
    sc = context.scene
    #pprint(type(context))
    #pprint(dir(context.scene))
    #pprint(dir(bpy.ops.export_scene.fds))
    #bpy.ops.export_scene.fds(filepath=os.path.join(path, 'validation', file_name))
    fds_file = sc.to_fds( context=context, full=True)
    file=open( os.path.join( path, file_name, "w+") )
    file.write( fds_file)
    file.close()

#==================================================================

#PATH_TO_PROJECT = 'F:\\Projects\\VVF\\blenderfds28x' # NB: Da modificare!!!!!!!
#PATH_TO_PROJECT = 'C:\\Users\\rupol\\AppData\\Roaming\\Blender Foundation\\Blender\\2.80\\scripts\\addons\\blenderfds28x'

# Test 1
# load from fds file, export to fds file
#try:
#    print("\nTest 1")
#    print("----------------------------")
#    load_fds(PATH_TO_PROJECT)
#    export_fds(PATH_TO_PROJECT, 'test1.fds')
#except:
#    print('\n\nAn error occurred')
#
# Test 2
# load from blend file, export to fds file
try:
    print("\nTest 2")
    print("----------------------------")
    load_blend(PATH_TO_PROJECT)
    export_fds(PATH_TO_PROJECT, 'test2.fds')
except:
    print('\n\nAn error occurred')


