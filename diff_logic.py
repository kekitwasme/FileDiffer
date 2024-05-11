"""
Create the file structure for the diff between the folders

Todo:
Consider files that are to be ignored by the gif
Integrate changable context for the diff
    Regex to find @@ .. @@ ?
"""

import difflib
import os
import filecmp
import json 


def get_all_files(directory):
    """
    Get all file paths in the given directory and its subdirectories.
    """
    file_paths = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            file_paths.append(os.path.join(root, file))
    return file_paths


def compare_directories(dir_old, dir_new):
    """
    Compare two directories to find files that are added, removed, common and modified.
    """
    dir_old_files = set(get_all_files(dir_old))
    dir_new_files = set(get_all_files(dir_new))

    # Adjust file paths to make them comparable
    dir_old_files_adjusted = {os.path.relpath(path, dir_old) for path in dir_old_files}
    dir_new_files_adjusted = {os.path.relpath(path, dir_new) for path in dir_new_files}

    added = dir_new_files_adjusted - dir_old_files_adjusted
    removed = dir_old_files_adjusted - dir_new_files_adjusted
    common = dir_old_files_adjusted & dir_new_files_adjusted

    modified = set()
    for file in common:
        if not filecmp.cmp(os.path.join(dir_old, file), os.path.join(dir_new, file), shallow=False):
            modified.add(file)
    unchanged = common - modified

    return added, removed, unchanged, modified


def file_diff(test1_file, test2_file):
    """
    Generate diff between two files, returning diff in a structure
    """

    diff = difflib.unified_diff(test1_file, test2_file, n=1000000)

    #Discard positional information (@@)
    for i in range(0,4):
        next(diff)
    

    file1_line_count = 0
    file2_line_count = 0


    structure = [
        {
            "Line_Old": (file1_line_count:= file1_line_count + 1) if  value[0] == " " else (file1_line_count:= file1_line_count + 1) if value[0] == "-" else "",
            "Line_New": (file2_line_count:= file2_line_count + 1) if  value[0] == " " else (file2_line_count:= file2_line_count + 1) if value[0] == "+" else "",
            "Line_Status": "" if value[0]== " " else "-" if value[0] == "-" else "+",
            "Line_Value": value[1:]
            }
    for value in diff]

    return structure


def create_folder_diff(dir_old, dir_new):

    added, removed, unchanged, modified = compare_directories(dir_old, dir_new)

    folder_diff = {}

    for file in modified:
        with open(os.path.join(dir_old, file), 'r') as file1, open(os.path.join(dir_new, file), 'r') as file2:
            file1_lines = file1.readlines()
            file2_lines = file2.readlines()
            folder_diff[file] = {"File_State": "modified", "File_Content": file_diff(file1_lines, file2_lines)}
                

    for file in removed:
        with open(os.path.join(dir_old, file), 'r') as file1:

            folder_diff[file] = {
                "File_State": "removed",
                "File_Content": [
                {
                    "Line_Old": count+1,
                    "Line_New": "",
                    "Line_Status": "-",
                    "Line_Value": lines
                }
            for count, lines in enumerate(file1.readlines())]
            }


    for file in added:
        with open(os.path.join(dir_new, file), 'r') as file2:

            folder_diff[file] = {
                "File_State": "added",
                "File_Content": [
                {
                    "Line_Old": "",
                    "Line_New": count+1,
                    "Line_Status": "+",
                    "Line_Value": lines
                }
            for count, lines in enumerate(file2.readlines())]
            }


    for file in unchanged:
        with open(os.path.join(dir_new, file), 'r') as file2:

            folder_diff[file] = {
                "File_State": "unchanged",
                "File_Content": [
                {
                    "Line_Old": count+1,
                    "Line_New": count+1,
                    "Line_Status": "",
                    "Line_Value": lines
                }
            for count, lines in enumerate(file2.readlines())]
        }
    
    return json.dumps(folder_diff, default=lambda o: o.__dict__, 
                sort_keys=True, indent=4)


