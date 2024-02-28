import difflib
from pprint import pprint
import os

def get_all_files(directory):
    """
    Get all file paths in the given directory and its subdirectories.
    """
    file_paths = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            file_paths.append(os.path.join(root, file))
    return file_paths

def compare_directories(dir1, dir2):
    """
    Compare two directories to find files that are added, removed, or modified.
    """
    dir1_files = set(get_all_files(dir1))
    dir2_files = set(get_all_files(dir2))

    # Adjust file paths to make them comparable
    dir1_files_adjusted = {os.path.relpath(path, dir1) for path in dir1_files}
    dir2_files_adjusted = {os.path.relpath(path, dir2) for path in dir2_files}

    added = dir2_files_adjusted - dir1_files_adjusted
    removed = dir1_files_adjusted - dir2_files_adjusted
    common = dir1_files_adjusted & dir2_files_adjusted

    return added, removed, common

def file_diff(test1_file, test2_file):

    # Generate a unified diff
    diff = difflib.unified_diff(test1_file, test2_file, n=1000000)

    """
    #discarding initial information
    for i in range(0,4):
        next(diff)
    """
    #Count each line independantly
    file1_line_count = 0
    file2_line_count = 0

    #Creating file structure
    list_out = [
        {
            "Old_Line": (file1_line_count:= file1_line_count + 1) if  value[0] == " " else (file1_line_count:= file1_line_count + 1) if value[0] == "-" else 0,
            "New_Line": (file2_line_count:= file2_line_count + 1) if  value[0] == " " else (file2_line_count:= file2_line_count + 1) if value[0] == "+" else 0,
            "Line_Status": "unchanged" if value[0]== " " else "removed" if value[0] == "-" else "new",
            "Line_Value": value[1:]
            }
    for value in diff]

    return list_out

dir1 = 'test/test_old'
dir2 = 'test/test_new'

added, removed, common = compare_directories(dir1, dir2)


folder_diff = {}

for file in common:
    with open(os.path.join(dir1, file), 'r') as file1, open(os.path.join(dir2, file), 'r') as file2:
        file1_lines = file1.readlines()
        file2_lines = file2.readlines()
        folder_diff[file] = file_diff(file1_lines, file2_lines)
              
              
pprint(folder_diff)

