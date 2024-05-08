from dulwich.repo import Repo
import os
from dulwich.diff_tree import tree_changes
import difflib
import json

def dulwich_get_data(repo_path,commit_new,commit_old):

    # Open the repository
    repo = Repo(repo_path)

    tree_test_1 = repo[commit_new].tree
    tree_test_2 = repo[commit_old].tree

    changes = tree_changes(repo.object_store, tree_test_2, tree_test_1)

    toReturn = []

    for item in changes:
        if item.type == 'delete':
            
            filename = item.old.path.decode('utf-8')
            toReturn.append(('D',filename,repo.get_object(item.old.sha)))

        elif item.type == 'add':
            
            filename = item.new.path.decode('utf-8')
            toReturn.append(('A',filename,repo.get_object(item.new.sha)))

        elif item.type == 'modify':

            name_old = item.old.path.decode('utf-8')

            name_new = item.new.path.decode('utf-8')

            if name_old != ".gitignore":
                toReturn.append(('M',name_old,name_new,repo.get_object(item.old.sha),repo.get_object(item.new.sha)))

    return toReturn

def diff_arrange_data(data):

    arr = []

    for item in data:
        if item[0] == 'D':
            lineD = item[2].data.decode('utf-8')
            content = lineD.splitlines()

            tup = ('D',item[1],content)

        elif item[0] == 'A':
            lineA = item[2].data.decode('utf-8')
            content = lineA.splitlines()

            tup = ('A',item[1],content)

        elif item[0] == 'M':
            
            old_content = item[3].data.decode('utf-8')
            new_content = item[4].data.decode('utf-8')

            lines1 = old_content.splitlines()
            lines2 = new_content.splitlines()
            
            diffData = difflib.unified_diff(lines1, lines2)

            for i in range(0,4):
                next(diffData)

            content = []
            for x in diffData:
                content.append(x)

            tup = ('M',item[1],item[2],content)

        arr.append(tup)

    return json.dumps(arr)