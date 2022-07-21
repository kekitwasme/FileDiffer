import json
import difflib
from unittest.util import _MAX_LENGTH
import os

class Added:
    def __init__(self, value, atPosition):
        self.value = value
        self.atPosition = atPosition

    def toJson(self):
        return json.dumps(self, default=lambda o: o.__dict__, 
            sort_keys=True, indent=4)

class Deleted:
    def __init__(self, value, atPosition):
        self.value = value
        self.atPosition = atPosition

    def toJson(self):
        return json.dumps(self, default=lambda o: o.__dict__, 
            sort_keys=True, indent=4)

class Line:
    def __init__(self):
        self.text = ""
        self.lineNumber = 0
        self.deleted = list()
        self.added = list()

    def toJson(self):
        return json.dumps(self, default=lambda o: o.__dict__, 
            sort_keys=True, indent=4)

class DiffResult:
    def __init__(self):
        self.lines = list()

    def toJson(self):
        return json.dumps(self, default=lambda o: o.__dict__, 
            sort_keys=True, indent=4)
        


def Diff(fileName1, fileName2):

    d = difflib.Differ()

    # open files for reading
    f1 = open(fileName1, "r")
    f2 = open(fileName2, "r")

    # read and split files by line
    t1 = f1.read().split("\n")
    t2 = f2.read().split("\n")

    result = DiffResult()

    maxLength = max(len(t1), len(t2))
    
    if len(t1) < maxLength:
        t1.extend([""] * (maxLength - len(t1)))

    if len(t2) < maxLength:
        t2.extend([""] * (maxLength - len(t2)))

    for i in range(max(len(t1), len(t2))):

        result.lines.append(Line())

        modPosition = 0
        
        # for every character in the diff result
        for diff in list(d.compare(t1[i], t2[i])):

            line = result.lines[i]
               
            line.lineNumber = i + 1
            line.text += diff[2]

            # add modification info
            if diff[0] == "+":
                line.added.append(Added(diff[2], modPosition))
            elif diff[0] == "-":
                line.deleted.append(Deleted(diff[2], modPosition))

            modPosition += 1

    return result.toJson()

