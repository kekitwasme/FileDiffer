import json
import difflib

class Added:
    def __init__(self, value, position):
        self.value = value
        self.position = position

    def toJson(self):
        return json.dumps(self, default=lambda o: o.__dict__, 
            sort_keys=True, indent=4)

class Deleted:
    def __init__(self, value, position):
        self.value = value
        self.position = position

    def toJson(self):
        return json.dumps(self, default=lambda o: o.__dict__, 
            sort_keys=True, indent=4)

class Line:
    def __init__(self):
        self.text = ""
        self.number = 0
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

    f1 = open(fileName1, "r")
    f2 = open(fileName2, "r")

    t1 = f1.read()
    t2 = f2.read()

    lineNumber = 0
    modPosition = 0

    result = DiffResult()
    result.lines.append(Line())

    # for every character in the diff result
    for i, diff in enumerate(list(d.compare(t1, t2))):

        # add to line string
        result.lines[lineNumber].text += diff[2]


        # add modification info
        if diff[0] == "+":
            result.lines[lineNumber].added.append(Added(diff[2], modPosition))
        elif diff[0] == "-":
            result.lines[lineNumber].deleted.append(Deleted(diff[2], modPosition))

        modPosition += 1
    
        # at the end of line
        if diff[2] == "\n":
            result.lines.append(Line())
            result.lines[lineNumber].number = lineNumber+1
            lineNumber += 1
            modPosition = 0

    return result.toJson()
