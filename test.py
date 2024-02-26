import difflib
from pprint import pprint

test1_file = """
one
two
three
four
five
six
seven
eight
nine
ten
31231asda
""".splitlines()

test2_file = """
one
two2
three
five
six
seven7
ni ne
tn
dwadaw
31231asda
gsfASfwAE
""".splitlines()

# Generate a unified diff
diff = difflib.unified_diff(test1_file, test2_file, fromfile="Old version", tofile="New version", n=1000000)


#discarding initial information
for i in range(0,4):
    next(diff)


file1_line_count = 0
file2_line_count = 0

#creating file structure
list_out = [
    [
        (file1_line_count:= file1_line_count + 1) if  value[0] == " " else (file1_line_count:= file1_line_count + 1) if value[0] == "-" else 0,
        (file2_line_count:= file2_line_count + 1) if  value[0] == " " else (file2_line_count:= file2_line_count + 1) if value[0] == "+" else 0,
        "unchanged" if value[0]== " " else "old" if value[0] == "-" else "new",
        value[1:]
        
        ]
 for value in diff]

pprint(list_out)
