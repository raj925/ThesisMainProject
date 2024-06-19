def processFile(file):
    # Read the JSON file
    txt = open(file).read()

    # Some weird formatting in the JSON file we need to deal with so it can be parsed properly.
    txt = txt.replace('}"}','}}')
    txt = txt.replace('"{','{')
    txt = txt.replace('}",','},')

    # We change the JSON as above and then rewrite the JSON file with the cleaned-up version.
    with open(file, mode='w') as json_file:
        json_file.write(txt)
