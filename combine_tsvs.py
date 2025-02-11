import os
import pandas as pd

directory = "D:\IFCB\IFCB_codes\ifcb-ecotaxa-labeled-uto-2021-new"

for subdirectory, root, files in os.walk(directory):

    for file in files:
        f = os.path.join(subdirectory, file)
        filename, ext = os.path.splitext(file)

        if ext == '.tsv':
            read = pd.read_csv(f, sep='\t')
            read = read.replace(['Ã¶'],'ö')
            read.to_csv(f"{filename}.csv", encoding='utf-8-sig')