#!/usr/bin/env python3
# -*- coding: UTF-8 -*-
"""
parse an input file of Hqdm Triples from Magma Core (and exported using Fuseki) to clean it up for
hadmMapToPure and further processing in hqdmHaskell.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Usage: PrepareHqdmTriplesForImport.py <input_file.csv> <output_file.csv>
"""
import sys
import os
import uuid
import csv
from pprint import pprint
import copy


def main():
    hqdmPrefix = "hqdm:"

    # Check and retrieve command-line arguments
    if len(sys.argv) != 3:
        print(__doc__)
        print('Oh dear')
        sys.exit(1)   # Return a non-zero value to indicate abnormal termination
    fileInRels  = sys.argv[1]
    fileOut = sys.argv[2]

    # Verify source file
    if not os.path.isfile(fileInRels):
        print("error: {} does not exist".format(fileInRels))
        sys.exit(1)

    inputRows = []
    # Process the file line-by-line
    with open(fileInRels, 'r', newline='') as fpInRels:

        for row in fpInRels:
            if "\"sub\" , \"pred\" , \"obj\" ," not in row:
                inputRows.append(row)

        # Replace " , " with ","
        outputRows = []
        for row in inputRows:
            row = row.replace(" , ", ",")
            row = row.replace("\"", "")
            row = row.replace(",\n", "\n")

            if row.endswith(" \n"):
                row = row[:-2]
                row = row + "\n"

            if row.startswith(' '):
                row = row[1:]

            if (row.count(',')>2):
                num_commas = row.count(',') - 2
                for i in range(num_commas):
                    tmpRow = row.rsplit(',',1)
                    row = tmpRow[0] + tmpRow[1]
            outputRows.append(row)

        finalRows = []
        
        a = iter(outputRows)
        newRow = next(a, None)
        while (newRow != None):
            nextRow = next(a, None)

            if nextRow != None:
                if (newRow.count(',') == 2) and (nextRow.count(',') == 2) :
                    finalRows.append(newRow)
                    newRow = None
            
                if newRow == None:
                    newRow = nextRow
                else:
                    newRow = newRow.rstrip() + nextRow
            
            if nextRow == None:
                newRow = None

        with open(fileOut, 'w+') as f:
            for row in finalRows:
                f.write('%s' %row)

if __name__ == '__main__':
    main()