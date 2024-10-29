'''
This script makes a complete list of variables, units and definitions
based on what is available on the common-definitions repository
https://github.com/IAMconsortium/common-definitions

It is a concise version of the make_variable_list.py script which has added functionaliy

IMPORTANT:
This script identifies variable lists based on a local copy of the 
following fork of the common-definitions repository:
https://github.com/VassilisDaioglou/common-definitions

Thus, in order to ensure it works appropriately, please ensure that this local
copy is up-to-date with the appropriate revision of the main branch of this 
repository.


FUTURE IMPROVEMENT:
Add option to update local copy of variable list within this script.

Vassilis Daioglou - July 2024
'''

from nomenclature import DataStructureDefinition
from nomenclature.codelist import VariableCodeList
from pathlib import Path
import pandas as pd
import numpy as np
from datetime import datetime

print("\nSet Definitions")
definitions_project = "common-definitions"
working_project = "IMAGE34"

# The working-directory of the Python console has to be set to the clone of the project repository.
definitions_directory = str('C:\\Users\\daioglouv\\Documents\\git\\' + definitions_project)
output_directory = str('C:\\Users\\daioglouv\\Documents\\git\\IMAGE\\output\\' + working_project + '\\')
Path(output_directory).mkdir(parents=True, exist_ok=True)

print("\nLoad Functions")
def get_complete_variable_list():
    '''
    # Get complete list 
    - To be used when all variable definitions are to be used
    '''
    dsd = DataStructureDefinition(definitions_directory + "/definitions/")
    df = dsd.variable.to_pandas()
    df = df[['Variable','Unit','Description']]
    return df

'''
 ********** STARTING PROCEDURE **********
'''
print("\nGetting Variable List & Definitions")
complete_variable_list = get_complete_variable_list()

print("\nProduce output")
# Write output excel
time = datetime.now()
timestamp = time.strftime("%d%m%Y_%H%M")
data_file = str(definitions_project + '_variable_list_' + timestamp + '.xlsx')

print("\ttimestamp:       ", timestamp)
print("\tOutput Location: ", output_directory)
print("\tOutput File:     ", data_file)

writer = pd.ExcelWriter(output_directory + data_file, engine = 'xlsxwriter')
complete_variable_list.to_excel(writer, sheet_name= 'variable_list', index=False)
writer.close()

print("\nDone!")