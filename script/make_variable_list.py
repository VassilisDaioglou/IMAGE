'''
This script makes a abridged list of variables, units and definitions
based on what is available on the common-definitions repository
https://github.com/IAMconsortium/common-definitions

It is a expanded version of the make_variable_list.py script, offering further functionaliy
- Get limited variable list based on a template
- Get limited variable definitions based on a template

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
import os

definitions_project = "common-definitions"
working_project = "OECD_EO_2025"
data_file = "Consolidated_IMAGE_OECD_EO_2025_results_05092024.xlsx"

# The working-directory of the Python console has to be set to the clone of the project repository.
definitions_directory = str('C:\\Users\\daioglouv\\Documents\\git\\' + definitions_project)
#template_directory = str('C:\\Users\\daioglouv\\Documents\\git\\IMAGE\\data\\' + working_project + '\\' + working_project + '_template.xlsx')
template_directory = str('C:\\Users\\daioglouv\\Documents\\git\\IMAGE\\data\\' + working_project + '\\' + data_file)
output_directory = str('C:\\Users\\daioglouv\\Documents\\git\\IMAGE\\output\\' + working_project + '\\')

print("Load Functions")

def get_limited_variable_list(variable_group):
    '''
    Get variables from specific sub-folder
        - To be used when only a specific sub-folder of the definitions is used
    '''
    path = Path(definitions_directory + 'definitions/variable/' + variable_group)
    codelist = VariableCodeList.from_directory(path=path, name='Variable')
    df = codelist.to_pandas()
    df = df[['Variable','Unit','Description']]
    return df

def get_complete_variable_list():
    '''
    # Get complete list 
    - To be used when all variable definitions are to be used
    '''
    dsd = DataStructureDefinition(definitions_directory + "/definitions/")
    df = dsd.variable.to_pandas()
    df = df[['Variable','Unit','Description']]
    return df

def get_template_description(template, variable_unit_description):
    '''
    inputs: 
        - template with variables to be reported
        - variable_unit_description contains full list of variables

        Both as pandas dataframes
    
    outputs:
        - Dataframe with template variables together with definitions
        - Dataframe with template variables that do not have definitions
    '''
    template_array = np.array(template.Variable)
    
    template_variable_description   = variable_unit_description[variable_unit_description['Variable'].isin(template_array)]

    # Identify template Variables that do not have a description
    missing = []
    for v in template_array:
        if v not in np.array(template_variable_description.Variable):
            missing.append(v)
    
    missing_df = pd.DataFrame(missing, columns=['Variable'])
    missing_df = missing_df.drop_duplicates()

    return template_variable_description, missing_df

'''
 ********** STARTING PROCEDURE **********
'''
print("Getting Variable Definitions")
# Load Variable Lists to assign definitions to
reported_data = pd.read_excel(template_directory, sheet_name='data')
reported_missing = pd.read_excel(template_directory, sheet_name='variables_missing')

# Get all variable and definition data frames
complete_variable_list = get_complete_variable_list()
vars_with_description, vars_missing_description = get_template_description(reported_data, complete_variable_list)

print("Produce output")
# Write output excel
writer = pd.ExcelWriter(output_directory + data_file, engine = 'xlsxwriter')
reported_data.to_excel(writer, sheet_name= 'data', index=False)
reported_missing.to_excel(writer, sheet_name= 'variables_missing', index=False)
vars_with_description.to_excel(writer, sheet_name = 'variable_definitions', index = False)
vars_missing_description.to_excel(writer, sheet_name = 'no_definitions', index = False)
writer.close()

print("Done!")