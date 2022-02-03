# -*- coding: utf-8 -*-
"""
Created on Wed Jan  6 10:19:20 2021

@author: ashymanskaya
"""

import pandas as pd
import warnings
warnings.filterwarnings('ignore')
three_samples_df_all_epdst
#load imputed values from R, with index EPDS values from original data. Full dataset available upon request under declaration of research aim.
df_imputed= pd.read_excel(r'imputed_data.xlsx')
#df_imputed contains all subjects. However, for lcmm analysis we are interested only in subjects who possess EPDS values. So we remove those without any EPDS values.
#if more any EPDS missing, revome the patient 
epdst=['EPDS T0', 'EPDS T1', 'EPDS T2', 'EPDS T3', 'EPDS T4']

three_samples_df_all_epdst= pd.read_excel(r'full_sample_w_EPDS_with_nans_AS.xlsx')

three_samples_df_all_epdst = three_samples_df_all_epdst.dropna(axis=0, subset=epdst)
three_samples_df=three_samples_df_all_epdst.iloc[three_samples_df_all_epdst.index]

data_df=df_imputed
data_df[ 'target']=three_samples_df_all_epdst.reset_index()[ 'target']
data_df[ 'EPDS_T0']=three_samples_df_all_epdst.reset_index()[ 'EPDS T0']
data_df[ 'EPDS_T1']=three_samples_df_all_epdst.reset_index()[ 'EPDS T1']
data_df[ 'EPDS_T2']=three_samples_df_all_epdst.reset_index()[ 'EPDS T2']
data_df[ 'EPDS_T3']=three_samples_df_all_epdst.reset_index()[ 'EPDS T3']
data_df[ 'EPDS_T4']=three_samples_df_all_epdst.reset_index()[ 'EPDS T4']

df_long=data_df.copy()
df_long= pd.read_excel(r'full_sample_17022021_AS.xlsx')

#demographics
demogr_features=['Age', 'Completed_professional_education',
        'Family_status', 
       'Highest_degree_of_education', 'Income', 'Marital_status',        
       'Total_amount_of_children']
#prehistory
anamnese_features=[
       'Complication_during_pregnancy', 
       'Familial_psychiatric_history',  'PMS_value',
       'Psychiatric_diagnosis_in_previous_pregancy', 
       'RLS_Wert', 'SLE_Family',
       'SLE_Personal', 'Stressful_life_events', 
       'Previous_depression', 'Other_psy_problems']
#immideately after birth
iab_features=['Breastfeeding_T0', 'Complications_during_birth',
       'Gender_Baby', 'Relocation_to_other_ward', 'baby_blues',
       'birth_related_psychological_and_physical_traumas', 'Spontaneous_Birth',
       'Ventous', 'Section', 'Emerg_Section', 'Labor_Induction']



##create long format of data needed for lcmm analysis
epdst=['EPDS_T0', 'EPDS_T1', 'EPDS_T2', 'EPDS_T3', 'EPDS_T4']
a_EPDS=df_long[['EPDS_T0', 'EPDS_T1', 'EPDS_T2', 'EPDS_T3', 'EPDS_T4','target']]
a_EPDS['index']=a_EPDS.index
a_EPDS=pd.melt(a_EPDS, id_vars=['index'], value_vars=epdst,
        var_name='EPDS', value_name='value').sort_values(by=['index','EPDS'])

writer = pd.ExcelWriter('long_melt_epds.xlsx', engine='xlsxwriter')
# Convert the dataframe to an XlsxWriter Excel object.
a_EPDS.to_excel(writer, sheet_name='Sheet1')
# Close the Pandas Excel writer and output the Excel file.
writer.save()


##create long format of data needed for multlcmm analysis
mpas=['MPAS_T1', 'MPAS_T2', 'MPAS_T3', 'MPAS_T4']
a_MPAS=df_long[['MPAS_T1', 'MPAS_T2', 'MPAS_T3', 'MPAS_T4','target']]
a_MPAS['index_MPAS']=a_MPAS.index
a_MPAS=pd.melt(a_MPAS, id_vars=['index_MPAS'], value_vars=mpas,
        var_name='MPAS', value_name='value_MPAS').sort_values(by=['index_MPAS','MPAS'])

a_EPDS_T1=df_long[['EPDS_T1', 'EPDS_T2', 'EPDS_T3', 'EPDS_T4','target']]
a_EPDS_T1['index_EPDS']=a_EPDS_T1.index
a_EPDS_T1=pd.melt(a_EPDS_T1, id_vars=['index_EPDS'], value_vars=['EPDS_T1', 'EPDS_T2', 'EPDS_T3', 'EPDS_T4'],
        var_name='EPDS', value_name='value_EPDS').sort_values(by=['index_EPDS','EPDS'])

df_concat = pd.concat([a_MPAS, a_EPDS_T1], axis=1)

writer = pd.ExcelWriter('long_melt_epds_mpas.xlsx', engine='xlsxwriter')
# Convert the dataframe to an XlsxWriter Excel object.
df_concat.to_excel(writer, sheet_name='Sheet1')
# Close the Pandas Excel writer and output the Excel file.
writer.save()

