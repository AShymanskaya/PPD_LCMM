# -*- coding: utf-8 -*-
"""
Created on Thu Feb  3 11:00:26 2022

@author: ashymanskaya
"""
import pandas as pd

#load clusters from lcmm classification  in R
df_full = pd.read_excel(r'lcmm_clusters_6.xlsx')
df=pd.DataFrame(df_full)
df=df.pivot(index='index', columns='EPDS', values='true_class')
df=df[3]
df=pd.DataFrame(df)
df_full = pd.read_excel(r'full_sample_lcmm6.xlsx')
df_full['cluster']=df[3]

df['target']=df_full['target']
writer = pd.ExcelWriter('clusters_6_LCMM.xlsx', engine='xlsxwriter')
# Convert the dataframe to an XlsxWriter Excel object.
df_full.to_excel(writer, sheet_name='Sheet1')
# Close the Pandas Excel writer and output the Excel file.
writer.save()
