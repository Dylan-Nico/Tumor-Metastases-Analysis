# %%


# %%


# %%
import pandas as pd


# %%

pd.set_option('display.max_columns', None)

df_1 = pd.read_excel("../dataset/d1.xlsx")

print(list(df_1))

df_1.head()



# %%
df_2 = pd.read_excel("../dataset/d2.xlsx")

print(list(df_2))
print(df_2.shape)

df_2.head()

# %% [markdown]
# ## Get patients with surgery on liver and find unique biomarkers

# %%
liver_patients = df_1.loc[df_1["sites of surgery"].isin([1, "1;2", "1;4", "1;5"])]["AccessionNumber"]
print(len(liver_patients))
biomarkers = df_2.loc[df_2["AccessionNumber"].isin(liver_patients)]["Biomarker"]
unique_biomarkers = list(set(biomarkers))
len(unique_biomarkers)
# unique_biomarkers

# %%
unique_biomarkers.insert(0, 'AccessionNumber')
new_df = pd.DataFrame(columns=unique_biomarkers)
unique_biomarkers.remove('AccessionNumber')

# %%
new_df["AccessionNumber"] = liver_patients
new_df

# %%
# for index, row in df_2.iterrows():
    # print (row)

# %%


# for access_num in liver_patients:
    


