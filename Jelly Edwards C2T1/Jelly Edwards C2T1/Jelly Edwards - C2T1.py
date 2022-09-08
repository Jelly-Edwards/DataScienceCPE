#!/usr/bin/env python
# coding: utf-8

# In[1]:


from sqlalchemy import create_engine
import pymysql
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sn


# Removing Connection Since the Database Connection is Not Working

# In[2]:


#db_connection_str = 'mysql+pymysql://deepanalytics:Sqltask1234!@34.73.222.197/deepanalytics'
#db_connection = create_engine(db_connection_str)
#df = pd.read_sql('SELECT * FROM credit', con=db_connection)


# Manualy Loading CSV of Data To Proceed

# In[3]:


data = pd.read_csv('credit.csv')
data


# In[4]:


data.describe()


# In[5]:


data.info()


# In[6]:


#Two headers, Removing One Header by Saving as New CSV
data.to_csv('credit_1.csv',index=False)


# In[7]:


data = pd.read_csv('credit_1.csv', header=1)
data


# In[8]:


data.info()


# In[9]:


data = data.drop_duplicates()


# In[10]:


print(data.isnull().sum())


# In[11]:


#Searching for Row where ID is NULL
data.loc[data['ID'].isnull()]


# In[12]:


Null_Row = data.loc[data['ID'].isnull()].index
Null_Row


# In[13]:


data = data.drop(Null_Row)
data


# In[14]:


print(data.isnull().sum())


# The Null Row looks like the Heading that I removed before, this leads to me believe that the Heading Rows are repeated

# In[15]:


#Searching for Other Heading Row
data.loc[data['ID'] == 'ID']


# In[16]:


WrongHeadingRow = data.loc[data['ID'] == 'ID'].index
WrongHeadingRow


# In[17]:


data = data.drop(WrongHeadingRow)
data


# In[18]:


#Updating SEX to be Numeric
FemaleSex = data['SEX']=='female'
MaleSex = data['SEX']=='male'
data.loc[FemaleSex, 'SEX'] = 2
data.loc[MaleSex, 'SEX'] = 1
data


# In[19]:


#Updating Education to be Numeric
Education1 = data['EDUCATION']=='graduate school'
Education2 = data['EDUCATION']=='university'
Education3 = data['EDUCATION']=='high school'
Education4 = data['EDUCATION']=='other'
data.loc[Education1, 'EDUCATION'] = 1
data.loc[Education2, 'EDUCATION'] = 2
data.loc[Education3, 'EDUCATION'] = 3
data.loc[Education4, 'EDUCATION'] = 4
data


# In[20]:


#Updating Default to be Numeric
Y1 = data['default payment next month']=='default'
Y0 = data['default payment next month']=='not default'
data.loc[Y1, 'default payment next month'] = 1
data.loc[Y0, 'default payment next month'] = 0
data


# In[21]:


#Saving Without Bad Row of Data
data.to_csv('credit_2.csv',index=False)


# In[22]:


data = pd.read_csv('credit_2.csv', header=0)
data


# In[23]:


print(data.isnull().sum())


# In[24]:


data.info()


# In[25]:


data


# In[27]:


# Using Seaborn to look at default and Limit Balance
sn.catplot(x='default payment next month', y='LIMIT_BAL', data=data, kind='bar')


# In[31]:


#Correlation Matrix for Data
corr_mat = data.corr()
print(corr_mat)


# In[32]:


# Using Seaborn to look at Default and AGe
sn.catplot(x='AGE', y='default payment next month', data=data, kind='bar')


# In[34]:


x = data['default payment next month']
y = data['AGE']
plt.scatter(x,y, marker='o')
plt.show()


# In[37]:


# Using Seaborn to look at Default and AGe
sn.catplot(x='EDUCATION', y='default payment next month', data=data, kind='bar')


# In[ ]:




