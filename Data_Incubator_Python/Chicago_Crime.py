
# coding: utf-8

# In[417]:

get_ipython().magic(u'matplotlib inline')
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm
plt.style.use('ggplot')


# In[2]:

# import Chicago Crime data into Pandas Dataframe 
raw_data = pd.read_csv('Crimes_-_2001_to_present.csv')


# In[3]:

# data types
raw_data.info()


# In[17]:

# total crimes for each year
year_crimes = raw_data.groupby('Year')['ID'].nunique()
year_crimes


# In[422]:

# plot of total crime vs. year
plt.rcParams['figure.figsize'] = 10, 5
year_crimes.plot(kind='bar', title='Total Crimes vs. Year', color='blue', alpha=0.5)


# In[419]:

# total crimes per year linear regression 
plt.rcParams['figure.figsize'] = 10, 5
df = pd.DataFrame({ 'year' : year_crimes.index, 'total' : year_crimes.values } )
model = pd.ols(y=df.total[0:15], x=df.year[0:15])
model.summary_as_matrix
slope = -1.697365e+04
intercept = 3.448121e+07
prediction = slope*df.year[0:15] + intercept
df2 = pd.DataFrame({ 'year' : df.year[0:15], 'total' : prediction})
ax = df[0:15].plot(kind='scatter', x='year',y='total')
df2.plot(kind='line', x='year', y='total', ax=ax, color='red', legend=False, title="Total Crimes vs. Year")


# In[420]:

# crime map 2001
plt.rcParams['figure.figsize'] = 15,15
df = raw_data[ raw_data['Year'] == 2001 ]
df = df[ df['X Coordinate'] > 0 ]
plot1 = df.plot(kind='scatter',x='X Coordinate',y='Y Coordinate', alpha=0.01, color='blue')   
df[df['Primary Type'] == 'CRIM SEXUAL ASSAULT'].plot(kind='scatter',x='X Coordinate',y='Y Coordinate', alpha=0.5, color='yellow', 
                                          ax=plot1)
df[df['Primary Type'] == 'HOMICIDE'].plot(kind='scatter',x='X Coordinate',y='Y Coordinate', alpha=0.75, color='red', 
                                          ax=plot1, title='Chicago Crime Map 2001')
plt.legend(['All Crimes','Sexual Assult', 'Homicide'], loc='upper right')


# In[423]:

# crime map 2007
plt.rcParams['figure.figsize'] = 15,15
df = raw_data[ raw_data['Year'] == 2007 ]
plot1 = df.plot(kind='scatter',x='X Coordinate',y='Y Coordinate', alpha=0.01, color='blue')   
df[df['Primary Type'] == 'CRIM SEXUAL ASSAULT'].plot(kind='scatter',x='X Coordinate',y='Y Coordinate', alpha=0.5, color='yellow', 
                                          ax=plot1)
df[df['Primary Type'] == 'HOMICIDE'].plot(kind='scatter',x='X Coordinate',y='Y Coordinate', alpha=0.75, color='red', 
                                          ax=plot1, title='Chicago Crime Map 2007')
plt.legend(['All Crimes','Sexual Assult', 'Homicide'], loc='upper right')


# In[424]:

# crime map 2015
plt.rcParams['figure.figsize'] = 15,15
df = raw_data[ raw_data['Year'] == 2015 ]
plot1 = df.plot(kind='scatter',x='X Coordinate',y='Y Coordinate', alpha=0.01, color='blue')   
df[df['Primary Type'] == 'CRIM SEXUAL ASSAULT'].plot(kind='scatter',x='X Coordinate',y='Y Coordinate', alpha=0.5, color='yellow', 
                                          ax=plot1)
df[df['Primary Type'] == 'HOMICIDE'].plot(kind='scatter',x='X Coordinate',y='Y Coordinate', alpha=0.75, color='red', 
                                          ax=plot1, title='Chicago Crime Map 2015')
plt.legend(['All Crimes','Sexual Assult', 'Homicide'], loc='upper right')


# In[102]:

# individual crime type numbers for each year
crime_year_number = raw_data.groupby('Primary Type')['Year'].value_counts()
crime_year_number


# In[105]:

# individual crime type percenage of total crime per year
crime_year_number2 = (crime_year_number[:]).astype(float)
for x in range(0,len(crime_year_number2)):
    year = crime_year_number2.index[x][1]
    total_crimes = year_crimes[year] 
    crime_year_number2[x] = crime_year_number2[x] / float(total_crimes) * 100
crime_year_number2


# In[106]:

# top crime type numbers per year
year_crime_number = raw_data.groupby('Year')['Primary Type'].value_counts()
year_crime_number


# In[113]:

# top crime type percentage per year
year_crime_number2 = (year_crime_number[:]).astype(float)
for x in range(0,len(year_crime_number2)):
    year = year_crime_number2.index[x][0]
    total_crimes = year_crimes[year] 
    year_crime_number2[x] = year_crime_number2[x] / float(total_crimes) * 100
year_crime_number2


# In[432]:

# plot of crime type numbers vs. year (removed 2016 due to partial data)
plt.rcParams['figure.figsize'] = 15, 125 
df = year_crime_number.unstack()
df['Year'] = df.index
df = df[df['Year'] < 2016]
df = df.drop('Year', axis=1)
axes = df.plot(subplots=True, kind='bar', legend=False, alpha=0.5)
for each in axes:
    each.set_ylabel('Number of Crime')


# In[427]:

# plot of crime type percentage vs. year
plt.rcParams['figure.figsize'] = 15, 125 
axes = year_crime_number2.unstack().plot(subplots=True, kind='bar', legend=False, alpha=0.5)
for each in axes:
    each.set_ylabel('Percent of Total Crime (%)')
axes


# In[ ]:




# In[ ]:



