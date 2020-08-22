#%%
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn import linear_model
import os
os.chdir(r"D:\Python model")
data={'Area':[2600,3000,3200,3600,4000],'price':[550000,565000,610000,680000,725000]}
df=pd.DataFrame(data) # x- area, y- price
#%%
%matplotlib inline #any graph which we are creating will appear in the same notebook and not in separate window
plt.xlabel('Area')
plt.ylabel('price')
plt.scatter(df.Area,df.price,color='red',marker='+')
#%%
new_df=df.drop('price',axis=1) #axis0 is rows and axis=1 for columns
price=df.price
#%% Linear reg model train
# Create linear regression object
reg=linear_model.LinearRegression()
reg.fit(new_df, price)
m=reg.coef_  #y=mx+c ; this is slope
c=reg.intercept_  #this is intercept so, y=135.79x + 180616.44
#%% Linear model prediction
reg.predict([[3300]]) #predict according to value
areas=pd.DataFrame([1000,1500,2300,3540,4120,4560,5490,3460,4750,2300,9000,8600,7100])
p=reg.predict(areas)     #predict according to DF
areas['Predicted_prices']=p
areas.rename(columns={0:'New_areas'},inplace=True)
areas.to_excel('Linear_predict.xlsx')
#%% plot the prediction
%matplotlib inline
plt.xlabel('Area',fontsize=20)
plt.ylabel('Price',fontsize=20)
plt.scatter(areas.New_areas, areas.Predicted_prices, color='red',marker='+')
plt.plot(areas.New_areas,reg.predict(areas[['New_areas']]),color='blue')

#%% Question
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn import linear_model
import os
os.chdir(r"D:\Python model\py-master\ML\1_linear_reg\Exercise")
df=pd.read_csv("canada_per_capita_income.csv")
# find predicted income in 2020
%matplotlib inline
plt.xlabel('Year')
plt.ylabel('per capita income')
plt.scatter(df.year,df['per capita income (US$)'],color='red')
#%%
reg=linear_model.LinearRegression()
reg.fit(df[['year']],df[['per capita income (US$)']])  #df[[]] to exrtract column
m=reg.coef_
c=reg.intercept_
p=reg.predict([[2020]])  #predicted value of 2020
