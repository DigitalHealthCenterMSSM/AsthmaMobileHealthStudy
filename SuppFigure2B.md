## Supplementary Figure 2B
```markdown
#DATAFRAME CONSTRUCTION FOR ORDINARY LEAST SQUARES MODEL


import numpy as np
import statsmodels.api as sm
import statsmodels.formula.api as smf
from scipy.special import logit

daily_survey_standardqs = [ 'day_symptoms', 'night_symptoms', 'peakflow', \
                           'get_worse', 'use_qr']



#number of days in study fory each healthcode
days_in_study_2015_9_9 = select_date(daily_weekly_df, 'date', (2015,9,8))

days_in_study_2015_9_9 = days_in_study_2015_9_9.study_entry.dt.date
days_in_study_2015_9_9 = pd.to_datetime(days_in_study_2015_9_9, unit='ms')
days_in_study_2015_9_9 = pd.datetime(2015,9,8) - days_in_study_2015_9_9
days_in_study_2015_9_9 = days_in_study_2015_9_9.dt.days
days_in_study_2015_9_9.name = 'days_in_study_2015_9_9'
days_in_study_2015_9_9.sort(inplace=True, ascending=False)
days_in_study_2015_9_9 = days_in_study_2015_9_9.reset_index().drop_duplicates().set_index('healthCode')


days_till_2015_09_09 = pd.datetime(2015,9,8).date() - daily_weekly_df['study_entry'].dropna()
days_till_2015_09_09 = days_till_2015_09_09.reset_index('healthCode').drop_duplicates().set_index('healthCode')
days_till_2015_09_09_180days = days_till_2015_09_09[days_till_2015_09_09.study_entry.dt.days>90]
days_till_2015_09_09_180days.columns = ['days_till_2015_09_09']
days_till_2015_09_09_180days['days_till_2015_09_09'] = 184 - days_till_2015_09_09_180days['days_till_2015_09_09'].dt.days
days_till_2015_09_09_180days.shape

#DAILY SURVEYS, AT LEAST 1 QUESTION ANSWERED
daily_weekly_df_response = select_date(daily_weekly_df, 'date', (2015,9,8))  #restrict to data before 9/19/2015

#calculate responses per healthcode
daily_survey_response_days = daily_weekly_df_response.query('study_day < 181').dropna(subset=[daily_survey_standardqs], how='all').groupby(level=0)['value'].sum()

#join days in study
daily_survey_response_days = pd.DataFrame(daily_survey_response_days).join(days_in_study_2015_9_9, how='inner')

#calculate rate #days responding / #days enrolled
daily_survey_response_days['ratio'] = daily_survey_response_days['value'].astype(float).divide(daily_survey_response_days['days_in_study_2015_9_9'].values, axis=0)  
daily_survey_response_days = daily_survey_response_days.reset_index().drop_duplicates(['healthCode', 'ratio']).set_index('healthCode')
daily_survey_response_days.head()



cohort_response_rate = daily_survey_response_days.join(aboutyou)
cohort_response_rate = cohort_response_rate.join(robust.set_index('healthCode')['gina'], how='inner')

cohort_response_rate['intercept'] = 1


cohort_response_rate = cohort_response_rate.join(days_till_2015_09_09_180days, how='inner')



replacement_dict = {'gina':{'Uncontrolled':2, 'PartlyControlled':1, 'WellControlled':0},\
                    'education':{'1':1, '2':1, '3':1, '4':2, '5':2, '6':3, '7':3, '8':np.NaN},\
                    'BiologicalSex':{'Female':0, 'Male':1},\
                   'health_insurance':{'3.0':0, '1.0':1, '2.0':1, '4.0':np.NaN},}

cohort_response_rate = cohort_response_rate.dropna(subset=['health_insurance'])
cohort_response_rate['education'] = cohort_response_rate['education'].astype(str)
cohort_response_rate['health_insurance'] = cohort_response_rate['health_insurance'].astype(str)
cohort_response_rate.replace(to_replace=replacement_dict, inplace=True)
cohort_response_rate['education'] = cohort_response_rate['education'].astype(float)#.astype(str)


cohort_response_rate = cohort_response_rate[(cohort_response_rate.education<8) ]
cohort_response_rate = cohort_response_rate.join(study_entry_month)

#DATA NORMALIZATION
cohort_response_rate['logit_response_rate'] = cohort_response_rate['ratio'].map(logit)
cohort_response_rate['logit_response_rate'] = cohort_response_rate['logit_response_rate'].map(lambda x: 1.0 if x > 1.0 else x)

def std_scale(x):
    
    mean_x = np.mean(x)
    std_x = np.std(x)
    return [(i - mean_x) / std_x for i in x]

cohort_response_rate = cohort_response_rate[cohort_response_rate.index.isin(cox_df.index)]
cohort_response_rate = cohort_response_rate.join(age)
cohort_response_rate['Current_Age_binned'] = pd.cut(cohort_response_rate['CurrentAge'], range(0,100,10), labels=False, retbins=False)
cohort_response_rate = cohort_response_rate.join(biosex)

for c in ['education', 'age_when_diagnosed', 'gina', 'CurrentAge', 'ethnicity', 'Income', 'health_insurance','study_entry_month']:
    cohort_response_rate[c + '_stdscale'] = cohort_response_rate[[c]].apply(std_scale)



results = smf.ols('logit_response_rate ~ I(study_entry_month) + I(CurrentAge) + C(health_insurance) + C(ethnicity) + C(BiologicalSex) + I(education_stdscale) ', \
                                  data=cohort_response_rate[cohort_response_rate.index.isin(cox_df.index)]).fit()

results.summary()
```

```markdown
cohort_response_rate[cohort_response_rate.index.isin(cox_df.index)]['ratio'].hist(bins=20, color='#C30E6F', figsize=(10,8))
plt.ylabel('Count')
plt.xlabel('Response Rate')
```
