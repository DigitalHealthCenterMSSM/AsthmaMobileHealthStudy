## Figure 2D and 2E
### Figure 2D:
```markdown
#Baseline
#Kaplan Meier Curve
from lifelines import KaplanMeierFitter
# kmf_control = KaplanMeierFitter()
# kmf_control.fit(last_daily.last_daily.values, label='Asthma App').plot()

#Baseline
from lifelines import KaplanMeierFitter
from lifelines.plotting import add_at_risk_counts

rcParams['text.color'] = 'black'
rcParams['ytick.color'] = 'black'
rcParams['xtick.color'] = 'black'
rcParams['axes.facecolor'] = 'white'
rcParams['axes.edgecolor'] = 'white'

fig, ax = plt.subplots(1,1, figsize=(12,12))
fig.patch.set_facecolor('white')


kmfs = []
row_counter = 0
col_counter = 0
for i,n in cox_df[cox_df.index.isin(robust.healthCode)].groupby(['study_entry_month']):
    
    
    
    kmf_control = KaplanMeierFitter()
    kmf_df = n
    label_dict = {0:'Study Month 1', 1:'Study Month 2', 2:'Study Month 3', 3:'Study Month 4'}
    label_i = label_dict[i]
    
    kmf_control.fit(kmf_df.last_survey_censored.values, event_observed=kmf_df.event_observed, label=label_i)
    
        
    kmf_control.plot(label=i, ax=ax, cmap='jet', show_censors=True)
    kmfs.append(kmf_df)
    ax.set_axis_bgcolor('white')
    ax.set_title('Study Participation by Study Entry Month' )
    
    
    ax.set_xlabel('Enrollment Day', color='black')
    
    ax.set_ylabel('Survival', color='black')

plt.legend(fontsize='xx-large')
plt.tight_layout()
plt.yticks(arange(0,1.1,.1))
plt.savefig('../analysis/feasibility_figures_ERS/KM_by_study.svg', format='svg', dpi=600)
plt.show()
```
### Figure 2E
```markdown
#Baseline
#Kaplan Meier Curve
from lifelines import KaplanMeierFitter

#Baseline
from lifelines import KaplanMeierFitter
from lifelines.plotting import add_at_risk_counts


rcParams['text.color'] = 'black'
rcParams['ytick.color'] = 'black'
rcParams['xtick.color'] = 'black'
rcParams['axes.facecolor'] = 'white'
rcParams['axes.edgecolor'] = 'white'

fig, ax = plt.subplots(1,1, figsize=(12,12))
fig.patch.set_facecolor('white')


kmfs = []
row_counter = 0
col_counter = 0


cox_df_age = cox_df.copy()

cox_df_age['Current Age binned'] = pd.cut(cox_df['CurrentAge'], [0,40,100], labels=False, retbins=False)
for i,n in cox_df_age[cox_df_age.index.isin(baseline.healthCode)].groupby(['Current Age binned']):
    
    print i
    if n.shape[0] < 10: continue
    kmf_control = KaplanMeierFitter()
    kmf_df = n
    
    if i ==0:
        label_i = '18-40 years, n=' + str(n.shape[0]) 
    else:
        label_i = '>40 years,    n=' + str(n.shape[0])
    kmf_control.fit(kmf_df.last_survey_censored.values, event_observed=kmf_df.event_observed, label=label_i)
    
        
    kmf_control.plot(label=i, ax=ax, cmap='jet', show_censors=True, ci_show=True)
    kmfs.append(kmf_df)
    ax.set_axis_bgcolor('white')
    ax.set_title('Study Participation by Age', fontsize=30)
    ax.set_ylabel('Survival', fontsize=30)
    ax.set_xlabel('Enrollment Day', fontsize=30)
    
    
    ax.set_xlabel('Enrollment Day', color='black')
    
    ax.set_ylabel('Survival', color='black')
plt.legend(fontsize='xx-large')
plt.tight_layout()
plt.yticks(arange(0,1.1,.1), fontsize=30)
plt.xticks(range(0,200, 20), fontsize=30)
plt.show()

```
