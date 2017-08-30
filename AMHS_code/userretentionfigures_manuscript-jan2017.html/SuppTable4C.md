## Supplementary Table 4C
```markdown
results.summary()
```

```markdown
last_study_day = daily_weekly_df[daily_survey_standardqs].dropna(how='all').reset_index().groupby('healthCode')['study_day'].max()
last_study_day = last_study_day.map(lambda x: 181 if x > 179 else x)
last_study_day.name = 'last_survey_censored'

last_study_day.ix[baseline.healthCode].hist(bins=50,  histtype='step', cumulative=True,  linewidth=5, color='#C30E6F', figsize=(10,8))
#plt.xlabel('Enrollment Day')
plt.ylabel('Cumulative User Counts', rotation=90)
plt.xlabel('Days Since Enrollment')
plt.grid(color='darkgray')
plt.title('Days in AMHA Study')
plt.xlim(0,180)
plt.vlines(x=180,ymin=0,ymax=3500, color='white', linewidth=6)
plt.tight_layout()
#plt.savefig('/Users/ers/Dropbox/feasibility_figures_ERS/CumulativeHist_Retention.svg',dpi=600, format='svg')
plt.show()
```

```markdown
cox_df.last_survey_censored.hist(bins=50, histtype='step', linewidth=5, color='#C30E6F', figsize=(16,5))
plt.ylabel('Participant Count', rotation=90)
plt.grid(color='darkgray')
plt.title('Days in AMHA Study')
plt.tight_layout()
```
