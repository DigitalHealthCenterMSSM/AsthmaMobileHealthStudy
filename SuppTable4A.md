## Supplementary Table 4A
```markdown
model_summary_univariate = []
for c in cox_cols:
    print c
    if c in ['last_survey_censored', 'event_observed']: continue
    cf.fit( cox_df[['last_survey_censored', 'event_observed', c]], \
               'last_survey_censored', event_col='event_observed',\
              )      

    model_summary = cf.summary
    model_summary.sort(columns=['p'], inplace=True)
    model_summary.index = [i.replace('_', ' ').upper().replace('BIOLOGICALSEX', 'BIOLOGICAL SEX').lower() for i in model_summary.index]
    model_summary['lower 0.95'] = model_summary['lower 0.95'].map(np.exp)
    model_summary['upper 0.95'] = model_summary['upper 0.95'].map(np.exp)

    model_summary_univariate.append( model_summary)

pd.set_option('display.precision',3)
univariate_models = pd.concat(model_summary_univariate)
univariate_models['abs_z'] = univariate_models['z'].map(np.abs)
univariate_models.sort(columns=['abs_z'], ascending=False, inplace=True)
del univariate_models['abs_z']
univariate_models = univariate_models.rename(index={'currentage':'current age'})
univariate_models
gina
last_survey_censored
event_observed
education
age_when_diagnosed
Income
health_insurance
```
