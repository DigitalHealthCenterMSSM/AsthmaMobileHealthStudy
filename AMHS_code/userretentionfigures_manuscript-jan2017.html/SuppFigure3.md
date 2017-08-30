## Supplementary Figure 3
```markdown
rcParams['text.color'] = 'black'
rcParams['ytick.color'] = 'black'
rcParams['xtick.color'] = 'black'



from matplotlib.colors import LinearSegmentedColormap
vmax = 2.0
cmap = LinearSegmentedColormap.from_list('mycmap', [(0 / vmax, 'black'),
                                                    (1 / vmax, 'yellow'),
                                                    (2 / vmax, 'red')]
                                        )


def heatmap_plot(heatmap_df, title, ylabel=True):
    fig, ax = plt.subplots(1,1, figsize=(15,8))
    
    heatmap_df = heatmap_df[range(0,180)]
    
    #Order participants by daily survey completion counts
    heatmap_df_order = heatmap_df.apply(lambda x: sum([1 if i in set([0,1]) else 0 for i in x]), axis=1)
    heatmap_df_order.sort(inplace=True, ascending=False)
    heatmap_df_order
    
    
    p = ax.pcolormesh(heatmap_df.ix[heatmap_df_order.index].values, cmap=cmap)
    
    
    cbar = fig.colorbar(p, aspect=10,  shrink=0.8, ticks=[-1,0,1])
    cbar.set_ticklabels([''])
    
    ax.set_xlabel('Days Post-Enrollment ', color='black')
    ax.set_axis_bgcolor('black')
    
    
    if ylabel:
        ax.set_ylabel('Participant')
    

    plt.show()

    
ids_180days_of_study = days_till_2015_09_09[days_till_2015_09_09>179].index

for n,symptom in enumerate(['day_symptoms', 'night_symptoms', 'use_qr']):
    
    symptom_title = symptom.replace('_', ' ').upper()
    print symptom_title
    
    if symptom != 'medicine':
        heatmap_df = daily_weekly_df[symptom].map(sum).reset_index().pivot_table(index='healthCode', columns='study_day', values=symptom)  
        
    else:
        heatmap_df = daily_weekly_df[[symptom]]
        heatmap_df[symptom] = heatmap_df[symptom].map(med_multi_parse)
        heatmap_df = heatmap_df.reset_index().pivot_table(index='healthCode', columns='study_day', values=symptom)
    
    #LIMITS PLOTS TO PEOPLE IN STUDY FOR AT LEAST 180 DAYS BY 9/9/2015
    heatmap_df = heatmap_df[heatmap_df.index.isin(ids_180days_of_study)]

    wellcontrolled = heatmap_df[heatmap_df.index.isin(baseline[baseline.gina=='WellControlled']['healthCode'].values)]
    heatmap_plot(wellcontrolled, 'Well Controlled,  n='+ str(wellcontrolled.shape[0]), True)

    partlycontrolled = heatmap_df[heatmap_df.index.isin(baseline[baseline.gina=='PartlyControlled']['healthCode'].values)]
    heatmap_plot(partlycontrolled, 'Partly Controlled,  n=' + str(partlycontrolled.shape[0]), False)

    uncontrolled = heatmap_df[heatmap_df.index.isin(baseline[baseline.gina=='Uncontrolled']['healthCode'].values)]
    heatmap_plot(uncontrolled, 'Uncontrolled,  n=' + str(uncontrolled.shape[0]), False)
```

```markdown
#DAILY CONTROLLER MEDICATION USAGE HEATMAP

rcParams['text.color'] = 'black'
rcParams['ytick.color'] = 'black'
rcParams['xtick.color'] = 'black'



from matplotlib.colors import LinearSegmentedColormap
vmax = 3.0
cmap = LinearSegmentedColormap.from_list('mycmap', [(0 / vmax, 'black'),
                                                    (1 / vmax, 'yellow'),
                                                    (2 / vmax, 'yellow'),
                                                    (3 / vmax, 'red')]
                                         
                                        )

def heatmap_plot(heatmap_df, title, ylabel=True):
    fig, ax = plt.subplots(1,1, figsize=(15,8))
    
    heatmap_df = heatmap_df[range(0,180)]
    
    #Order participants by daily survey completion counts
    
    heatmap_df_order = heatmap_df.apply(np.sum, axis=1)
    heatmap_df_order.sort(inplace=True, ascending=False)
    heatmap_df_order
    
    
    p = ax.pcolormesh(heatmap_df.ix[heatmap_df_order.index].values, cmap=cmap)
    
    
    cbar = fig.colorbar(p, aspect=10,  shrink=0.8, ticks=[0, 1, 2, 3])
    cbar.set_ticklabels([''])
    
    ax.set_axis_bgcolor('black')
    
    
    if ylabel:
        ax.set_ylabel('Participant')
    
    
    plt.show()


def med_multi_parse(x):
    if ',' in str(x):
        x = map(float, x.split(','))
        return min(x)
    return float(x)

    
ids_180days_of_study = days_till_2015_09_09[days_till_2015_09_09>179].index

for n,symptom in enumerate(['medicine']):
    
    symptom_title = symptom.replace('_', ' ').upper()
    print symptom_title
    
    if symptom != 'medicine':
        heatmap_df = daily_weekly_df[symptom].map(sum).reset_index().pivot_table(index='healthCode', columns='study_day', values=symptom)  
        
    else:
        heatmap_df = daily_weekly_df[[symptom]]
        heatmap_df[symptom] = heatmap_df[symptom].map(med_multi_parse)
        heatmap_df = heatmap_df[heatmap_df[symptom]!=4]
        heatmap_df = heatmap_df.reset_index().pivot_table(index='healthCode', columns='study_day', values=symptom)
    
    #LIMITS PLOTS TO PEOPLE IN STUDY FOR AT LEAST 180 DAYS BY 9/9/2015
    
    heatmap_df = heatmap_df.fillna(0)
    
    wellcontrolled = heatmap_df[heatmap_df.index.isin(baseline[baseline.gina=='WellControlled']['healthCode'].values)]
    heatmap_plot(wellcontrolled, 'Well Controlled,  n='+ str(wellcontrolled.shape[0]), True)

    partlycontrolled = heatmap_df[heatmap_df.index.isin(baseline[baseline.gina=='PartlyControlled']['healthCode'].values)]
    heatmap_plot(partlycontrolled, 'Partly Controlled,  n=' + str(partlycontrolled.shape[0]), False)

    uncontrolled = heatmap_df[heatmap_df.index.isin(baseline[baseline.gina=='Uncontrolled']['healthCode'].values)]
    heatmap_plot(uncontrolled, 'Uncontrolled,  n=' + str(uncontrolled.shape[0]), False)
```
