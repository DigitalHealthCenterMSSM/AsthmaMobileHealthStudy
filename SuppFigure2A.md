## Supplementary Figure 2A
```markdown
#Covariance Matrix for Numeric Predictors (Standardized)
#Pearson's correlation
cov_df_numeric = cox_df[['study_entry_month', 'CurrentAge', 'age_when_diagnosed', 'Income', 'gina', 'health_insurance','education', 'ethnicity', 'BiologicalSex']]
cov_df_numeric = cov_df_numeric.corr()
cov_df_numeric.columns = [i.replace('_', ' ').replace('CurrentAge', 'current age').replace('BiologicalSex', 'biological sex').lower() for i in cov_df_numeric.columns]
cov_df_numeric.index = [i.replace('_', ' ').replace('CurrentAge', 'current age').replace('BiologicalSex', 'biological sex').lower() for i in cov_df_numeric.columns]

cov_df_numeric_heatmap = seaborn.heatmap(cov_df_numeric, annot=True)

#--------------------------------------------------------------------------------------------------------------------------------------------------------
#Pearson's p-value
from scipy import stats
cov_df = cox_df[['study_entry_month', 'CurrentAge', 'age_when_diagnosed', 'Income', 'gina', 'health_insurance','education', 'ethnicity', 'BiologicalSex']]

cor_pval = []
for c1 in cov_df.columns:
    cor_pval.append([])
    for c2 in cov_df.columns:
        cor_pval[-1].append(stats.pearsonr(cov_df[c1], cov_df[c2])[-1])
cor_pval = pd.DataFrame(cor_pval, columns=cov_df.columns, index=cov_df.columns)
cor_pval.columns = [i.replace('_', ' ').replace('CurrentAge', 'current age').replace('BiologicalSex', 'biological sex').lower() for i in cor_pval.columns]
cor_pval.index = [i.replace('_', ' ').replace('CurrentAge', 'current age').replace('BiologicalSex', 'biological sex').lower() for i in cor_pval.columns]
seaborn.heatmap(cor_pval, annot=True, cmap='Blues')

#--------------------------------------------------------------------------------------------------------------------------------------------------------
#Merged Figure

def triangle_merge(df_triu, df_tril):
    '''
    Combines values from upper triangle
    and lower triangle from dfs
    '''
    triu = pd.DataFrame(np.triu(df_triu.values))
    triu.replace(0.0, np.NaN, inplace=True)
    tril = pd.DataFrame(np.tril(df_tril))
    tril.replace(0.0, np.NaN, inplace=True)

    combined_df = triu.combine_first(tril)
    combined_df.columns = df_triu.columns
    combined_df.index = df_triu.index
    return combined_df

combined_df = triangle_merge(cor_pval, cov_df_numeric)
seaborn.heatmap(combined_df.replace(0.0, 1.0), annot=True,  center=0.05)
plt.xticks(fontsize='x-large')
plt.yticks(fontsize='x-large')
#plt.tight_layout()
#plt.savefig('/Users/ers/Dropbox/feasibility_figures_ERS/CorrelationMatrix.svg', dpi=600, format='svg')
```
