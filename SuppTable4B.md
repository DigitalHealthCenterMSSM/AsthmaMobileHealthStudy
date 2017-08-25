## Supplementary Table 4B
```markdown
#Multivariate Cox model 
# outcome measurement: number of days until last daily survey completion

pd.set_option('display.precision',4)
m_summary.index = [i.replace('_', ' ').replace('CurrentAge', 'current age').replace('BiologicalSex', 'biological sex').lower() for i in m_summary.index]
m_summary['lower 0.95'] = m_summary['lower 0.95'].apply(np.exp)
m_summary['upper 0.95'] = m_summary['upper 0.95'].apply(np.exp)

m_summary
```
