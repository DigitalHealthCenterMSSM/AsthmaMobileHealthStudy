## Supplementary Tables 5D,E
[1], [2], [3] Yes, no, don't know
```markdown
a = table(factor(GINA.cal(cohorts$milestone), levels = c("Well Controlled", "Partly Controlled", 
    "Uncontrolled")), Mile.data[match(cohorts$milestone, Mile.data$healthCode), "prevent_ed"])
a
a/apply(a, 1, sum)
a = table(factor(GINA.cal(cohorts$milestone), levels = c("Well Controlled", "Partly Controlled", 
    "Uncontrolled")), Mile.data[match(cohorts$milestone, Mile.data$healthCode), "prevent_visit"])
a
a/apply(a, 1, sum)
```
