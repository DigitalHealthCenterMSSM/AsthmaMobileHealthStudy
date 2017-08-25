## Supplementary Figure 5
```markdown
par(mfrow = c(3, 1))
trig.colors = sample(rainbow(length(bl.north)), length(bl.north))
names(trig.colors) = names(bl.north)
get_stacks(out[[1]], bl.north, "north")
get_stacks(out[[2]], bl.south, "south")
```
