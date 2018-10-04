# ggCtree: A better regression ctree plot using ggplot2 

```{r}
#install.packages(c("tidyverse","partykit","data.tree","igraph","caret","ggthemes"))

source("ggCtreeReg.R")

```

## Example: 

```{r}
library(partykit)

# air quality dataset
airq <- subset(airquality, !is.na(Ozone))

# fit regression tree 
airct <- ctree(Ozone ~ ., data = airq)

# Standard plot from partykit package
plot(airct)

```

![Standard plot](ctree_plot.png)

```{r}
# Point wisker plot is the default

ggCtreeReg(airct)


```

![ggCtree plot](ggctree1.png)

```{r}
# Now also has density ridges 

ggCtreeReg(airct, type = "ridges", ridges_bandwidth = 10)

```

![ggCtree plot](ggctree2.png)

