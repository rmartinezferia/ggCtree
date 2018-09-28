
# Check needed packages 

reqPks <- c("tidyverse","partykit","data.tree","igraph","caret","ggthemes")

missPks <- reqPks[!(reqPks %in% installed.packages()[,"Package"])]

if(length(missPks)) install.packages(missPks)


# Functions to plot a regression tree with ggplot2

get_node_splits <- function(fit, node) { 
  
  d <- depth(fit)
  
  
  if(is.terminal(node_party(fit[node]))) {
    
    data.frame(node = node,
               name = NA,
               levels = NA,
               depth = d,
               split = NA)
    
  } else {
    
    data.frame(node = node,
               character_split(split_node(node_party(fit[node])),data = data_party(fit)),
               depth = d - depth(fit[node]),
               split = c(1,2))
    
  }
}

ggCtreeReg <- function(fit, y_units = "") {
  
  require(tidyverse)
  require(partykit)
  require(data.tree)
  require(igraph)
  require(caret)
  require(ggthemes)
  
  # Error check #################################
  
  if(!(any(class(fit) %in% c("constparty","party")))) stop("fit object class is not constparty or party")
  if(!is.numeric(data_party(fit)[,as.character(formula(fit)[[2]])])) stop("Reponse variablie is not numeric")

  # Get data out of fit object #################
  
  # Predictions for nodes
  nodes_pred <- data.frame(node = predict(fit, type = "node"),
                           pred = round(predict(fit),1)) %>%
    bind_cols(as.data.frame(predict(fit, type = "q"))) %>%
    unique() %>%
    arrange(node) %>%
    mutate(n = as.vector(table(predict(fit, type = "node"))))
  
  # Get nodes
  nodes <- suppressWarnings(data.frame(node = nodeids(fit)) %>%
                              rowwise() %>% 
                              do(x = get_node_splits(fit,.$node)) %>%
                              unnest()) 
  
  #  Get paths
  path <- as.data.frame(get.edgelist(as.igraph.Node(as.Node(fit)))) %>%
    `names<-`(c("node_from","node_to")) %>%
    mutate_all(function(x) as.numeric(as.character(x))) %>%
    mutate(split = rep(1:2,length(node_from)/2))
  
  path <- path %>%
    left_join(nodes %>% transmute(node, depth_from = depth),
              by = c("node_from" = "node")) %>%
    left_join(path %>%
                left_join(nodes %>% transmute(node, depth_to = depth),
                          by = c("node_to" = "node"))) %>%
    mutate(depth_from = depth_from) %>%
    left_join(path %>%
                left_join(nodes %>% transmute(node, depth_from = depth, levels, split),
                          by = c("node_from" = "node",
                                 "split" = "split"))) %>%
    unique()
  
  # Plot the regression tree ##################
  
  # Ajustment factor for plot
  xadj <- max(nodes$depth)
  
  xlims <- c(min(nodes_pred$`10%`), max(nodes_pred$`90%`))
  
  # Re-scaled node prediction data
  nodes_pred_scaled <- data.frame(lapply(nodes_pred[,2:5],function(x)(x - xlims[1])/(xlims[2] - xlims[1])),
                                  node = nodes_pred$node,
                                  n = nodes_pred$n)
  #Base plot
  p <- ggplot(data = path) +
    # Elbow connector
    geom_segment(aes(y = node_from + xadj/30 ,
                     yend = node_to, 
                     x = depth_from + xadj/30, 
                     xend = depth_from + xadj/30)) + 
    geom_segment(aes(y = node_to,
                     yend = node_to, 
                     x = depth_from + xadj/30 ,
                     xend = depth_to),
                 arrow = arrow(ends = "last", length = unit(0.1, "inches"), type = "closed")) + 
    geom_label(aes(x = depth, y = node, label = gsub("_"," ",name)), fill = "gray90", 
               data = nodes %>% left_join(nodes_pred),
               label.size = 1.2,hjust = 0, size = 5) + 
    geom_label(aes(y = node_to, x = depth_from + 0.25, label = levels), fill = "white", 
               label.size =0,hjust = 0, size = 3) + 
    scale_y_reverse(breaks = 1:max(nodes$node),name = "Node") #
  
  # Add point-wisker point
  p <- p + 
    geom_errorbarh(aes(y = node, 
                       xmin = xadj + X10., 
                       xmax = xadj + X90.),
                   data = nodes_pred_scaled) +
    geom_text(aes(y = node, 
                  x = xadj + X90. + 0.02, 
                  label = paste("n =",n)), 
              hjust = 0, fontface = "italic",  size = 2.5,
              data = nodes_pred_scaled) +
    geom_point(aes(y = node, 
                   x = xadj + pred),
               fill = "red", shape = 21, size = 4,
               data = nodes_pred_scaled) +
    scale_x_continuous(breaks = xadj + seq(0,1, by = 0.2),
                       labels = round(seq(xlims[1],xlims[2], by = diff(xlims)/5))) + 
    xlab(paste(as.character(formula(fit)[[2]]),y_units))
  
  # Add Annoation with fit statistics
  a <- defaultSummary(data.frame(obs = data_party(fit)[,as.character(formula(fit)[[2]])],
                                 pred = predict(fit,newdata = data_party(fit))))
  stat_lab <- paste0("R^2 ==",round(a[2],2),"~~~MAE==",round(a[3],1),"~~~RMSE==",round(a[1],1))
  
  p <- p +
    annotate("text",y= 0.5 + max(nodes$node),x=0, label = "Tree fit statistics:", hjust = 0, size = 5) + 
    annotate("text",y=   1 + max(nodes$node),x=0, label = stat_lab, parse = T, hjust = 0)
  
  p <- p +
    theme_base() + 
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_text(hjust = (0.5 + xadj)/(xadj + 1)),
          plot.title = element_text(size = 32, hjust = 0.5),
          panel.background = element_rect(colour = "black"),
          axis.line.x.top = element_line(colour = "black"))
  
  # Return 
  return(p)
} 




