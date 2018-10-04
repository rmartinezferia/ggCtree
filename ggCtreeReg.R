
# Check needed packages 

reqPks <- c("tidyverse","partykit","data.tree","igraph","caret","ggpubr","ggridges")

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


base_breaks_x <- function(x){
  b <- pretty(x)
  d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_x_continuous(breaks=b))
}


ggCtreeReg <- function(fit, type = c("wisker","ridges"), 
                       xlabel = NULL, vline = NULL, ridges_bandwidth = NULL,
                       labels_fill = "gray90",ridges_fill = alpha("blue",0.5),labels_colour = "black") {
  
  require(tidyverse)
  require(partykit)
  require(data.tree)
  require(igraph)
  require(caret)
  require(ggridges)
  require(ggpubr)
  
  # Error check #################################
  
  if(!(any(class(fit) %in% c("constparty","party")))) stop("fit object class is not constparty or party")
  if(!is.numeric(data_party(fit)[,as.character(formula(fit)[[2]])])) stop("Reponse variable is not numeric")
  if(!(type[1] %in% c("wisker","ridges"))) stop("type should be 'wisker' or 'ridges")
  
  # Get data out of fit object #################
  
  # Get nodes
  nodes <- suppressWarnings(data.frame(node = nodeids(fit)) %>%
                              rowwise() %>% 
                              do(x = get_node_splits(fit,.$node)) %>%
                              unnest()) 
  
  # Predictions for nodes
  nodes_pred <- data.frame(node = predict(fit, type = "node"),
                           pred = round(predict(fit),2)) %>%
    bind_cols(as.data.frame(predict(fit, type = "q"))) %>%
    unique() %>%
    arrange(node) %>%
    mutate(n = as.vector(table(predict(fit, type = "node")))) %>%
    bind_rows(data.frame(node = nodes$node[!is.na(nodes$name)])) %>%
    mutate(node = factor(node, max(nodes$node):1))
  
  # Get original data for ridges
  nodes_data<- data_party(fit)[,c("(fitted)","(response)")] %>%
    transmute(y = `(response)`, node = `(fitted)`) %>%
    bind_rows(data.frame(y = NA, node = nodes$node[!is.na(nodes$name)])) %>%
    mutate(node = factor(node, max(nodes$node):1))
  
  
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
  
  # Look for xlabel
  if(is.null(xlabel))  xlabel <- as.character(formula(fit)[[2]])
  
  
  # Theme for plotting
  
  basetheme <-  theme_gray()+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = unit(c(0,0,2,0),"mm"),
          axis.title.y = element_blank(),
          legend.position = "none")
  
  # Re-scaled node prediction data
  #nodes_pred_scaled <- data.frame(lapply(nodes_pred[,2:5],function(x)(x - xlims[1])/(xlims[2] - xlims[1])), node = nodes_pred$node,                                  n = nodes_pred$n)
  
  #Base plot
  p1 <- ggplot(data = path) +
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
    geom_label(aes(x = depth, y = node, label = gsub("_"," ",name)),
               data = nodes %>% left_join(nodes_pred %>%
                                            mutate(node = as.numeric(as.character(node)))),
               label.size = 0.5, hjust = 0, size = 5, fill = labels_fill,
               colour = labels_colour) + 
    geom_label(aes(y = node_to, x = depth_from + 0.25, label = levels), fill = "white", 
               label.size =0,hjust = 0, size = 3) + 
    xlab(" ") +
    scale_y_reverse(breaks = 1:max(nodes$node),name = "Node") 
  
  
  # Add Annoation with fit statistics
  a <- defaultSummary(data.frame(obs = data_party(fit)[,as.character(formula(fit)[[2]])],
                                 pred = predict(fit,newdata = data_party(fit))))
  
  stat_lab <- paste0("r^2 ==",round(a[2],2),"~~~MAE==",round(a[3],1),"~~~RMSE==",round(a[1],1))
  
  p1 <- p1 +
    annotate("text",y= max(nodes$node) - 1,x=0, label = "Tree fit statistics:", hjust = 0, size = 5) + 
    annotate("text",y= max(nodes$node) - 0,x=0, label = stat_lab, parse = T, hjust = 0)
  
  # Add theme 
  p1 <- p1 + 
    basetheme + 
    theme(axis.text.x = element_text(colour = "white"),
          axis.ticks.x = element_line(colour = "white"))
  
  
  # Add plot of data 
  
  if(type[1] == "ridges"){
    
    p2 <- nodes_data %>%
      ggplot(aes(y, factor(node))) +
      stat_density_ridges(geom = "density_ridges_gradient",
                          fill = ridges_fill,
                          quantile_lines = TRUE, scale = 0.8,
                          bandwidth = ridges_bandwidth, 
                          size = 0.5,calc_ecdf = TRUE, quantiles = c(0.05,0.5,0.95)) +
      geom_text(aes(x = pred, label = paste("n =",n)), 
                fontface = "italic",  size = 2.5,
                position = position_nudge(y=-0.25),
                data = nodes_pred) +
      geom_point(aes(x = pred), data = nodes_pred, size = 2, position = position_nudge(y=0)) + 
      labs(y="", x = xlabel) +
      basetheme + 
      base_breaks_x(nodes_data$y)
    
  }
  
  if(type[1] == "wisker"){
    
    p2 <- nodes_pred %>%
      ggplot(aes(y=node)) +
      geom_errorbarh(aes(y = node, xmin =`10%`, xmax = `90%`)) +
      geom_point(aes(x = pred), fill = "red", shape = 21, size = 4) + 
      geom_text(aes(x =`90%`, label = paste("  n =",n)), 
                fontface = "italic",  size = 2.5, hjust = 0,
                data = nodes_pred) +
      labs(y="", x = xlabel) +
      theme(axis.title.y = element_blank()) + 
      basetheme + 
      base_breaks_x(nodes_data$y)
    
  }
  
  if(!is.null(vline)) p2 <- p2 + geom_vline(xintercept = vline, linetype = 3)
  
  # Return 
  return(ggarrange(p1,p2, widths = c(0.7,0.3)))
} 
