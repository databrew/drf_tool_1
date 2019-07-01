library(databrew)

## Other functions for the tool


## This calculates the RPs and AAL for the perils
calculate.percentiles <- function(data,quantiles) {
  
  output <- quantile(data,quantiles) 
  
  return(output)
  
}




###################################################################
### Some functions 
###################################################################


calculate.financial.strategy.ceding.fast <- function(simulation.data,layer.attach,layer.exhaust,insurance.flag,pct.layer,scale.up) {
  
  
  simulation.data <- simulation.data[order(simulation.data)]
  
  fin.results <- setNames(data.frame(matrix(0,ncol=9,nrow=length(simulation.data))), c('Layer1.used',
                                                                                       'Layer2.used',
                                                                                       'Layer3.used',
                                                                                       'Layer4.used',
                                                                                       'Layer5.used',
                                                                                       'Layer6.used',
                                                                                       'Layer7.used',
                                                                                       'Layer8.used',
                                                                                       'Shortfall'))
  
  nlay <- length(layer.attach)
  lsd <- 1000 ## there are 1000 simulations
  for (layer in 1:nlay) {
    
    if (insurance.flag[layer] == 0) { 
      if (layer == 1) fin.results[,layer] <- (pmin(simulation.data,layer.exhaust[layer]))*pct.layer[layer]/scale.up
      if (layer == 2) fin.results[,layer] <- (pmin(simulation.data - fin.results[,1],layer.exhaust[layer] - 0))*pct.layer[layer]/scale.up
      if (layer > 2) {fin.results[,layer] <- (pmin(simulation.data - rowSums(fin.results[,1:(layer-1)]),layer.exhaust[layer] - 0))*pct.layer[layer]/scale.up }
    } ## end if it is an insurance layer
    
    else { 
      if(layer == 1) fin.results[,layer] <- (pmin(simulation.data - layer.attach[layer],layer.exhaust[layer] - layer.attach[layer]))*pct.layer[layer]/scale.up
      if (layer == 2) {fin.results[,layer] <- pmin(simulation.data - layer.attach[layer], layer.exhaust[layer] - layer.attach[layer], simulation.data - fin.results[,1])*pct.layer[layer]/scale.up}
      if (layer > 2) { fin.results[,layer] <- pmin(simulation.data - layer.attach[layer], layer.exhaust[layer] - layer.attach[layer], simulation.data - rowSums(fin.results[,1:(layer-1)])*pct.layer[layer]/scale.up ) }
      attach.check <- ifelse(simulation.data < rep(layer.attach[layer],lsd), yes = 0, no = 1  ) 
      fin.results[,layer] <- fin.results[,layer]*attach.check
    }
    
  }
  fin.results[,9] <- simulation.data - rowSums(fin.results[,c(1:8)]) 
  fin.results$loss <- simulation.data
  
  return(fin.results)  
}


## NNDIS, Contingency Fund, Contingent Credit, Budget Reallocation, Reins, Borrowing
plot.financial.strategy <- function(strategy.result) {
  
  
  
  lbl <- c(rep(c("Strategy A", "Strategy B","Strategy C"),4))
  lbls <- rev(c("NNDIS","Contingency Fund","Contingent Credit", "Budget Reallocation", "Reinsurance","Unfunded"))
  g <- ggplot(strategy.result, aes(x=RP.type,y=vec/1e+6))
  g <- g + geom_col(aes(fill=fin.type), alpha = 0.5) + theme_economist_white(gray_bg =F) +
    scale_fill_brewer(palette="RdYlBu", labels = lbls, name="") +
    scale_x_continuous(labels = lbl, breaks = seq(1:12)) + theme(axis.text.x = element_text(size=8), axis.title.x = element_text(vjust = - 0.45), axis.title.y = element_text(vjust = 2.45) ) +
    labs(x = 'Amount of Total Loss Funded by Each Instrument', y= 'Estimated Annual Loss (million LKR)') +
    facet_grid(.~lbltop,scales="free") + theme(strip.text.x = element_text(size = 10))
  return(g)
}

##scale_fill_manual(values = c("Strategy A" = 'blue', "Strategy B" = 'red', "Strategy C" = 'yellow'

plot.opportunity.cost <- function(strategy.result) {
  
  
  lbl <- c(rep(c("Strategy A", "Strategy B","Strategy C"),4))
  strategy.result$lbl <- c(rep("Strategy A",4), rep("Strategy B",4),rep("Strategy C",4))
  
  g <- ggplot(strategy.result, aes(x=RP.type,y=vec/1e+6))
  g <- g + geom_col(alpha = 0.7, aes(fill = lbl)) + theme_economist_white(gray_bg =F) + 
    scale_x_continuous(labels = lbl, breaks = seq(1:12)) + theme(axis.text.x = element_text(size=8), axis.title.x = element_text(vjust = - 0.45), axis.title.y = element_text(vjust = 2.45) ) +
    scale_fill_brewer(palette="Reds", labels = lbl, name = "") + 
    labs(x = ' ', y= 'Expected Cost of Financing Instruments (million LKR)') +
    facet_grid(.~lbltop,scales="free") + theme(strip.text.x = element_text(size = 10))
  return(g)  
}



plot.peril.exceedance.curve <- function(data) {
  
  probability <- rev(seq(0.02,0.5,by=0.002))
  data <- data
  
  plot.data <- data.frame(cbind(probability,data))
  
  p <- ggplot(plot.data,aes(probability, data/1e6)) 
  p <- p + geom_line(linetype='longdash',color='red') + scale_x_reverse(labels=scales::percent_format(accuracy = 1)) + theme_economist_white(gray_bg =F) +
    labs(x="Probability of Exceeding Loss", 
         y="Value of Loss (million LKR)")
  
  return(p)  
  
  
}

build.plot.array <- function(input.data,input.data2,input.data3) {
  
  vec <- c(input.data,input.data2,input.data3)
  
  fin.type <- c(rep(c('F','E','D','C','B','A'),12))
  
  RP.type <- c(rep(1,6),rep(4,6),rep(7,6),rep(10,6),
               rep(2,6),rep(5,6), rep(8,6),rep(11,6),
               rep(3,6),rep(6,6), rep(9,6),rep(12,6))
  
  lbl <- c(rep("a) Average Cost",6),rep("b) 1 in 2 year cost",6),rep('c) 1 in 10 year cost',6), rep('d) 1 in 50 year cost',6),
           rep("a) Average Cost",6),rep("b) 1 in 2 year cost",6),rep('c) 1 in 10 year cost',6), rep('d) 1 in 50 year cost',6),
           rep("a) Average Cost",6),rep("b) 1 in 2 year cost",6),rep('c) 1 in 10 year cost',6), rep('d) 1 in 50 year cost',6))
  
  output <- data.frame(cbind(fin.type,RP.type,vec),stringsAsFactors = F)
  names(output) <- c("fin.type","RP.type","vec")
  output$vec <- as.numeric(output$vec)
  output$RP.type <- as.numeric(output$RP.type)
  output$lbltop <- lbl
  
  
  
  return(output)
}

## This builds the array input for the cost benefit funding plot


build.cba.plot.array <- function(cost.benefit.percentiles1,cost.benefit.percentiles2,cost.benefit.percentiles3) {
  
  vec <- c(cost.benefit.percentiles1,cost.benefit.percentiles2,cost.benefit.percentiles3)
  
  RP.type <- c(rep(1,1),rep(4,1),rep(7,1),rep(10,1),
               rep(2,1),rep(5,1), rep(8,1),rep(11,1),
               rep(3,1),rep(6,1), rep(9,1),rep(12,1))
  
  lbl <- c(rep("a) Average Cost",1),rep("b) 1 in 2 year cost",1),rep('c) 1 in 10 year cost',1), rep('d) 1 in 50 year cost',1),
           rep("a) Average Cost",1),rep("b) 1 in 2 year cost",1),rep('c) 1 in 10 year cost',1), rep('d) 1 in 50 year cost',1),
           rep("a) Average Cost",1),rep("b) 1 in 2 year cost",1),rep('c) 1 in 10 year cost',1), rep('d) 1 in 50 year cost',1))
  
  output <- data.frame(cbind(RP.type,vec),stringsAsFactors = F)
  names(output) <- c("RP.type","vec")
  output$vec <- as.numeric(output$vec)
  output$RP.type <- as.numeric(output$RP.type)
  output$lbltop <- lbl
  
  
  
  return(output)
}


# create plot to visualize population data
plot_pop <- function(temp_dat){
  temp_dat$Year <- as.numeric(as.character(temp_dat$Year))
  p <- ggplot(temp_dat, aes(Year, Population, color = Region)) +
    geom_line(alpha = 0.6, size = 2) +
    scale_color_manual(name = '',
                       values = c('black', 'grey', 'blue', 'red', 
                                  'green')) +
    labs(x="Year", 
         y="Population",
         title= 'Population by year') +
    theme_databrew() +
    theme(legend.text = element_text(size = 6))
  return(p)
}


# create plot to visualize archetype data
plot_archetype<- function(temp_dat, region){
  names(temp_dat) <- c('Year', 'Peril', 'Total NNDIS Loss')
  p <- ggplot(temp_dat, aes(Year, `Total NNDIS Loss`, fill = Peril)) +
    geom_bar(stat = 'identity', alpha = 0.6) +
    scale_fill_manual(name = '',
                      values = c('blue', 'red')) +
    labs(x="Year", 
         y="Total NNDIS Loss (LKR, millions)",
         title=paste("Burning cost losses for ",region, sep='')) +
    theme_databrew()
  return(p)
}

# create plot to visualize simulationdata data
plot_sim <- function(temp_dat){
  p <- ggplot(temp_dat, aes(SimulatedNNDISLoss)) +
    geom_histogram(alpha = 0.6) +
    labs(x="Simulated Loss", 
         y="Frequency",
         title='Distribution of simulated loss',
         subtitle = '10k simulations') +
    theme_databrew()
  return(p)
}


# plot.historical <- function(regiondata, region) {
#   agg.data2 <- aggregate(regiondata[,3], by=list(regiondata$Year,regiondata$Peril),FUN="sum")    
#   names(agg.data2) <- c('Year','Peril','Total NNDIS Loss')
#   p <- ggplot(agg.data2,aes(Year, agg.data2$`Total NNDIS Loss`))
#   p <- p + geom_col(aes(fill=Peril)) +  theme_economist_white(gray_bg =F) + 
#     labs(x="Year", 
#          y="Total NNDIS Loss (LKR, millions)",
#          title=paste("Burning cost losses for ",region, sep='')) +
#     scale_fill_manual(name = '',
#                       values = c('blue', 'red'))
#   
#   return(p)  
#   
# }
