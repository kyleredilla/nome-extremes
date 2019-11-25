# Helper functions for analysis of Nome meteorological variables

# Custom quantile mapping function
qMap <- function(obs = NULL, sim, 
                 ret.deltas = FALSE, 
                 use.deltas = NULL){
  
  if(is.null(use.deltas)){
    qn <- min(length(obs), length(sim))
    qx <- quantile(sim, seq(0, 1, length.out = qn), type = 8)
    qy <- quantile(obs, seq(0, 1, length.out = qn), type = 8)
    q_deltas <- qx - qy
  } else {
    qx <- quantile(sim, seq(0, 1, length.out = length(use.deltas)), 
                   type = 8)
    q_deltas = use.deltas
  }
  
  # bin "sim" observations into quantiles. Will use these indices to 
  #   index deltas vector for adjustment
  qi <- .bincode(sim, qx, include.lowest = TRUE)
  
  df <- data.frame(lower=sort(unique(qi)), freq=as.integer(table(qi)))
  df$upper <- c(df$lower[-1] - df$lower[-nrow(df)], 1) + df$lower - 1
  # want to omit this adjustment if the first quantile is also the first
  #   duplicate
  ub <- df$lower != 1
  df$upper[ub] <- df$upper[ub] - as.numeric(df$upper[ub] > df$lower[ub] & 
                                              qx[df$upper[ub]] < qx[df$upper[ub] + 1])
  
  recycled <- apply(df, 1, function(x) {
    out <- rep(x["lower"]:x["upper"], length.out=x["freq"])
    
    return(out)
  })
  
  qi <- unlist(recycled)[order(order(qi))]
  
  sim_adj <- sim - as.numeric(q_deltas)[qi]
  df <- data.frame(obs = obs, 
                   sim = sim,
                   sim_adj = sim_adj)
  # return adjusted
  if(ret.deltas == TRUE){
    return(list(deltas = q_deltas, df = df))
  } else {return(df)}
}

# compare ECDFs to validate quantile mapping
ggECDF_compare <- function(df, p_title = " "){
  require(gridExtra)
  require(ggplot2)
  
  obs <- df$obs
  sim <- df$sim
  sim_adj <- df$sim_adj
  
  df1 <- data.frame(sped = c(sim, obs),
                    src = c(rep("1", length(sim)),
                            rep("2", length(obs))))
  
  df2 <- data.frame(sped = c(sim_adj, obs),
                    src = c(rep("1", length(sim_adj)),
                            rep("2", length(obs))))
  
  # extract legend, code borrowed from SO (for sharing legend between plots)
  # https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  # original data
  xmax <- quantile(obs, probs = seq(0, 1, 1/100))[100] + 10
  p1 <- ggplot(df1, aes(sped, color = src)) + 
    stat_ecdf(size = 1) + 
    xlab(bquote(T[min])) + ylab("Cumulative Probability") + 
    xlim(c(-40, xmax)) + scale_color_discrete(name = "Data", 
                                              labels = c("Sim", "Obs")) + 
    theme(legend.position = "bottom") +
    ggtitle(p_title)
  
  # corrected data
  p2 <- ggplot(df2, aes(sped, color = src)) + 
    stat_ecdf(size = 1) + 
    xlab(bquote(T[min])) + ylab(element_blank()) + 
    xlim(c(-40, xmax))  + ggtitle(" ")
  
  # legend code adapted from:
  # https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  tmp <- ggplot_gtable(ggplot_build(p1))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  mylegend <- tmp$grobs[[leg]]
  
  p <- arrangeGrob(arrangeGrob(p1 + theme(legend.position = "none"),
                               p2 + theme(legend.position = "none"), 
                               nrow = 1),
                   mylegend, nrow=2, heights = c(10, 1))
  return(p)
}
