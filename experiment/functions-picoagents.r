### Load the libs 

library(plyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

#alg.palette <- brewer.pal(length(unique(runs$alg)), "Set1")[1:length(unique(runs$alg))-1]
#alg.palette <- brewer.pal(length(unique(runs$alg)), "Set1")[1:length(unique(runs$alg))-1]

orange <-"#E69F00"
blue <- "#56B4E9"
green <- "#009E73"
yellow <- "#F0E442"

get.color <- function(algs) {
  pal <- c()
  for (alg in algs) {
    if (is.na(alg)) {
      pal <- c(pal, "#888888")
    }
    else if (alg == "COBRA-singleProcess") {
      pal <- c(pal, "gray")
    }
    else if (alg == "COBRA-1Core") {
      pal <- c(pal, "springgreen3")
    } else if (alg == "COBRA-4Core") {
      pal <- c(pal, "brown")
    } else if (alg == "COBRA-7Core") {
      pal <- c(pal, "magenta")
    } else if (alg == "COBRA-8Core") {
      pal <- c(pal, "firebrick3")
    } else if (alg == "COBRA-Distributed") {
      pal <- c(pal, "deepskyblue1")
    }
  }
  return(pal)
}

get.shape <- function(algs) {
  pal <- c()
  for (alg in algs) {
    if (is.na(alg)) {
      pal <- c(pal, 7)
    } else if (alg == "COBRA-singleProcess") {
      pal <- c(pal, 1)
    }
    else if (alg == "COBRA-1Core") {
      pal <- c(pal, 12)
    } else if (alg == "COBRA-4Core") {
      pal <- c(pal, 2)
    } else if (alg == "COBRA-7Core") {
      pal <- c(pal, 5)
    }else if (alg == "COBRA-8Core") {
      pal <- c(pal, 22)
    } else if (alg == "COBRA-Distributed") {
      pal <- c(pal, 17)
    }
  }
  return(pal)
}

get.linetype <- function(algs) {
  pal <- c()
  for (alg in algs) {
    if (is.na(alg)) {
      pal <- c(pal, "twodash")
    } else if (alg == "COBRA-singleProcess" | alg == "COBRA-1Core" | alg == "COBRA-4Core" | alg == "COBRA-7Core" | alg == "COBRA-8Core" | alg == "COBRA-Distributed") {
      pal <- c(pal, "solid")
    } else {
      pal <- c(pal, "dashed")
    }
  }
  return(pal)
}

###

common.runs <- function(runs, algs) {
  return(runs)
}


############################################
######### MAIN GRAPHS BEGIN ################
############################################

### runtime ###

runtime.vs.nagents <- function(runs, min.instances, maxagents) {
  makespan.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                    N = sum(!is.na(makespan)),
                    mean = mean(makespan),
                    med = median(makespan),
                    sd = sd(makespan),
                    se = sd / sqrt(N),
                    min = min(makespan),
                    max = max(makespan))
  makespan.sum <- makespan.sum[makespan.sum$N >= min.instances, ]

  plot <- ggplot(makespan.sum, aes(x=nagents, y=mean/1000, color=alg, linetype=alg, shape=alg))+
    geom_errorbar(aes(ymin=(mean-se)/1000, ymax=(mean+se)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-sd)/1000, ymax=(mean+sd)/1000), width=0.1, position=pd, size=2, alpha=1) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, position=pd, fill="white")+
    #geom_text(aes(label=N, y=0, size=2), colour="black") +

    scale_color_manual(values=get.color(unique(makespan.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(makespan.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(makespan.sum$alg)), name="method") +

    scale_y_continuous(name="problem solve time [s]") +
    scale_x_continuous(limits=c(0,maxagents+3), name="number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. time to solution")

  return(plot)
}

## speedup ~ no of agents ##

speedup.vs.nagents <- function(runs, min.instances, maxagents) {
  x <-runs
  for (alg in c("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core", "COBRA-7Core", "COBRA-8Core", "COBRA-Distributed")) {
    x$speedup[x$alg==alg] <- 1/(x[x$alg==alg, "makespan"]/x[x$alg=="COBRA-singleProcess", "makespan"])
  }

  # summarize

  speedup.sum <- ddply(x, .(nagents, alg, radius),
                       summarise,
                       N = sum(!is.na(speedup)),
                       mean = mean(speedup),
                       med = median(speedup),
                       sd = sd(speedup),
                       se = sd / sqrt(N),
                       min = min(speedup),
                       max = max(speedup))

  speedup.sum <- speedup.sum[speedup.sum$N >= min.instances, ]

  maxy <- max(speedup.sum$mean+speedup.sum$se, na.rm=TRUE)
  plot <- ggplot(speedup.sum, aes(x=nagents, y=mean, color=alg, shape=alg, linetype=alg))+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0, position=pd, size=2, alpha=0.7) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, fill="white", position=pd)+
    #geom_point(aes(y=med), size=3, shape=18, position=pd)+
    #geom_text(aes(label=N, y=0, size=2), colour="black") +
    scale_y_continuous(limits=c(0,maxy), name="avg. speed-up rel. to original impl. [-]") +
    scale_x_continuous(limits=c(0, maxagents+3), name="number of robots [-]") +
    geom_hline(yintercept = 1, linetype = "longdash", colour="black", alpha=0.5) +

    scale_color_manual(values=get.color(unique(speedup.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(speedup.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(speedup.sum$alg)), name="method") +
    theme_bw() +
    ggtitle("Avg. speed-up relative to original implementation")

  return(plot)
}

## planning time total ###

planningTime.vs.nagents <- function(runs, min.instances, maxagents) {
  planningTime.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                    N = sum(!is.na(avgPlan)),
                    mean = mean(avgPlan, na.rm=TRUE),
                    med = median(avgPlan),
                    sd = mean(varProlongR, na.rm=TRUE),
                    se = sd / sqrt(N*max(nagents)*4),
                    min = min(avgPlan),
                    max = max(avgPlan))
  planningTime.sum <- planningTime.sum[planningTime.sum$N >= min.instances, ]

  plot <- ggplot(planningTime.sum, aes(x=nagents, y=mean/1000, color=alg, linetype=alg, shape=alg))+
    geom_errorbar(aes(ymin=(mean)/1000, ymax=(mean)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    # geom_errorbar(aes(ymin=(mean-se)/1000, ymax=(mean+se)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-sd)/1000, ymax=(mean+sd)/1000), width=0.1, position=pd, size=2, alpha=1) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, position=pd, fill="white")+
    #geom_text(aes(label=N, y=0, size=2), colour="black") +

    scale_color_manual(values=get.color(unique(planningTime.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(planningTime.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(planningTime.sum$alg)), name="method") +

    scale_y_continuous(name="planning time [s]") +
    scale_x_continuous(limits=c(0,maxagents+3), name="number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. planning time")

    return(plot)
}

## planning window ###

planningWindow.vs.nagents <- function(runs, min.instances, maxagents) {
  planningWindow.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                            N = sum(!is.na(avgPWindow)),
                            mean = mean(avgPWindow, na.rm=TRUE),
                            med = median(avgPWindow),
                            sd = mean(varProlongR, na.rm=TRUE),
                            se = sd / sqrt(N*max(nagents)*4),
                            min = min(avgPWindow),
                            max = max(avgPWindow))
  planningWindow.sum <- planningWindow.sum[planningWindow.sum$N >= min.instances, ]

  plot <- ggplot(planningWindow.sum, aes(x=nagents, y=mean/1000, color=alg, linetype=alg, shape=alg))+
    geom_errorbar(aes(ymin=(mean)/1000, ymax=(mean)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-se)/1000, ymax=(mean+se)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-sd)/1000, ymax=(mean+sd)/1000), width=0.1, position=pd, size=2, alpha=1) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, position=pd, fill="white")+
    #geom_text(aes(label=N, y=0, size=2), colour="black") +

    scale_color_manual(values=get.color(unique(planningWindow.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(planningWindow.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(planningWindow.sum$alg)), name="method") +

    scale_y_continuous(name="planning window [s]") +
    scale_x_continuous(limits=c(0,maxagents+3), name="number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. planning window")

  return(plot)
}


## waiting time ###

wait.vs.nagents <- function(runs, min.instances, maxagents) {
  wait.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                              N = sum(!is.na(avgWait)),
                              mean = mean(avgWait, na.rm=TRUE),
                              med = median(avgWait),
                              sd = mean(varProlongR, na.rm=TRUE),
                              se = sd / sqrt(N*max(nagents)*4),
                              min = min(avgWait),
                              max = max(avgWait))
  wait.sum <- wait.sum[wait.sum$N >= min.instances, ]

  plot <- ggplot(wait.sum, aes(x=nagents, y=mean/1000, color=alg, linetype=alg, shape=alg))+
    geom_errorbar(aes(ymin=(mean)/1000, ymax=(mean)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-se)/1000, ymax=(mean+se)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-sd)/1000, ymax=(mean+sd)/1000), width=0.1, position=pd, size=2, alpha=1) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, position=pd, fill="white")+
    #geom_text(aes(label=N, y=0, size=2), colour="black") +

    scale_color_manual(values=get.color(unique(wait.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(wait.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(wait.sum$alg)), name="method") +

    scale_y_continuous(name="waiting [s]") +
    scale_x_continuous(limits=c(0,maxagents+3), name="number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. time waiting for token")

  return(plot)
}

# base time ###

base.vs.nagents <- function(runs, min.instances, maxagents) {
  base.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                    N = sum(!is.na(avgBase)),
                    mean = mean(avgBase, na.rm=TRUE),
                    med = median(avgBase),
                    sd = mean(varProlongR, na.rm=TRUE),
                    se = sd / sqrt(N*max(nagents)*4),
                    min = min(avgBase),
                    max = max(avgBase))
  base.sum <- base.sum[base.sum$N >= min.instances, ]

  plot <- ggplot(base.sum, aes(x=nagents, y=mean/1000, color=alg, linetype=alg, shape=alg))+
    #geom_errorbar(aes(ymin=(mean)/1000, ymax=(mean)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    geom_errorbar(aes(ymin=(mean-se)/1000, ymax=(mean+se)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-sd)/1000, ymax=(mean+sd)/1000), width=0.1, position=pd, size=2, alpha=1) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, position=pd, fill="white")+
    #geom_text(aes(label=N, y=0, size=2), colour="black") +

    scale_color_manual(values=get.color(unique(base.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(base.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(base.sum$alg)), name="method") +

    scale_y_continuous(name="base task time [s]") +
    scale_x_continuous(limits=c(0,maxagents+3), name="number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. base time to reach target")

  return(plot)
}

# trajectory prolongation time ###

prolongT.vs.nagents <- function(runs, min.instances, maxagents) {
  prolongT.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                    N = sum(!is.na(avgProlongT)),
                    mean = mean(avgProlongT, na.rm=TRUE),
                    med = median(avgProlongT),
                    sd = mean(varProlongR, na.rm=TRUE),
                    se = sd / sqrt(N*max(nagents)*4),
                    min = min(avgProlongT),
                    max = max(avgProlongT))
  prolongT.sum <- prolongT.sum[prolongT.sum$N >= min.instances, ]

  plot <- ggplot(prolongT.sum, aes(x=nagents, y=mean/1000, color=alg, linetype=alg, shape=alg))+
    #geom_errorbar(aes(ymin=(mean)/1000, ymax=(mean)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    geom_errorbar(aes(ymin=(mean-se)/1000, ymax=(mean+se)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-sd)/1000, ymax=(mean+sd)/1000), width=0.1, position=pd, size=2, alpha=1) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, position=pd, fill="white")+
    #geom_text(aes(label=N, y=0, size=2), colour="black") +

    scale_color_manual(values=get.color(unique(prolongT.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(prolongT.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(prolongT.sum$alg)), name="method") +

    scale_y_continuous(name="prolongation [s]") +
    scale_x_continuous(limits=c(0,maxagents+3), name="number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. prolongation time on trajectory")

  return(plot)
}

# resting prolongation time ###

prolongR.vs.nagents <- function(runs, min.instances, maxagents) {
  prolongR.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                        N = sum(!is.na(avgProlongR - avgProlongT)),
                        mean = mean(avgProlongR - avgProlongT, na.rm=TRUE),
                        med = median(avgProlongR - avgProlongT),
                        sd = mean(varProlongR - avgProlongT, na.rm=TRUE),
                        se = sd / sqrt(N*max(nagents)*4),
                        min = min(avgProlongR - avgProlongT),
                        max = max(avgProlongR - avgProlongT))
  prolongR.sum <- prolongR.sum[prolongR.sum$N >= min.instances, ]

  plot <- ggplot(prolongR.sum, aes(x=nagents, y=mean/1000, color=alg, linetype=alg, shape=alg))+
    geom_errorbar(aes(ymin=(mean)/1000, ymax=(mean)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-se)/1000, ymax=(mean+se)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-sd)/1000, ymax=(mean+sd)/1000), width=0.1, position=pd, size=2, alpha=1) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, position=pd, fill="white")+
    #geom_text(aes(label=N, y=0, size=2), colour="black") +

    scale_color_manual(values=get.color(unique(prolongR.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(prolongR.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(prolongR.sum$alg)), name="method") +

    scale_y_continuous(name="prolongation [s]") +
    scale_x_continuous(limits=c(0,maxagents+3), name="number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. resting prolongation time")

  return(plot)
}


### plot everything ###

make.grid.plot <- function(env, plotsdir, min.instances.for.summary) {

  dir <- paste("instances/",env, sep="")
  imgdir <- paste(dir, "/figs/", sep="")
  runs <- read.csv(file=paste(dir, "/data.out.head", sep=""), head=TRUE, sep=";")
  runs <- runs[order(runs$instance, runs$alg),]
  runs$makespan[runs$makespan==0] <- NA

  maxagents <- max(runs$nagents)

  runs$alg = factor(runs$alg,levels=c("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core", "COBRA-7Core", "COBRA-Distributed"))

  runtime <-
    runtime.vs.nagents(common.runs(runs, .("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core","COBRA-7Core", "COBRA-Distributed")), min.instances.for.summary, maxagents)

  speedup <-
    speedup.vs.nagents(common.runs(runs, .("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core","COBRA-7Core", "COBRA-Distributed")), min.instances.for.summary, maxagents)

  planningTime <-
   planningTime.vs.nagents(common.runs(runs, .("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core","COBRA-7Core", "COBRA-Distributed")), min.instances.for.summary, maxagents)

  planningWindow <-
    planningWindow.vs.nagents(common.runs(runs, .("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core","COBRA-7Core", "COBRA-Distributed")), min.instances.for.summary, maxagents)

  waiting <-
    wait.vs.nagents(common.runs(runs, .("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core","COBRA-7Core", "COBRA-Distributed")), min.instances.for.summary, maxagents)

  base <-
    base.vs.nagents(common.runs(runs, .("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core","COBRA-7Core", "COBRA-Distributed")), min.instances.for.summary, maxagents)

  prolongT <-
    prolongT.vs.nagents(common.runs(runs, .("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core","COBRA-7Core", "COBRA-Distributed")), min.instances.for.summary, maxagents)

  prolongR <-
    prolongR.vs.nagents(common.runs(runs, .("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core","COBRA-7Core", "COBRA-Distributed")), min.instances.for.summary, maxagents)


  #   runtimeNorm <-
#     runtime.vs.nagents.norm(common.runs(runs, .("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core","COBRA-7Core",  "COBRA-Distributed")), min.instances.for.summary, maxagents)
#
#   speedupNorm <-
#     speedup.vs.nagents.norm(common.runs(runs, .("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core","COBRA-7Core", "COBRA-Distributed")), min.instances.for.summary, maxagents)
#
#   planningTimeTotalNorm <-
#    planningTimeTotal.per.agent.vs.nagents.norm(common.runs(runs, .("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core","COBRA-7Core", "COBRA-Distributed")), min.instances.for.summary, maxagents)
#
#   planningTimeNorm <-
#    planningTime.per.agent.vs.nagents.norm(common.runs(runs, .("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core","COBRA-7Core", "COBRA-Distributed")), min.instances.for.summary, maxagents)
#
#   noPlanTimeNorm <-
#    noPlanTime.per.agent.vs.nagents.norm(common.runs(runs, .("COBRA-singleProcess", "COBRA-1Core", "COBRA-4Core","COBRA-7Core", "COBRA-Distributed")), min.instances.for.summary, maxagents)



  ggsave(filename=paste(plotsdir, env,"-runtime.pdf", sep=""), plot=runtime + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-speedup.pdf", sep=""), plot=speedup + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-planningTime.pdf", sep=""), plot=planningTime + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-planningWindow.pdf", sep=""), plot=planningWindow + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-waiting.pdf", sep=""), plot=waiting + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-base.pdf", sep=""), plot=base + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-prolongT.pdf", sep=""), plot=prolongT + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-prolongR.pdf", sep=""), plot=prolongR + theme(legend.position="bottom"), width=8, height=4)

#   ggsave(filename=paste(plotsdir, env,"-prolong.pdf", sep=""), plot=prolong, width=8, height=5)Norm

  # create a table of individual plots...

  g_legend<-function(p){
    tmp <- ggplotGrob(p)
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}

  legend <- g_legend(runtime)
  lwidth <- sum(legend$width)
  lheight <- sum(legend$heights)

  grid.plots <- arrangeGrob(
    runtime + theme(legend.position="bottom"),
    speedup + theme(legend.position="bottom"),
    planningTime + theme(legend.position="bottom"),
    planningWindow + theme(legend.position="bottom"),
    waiting + theme(legend.position="bottom"),
    base + theme(legend.position="bottom"),
    prolongT + theme(legend.position="bottom"),
    prolongR + theme(legend.position="bottom"),
    ncol=1)
  
  # some versions of ggplot2 and gridExtra are incompatible, causing the following line to fail
  ggsave(filename=paste(plotsdir, env,".pdf", sep=""), plot=grid.plots, width=10, height=60, limitsize = FALSE)
  
  grid.plots
}