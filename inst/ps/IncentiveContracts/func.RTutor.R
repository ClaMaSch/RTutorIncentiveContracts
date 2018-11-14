contract.designs = function(H,r,K,tau,L,cD,dT,minus,plus){
  # marginal benefit of delay function (also with good and bad shocks)
  planned_days = dT
  d = seq(1,3*planned_days,0.2)
  b = (tau*H^2)/(L*d^2) - r*K
  b_good_theta = (tau*(minus*H)^2)/(L*d^2) - r*K
  b_bad_theta = (tau*(plus*H)^2)/(L*d^2) - r*K
  
  len_d = max(d)
  
  # create data frame for ggplot
  df = data.frame(d,b,b_good_theta,b_bad_theta)
  
  # plotting marginal benefit of delay function and the cost of delay (LANE RENTAL CONTRACT DESIGN)
  lanerental= ggplot(df,aes(d)) +
    geom_line(aes(y=b),colour = "#327183",lwd=1) +
    geom_line(aes(y=b_good_theta),colour = "#199b13",lwd=1) + 
    geom_line(aes(y=b_bad_theta),colour = "#85104f",lwd=1) +
    geom_segment(aes(x=0,y=cD,xend=len_d,yend=cD),colour = "#de1c24",lwd = 1.25) +
    geom_point(aes(x=sqrt(tau*H^2/((cD+r*K)*L)),y=cD),size = 2.5) +
    geom_segment(aes(x=sqrt(tau*H^2/((cD+r*K)*L)),y=0,xend=sqrt(tau*H^2/((cD+r*K)*L)),yend=cD),colour = "black",linetype='dashed') +
    geom_point(aes(x=sqrt(tau*(minus*H)^2/((cD+r*K)*L)),y=cD),size = 2.5) +
    geom_segment(aes(x=sqrt(tau*(minus*H)^2/((cD+r*K)*L)),y=0,xend=sqrt(tau*(minus*H)^2/((cD+r*K)*L)),yend=cD),colour = "black",linetype='dashed') +
    geom_point(aes(x=sqrt(tau*(plus*H)^2/((cD+r*K)*L)),y=cD),size = 2.5) +
    geom_segment(aes(x=sqrt(tau*(plus*H)^2/((cD+r*K)*L)),y=0,xend=sqrt(tau*(plus*H)^2/((cD+r*K)*L)),yend=cD),colour = "black",linetype='dashed') +
    ylim(-0.9*r*K,1.5*cD) +
    annotate("text", label = "d*", x = sqrt(tau*(minus*H)^2/((cD+r*K)*L)), y = -0.1*cD, size = 4, colour = "#199b13") +
    annotate("text", label = "d*", x = sqrt(tau*H^2/((cD+r*K)*L)), y = -0.1*cD, size = 4, colour = "#327183") +
    annotate("text", label = "d*", x = sqrt(tau*(plus*H)^2/((cD+r*K)*L)), y = -0.1*cD, size = 4, colour = "#85104f") +
    annotate("text", label = "rental rate", x = 0.85*len_d, y = 1.07*cD, size = 4, colour = "#de1c24") +
    annotate("text", label = "b'(d,bad)", x = 0.85*len_d, y = 0.75*cD, size = 3, colour = "#85104f") +
    annotate("text", label = "b'(d,average)", x = 0.85*len_d, y = 0.6*cD, size = 3, colour = "#327183") +
    annotate("text", label = "b'(d,good)", x = 0.85*len_d, y = 0.45*cD, size = 3, colour = "#199b13") +
    ggtitle("Lane Rental Contract Design") + xlab("Days d") + ylab("Marg. Benefit/Cost of Delay b'(d)") + 
    theme_bw()
  
  if (((tau*H^2)/(L*planned_days^2) - r*K) < 0){
    d_st = sqrt(tau*H^2/(r*K*L))
  } else {
    if (((tau*H^2)/(L*planned_days^2) - r*K) > cD){
      d_st = sqrt(tau*H^2/((r*K+cD)*L))
    } else {
      d_st = planned_days
    }
  }
  
  if (((tau*(minus*H)^2)/(L*planned_days^2) - r*K) < 0){
    dg_st = sqrt(tau*(minus*H)^2/(r*K*L))
  } else {
    if (((tau*(minus*H)^2)/(L*planned_days^2) - r*K) > cD){
      dg_st = sqrt(tau*(minus*H)^2/((r*K+cD)*L))
    } else {
      dg_st = planned_days
    }
  }
  
  if (((tau*(plus*H)^2)/(L*planned_days^2) - r*K) < 0){
    db_st = sqrt(tau*(plus*H)^2/(r*K*L))
  } else {
    if (((tau*(plus*H)^2)/(L*planned_days^2) - r*K) > cD){
      db_st = sqrt(tau*(plus*H)^2/((r*K+cD)*L))
    } else {
      db_st = planned_days
    }
  }
  
  # plotting marginal benefit of delay function and the cost of delay (STANDARD CONTRACT DESIGN)
  standard = ggplot(df,aes(d)) +
    geom_line(aes(y=b),colour = "#327183",lwd=1) +
    geom_line(aes(y=b_good_theta),colour = "#199b13",lwd=1) + 
    geom_line(aes(y=b_bad_theta),colour = "#85104f",lwd=1) +
    geom_segment(aes(x=0,y=0,xend=planned_days,yend=0),colour = "#de1c24",lwd = 1.25) + 
    geom_segment(aes(x=planned_days,y=0,xend=planned_days,yend=cD),colour = "#de1c24",lwd = 1.25) +
    geom_segment(aes(x=planned_days,y=cD,xend=len_d,yend=cD),colour = "#de1c24",lwd = 1.25) + 
    geom_point(aes(x=d_st,y=((tau*H^2)/(L*d_st^2) - r*K)),size = 2.5) +
    geom_point(aes(x=dg_st,y=(tau*(minus*H)^2)/(L*dg_st^2) - r*K),size = 2.5) +
    geom_point(aes(x=db_st,y=(tau*(plus*H)^2)/(L*db_st^2) - r*K),size = 2.5)+
    ylim(-0.9*r*K,1.5*cD) +
    annotate("text", label = "d^T", x = planned_days, y = -0.1*cD, size = 4, colour = "black") +
    annotate("text", label = "time penalty", x = 0.85*len_d, y = 1.07*cD, size = 4, colour = "#de1c24") +
    annotate("text", label = "b'(d,bad)", x = 0.85*len_d, y = 0.75*cD, size = 3, colour = "#85104f") +
    annotate("text", label = "b'(d;average)", x = 0.85*len_d, y = 0.6*cD, size = 3, colour = "#327183") +
    annotate("text", label = "b'(d;good)", x = 0.85*len_d, y = 0.45*cD, size = 3, colour = "#199b13") +
    ggtitle("Standard Contract Desgin") + xlab("Days d") + ylab("Marg. Benefit/Cost of Delay b'(d)") +
    theme_bw()
  library(gridExtra)
  grid.arrange(lanerental,standard,nrow=1)
}

plot.analysis = function(dat,y.var,y.var.name){
  adap.dat = data.frame(dat)
  adap.dat$y.var = y.var
  mean.ontime = mean(filter(adap.dat,dw_norm ==1)$y.var)
  mean.early.x = mean(filter(adap.dat,dw_norm<1)$dw_norm)
  mean.early.y = mean(filter(adap.dat,dw_norm<1)$y.var)
  mean.late.x = mean(filter(adap.dat,dw_norm>1)$dw_norm)
  mean.late.y = mean(filter(adap.dat,dw_norm>1)$y.var)
  output = ggplot(data = dat, aes(x=dw_norm, y=y.var,group=outcome,color=outcome)) +
    geom_point(alpha = 0.3)+ geom_smooth(method="loess", formula=y~x,se=TRUE,n=80,span=0.9)+
    geom_point(aes(x=1,y=mean.ontime),color = "blue",size=3)+
    geom_point(aes(x=mean.early.x,y=mean.early.y),color = "red",size=3)+
    geom_point(aes(x=mean.late.x,y=mean.late.y),color = "green",size=3)+
    xlab("Normalized Days")+ylab(y.var.name)
  return(output)
}

figure.linear.marginal.benefit = function(){
  H = 15000
  r = 0.05
  K = 15000
  L = 20
  tau = 1
  
  # intersection (d_opti,0) to generate nice x-axis range (d)
  d_opti = sqrt(tau*H^2/(r*K*L))
  d = seq(1,250)
  
  # marginal benefit of delay function and the linear approx.
  b = (tau*H^2)/(L*d^2) - r*K
  b.lin = -5000*d + 35*5000
  
  # data frame for ggplot
  df = data.frame(d,b,b.lin)
  
  #plot marginal benefit of delay function, the x-axis and the intersection
  ggplot(df,aes(x=d))+
    geom_line(aes(y=b),colour = "#327183")+
    geom_line(aes(y=b.lin),colour = "#b42222")+
    annotate("text", label = "b'(d) [in theory]", x = 45, y = 40000, size = 4, colour = "#327183")+
    annotate("text", label = "b'(d) [lin. approx.]", x = 45, y = 30000, size = 4, colour = "#b42222")+
    ylim(-5000,180000) + xlim(0,50)+
    geom_segment(aes(x=0,y=0,xend=50,yend=0)) + geom_segment(aes(x=0,y=0,xend=0,yend=175000))+
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
    ggtitle("Linear specification choice of the marginal benefit of delay function")+
    xlab("d (days)") + ylab("Marginal Benefit of Delay")
}

check.error = function(estimates.current,estimates.next,planned_days,X){
  estimates=estimates.current
  # average estimates for the marginal benefit function (just estimated)
  slope = estimates[3]
  intercept = mean(planned_days*X%*%estimates[4:13])
  std.costshock = mean(planned_days*estimates[2])
  enf.prob = estimates[1]
  estimates = c(slope,intercept,std.costshock,enf.prob)
  
  # average estimates for the marginal benefit function (used next)
  slope.next = estimates.next[3]
  intercept.next = mean(planned_days*X%*%estimates.next[4:13])
  std.costshock.next = mean(planned_days*estimates.next[2])
  enf.prob.next = estimates.next[1]
  estimates.next = round(c(slope.next,intercept.next,std.costshock.next,enf.prob.next),digits = 4)
  
  # relative error
  rel.error = (estimates-estimates.next)/estimates.next
  
  # generate data frame
  df.error = data.frame("Slope"=c(slope,slope.next,rel.error[1]),"Intercept"=c(intercept,intercept.next,rel.error[2]),"sigma"=c(std.costshock,std.costshock.next,rel.error[3]),"p"=c(enf.prob,enf.prob.next,rel.error[4]),row.names = c("your current estimates","estimates used next","rel. error"))
  return(grid.table(round(df.error,digits = 6)))
}

map_subsample = function(center,project){
  library(ggmap)
  df = data.frame("lon"=c(center[1],project[1]),"lat"=c(center[2],project[2]))
  
  subsample = get_map(location = c(lon = center[1],lat = center[2]),color = "color",source = "google",
                      maptype = "roadmap",zoom = 7)
  
  ggmap(subsample) +
    geom_point(aes(x=lon[1],y=lat[2]),data=df,color="red",fill = "red",size=125,alpha=0.1) +
    geom_point(aes(x=lon[2],y=lat[2]),data=df,color="red",fill = "red",size=5) +
    labs(x = "Longitude",y = "Latitude")
}

map_detour = function(project,detour_project){
  library(ggmap)
  detour = detour_project

  project_map = get_map(location = c(lon = project[1],lat = project[2]),source = "google",
                        maptype = "roadmap",zoom = 13)
  
  ggmap(project_map) +
    geom_path(aes(x=lon,y=lat),data=detour,colour = "red",size = 2) + 
    geom_point(aes(x=lon[1],y=lat[1]),data=detour,shape=21,colour="black",fill = "green",size=6) +
    annotate("text", label = "A",x=detour$lon[1],y=detour$lat[1],size = 3, colour = "black") +
    geom_point(aes(x=lon[180],y=lat[180]),data=detour,shape=21,colour="black",fill = "green",size=6) +
    annotate("text", label = "B",x=detour$lon[180],y=detour$lat[180], size = 3, colour = "black") +
    ylim(46.458,46.505) +
    labs(x = "Longitude",y = "Latitude") +
    
    # extras (for a nicer plot)
    geom_rect(aes(xmin=-93.912,xmax=-93.912+0.027,ymin=46.491,ymax=46.491+0.005),colour = "black",
              fill = "white",alpha = 0.15) +
    annotate("text",label = "4 Min. (2.9 miles)",x = (-93.912+(-93.912+0.027))/2,y = (46.491+(46.491+0.005))/2,
             size = 4, colour = "black") +
    geom_rect(aes(xmin=-93.950,xmax= -93.95+0.027,ymin=46.465,ymax=46.465+0.005),colour = "black",
              fill = "white",alpha = 0.15) +
    annotate("text",label = "12 Min. (5.5 miles)",x = (-93.950+(-93.950+0.027))/2,y = (46.465+(46.465+0.005))/2,
             size = 4, colour = "black")
}

KPI.result.table = function(dat){
  df=data.frame("Current Policy"=c(dat[,1]),
                "Full Enforcement"=c(dat[,2]),
                "New Penalties"=c(dat[,3]),
                "Lane Rental"=c(dat[,4]),
                row.names = c("Days taken","Commuter Gain [$K]",
                              "Acc. Cost [$K]","Penalties Paid [$K]",
                              "Welfare Gain [$K]","Std. Deviation of Costs [$K]"))
  as.data.frame(df)
  df=grid.table(round(df,digits=2))
  return(df)
}
