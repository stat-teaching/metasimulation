roc_plot <- function(data){
  
  # Calculate Jouden J
  
  J <- data$tpr + data$tnr - 1
  wJ <- which.max(J)
  txt <- sprintf("$J = %.3f$, ($c = %.3f$, $Sens = %.3f$, $Spec = %.3f$)",
                 J[wJ], data$c[wJ], data$tpr[wJ], data$tnr[wJ])
  
  plot(data$tnr, 
       data$tpr,
       type = "l",
       xlim = rev(range(data$tnr)),
       xlab = "Specificity",
       ylab = "Sensitivity",
       lwd = 2,
       cex.lab = 1.3,
       main = latex2exp::TeX(txt))
  
  abline(coef = c(1, -1), col = "grey")
  segments((data$tnr)[wJ], -1, (data$tnr)[wJ], data$tpr[wJ], lty = "dashed")
  segments(2, data$tpr[wJ], data$tpr[wJ], (data$tnr)[wJ], lty = "dashed")
  
  points((data$tnr)[wJ], data$tpr[wJ],
         pch = 19,
         col = "firebrick",
         cex = 2)
}

ggROC <- function(data, size = 15, fill = FALSE){
  data |> 
    ggplot(aes(x = tnr, y = tpr)) +
    scale_x_reverse(limits = c(1, 0)) +
    ylim(c(0, 1)) +
    geom_abline(intercept = 1, color = "darkgrey") +
    geom_line(linewidth = 1) +
    coord_fixed() +
    theme_minimal(size) +
    xlab("Specificity") +
    ylab("Sensitivity") + {
      if(fill){
        geom_ribbon(aes(ymax = tpr, ymin = 1 - tnr),
                    alpha = 0.1,
                    fill = "firebrick")
      }
    }
}