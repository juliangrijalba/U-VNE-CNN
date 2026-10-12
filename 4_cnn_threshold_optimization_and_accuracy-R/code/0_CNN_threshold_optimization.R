# authors: Julián Grijalba <juliangrijalba@unimayor.edu.co> and Carlos Duran <duranurbanphony@unimayor.edu.co> based on Quinn et al. (2023)
# output: thresholds optimization for CNN inferences (including confusion matrices)

library(showtext)
showtext_auto()  # activate showtext for ggplot y ggsave
# metropolis font
font_path <- "C:/Users/Acer/AppData/Local/Microsoft/Windows/Fonts/"

font_add("Metropolis-Regular",      paste0(font_path, "Metropolis-Regular.otf"))
font_add("Metropolis-Bold",         paste0(font_path, "Metropolis-Bold.otf"))
font_add("Metropolis-SemiBold",     paste0(font_path, "Metropolis-SemiBold.otf"))
font_add("Metropolis-Light",        paste0(font_path, "Metropolis-Light.otf"))
font_add("Metropolis-Medium",       paste0(font_path, "Metropolis-Medium.otf"))
font_add("Metropolis-Black",        paste0(font_path, "Metropolis-Black.otf"))
font_add("Metropolis-ExtraBold",    paste0(font_path, "Metropolis-ExtraBold.otf"))
font_add("Metropolis-Italic",       paste0(font_path, "Metropolis-RegularItalic.otf"))

library(dplyr)
library(ggplot2)
library(WeightedROC)
library(e1071)
library(caret)  
library(patchwork)

# DATA PREPARATION

# CNN inference csvs (here, a test and train/val csv for each class; 9 in total)
inf_wd = "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/3_cnn_inference-py/results"   

datainftest <- read.csv("C:/Users/Acer/Desktop/TSI/U_VNE_CNN/3_cnn_inference-py/results/test_inf.csv")
datainftrval <- read.csv("C:/Users/Acer/Desktop/TSI/U_VNE_CNN/3_cnn_inference-py/results/trval_inf.csv")


# desired beta values for F-score optimization
betas = c('0.50', '0.75', '1.00', '1.25', '1.50') 

# label classes
labels = c("TM", "MH", "VO", "MU", "EM", "SO", "BI", "GE", "SI")
csvs = list.files(inf_wd, pattern = "*.csv", full.names = F)

# combine CSV files to concatenate test and trval labels
csv_ls = list()
counter = 1

for(file in csvs){
  print(file)
  split = strsplit(file, "_")[[1]][1]
  split = strsplit(split, ".csv")[[1]]
  
  # detect trval or test split from filename
  if (grepl("^trval", file, ignore.case = TRUE)) split = "trval"
  if (grepl("^test",  file, ignore.case = TRUE)) split = "test"
  
  temp = read.csv(paste0(inf_wd,"/",file))
  colnames(temp)[1:9] = c("TM*","MH*","VO*","MU*","EM*","SO*","BI*","GE*", "SI*")
  temp$split = split
  csv_ls[[counter]] = temp
  counter = counter + 1 
}

# dataframes for trval and test labells combined and separated
full_df = do.call("rbind", csv_ls)
full_test_df = subset(full_df, split == "test")
full_trval_df = subset(full_df, split == "trval")

# MODEL ANALYSIS BASED ON TEST DATA FOR EACH LABEL AND A RANGE OF F(BETA) VALUE

# iterate over each sound class (inner loop) for each beta value (outer loop)
for(b in betas) {
  beta_label <- b 
  print(paste0("Optimizing beta = ", b))
  b_char = gsub('.','',b, fixed = TRUE)
  b_num = as.numeric(b)
  
  # where results will be saved in this loop
  out_dir = paste0("C:/Users/Acer/Desktop/TSI/U_VNE_CNN/4_cnn_threshold_optimization_and_accuracy-R/results/performance_fscore_", b_char ,'/') # to obtain all beta levels
  dir.create(out_dir)
  
  # containers for thresholds
  final_thresh_ls = list()
  threshold_ls = list()
  
  for(i in 1:length(labels)){
    target_label = labels[i]
    print(target_label)
    
    # assign ground truth label for the current target class
    full_trval_df$label  <- ifelse(grepl(target_label, full_trval_df$label_comb), target_label, "OT") # OT = other class
    full_test_df$label   <- ifelse(grepl(target_label, full_test_df$label_comb),  target_label, "OT") # OT = other class
    
    # subset prediction probability and true class
    tr_df  = full_trval_df[c(paste0(target_label,"*"), "label")]
    test_df = full_test_df[c(paste0(target_label,"*"), "label")] 
    
    # extract predicted probability values and one-hot encode labels
    probabilities = test_df
    probabilities_tr = tr_df    
    pred_prob_vec = probabilities[paste0(target_label,"*")]
    pred_prob_vec_tr = probabilities_tr[paste0(target_label,"*")] 
    ts_y_ohe = ifelse(probabilities$label == (target_label), 1, 0)
    tr_y_ohe = ifelse(probabilities_tr$label == (target_label), 1, 0) 
    
    # threshold Optimization
    thresh_dt_list = list()
    tsearch = seq(0.0001, 0.9999, by = 0.0001) # incremental steps to find best threshold value
    for (t in tsearch) {
      pred_class = ifelse(pred_prob_vec >= t, 1, 0) # pull out positive vector and binarize based on threshold
      
      # calculations to clean up Spec and Sens section, below
      a = sum(pred_class==1 & ts_y_ohe==1) # True Positive
      b = sum(pred_class==1 & ts_y_ohe==0) # False Positive
      c = sum(pred_class==0 & ts_y_ohe==1) # False Negative
      d = sum(pred_class==0 & ts_y_ohe==0) # True Negative
      precision = a / (a + b) #PPV
      recall = a / (a + c) # aka sensitivity/TPR
      fscore = (1 + b_num^2) * ((precision * recall) / ((b_num^2 * precision) + recall))
      f050 = (1 + 0.5^2) * ((precision * recall) / ((0.5^2 * precision) + recall))
      f075 = (1 + 0.75^2) * ((precision * recall) / ((0.75^2 * precision) + recall))
      f100 = (1 + 1^2) * ((precision * recall) / ((1^2 * precision) + recall))
      
      # assign threshold and scores to list
      thresh_dt_list[[t*10000]] <- data.table::data.table(threshold = t, f = fscore, f050 = f050, f075 = f075, f100 = f100)
      }
      thresh_dt <- do.call(rbind, thresh_dt_list)
      final_thresh <- thresh_dt[which.max(thresh_dt$f),] #select maximum fbeta value and its threshold
      pred_class_vec <- ifelse(pred_prob_vec >= final_thresh$threshold, 1, 0) # calculate classes based on optimized threshold
      pred_class_vec_tr = ifelse(pred_prob_vec_tr >= final_thresh$threshold, 1, 0)
      
      # create confusion matrix
      a_tr = sum(pred_class_vec_tr==1 & tr_y_ohe==1) # True Positive
      b_tr = sum(pred_class_vec_tr==1 & tr_y_ohe==0) # False Positive
      c_tr = sum(pred_class_vec_tr==0 & tr_y_ohe==1) # False Negative
      d_tr = sum(pred_class_vec_tr==0 & tr_y_ohe==0) # True Negative
      precision_tr = a_tr / (a_tr + b_tr) #PPV
      recall_tr = a_tr / (a_tr + c_tr) # aka sensitivity/TPR
      fscore_tr = (1 + b_num^2) * ((precision_tr * recall_tr) / ((b_num^2 * precision_tr) + recall_tr))
      f050_tr = (1 + 0.5^2) * ((precision_tr * recall_tr) / ((0.5^2 * precision_tr) + recall_tr))
      f075_tr = (1 + 0.75^2) * ((precision_tr * recall_tr) / ((0.75^2 * precision_tr) + recall_tr))
      f100_tr = (1 + 1^2) * ((precision_tr * recall_tr) / ((1^2 * precision_tr) + recall_tr))
      confMat_ts <- caret::confusionMatrix(data = as.factor(pred_class_vec), reference = as.factor(ts_y_ohe), positive = "1")	
      confMat_tr <- caret::confusionMatrix(data = as.factor(pred_class_vec_tr), reference = as.factor(tr_y_ohe), positive = "1")
      roc_df <- WeightedROC::WeightedROC(guess = pred_prob_vec[,1], label = as.factor(ts_y_ohe))
      roc_df_tr <- WeightedROC::WeightedROC(guess = pred_prob_vec_tr[,1], label = as.factor(tr_y_ohe))
      
        ######## PREPARE CONFUSION MATRIX PLOTS ########
        # prepare data (test data only)
        cm <- confMat_ts$table
        cm_df <- as.data.frame(cm)
        colnames(cm_df) <- c("Predicted", "True", "Freq")
      
        # visual order
        cm_df$Predicted <- factor(cm_df$Predicted, levels = c("0", "1"))
        cm_df$True      <- factor(cm_df$True,      levels = c("1", "0"))
        
        # convert prediction counts to percentages
        total_n <- sum(cm_df$Freq)
        cm_df$Perc <- 100 * cm_df$Freq / total_n
        cm_df$label_pct <- sprintf("%.1f %%", cm_df$Perc)
        
        # classify data as TP, TN, FP and FN
        cm_df$type <- dplyr::case_when(
          cm_df$True == "1" & cm_df$Predicted == "1" ~ "TP",
          cm_df$True == "0" & cm_df$Predicted == "0" ~ "TN",
          cm_df$True == "0" & cm_df$Predicted == "1" ~ "FP",
          cm_df$True == "1" & cm_df$Predicted == "0" ~ "FN"
        )
        
        # discretize percentages into 10% bins
        cm_df$alpha_group <- cut(
          cm_df$Perc,
          breaks = c(0,10,20,30,40,50,60,70,80,90,100),
          include.lowest = TRUE
        )
        
        # alpha values to create a visual ramp effect
        alpha_vals <- seq(0.15, 1.0, length.out = length(levels(cm_df$alpha_group)))
        names(alpha_vals) <- levels(cm_df$alpha_group)
        
        # base color by correct and error predictions and ensure percentage label legibility
        cm_df$fill_base  <- ifelse(cm_df$type %in% c("TP","TN"), "correct", "error")
        cm_df$text_color <- ifelse(cm_df$Perc >= 40, "white", "black")
        
        # draw confusion matrices
        p_cm <- ggplot(cm_df, aes(x = Predicted, y = True)) +
          # base color and alpha values
          geom_tile(
            aes(fill = fill_base, alpha = alpha_group),
            color = "black",
            linewidth = 0.6
          ) +
          geom_text(
            aes(label = label_pct, color = text_color),
            size = 16,
            family = "Metropolis-SemiBold"
          ) +
          scale_color_identity() +
          scale_fill_manual(
            values = c(
              correct = "#ff007f",
              error   = "#00f6ff"
            ),
            guide = "none"
          ) +
          scale_alpha_manual(values = alpha_vals, guide = "none") +
          
          # title and axis
          ggtitle(paste("Confusion Matrix -", target_label, "- Beta", beta_label)) +
          theme_minimal(base_family = "Metropolis-Regular") +
          theme(
              plot.title = element_text(
              size   = 60,
              family = "Metropolis-Bold",  
              face   = "plain",            
              hjust  = 0.5
            ),            ,
            
            axis.title = element_text(size = 60),
            axis.title.x = element_text(
              margin = margin(t = 15)   
            ),
            axis.title.y = element_text(
              margin = margin(r = 15)
            ),
            axis.text  = element_text(size = 30)
          )
        
        # matrices leyend
        
        legend_df <- data.frame(
          y     = seq(30, 70, by = 1),
          alpha = seq(0.15, 1.0, length.out = 41)
        )
        
        make_legend <- function(fill_color, subtitle_text) {
          ggplot(legend_df, aes(x = 1, y = y)) +
            
            geom_tile(aes(alpha = alpha),
                      fill  = fill_color,
                      width = 0.06) +
            annotate(
              "text",
              x = 1, y = 75,
              label = subtitle_text,
              family = "Metropolis-SemiBold",
              size = 12
            ) +
            
            annotate("segment",
                     x = 0.96, xend = 1.04,
                     y = 29.5, yend = 29.5) + # 30 - 0.5
          
            annotate("segment",
                     x = 0.96, xend = 1.04,
                     y = 70.5, yend = 70.5) + # 70 + 0.5
            
            annotate("text",
                     x = 0.94, y = 30,
                     label = "0%",
                     family = "Metropolis-Light",
                     size = 8,
                     hjust = 1) +
            
            annotate("text",
                     x = 0.94, y = 70,
                     label = "100%",
                     family = "Metropolis-Light",
                     size = 8,
                     hjust = 1) +
            
            scale_alpha_identity() +
            scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
            scale_x_continuous(limits = c(0.93, 1.07)) +
            coord_cartesian(clip = "off") +
            theme_void() +
            theme(
              text = element_text(family = "Metropolis-Regular"),
              
              # space between matrix and legend
              plot.margin = margin(
                t = 0, r = 0, b = 0, l = 30
              )
            )
        }
        
        p_legend_errors  <- make_legend("#00f6ff", "FP/FN")
        p_legend_correct <- make_legend("#ff007f", "TP/TN")
        
        ######## PREPARE CONFUSION MATRIX NUMBERS ########
        
        out_ConfMat = data.frame("train" = c(confMat_tr$overall, 
                                             confMat_tr$byClass, 
                                             "positive" = confMat_tr$positive, 
                                             "th" = final_thresh$threshold,
                                             RMSE = sqrt(mean((pred_class_vec_tr - (tr_y_ohe))^2)),
                                             fbeta = b_num, # the beta value
                                             fscore = fscore_tr, # fscore based on current fbeats (e.g. is variable - hard to compare across)
                                             f050 = f050_tr,
                                             f075 = f075_tr,
                                             f100 = f100_tr,
                                             auc = WeightedROC::WeightedAUC(roc_df_tr)),
                                 "test" = c(confMat_ts$overall, 
                                            confMat_ts$byClass, 
                                            "positive" = confMat_ts$positive, 
                                            "th" = final_thresh$threshold,
                                            RMSE = sqrt(mean((pred_class_vec - ts_y_ohe)^2)),
                                            fbeta = b_num,
                                            fscore = final_thresh$f,
                                            f050 = final_thresh$f050,
                                            f075 = final_thresh$f075,
                                            f100 = final_thresh$f100,
                                            auc = WeightedROC::WeightedAUC(roc_df)))
        
        ######### SAVE CONFUSION MATRICES ##########
        
          # save confusion matrix plots
          final_plot <- wrap_plots(
            p_cm,
            p_legend_errors,
            p_legend_correct,
            ncol = 3,
            widths = c(1, 0.15, 0.15) # space
          )
          
          ggsave(
            filename = paste0(out_dir, target_label, "_confmat_beta_", b_char, ".png"),
            plot  = final_plot,
            width = 9,
            height = 5,
            dpi   = 300
          )  
            
          # save confusion matrix numeric values
          write.csv(out_ConfMat, file = paste0(out_dir, target_label, "_model_acc_metrics.csv"))
          
          thresh_dt$label = target_label
          final_thresh_ls[[i]] = final_thresh
          threshold_ls[[i]] = thresh_dt
          }
          
  
          ######## PREPARE THRESHOLDS PLOTS ########
          
          # turn threshold data into dataframes
          threshold_df <- do.call(rbind, threshold_ls)
          threshold_df <- as.data.frame(threshold_df[complete.cases(threshold_df), ])
          final_thresholds <- as.data.frame(do.call(rbind, final_thresh_ls))
          
          # translate Spanish class abbreviations into English
          label_translation <- c(
            TM = "MT",  # Motorised Transport
            MH = "HM",  # Human Movement
            VO = "VO",  # Voices
            MU = "MU",  # Music
            EM = "EM",  # Electro-mechanical sounds
            SO = "SO",  # Social sounds
            BI = "BI",  # Biophony
            GE = "GE",  # Geophony
            SI = "SI"   # Silence
          )
          
          # apply English labels while preserving the original class order
          final_thresholds$label <- unname(label_translation[labels])
          
          threshold_df$label <- unname(
            label_translation[as.character(threshold_df$label)]
          )
          
          final_thresholds$label <- factor(
            final_thresholds$label,
            levels = unname(label_translation[labels])
          )
          
          threshold_df$label <- factor(
            threshold_df$label,
            levels = unname(label_translation[labels])
          )
          
          # add threshold values to the legend labels
          final_thresholds$label2 <- paste0(
            final_thresholds$label,
            ": ",
            sprintf("%.4f", final_thresholds$threshold)
          )
          
          threshold_df$label2 <- final_thresholds$label2[
            match(threshold_df$label, final_thresholds$label)
          ]
          
          # preserve class order in the legend
          order <- unname(label_translation[labels])
          
          label2_levels <- paste0(
            order,
            ": ",
            sprintf(
              "%.4f",
              final_thresholds$threshold[
                match(order, final_thresholds$label)
              ]
            )
          )
          
          final_thresholds$label2 <- factor(
            final_thresholds$label2,
            levels = label2_levels
          )
          
          threshold_df$label2 <- factor(
            threshold_df$label2,
            levels = label2_levels
          )
          
          # draw threshold plot
          ggFscore <- ggplot(data = threshold_df) +
            
            geom_line(
              aes(x = threshold, y = f, colour = label2),
              linewidth = 1,
              alpha = 0.8
            ) +
            
            geom_point(
              data = final_thresholds,
              aes(x = threshold, y = f, fill = label2),
              size = 5,
              pch = 21
            ) +
            
            theme_bw(base_size = 10) +
            
            xlab("Threshold") +
            ylab(expression(paste("F(", beta, ") value"))) +
            
            ggtitle(
              paste("Threshold accuracy plot for beta =", as.character(b_num))
            ) +
            
            scale_colour_hue(name = "") +
            scale_fill_hue(name = "") +
            
            coord_cartesian(xlim = c(0, 1)) +
            
            theme(
              plot.title = element_text(
                family = "Metropolis-Bold",
                face = "bold",
                size = 60,
                hjust = 0.5
              ),
              axis.title = element_text(size = 40),
              axis.text = element_text(size = 40),
              legend.text = element_text(
                family = "Metropolis-Regular",
                size = 40
              ),
              legend.position = "bottom"
            )
          
          ######## SAVE THRESHOLDS PLOTS ########
          
          ggsave(
            plot = ggFscore,
            filename = paste0(
              out_dir,
              "fscore", b_char,
              "_performance_across_thresholds_u_vne_cnn.png"
            ),
            width = 16,
            height = 6,
            units = "in"
          )
          
          print("")
  
}









