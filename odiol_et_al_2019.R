# AUC, higher is better
#Scenario	RF	LR	SVM
#Raw data	0.85	0.83	0.82
#With clinical data	0.89	0.84	0.82
#SNP-value	0.81	0.79	0.77

t_auc_wide <- tibble::tribble(
  ~scenario,	          ~RF , ~LR , ~SVM,
  "Raw data",           0.85, 0.83, 0.82,
  "With clinical data", 0.89, 0.84, 0.82,
  "SNP-value",          0.81, 0.79, 0.77

)
t_auc_wide$scenario <- as.factor(t_auc_wide$scenario)
t_auc <- tidyr::pivot_longer(t_auc_wide, cols = -scenario)
names(t_auc) <- c("scenario", "technique", "auc")
t_auc$technique <- as.factor(t_auc$technique)


# error
#Scenario	RF	LR	SVM
t_error_wide <- tibble::tribble(
  ~scenario,	          ~RF , ~LR , ~SVM,
  "Raw data",           .050,	.054,	.044,
  "With clinical data", .041,	.050,	.038,
  "SNP-value",          .046,	.062,	.059
)
t_error_wide$scenario <- as.factor(t_error_wide$scenario)
t_error <- tidyr::pivot_longer(t_error_wide, cols = -scenario)
names(t_error) <- c("scenario", "technique", "auc")
t_error$technique <- as.factor(t_error$technique)

testthat::expect_equal(t_auc$scenario, t_error$scenario)
testthat::expect_equal(t_auc$technique, t_error$technique)
t_auc$ymin <- t_auc$auc - t_error$auc
t_auc$ymax <- t_auc$auc + t_error$auc

ggplot2::ggplot(t_auc, ggplot2::aes(x = technique, y = auc)) +
  ggplot2::scale_y_continuous("AUC", limits = c(0.5, 1.0), oob = scales::squish) +
  ggplot2::geom_col() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = ymin, ymax = ymax)) +
  ggplot2::facet_grid(. ~ scenario, scales = "free") +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 90, vjust = 0.5, hjust = 1
    ),
    text = ggplot2::element_text(size = 24)
  ) ; ggplot2::ggsave("~/odiol_et_al_2019.png", width = 7, height = 7)
