#' Calculate PIFI (Pancreatic Islet Function Index)
#'
#' @description
#' This function calculates the Pancreatic Islet Function Index based on
#' glucose, insulin, and C-peptide measurements from OGTT tests.
#' It automatically detects the available time points and selects the
#' appropriate calculation method.
#'
#'cite:Fu Q, Dai H, Wang J, et al. Multidimensional Pancreatic Islet β-cell Function Assessment Improves Predictive Effect of Diabetes Risk Scores. J Clin Endocrinol Metab. 2026;111(2):541-552.
#'
#' @param df A data frame containing glucose, insulin, and/or C-peptide measurements.
#'            Column names must follow the standard naming convention (e.g., GLU_A, INS_B, CP_C).
#'
#' @return A data frame with calculated PIFI indices added as new columns.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with 5 time points
#' data_5point <- data.frame(
#'   GLU_A = c(5.79, 6.63), GLU_B = c(11.06, 13.83),
#'   GLU_C = c(14.65,18.33), GLU_D = c(19.08,22.43),
#'   GLU_E = c(18.07,17.42),
#'   INS_A = c(40.5, 9.9), INS_B = c(46.2,90.64),
#'   INS_C = c(90.14,149.6), INS_D = c(126,343.7),
#'   INS_E = c(116.1,231.1),
#'   CP_A = c(665.5, 305.7), CP_B = c(592,661.8),
#'   CP_C = c(801.6,972.9), CP_D = c(1312,2657),
#'   CP_E = c(1537,2681),
#'   Age = c(54,80),
#'   Sex = c(1,1),
#'   Height = c(178,171)
#' )
#' result <- PIFI(data_5point)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
PIFI <- function(df) {

  # 获取列名
  colnames <- colnames(df)

  # 定义各个时间点组合
  base <- c("GLU_A","GLU_D")
  glucose_5 <- c("GLU_A", "GLU_B", "GLU_C", "GLU_D", "GLU_E")
  ins_5 <- c("INS_A", "INS_B", "INS_C", "INS_D", "INS_E")
  cp_5 <- c("CP_A", "CP_B", "CP_C", "CP_D", "CP_E")
  glucose_3_1 <- c("GLU_A", "GLU_B", "GLU_D")
  ins_3_1 <- c("INS_A", "INS_B", "INS_D")
  cp_3_1 <- c("CP_A", "CP_B", "CP_D")
  glucose_3_2 <- c("GLU_A", "GLU_C", "GLU_D")
  ins_3_2 <- c("INS_A", "INS_C", "INS_D")
  cp_3_2 <- c("CP_A", "CP_C", "CP_D")

  # 检查基础要求
  if (!all(base %in% colnames)) {
    stop("The data does not meet the minimum requirements for calculation.Please check the column names or verify the input data.")
  }
  memory_Age=df$Age
  memory_Height=df$Height
  df=df %>%mutate(Age=case_when(Age<18~18,
                                Age>65~65,
                                T~Age),
                  Height=case_when(Height<155~155,
                                   Height>180~180,
                                   T~Height))
  # 根据可用列选择计算方法
  if (all(glucose_5 %in% colnames)) {
    if (all(ins_5 %in% colnames)) {
      # 调用5点胰岛素计算方法
      df=PIFI_5point_INS(df)
    }
    if (all(cp_5 %in% colnames)) {
      # 调用5点C肽计算方法
      df=PIFI_5point_CP(df)
    }
    message("DONE! Use 5 Point method!")
    df$Age=memory_Age
    df$Height=memory_Height
    conditions <- c()
    if("PIF-Si" %in% names(df)) conditions <- c(conditions, "`PIF-Si` < 0")
    if("PIF-Li" %in% names(df)) conditions <- c(conditions, "`PIF-Li` < 0")
    if("PIF-Ai" %in% names(df)) conditions <- c(conditions, "`PIF-Ai` < 0")
    if("PIF-Sc" %in% names(df)) conditions <- c(conditions, "`PIF-Sc` < 0")
    if("PIF-Lc" %in% names(df)) conditions <- c(conditions, "`PIF-Lc` < 0")
    if("PIF-Ac" %in% names(df)) conditions <- c(conditions, "`PIF-Ac` < 0")

    # 合并条件并找出要删除的行
    if(length(conditions) > 0) {
      full_condition <- paste(conditions, collapse = " | ")
      rows_to_remove <- which(eval(parse(text = full_condition), envir = df))

      # 播报删除信息
      if(length(rows_to_remove) > 0) {
        message("Deleted the following lines: ", paste(rows_to_remove, collapse = ", "))
        message("Total deleted ", length(rows_to_remove), " 行")
        message("Reason: Outside the scope of application")
        df <- df[-rows_to_remove, ]
      }
    }
    return(df)
  } else if (all(glucose_3_1 %in% colnames)) {
    if (all(ins_3_1 %in% colnames)) {
      # 调用3点胰岛素计算方法1
      df=PIFI_3point_INS1(df)
    }
    if (all(cp_3_1 %in% colnames)) {
      # 调用3点C肽计算方法1
      df=PIFI_3point_CP1(df)
      message("DONE! Use 3 Point method A!")
      df$Age=memory_Age
      df$Height=memory_Height
      conditions <- c()
      if("PIF-Si" %in% names(df)) conditions <- c(conditions, "`PIF-Si` < 0")
      if("PIF-Li" %in% names(df)) conditions <- c(conditions, "`PIF-Li` < 0")
      if("PIF-Ai" %in% names(df)) conditions <- c(conditions, "`PIF-Ai` < 0")
      if("PIF-Sc" %in% names(df)) conditions <- c(conditions, "`PIF-Sc` < 0")
      if("PIF-Lc" %in% names(df)) conditions <- c(conditions, "`PIF-Lc` < 0")
      if("PIF-Ac" %in% names(df)) conditions <- c(conditions, "`PIF-Ac` < 0")

      # 合并条件并找出要删除的行
      if(length(conditions) > 0) {
        full_condition <- paste(conditions, collapse = " | ")
        rows_to_remove <- which(eval(parse(text = full_condition), envir = df))

        # 播报删除信息
        if(length(rows_to_remove) > 0) {
          message("删除了以下行: ", paste(rows_to_remove, collapse = ", "))
          message("共删除了 ", length(rows_to_remove), " 行")
          message("原因:超出适用范围")
          df <- df[-rows_to_remove, ]
        }
      }
      return(df)
    }
  } else if (all(glucose_3_2 %in% colnames)) {
    if (all(ins_3_2 %in% colnames)) {
      # 调用3点胰岛素计算方法2
      df=PIFI_3point_INS2(df)
    }
    if (all(cp_3_2 %in% colnames)) {
      # 调用3点C肽计算方法2
      df=PIFI_3point_CP2(df)
      message("DONE! Use 3 Point method B!")
      df$Age=memory_Age
      df$Height=memory_Height
      conditions <- c()
      if("PIF-Si" %in% names(df)) conditions <- c(conditions, "`PIF-Si` < 0")
      if("PIF-Li" %in% names(df)) conditions <- c(conditions, "`PIF-Li` < 0")
      if("PIF-Ai" %in% names(df)) conditions <- c(conditions, "`PIF-Ai` < 0")
      if("PIF-Sc" %in% names(df)) conditions <- c(conditions, "`PIF-Sc` < 0")
      if("PIF-Lc" %in% names(df)) conditions <- c(conditions, "`PIF-Lc` < 0")
      if("PIF-Ac" %in% names(df)) conditions <- c(conditions, "`PIF-Ac` < 0")

      # 合并条件并找出要删除的行
      if(length(conditions) > 0) {
        full_condition <- paste(conditions, collapse = " | ")
        rows_to_remove <- which(eval(parse(text = full_condition), envir = df))

        # 播报删除信息
        if(length(rows_to_remove) > 0) {
          message("删除了以下行: ", paste(rows_to_remove, collapse = ", "))
          message("共删除了 ", length(rows_to_remove), " 行")
          message("原因:超出适用范围")
          df <- df[-rows_to_remove, ]
        }
      }
      return(df)
    }
  } else {
    stop("No suitable calculation method found. Please ensure the data includes blood glucose, insulin, and C-peptide values for at least 0, 30/60, and 120 minutes.")
  }
}
