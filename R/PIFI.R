#' Calculate PIFI (Pancreatic Islet Function Index)
#'
#' @description
#' This function calculates the Pancreatic Islet Function Index based on
#' glucose, insulin, and C-peptide measurements from OGTT tests.
#' It automatically detects the available time points and selects the
#' appropriate calculation method.
#'
#' @param data A data frame containing glucose, insulin, and/or C-peptide measurements.
#'            Column names must follow the standard naming convention (e.g., GLU_A, INS_A, CP_A).
#'
#' @return A data frame with calculated PIFI indices added as new columns.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with 5 time points
#' data_5point <- data.frame(
#'   GLU_A = c(5.2, 5.5), GLU_B = c(8.1, 8.5),
#'   GLU_C = c(7.3, 7.8), GLU_D = c(6.2, 6.5),
#'   GLU_E = c(5.8, 6.0),
#'   INS_A = c(8.5, 9.0), INS_B = c(45.2, 48.5),
#'   INS_C = c(38.7, 40.2), INS_D = c(25.3, 28.1),
#'   INS_E = c(12.5, 14.2)
#' )
#' result <- PIFI(data_5point)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr %>%
#' @importFrom writexl write_xlsx
PIFI <- function(df) {

  # 获取列名
  colnames <- colnames(df)

  # 定义各个时间点组合
  base <- c("GLU_A", "GLU_C", "GLU_D")
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
      return(df)
    }
  } else {
    stop("No suitable calculation method found. Please ensure the data includes blood glucose, insulin, and C-peptide values for at least 0, 30/60, and 120 minutes.")
  }
}
