#' WechatHelper
#'
#' @param userid 请关注公众号"生信通知助手"，回复id，扫描二维码获取userid
#' @param text 需要发生的通知内容
#' @param status 当前数据状态
#' @param name 项目名称
#' @param number 通知编号
#'
#' @return
#' @export
#'
#' @examples
WechatMsg <- function(userid, text, status = NULL, name = NULL, number = NULL) {
  library(httr)

  base_url <- "http://bj.s1f.ren/gzh/sendMsg"

  # 构建查询参数
  query_params <- list(
    userid = userid,
    text = text
  )

  # 添加可选参数（如果提供）
  if (!is.null(status)) {
    query_params$status <- status
  }
  if (!is.null(name)) {
    query_params$name <- name
  }
  if (!is.null(number)) {
    query_params$number <- number
  }

  # 发送GET请求
  response <- GET(base_url, query = query_params)

  # 检查响应状态
  if (status_code(response) == 200) {
    content(response, "text", encoding = "UTF-8")
  } else {
    stop("Request failed with status code: ", status_code(response))
  }
}


#' WechatRobot_R

#' @param key
#' @param msg
#' @param qyapi
#' @param msgtype
#'
#' @return
#' @export
#'
#' @examples
WechatRobot_R <- function(key, msg = "", qyapi = "https://qyapi.weixin.qq.com/cgi-bin/webhook/send?key=", msgtype = "text") {
  headers <- c("Content-Type" = "application/json")
  url <- paste0(qyapi, key)
  mypayload <- list(
    "msgtype" = msgtype,
    "text" = list("content" = msg)
  )
  response <- httr::POST(url, httr::add_headers(.headers = headers),
                         body = mypayload,
                         encode = "json"
  )
  return(response)
}

#' WechatRobot
#'
#' @param key
#' @param msg
#' @param meth
#'
#' @return
#' @export
#'
#' @examples
WechatRobot <- function(key = "", msg = "", meth = "R") {
  if (meth == "python") {
    reticulate::py_run_string("from easyBio import WechatRobot")
    reticulate::py_run_string(sprintf("wcr = WechatRobot(key = '%s')", key))
    reticulate::py_run_string(sprintf("wcr.send_mes(mes_info = '%s')", msg))
  } else if (meth == "R") {
    WechatRobot_R(key = key, msg = msg)
  }
}
