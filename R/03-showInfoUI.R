# Show Info UI
#
# @param infoText charackter info text to show when hoovering
# @param color (character) text color
showInfoUI <- function(infoText, color = "#0072B2") {
  tags$i(
    class = "glyphicon glyphicon-info-sign",
    style = sprintf("color:%s;", color),
    title = infoText)
}
