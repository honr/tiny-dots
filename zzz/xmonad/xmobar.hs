Config {
  font = "-b&h-lucidatypewriter-medium-r-*-*-12-*-*-*-*-*-*-*",
  bgColor = "#888888",
  fgColor = "#000000",
  position = Top,
  commands = [ Run Date "%a %B %_d %H:%M" "date" 10,
               Run StdinReader ],
  sepChar = "%",
  alignSep = "}{",
  template = " %StdinReader% }{ <fc=#FFFFFF>%date%</fc> "}
