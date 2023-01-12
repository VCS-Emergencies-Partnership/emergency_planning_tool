library(rsconnect)

rsconnect::deployApp('.', account = 'vcsep',
                     appName = 'emergency_planning_tool')
