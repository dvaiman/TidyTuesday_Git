

# How to use Git and GitHub with R (youtube title)



# to reset to certain version: go to "Terminal" select " new terminal" and then write
# git reset --hard (SHA goes inside here instead of the brackets and this text, find it in commit and history tab)

# Make syncing easier with usethis
library(usethis)

# go to helpfile and click link "GitHub personal access token (PAT)" 
 ?use_github

# click the box "repo" on github and then generrate token

edit_r_environ()

# and save link in .renviron as GITHUB_PAT = "61639b8fb23521818791f77b22391b50734a5ca5"
# restart R (in session tab)

use_github("https", auth_token = Sys.getenv("GITHUB_PAT"))


# run this to create reposatory on github
use_github()

