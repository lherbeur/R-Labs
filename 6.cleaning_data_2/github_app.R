
library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
# 
url <- 'https://api.github.com/users/jtleek/repos'
# endpoints <- oauth_endpoints('github')
# endpoints

# # 2. To make your own application, register at 
# #    https://github.com/settings/developers. Use any URL for the homepage URL
# #    (http://github.com is fine) and  http://localhost:1410 as the callback url
# #
#    Replace your key and secret below.
# CouseraQuizApp, github
myapp <- oauth_app("CouseraQuizApp",
                   key = "239983654c952782001a",
                   secret = "1cf1c0392ba14d74602b8cc88a02d8b76b3714c8")

# 3d243be157458c9bfde9cf285de1439682f7febe
myapp

# # 3. Get OAuth credentials
endpoint <- oauth_endpoint(NULL, "authorize", "access_token",
                         base_url = "https://github.com/login/oauth")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
# endpoint
github_token


# # # 4. Use API
gtoken <- config(token = github_token)
req <- GET(url, gtoken)
stop_for_status(req)
k <- content(req)
k

# # OR:
# req <- with_config(gtoken, GET("https://api.github.com/rate_limit"))
# stop_for_status(req)
# k <- content(req)
# k