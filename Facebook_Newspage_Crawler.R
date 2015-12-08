########################### Setup for grabbing facebook page history of 100 news pages:######################
### The packages
pkgs = c("rvest", "plyr", "stringr", "devtools", "httr", "RCurl", "curl", "XML")
lapply(pkgs, library, character.only = TRUE)

### First steps for crawling fanpagelist.com for facebook pages
  # Website structure : http://.../sort/fans/page1, http://.../sort/fans/page2, etc.: Shows 20 fb-pages
  # Create vector of 5 -> insert as to have 5 different links -> 100 fb-pages
vec5 = c(1:5)
linksub.li = "http://fanpagelist.com/category/news/view/list/sort/fans/pageLANK"

link_str_replace = function(vec5){
  link.sub = gsub("\\LANK", vec5, linksub.li)
}

link.sub = llply(vec5, link_str_replace) # Coerce into list by applying link_str_replace fct.

### Crawling strategy:
  # As the website does not have href's for FB on front page, we need to visit each fanpagelist.com's page for the facebook-page
  # From there we can retrieve the link to facebook. 
      # Fetch URL's for each fanpagelist.com/page 

css.selector_1 = "a:nth-child(3)"   #URL and/or TITLE depends on 'html_attr(name = href/title)'
css.selector_2 = "div.listing_profile > a"
scrape_links_fanpage = function(link.sub){
  link.url = read_html(link.sub, encoding = "UTF-8") %>%
    html_nodes(css = css.selector_2) %>%
    html_attr(name = "href")
  return(rbind(link.url))
}

### Apply function using simple for-loop
fanpage.data = list()
for(i in link.sub){
  print(paste("processing", i, sep = " "))
  fanpage.data[[i]] = scrape_links_fanpage(i)
  Sys.sleep(1)
  cat("done!\n")
}
### Setting up data for next step
  # Each URL yields list with 20 elements 
  # Manipulate into dataframe, transpose, read as charactors rather than factors
fanpage.df = data.frame(fanpage.data)
fanpage.df = t(fanpage.df)
fanpage.df = as.character(fanpage.df)

### Preparing for crawling fanpagelist.com's subsites for facebook links
  # http://www.fanpagelist.com only provides href in condensed form: /page/bbc
  # We need the full URL to actaully visit the website
    
fanpageurl = "http://fanpagelist.comLANK" # The URL-part that is not provided by scraping
fanpage.fct = function(fanpage.df){
  link.sub2 = gsub("\\LANK", fanpage.df, fanpageurl)
}
fanpage.li = llply(fanpage.df, fanpage.fct)

### The actual crawler:
  # Look at the complete link and fetch only the facebook href-link
css.selector_3 = "a:nth-child(3)"
scrape_links_facebook = function(link.sub2){
  facebook.link.url = read_html(link.sub2, encoding = "UTF-8") %>%
    html_nodes(css = css.selector_3) %>%
    html_attr(name = "href")
  return(cbind(facebook.link.url))
}
  # Do this for all links contained in fanpage.li
facebook.data = list()
for(i in fanpage.li){
  print(paste("processing", i, sep = " "))
  facebook.data[[i]] = scrape_links_facebook(i)
  Sys.sleep(1)
  cat("done!\n")
}

### Reformatting the data
  # As before the format is not really workable
  # Some manipulations are required
facebook.data = unlist(facebook.data)
facebook.frame = data.frame(facebook.data)
new_DF = facebook.data
new_DF = new_DF[grep("www.facebook.com", new_DF)]
new_DF = data.frame(new_DF)
page.names = gsub("\\https://www.facebook.com/", "", new_DF$new_DF)
page.names = gsub("/", "", page.names)
  #Remove duplicates
page.names = page.names[!duplicated(page.names)]

############################## Utilizing the Rfacebook-package for gathering posts #####################
### SETTING UP
  # Facebook API Oauth procedure - setting up using Guide from Pablo Barberas Rfacebook package
install_github("pablobarbera/Rfacebook/Rfacebook")
library("Rfacebook")
  ## Create oauth in order to avoid having to fetch token every 2nd hr manually
fb_oauth <- fbOAuth(app_id = "558342837655130", app_secret = "9e001e86aed303b7579da57bbb3f7e0d", extended_permissions = FALSE)
save(fb_oauth, file = "fb_oauth")
  ## Load oauth-key/token
load("fb_oauth")

### Creating function for gathering all 2015 posts
Facebook_Page_Fetch = function(x){
  fb.feed = getPage(x, token = fbkey, n = 99999, since = '2015/01/01', until = '2015/12/05')
  return(cbind(fb.feed, x))
}
  # Token
fbkey = "CAACEdEose0cBACU13XBqlYotJyJU6V9ohCsTFhnjS5tRKYEnpGt8iueSiQiXzzJwN9dFlFMeHRYCS4MTQoUchUfptAfZC8o5MZAVq3XVD8zYKx3ykgYJdcNki4r6M3HycvNY8yD3DxKx3OLe9vG6aftHycZBwXY34YrihhuEfLstghJsC1Tw737SIZBlyCOqLKjjm0RndQZDZD"
page.names.static = page.names
page.names.1 = page.names.static[43:95]

# Skipped:
  # Huffington Post
  # Fox Sports

  # Example of how the function works
getPage("natgeo", token = fb_oauth, n = 100000, since = '2015/01/01', until = '2015/12/05')

### Looping through page.names
  # Plyr loop is not too functional
Ply.Fetch = data.frame(ldply(page.names, Facebook_Page_Fetch, .inform = TRUE))

  # For loop allows for sleep-timer and more elaborate costumization of output
options(warn=1)
News_Facebook.ldf11 = list()
for(i in page.names.1){
  print(paste("processing", i, sep = " :: "))
  News_Facebook.ldf11[[i]] = Facebook_Page_Fetch(i)
  Sys.sleep(0.01)
  cat("done!\n")
}


  # All we need to do now is to ldply(..., data.frame) in order to get tidy dataframe
first.fb.list = News_Facebook.ldf
second.fb.list = News_Facebook.ldf1
third.fb.list = News_Facebook.ldf2
fourth.fb.list = News_Facebook.ldf3
fifth.fb.list = News_Facebook.ldf4
sixth.fb.list = News_Facebook.ldf5
seventh.fb.list = News_Facebook.ldf6
eighth.fb.list = News_Facebook.ldf7
ninth.fb.list = News_Facebook.ldf8
tenth.fb.list = News_Facebook.ldf9
eleventh.fb.list = News_Facebook.ldf10
twelfth.fb.list = News_Facebook.ldf11
thirteenth.fb.list = News_Facebook.ldf12

Temporary.df = ldply(first.fb.list, data.frame)
Temporary2.df = ldply(second.fb.list, data.frame)
Temporary3.df = ldply(third.fb.list, data.frame)
Temporary4.df = ldply(fourth.fb.list, data.frame)
Temporary5.df = ldply(fifth.fb.list, data.frame)
Temporary6.df = ldply(sixth.fb.list, data.frame)
Temporary7.df = ldply(seventh.fb.list, data.frame)
Temporary8.df = ldply(eighth.fb.list, data.frame)
Temporary9.df = ldply(ninth.fb.list, data.frame)
Temporary10.df = ldply(tenth.fb.list, data.frame)
Temporary11.df = ldply(eleventh.fb.list, data.frame)
Temporary12.df = ldply(twelfth.fb.list, data.frame)
Temporary13.df = ldply(thirteenth.fb.list, data.frame)

names(Temporary11.df)[12] = "page.names"

library(dplyr)
Mixed.df = rbind(Temporary.df, Temporary2.df, Temporary3.df, Temporary4.df, Temporary6.df, Temporary7.df, Temporary8.df, Temporary9.df, Temporary10.df, Temporary11.df, Temporary12.df, Temporary13.df)

head(page.names.1)
 
TerrorFB.df = Mixed.df$message[grep("Boko Haram|Kidnapping|Terror|Terrorism|Suicide Bomb|")]






