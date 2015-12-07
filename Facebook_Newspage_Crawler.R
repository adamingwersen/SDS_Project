# Setup for grabbing facebook page history of 100 news pages

# The packages
pkgs = c("rvest", "plyr", "stringr", "devtools", "httr", "RCurl", "curl", "XML")
lapply(pkgs, library, character.only = TRUE)

# Crawling fanpagelist.com for facebook pages
vec5 = c(1:5)
linksub.li = "http://fanpagelist.com/category/news/view/list/sort/fans/pageLANK"

link_str_replace = function(vec5){
  link.sub = gsub("\\LANK", vec5, linksub.li)
}

link.sub = llply(vec5, link_str_replace)

css.selector_1 = "a:nth-child(3)"   #URL and/or TITLE depends on 'html_attr(name = href/title)'
css.selector_2 = "div.listing_profile > a"
scrape_links_fanpage = function(link.sub){
  link.url = read_html(link.sub, encoding = "UTF-8") %>%
    html_nodes(css = css.selector_2) %>%
    html_attr(name = "href")
  return(rbind(link.url))
}


fanpage.data = list()
for(i in link.sub){
  print(paste("processing", i, sep = " "))
  fanpage.data[[i]] = scrape_links_fanpage(i)
  Sys.sleep(1)
  cat("done!\n")
}

fanpage.df = data.frame(fanpage.data)
fanpage.df = t(fanpage.df)
fanpage.df = as.character(fanpage.df)
fanpage.li = fanpage.df


##### second loop
fanpageurl = "http://fanpagelist.comLANK"

fanpage.fct = function(fanpage.df){
  link.sub2 = gsub("\\LANK", fanpage.df, fanpageurl)
}

fanpage.li = llply(fanpage.df, fanpage.fct)

css.selector_3 = "a:nth-child(3)"
scrape_links_facebook = function(link.sub2){
  facebook.link.url = read_html(link.sub2, encoding = "UTF-8") %>%
    html_nodes(css = css.selector_3) %>%
    html_attr(name = "href")
  return(cbind(facebook.link.url))
}

facebook.data = list()
for(i in fanpage.li){
  print(paste("processing", i, sep = " "))
  facebook.data[[i]] = scrape_links_facebook(i)
  Sys.sleep(1)
  cat("done!\n")
}

facebook.data=unlist(facebook.data)
facebook.frame=data.frame(facebook.data)
new_DF = facebook.data
new_DF = new_DF[grep("www.facebook.com", new_DF)]
new_DF=data.frame(new_DF)
page.names = gsub("\\https://www.facebook.com/", "", new_DF$new_DF)
page.names = gsub("/", "", page.names)

#Remove duplicates
page.names = page.names[!duplicated(page.names)]


# SETTING UP
  # Facebook API Oauth procedure - setting up using Guide from Pablo Barberas Rfacebook package
install_github("pablobarbera/Rfacebook/Rfacebook")
library("Rfacebook")
  ## Create oauth in order to avoid having to fetch token every 2nd hr manually
fb_oauth <- fbOAuth(app_id = "558342837655130", app_secret = "9e001e86aed303b7579da57bbb3f7e0d", extended_permissions = FALSE)
save(fb_oauth, file = "fb_oauth")
  ## Load oauth-key/token
load("fb_oauth")

# Creating function for gathering all 2015 posts

Facebook_Page_Fetch = function(page.names){
  fb.feed = getPage(page.names, token = fbkey, n = 9999, since = '2015/01/01', until = '2015/12/05')
  return(cbind(fb.feed, page.names))
}

fbkey = "CAACEdEose0cBACiZAIT5ElInPxTwUM5mI6XDNTNQCW1aEXeet5kzhBx96Ie91dutCME65FKep0aHL9FlYCPZBOGuvRA4t6wZCiuMDSWd1pZAFMdwWpAwM2ldZA7PmJ8vmu6nxtrZBStfx2SGUXTo2PaeyiN4TlgGcVTQzf8exii6J1s1xN9y43rCwkM9rqEZAlMRBxJTowiHQZDZD"

Facebook_Page_Fetch("natgeo")

getPage("natgeo", token = fb_oauth, n = 100000, since = '2015/01/01', until = '2015/12/05')



Ply.Fetch = data.frame(ldply(page.names, Facebook_Page_Fetch, .inform = TRUE))

News_Facebook.df = ldply(Ply.Fetch, data.frame)


install.packages("reshape2")
library("reshape2")

first.fb.list = News_Facebook.ldf
Temporary.df = ldply(first.fb.list, data.frame)
Temporary.df = as.data.frame(News_Facebook.ldf)
Temporary.df = data.frame(Temporary.df)

as.data.frame()


 

