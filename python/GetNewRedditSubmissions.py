import praw
 
# Set the user agent information
# IMPORTANT: Change this if you borrow this code. Reddit has very strong 
# guidelines about how to report user agent information 
r = praw.Reddit('Check New Articles script based on code by ProgrammingR.com')
    
# Create a (lazy) generator that will get the data when we call it below
new_subs = r.get_new(limit=100)
 
# Get the data and put it into a usable format
new_subs=[str(x) for x in new_subs]
