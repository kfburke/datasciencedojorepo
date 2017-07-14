import tweepy

# https://apps.twitter.com/app/7773403/show

# override tweepy.StreamListener to add logic to on_status
class MyStreamListener(tweepy.StreamListener):

    def on_status(self, status):
        print(status.text.encode('utf-8'))

    def on_error(self, status_code):
        if status_code == 420:
            # returning False in on_data disconnects the stream
            return True

consumer_token = ''
consumer_secret = ''
access_token = ''
access_token_secret = ''

oAuthHeader = tweepy.OAuthHandler(consumer_token, consumer_secret)
oAuthHeader.set_access_token(access_token, access_token_secret)
myAPI = tweepy.API(oAuthHeader)

try:
    myAPI.verify_credentials()
    print("Valid User.")
except:
    print("Invalid User.")

myStreamListener = MyStreamListener()
tweetStream = tweepy.Stream(auth=oAuthHeader, listener=myStreamListener)
tweetStream.filter(languages=['en'], async=False, locations=[-180, -90, 180, 90])
