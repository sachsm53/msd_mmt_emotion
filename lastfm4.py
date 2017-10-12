#!/usr/bin/env python

import pylast
import csv

# You have to have your own unique two values for API_KEY and API_SECRET
# Obtain yours from http://www.last.fm/api/account for Last.fm
API_KEY = "cf19f403740b65f0150361c4d3cc504e" # this is a sample key
API_SECRET = "acf374f9ba218a4cd025122e67db7139"

# In order to perform a write operation you need to authenticate yourself
username = "sachsm53"
password_hash = pylast.md5("Mipsolat1")

network = pylast.LastFMNetwork(api_key = API_KEY, api_secret =
    API_SECRET, username = username, password_hash = password_hash)


tweetfile = "/Users/mattsachs/Desktop/mmtd_full"
outputfile = "/Users/mattsachs/Desktop/tweet_tags3.csv"

sad_words =['sad','depress','grief','gloom','mourn','tragic']
happy_words = ['happy','cheer']

csvfile = open(outputfile,'w')
csvwriter = csv.writer(csvfile)
header = ['artist_name','track_name','tweet_datetime','tweet_latitude','tweet_longitude','tweet_weekday','country','countryName','city','sad','happy','beautiful']
csvwriter.writerow(header)


print "Opening tweet file"
csvfile = open(tweetfile, 'rb')
reader = csv.reader(csvfile)


counter = 0
for row in reader:
	counter = counter + 1
	if counter < 170450: 
		continue
	else:
		artist_name = row[14]
		track_name = row[15]
		track_7digtalid = row[16]
		tweet_datetime = row[0]
		tweet_unixtime = row[1]
		tweet_weekday = row[2]
		tweet_longitude = row[3]
		tweet_latitude = row[4]
		country = row[5]
		state = row[6]
		city = row[8]
		postalCode = row[9]
		timezone = row[11]
		countryName = row[12]
		tags_full = [] 
		weights_full = []
		print 'Row: %d; Artist: %s; Track: %s' %(counter,artist_name,track_name)
		track = network.get_track(artist_name, track_name) 
		try: 
			topItems = track.get_top_tags(limit=None)
		except:
			print "Could not find file" 
		else:
			print "Found file" 
			for topItem in topItems:
				tag = topItem.item.get_name() 
				weight = topItem.weight
				if weight > 0: 
					tags_full.append(tag)
					weights_full.append(weight)

			sad = 0
	  		for syn in sad_words:
	  			if any(syn in elem.lower() for elem in tags_full):
	  				# print "Sad Song! Tag is: %s" %syn
	  				sad = 1
	  				break

	  		beautiful = 0
			if any("beaut" in elem.lower() for elem in tags_full):
				# print "Beautiful Song!"
				beautiful = 1

  			happy = 0
  			for syn in happy_words:
  				if any(syn in elem.lower() for elem in tags_full):
  					# print "Happy Song! Tag is: %s" %syn
  					happy = 1
  					break
  			row = [artist_name,track_name,tweet_datetime,tweet_latitude,tweet_longitude,tweet_weekday,country,countryName,city,sad,happy,beautiful]
  			csvwriter.writerow(row)




	# 	print tags_full
	# 	print weights_full




# track7ids = [elem[16] for elem in reader]
# track_names = [elem[15] for elem in reader]
tweetfile.close()



# # now you can use that object everywhere
# artist = network.get_artist("System of a Down")
# artist.shout("<3")



# #Get the tags a a TopItem object. 
# track = network.get_track("Cher", "Believe")
# topItems = track.get_top_tags(limit=None)
# for topItem in topItems:
#     print topItem.item.get_name(), topItem.weight

# #Search for tags 
# track = network.tag_search("sad")
# topItems = track.get_top_tags(limit=None)
# for topItem in topItems:
#     print topItem.item.get_name(), topItem.weight

# #Looks up an artist by its MusicBrainz ID
# artist = network.get_artist_by_mbid(self, mbid)

# # Get the most popular artists on Last.fm by country.
# # Parameters:
# # country (Required) : A country name, as defined by the ISO 3166-1 country names standard.
# # limit (Optional) : The number of results to fetch per page. Defaults to 50.
# country = network.get_geo_top_artists(self, country, limit=None, cacheable=True)

# # Get the most popular tracks on Last.fm last week by country.
# # Parameters:
# # country (Required) : A country name, as defined by the ISO 3166-1 country names standard
# # location (Optional) : A metro name, to fetch the charts for (must be within the country specified)
# get_geo_top_tracks(self, country, location=None, limit=None, cacheable=True)

# # Looks up a track by its MusicBrainz ID
# get_track_by_mbid(self, mbid)

# #Searches of a tag by its name. Returns a TagSearch object. Use get_next_page() to retrieve sequences of results.
# tag = network.search_for_tag("sad")
# for item in tag:
# 	print item
# topItem = tag.search_for_tag(limit=None)
# search_for_tag(self, tag_name)
# search_for_artist
# search_for_track







# track = network.get_track("Iron Maiden", "The Nomad")
# track.love()
# track.add_tags(("awesome", "favorite"))

# type help(pylast.LastFMNetwork) or help(pylast) in a Python interpreter to get more help
# about anything and see examples of how it works