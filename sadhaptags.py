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


infile = "/Users/mattsachs/Desktop/sad_hap.csv"
outputfile = "/Users/mattsachs/Desktop/sad_hap_tags.csv"


csvfile = open(outputfile,'w')
csvwriter = csv.writer(csvfile)
header = ['artist_name','track_name','tags','weights']
csvwriter.writerow(header)


print "Opening file"
csvfile = open(infile, 'rb')
reader = csv.reader(csvfile)


counter = 0
for row in reader:
	counter = counter + 1
	if counter == 1: 
		continue
	else:
		artist_name = row[1]
		track_name = row[2]
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
  					break
  			row = [artist_name,track_name,tags_full,weights_full]
  			csvwriter.writerow(row)

	# 	print tags_full
	# 	print weights_full

infile.close()