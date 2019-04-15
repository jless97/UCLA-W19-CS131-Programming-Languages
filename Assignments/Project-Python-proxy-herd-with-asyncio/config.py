#  Host
HOST = '127.0.0.1'

#  Server IDs
SERVER_IDS = ['Goloman', 'Hands', 'Holiday', 'Welsh', 'Wilkes']

#  Server ID to Port Mapping
SERVERS_TO_PORTS = {
  'Goloman' : 11760,
  'Hands' : 11761,
  'Holiday' : 11762,
  'Welsh' : 11763,
  'Wilkes' : 11764
}

#  Server Neigbors
SERVER_NEIGHBORS = {
  'Goloman' : ['Hands', 'Holiday', 'Wilkes'],
  'Hands' : ['Goloman', 'Wilkes'],
  'Holiday' : ['Goloman', 'Welsh', 'Wilkes'],
  'Welsh' : ['Holiday'],
  'Wilkes' : ['Goloman', 'Hands', 'Holiday']
}

#  Google Places API URL
API_URL_BASE = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?'

#  Google Places API Key
API_KEY = 'AIzaSyC8l8SALEzexrOUT8U4_2yqDWZyRQPvaws'