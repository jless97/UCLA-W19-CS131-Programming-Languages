import aiohttp
import asyncio
import config
import json
import logging
import re
import sys
import time


#  Maintain cache of clients for current session
cache = {}

#==================================================
#  SERVER IMPLEMENTATION
#==================================================
class EchoServerClientProtocol(asyncio.Protocol):

  def __init__(self, server_id, loop):
    self.loop = loop
    self.server_id = server_id
    self.peername = None
    self.message = ""

  #==================================================
  #  NEW CONNECTION
  #==================================================
  def connection_made(self, transport):
    self.peername = transport.get_extra_info('peername')
    self.log_message('Connection made: {}', self.peername)
    self.transport = transport

  #==================================================
  #  PROCESS DATA
  #==================================================
  def data_received(self, data):
    #  Decode data
    decoded_data = data.decode()
    self.log_message('Data received: {!r}', decoded_data)
   
    #  Process data
    #self.message += decoded_data

    # TEMPORARY
    self.message = decoded_data
    self.process_data(self.message)

  # def eof_received(self):
  #   self.process_data(self.message)

  def process_data(self, message):
    command = self.message.split()
    opcode = command[0]
    payload = command[1:]

    #  Process IAMAT command
    if opcode == 'IAMAT' and self.valid_iamat(payload):
      self.process_iamat(payload)
    #  Process WHATSAT command
    elif opcode == 'WHATSAT' and self.valid_whatsat(payload):
      self.process_whatsat(payload)
    #  Process AT command
    elif opcode == 'AT' and self.valid_at(payload):
      self.process_at(payload)
    #  Process INVALID command
    else:
      response = '? {}\n'.format(command)
      self.transport.write(response.encode())
      self.log_message('Data sent: {!r}', response)

  #==================================================
  #  LOGGING
  #==================================================
  def log_message(self, message, param):
    print(message.format(param))
    logging.info(message.format(param))

  #==================================================
  #  CACHE UPDATES
  #==================================================
  def update_cache(self, client_id, client_info):
    cached_client_info = cache.get(client_id)

    #  Client ID exists in cache
    if cached_client_info is not None:
      #  Client info is up-to-date
      if client_info == cached_client_info:
        self.log_message('New client info for {} is up-to-date', client_id)
        return False
      #  Client info is stale
      cached_time = cached_client_info[3]
      if float(cached_time) >= float(client_info[3]):
        self.log_message('New client info for {} is stale', client_id)
        return False

    #  Client info is new
    cache[client_id] = client_info
    self.log_message('New client info for {} is updated', client_id)
    return True

  #==================================================
  #  PAYLOAD FIELD VALIDATORS
  #==================================================
  def valid_location(self, location):
    if not re.fullmatch('[+-]\d*\.?\d+[+-]\d*\.?\d+', location):
      return False

    coords = re.split('[+-]', location[1:])
    latitude, longitude = float(coords[0]), float(coords[1])
    if latitude < -90 or latitude > 90 or longitude < -180 or longitude > 180:
      return False

    return True

  def valid_time(self, time):
    try:
      float(time)
    except ValueError:
      return False

    return True

  #==================================================
  #  IAMAT
  #==================================================
  def valid_iamat(self, payload):
    #  Verify arg length
    if len(payload) != 3:
      print("Error: Invalid number of args for IAMAT")
      return False

    location = payload[1]
    time = payload[2]

    #  Verify location
    if not self.valid_location(location):
      print("Error: Invalid location format")
      return False

    #  Verify time
    if not self.valid_time(time):
      print("Error: Invalid time format")
      return False

    return True

  def process_iamat(self, payload):
    client_id = payload[0]
    client_location = payload[1]
    client_time = payload[2]

    #  Format time
    time_diff = time.time() - float(client_time)
    if time_diff > 0:
      time_diff_str = '+' + str(time_diff)
    else:
      time_diff_str = str(time_diff)

    #  Format AT message
    response = 'AT {} {} {} {} {}\n'.format(
      self.server_id, time_diff_str, client_id, client_location, client_time
    )

    #  Send response to client
    self.transport.write(response.encode())
    self.log_message('Data sent: {!r}', response)

    #  Update cache
    new_client_info = [self.server_id, time_diff_str, client_location, client_time]
    updated_cache = self.update_cache(client_id, new_client_info)

    #  Propagate client info to neighbors
    if updated_cache:
      message = 'AT {} {} {} {} {} {}\n'.format(
        self.server_id, time_diff_str, client_id, client_location, client_time, self.server_id
      )
      self.log_message('Attempting to flood client info: {}', message)
      self.loop.create_task(self.flood_to_neighbors(message, []))

  #==================================================
  #  WHATSAT
  #==================================================
  def valid_whatsat(self, payload):
    #  Verify arg length
    if len(payload) != 3:
      print("Error: Invalid number of args for WHATSAT")
      return False

    client_id = payload[0]
    radius = payload[1]
    num_results = payload[2]

    #  Verify client
    if client_id not in cache.keys():
      print("Error: Invalid client ID")
      return False

    #  Verify radius and upper bound
    try:
      radius = int(radius)
      num_results = int(num_results)
    except ValueError:
      print("Error: Invalid radius/num_results type")
      return False

    if radius < 0 or radius > 50 or num_results < 0 or num_results > 20:
      print("Error: Radius/num_results out of bounds")
      return False

    return True

  async def fetch(self, session, url):
    async with session.get(url, ssl=False) as response:
      return await response.text()

  async def query(self, location, radius, num_results):
    url = '{}location={}&radius={}&key={}'.format(config.API_URL_BASE, location, radius, config.API_KEY)
    async with aiohttp.ClientSession() as session:
      response = await self.fetch(session, url)
      deserialized_response = json.loads(response)
      deserialized_response['results'] = deserialized_response['results'][:num_results]
      serialized_response = '{}\n\n'.format(json.dumps(deserialized_response, indent=2))
      self.transport.write(serialized_response.encode())
      self.log_message('Data sent: {!r}', serialized_response)

  def process_whatsat(self, payload):
    #  Retrieve client data from cache
    client_id = payload[0]
    radius = int(payload[1]) * 1000
    num_results = int(payload[2])
    client_server = cache[client_id][0]
    client_time_diff = cache[client_id][1]
    client_location = cache[client_id][2]
    client_time = cache[client_id][3]

    #  Format AT message
    at_response = 'AT {} {} {} {} {}\n'.format(
      client_server, client_time_diff, client_id, client_location, client_time
    )
    self.transport.write(at_response.encode())

    #  Format latitude and longitude
    coords = re.split('[+-]', client_location[1:])
    latitude_sign = (1 if client_location[0] == '+' else -1)
    longitude_sign = (1 if ('+' in client_location[1:]) else -1)
    latitude, longitude = float(coords[0]) * latitude_sign, float(coords[1]) * longitude_sign
    latitude_and_longitude = '{},{}'.format(latitude, longitude)
    
    #  Google Places API
    self.loop.create_task(self.query(latitude_and_longitude, radius, num_results))

  #==================================================
  #  AT
  #==================================================
  def valid_at(self, payload):
    #  Verify arg length
    if len(payload) != 6:
      print("Error: Invalid number of args for AT")
      return False

    server_id = payload[0]
    client_location = payload[3]
    client_time = payload[4]

    #  Verify server ID
    if server_id not in config.SERVER_IDS:
      print("Error: Invalid server ID")
      return False

    #  Verify arg info
    if not self.valid_location(client_location) or not self.valid_time(client_time):
      print("Error: Invalid location/time format")
      return False

    return True

  async def flood_to_neighbors(self, message, senders):
    for neighbor in config.SERVER_NEIGHBORS[self.server_id]:
      if neighbor not in senders:
        try:
          #  Flood to neighbor
          port = config.SERVERS_TO_PORTS[neighbor]
          reader, writer = await asyncio.open_connection(host=config.HOST, port=port, loop=self.loop)
          writer.write(message.encode())
          await writer.drain()
          writer.close()
          self.log_message('Success: Flooded data to {}', neighbor)
        except ConnectionRefusedError:
          self.log_message('Failure: Unable to connect to {}', neighbor)

  def process_at(self, payload):
    source_server = payload[0]
    time_diff = payload[1]
    client_id = payload[2]
    client_location = payload[3]
    client_time = payload[4]
    client_server = payload[5]

    #  Update cache
    new_client_info = [source_server, time_diff, client_location, client_time]
    updated_cache = self.update_cache(client_id, new_client_info)

    #  Propagate client info to neighbors
    if updated_cache:
      message = 'AT {} {}\n'.format(' '.join(payload[:-1]), self.server_id)
      self.log_message('Attempting to flood client info: {}', message)
      self.loop.create_task(self.flood_to_neighbors(message, [source_server, client_server]))

  #==================================================
  #  LOST CONNECTION
  #==================================================
  def connection_lost(self, exc):
    self.log_message('Connection lost: {}', self.peername)

#==================================================
#  MAIN
#==================================================
def main():
  #  Check command line arguments
  if len(sys.argv) != 2:
    print("Error: Incorrect number of arguments")
    print("Usage: python3 server.py <server_id>")
    sys.exit(1)

  if sys.argv[1] not in config.SERVER_IDS:
    print("Error: Incorrect server ID")
    print("Usage: [Golomon Hands Holiday Welsh Wilkes]")
    sys.exit(1)

  #  Set server_id and port
  server_id = sys.argv[1]
  port = config.SERVERS_TO_PORTS[server_id]

  #  Create logger
  logging.basicConfig(filename='server.{}.log'.format(server_id),
                      filemode='w',
                      format='%(asctime)s %(levelname)s %(message)s',
                      level=logging.INFO)

  #  Create event loop
  loop = asyncio.get_event_loop()
  #  Each client connection will create a new protocol instance
  coro = loop.create_server(lambda: EchoServerClientProtocol(server_id, loop),
                            '127.0.0.1', port)
  server = loop.run_until_complete(coro)

  #  Serve requests until ^C is pressed
  print('Serving server {} on {}'.format(server_id, server.sockets[0].getsockname()))
  logging.info('Serving server {} on {}'.format(server_id, server.sockets[0].getsockname()))
  try:
    loop.run_forever()
  except KeyboardInterrupt:
    pass

  #  Close the server
  server.close()
  loop.run_until_complete(server.wait_closed())
  loop.close()

if __name__ == "__main__":
  main()
