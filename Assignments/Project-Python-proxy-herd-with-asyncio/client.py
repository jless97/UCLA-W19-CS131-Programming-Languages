import asyncio
import config
import logging

class EchoClientProtocol(asyncio.Protocol):
    def __init__(self, message, loop):
      self.message = message
      self.loop = loop

    def connection_made(self, transport):
      #import pdb; pdb.set_trace()
      msg1 = 'IAMAT'
      msg2 = ' kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997'

      transport.write(msg1.encode())
      print('Data sent: {!r}'.format(msg1))

      transport.write(msg2.encode())
      print('Data sent: {!r}'.format(msg2))
      
      if transport.can_write_eof():
        transport.write_eof()

    def data_received(self, data):
      logging.info('Data received: {!r}'.format(data.decode()))
      print('Data received: {!r}'.format(data.decode()))

    def connection_lost(self, exc):
      print('The server closed the connection')
      print('Stop the event loop')
      self.loop.stop()

def main():
  logging.basicConfig(filename='client.log',
                      filemode='w',
                      format='%(asctime)s %(levelname)s %(message)s',
                      level=logging.INFO)
  loop = asyncio.get_event_loop()
  msg1 = 'IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997'
  msg2 = 'WHATSAT kiwi.cs.ucla.edu 10 5'
  coro = loop.create_connection(lambda: EchoClientProtocol(msg2, loop),
                                '127.0.0.1', 11760)
  loop.run_until_complete(coro)
  loop.run_forever()
  loop.close()

if __name__ == '__main__':
  main()