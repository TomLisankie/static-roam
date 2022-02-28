#!/usr/bin/python

# usage: bin/simple_server.sh port base-dir


import SimpleHTTPServer
import SocketServer
import sys
import os

PORT = int(sys.argv[1])
BASE = sys.argv[2]

os.chdir(BASE)                  # I'm sure there's a better way to do this

Handler = SimpleHTTPServer.SimpleHTTPRequestHandler
# Make extensionless files serve as html
Handler.extensions_map.update({
    '': 'text/html',
});

httpd = SocketServer.TCPServer(("", PORT), Handler)

print "Serving at port", PORT
httpd.serve_forever()
