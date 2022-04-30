#!/usr/local/bin/python3

# usage: bin/simple_server.sh port base-dir


import http.server
import socketserver
import sys
import os

PORT = int(sys.argv[1])
BASE = sys.argv[2]

os.chdir(BASE)                  # I'm sure there's a better way to do this

Handler = http.server.SimpleHTTPRequestHandler
# Make extensionless files serve as html
Handler.extensions_map.update({
    '': 'text/html',
});

httpd = socketserver.TCPServer(("", PORT), Handler)

print("Serving at port", PORT)
httpd.serve_forever()
