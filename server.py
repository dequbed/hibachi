import os
import http.server
from urllib.parse import urlparse

ADDRESS = "localhost"
PORT = 8000

BASE = "out"

os.chdir(BASE)

class HibachiHandler(http.server.SimpleHTTPRequestHandler):
    def do_GET(self):
        parsed = urlparse(self.path)
        path = "./" + parsed.path
        actpath = BASE + parsed.path
        print("Called with path {} ({})".format(parsed.path, actpath))
        if not os.path.exists(path):
            print("path {} ({}) not found, trying rewrites:".format(path, actpath))
            htmlpath = path + ".html"
            print("Trying", htmlpath)
            if os.path.exists(htmlpath):
                print("Success")
                self.path = parsed.path + ".html"
            else:
                print("No rewrite successful")
        print("do_GET(path={} => {})".format(path, self.path))
        super(HibachiHandler, self).do_GET()

with http.server.HTTPServer(("localhost", PORT), HibachiHandler) as httpd:
    print("Serving on {:1} port {:1}".format(ADDRESS, PORT))
    httpd.serve_forever()
