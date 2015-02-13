#!/usr/bin/python

import json
import oauth2
import requests

r = requests.get(
    'https://10.173.161.111/api/v1.0/storage/volume/',
    headers={'Content-Type': 'application/json'},
    auth=('root', 'xxx'),
    verify=False
)
    #params={'offset': 0, 'limit': 10000}
print r.text
print ""
print r.url
print r.headers
print ""
print r.auth
