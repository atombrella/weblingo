#!/bin/bash

DOMAIN='monad.dk'

stack exec site build

rsync -az --force --delete --progress --exclude-from="rsync.exclude.txt" -e "ssh -p22" ./_site droplet:/var/www/html
rsync -aze ssh src/nginx.conf droplet:/etc/nginx/sites-enabled/${DOMAIN}
