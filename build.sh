#!/bin/bash
rm -rf official && ./node_modules/.bin/webpack --env.production --env.token=$1 && cp -r dist official && rm official/*.js && mv official/officialpage.html official/index.html
