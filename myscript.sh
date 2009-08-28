#!/bin/bash
echo -en "Content-Type: text/html; charset=utf-8\r\n"
echo -en "\r\n"

echo "test<br>"
echo "$ZOMAAR <br>"

echo "<hr>"
echo "<pre>"
export
echo "</pre>"

echo "<hr>"
cat `find src -name "*.hs"`
echo "<pre>"
echo "</pre>"

echo "<hr>"
echo "<pre>"
uptime
wc
date
echo "</pre>"

