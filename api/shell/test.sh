#/bin/sh

curl -XGET localhost:8080/public/greeting &> /dev/null && echo "OK"
curl -XGET localhost:8080/private/all -H "email: user1@mail.com" -H "password: pass" &> /dev/null && echo "OK"
