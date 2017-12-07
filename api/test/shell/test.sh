#/bin/sh

#some random shell

date -u +"%Y-%m-%dT%H:%M:%SZ" --date "10 seconds"

sqlite3 sqlite.db  'INSERT INTO user VALUES ("user1@mail.com", "pass", "normal", 0);'

find . -name '*.hs' | xargs wc -l

sqlite3 sqlite.db "DELETE FROM user WHERE email = 'test@mail.com'; DELETE FROM device WHERE ip = '127.0.0.1'"

#Old authentication

curl -XPOST localhost:8080/private2/medicament/add -H "email: user1@mail.com" -H "password: pass" -H "Content-Type: application/json" -d '{"name": "Aspirin", "diameter": 1, "height": 1}'

curl -XGET localhost:8080/public/greeting
curl -XGET localhost:8080/private2/medicament/all -H "email: user1@mail.com" -H "password: pass"

curl -XPOST localhost:8080/private2/treatment/new/plan -H "email: user1@mail.com" -H "password: pass" -H "Content-Type: application/json" -d '{"userId":"user1@mail.com"}'

curl -XGET localhost:8080/private2/treatment/my -H "email: user1@mail.com" -H "password: pass"

curl -XPOST localhost:8080/private/treatment/new/row -H "Authorization: Bearer JWT" -H "Content-Type: application/json" -d '{"at":"`date -u +"%Y-%m-%dT%H:%M:%SZ" --date "10 seconds"`", "medicamentId":1, "treatmentPlan":1}'

# Bearer JWT Authentication

curl -v -POST localhost:8080/public/jwt/login -H "Content-Type: application/json" -d '{"email": "user1@mail.com", "pass":"pass"}'

curl -v -POST localhost:8080/private/device/my/stored/1 -H "Authorization: Bearer JWT"

# IoT Client

curl -v -GET localhost:8070/dispence/1/3

