#/bin/sh

#some random shell

sqlite3 sqlite.db  'INSERT INTO user VALUES ("user1@mail.com", "pass", "normal", 0);'

curl -XPOST localhost:8080/private/medicament/add -H "email: user1@mail.com" -H "password: pass" -H "Content-Type: application/json" -d '{"name": "Aspirin", "diameter": 1, "height": 1}'

curl -XGET localhost:8080/public/greeting
curl -XGET localhost:8080/private/medicament/all -H "email: user1@mail.com" -H "password: pass"

curl -XPOST localhost:8080/private/treatment/new/plan -H "email: user1@mail.com" -H "password: pass" -H "Content-Type: application/json" -d '{"userId":"user1@mail.com"}'

curl -XGET localhost:8080/private/treatment/my -H "email: user1@mail.com" -H "password: pass"

curl -XPOST localhost:8080/private/treatment/new/row -H "email: user1@mail.com" -H "password: pass" -H "Content-Type: application/json" -d '{"at":"1997-07-16T19:20:30+01:00", "medicamentId":1, "treatmentPlan":1}'
