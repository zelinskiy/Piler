
curl -v -GET localhost:8080/private2/treatment/my/full -H "email: user1@mail.com" -H "password: pass"

curl -v -GET localhost:8080/private2/treatment/new/plan -H "email: user1@mail.com" -H "password: pass"

curl -v -XDELETE localhost:8080/private2/treatment/delete/plan/2 -H "email: user1@mail.com" -H "password: pass"
