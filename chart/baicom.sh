while true; do
  curl "http://bolsa.baicom.com/2" | ../baicom/baicomparser | ./rttodaily
  sleep 10
  curl "http://bolsa.baicom.com/" | ../baicom/baicomparser | ./rttodaily
  sleep 30
done
