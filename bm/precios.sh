curl -d "idNumber=24471579&password=martin13" \
  -k \
  -A "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:33.0) Gecko/20100101 Firefox/33.0" \
  -c cookies.txt \
  -e "https://www.bullmarketbrokers.com.ar/" \
  "https://www.bullmarketbrokers.com.ar/Home/Login"

curl -k \
  -A "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:33.0) Gecko/20100101 Firefox/33.0" \
  -b cookies.txt \
  -e "https://www.bullmarketbrokers.com.ar/" \
  "https://www.bullmarketbrokers.com.ar/Operations/StockOperator/?prec.aspx?cons=1"

