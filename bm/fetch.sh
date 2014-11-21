cookies() {
    res=$1;
    echo "${res}"
    if [ "$res" = "Please read cookies and try again." ]; then
      echo "Reading Cookies."
      casper/bin/casperjs ./cookies.js
      echo "After reading cookies."
      # una vez que lee las cookies lee fecha y mercado
      echo "Reading FechaYMercado..."
      ../datafiller/datafiller --fechaymercado;
      echo "After reading FechaYMercado."
    fi
    if [ "$res" = "Please read fechaymercado again." ]; then
      echo "Reading FechaYMercado..."
      ../datafiller/datafiller --fechaymercado;
      echo "After reading FechaYMercado."
    fi
    if [ "$res" = "Servidor inactivo." ]; then
      echo "Servidor inactivo, esperando 30 segundos..."
      sleep 30
      ../datafiller/datafiller --fechaymercado;
    fi
}

while true; do 
  echo "Fetching lideres..."
  res="$(../datafiller/datafiller --lideres)"
  cookies "${res}"
  echo "Fetching opciones..."
  res="$(../datafiller/datafiller --opciones)"
  cookies "${res}"
  sleep 2; 
done
