var system = require('system');
var args = system.args;

var casper = require('casper').create({
         pageSettings: {
            userAgent: "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/30.0.1599.101 Safari/537.36"
        }
    });

casper.start();

casper.then(function(){
    if (args.length < 6) {
      console.log('Este script recibe dos argumentos ESPECIE y FILTROVTO:');
      console.log('ESPECIE: Accion, Opcion, Bono (en mayúsculas), ej: GGAL.');
      console.log('FILTROVTO: 0 (opcion), 1 (CI), 2 (24hs), 3 (48hs), 4(72hs).');
     casper.exit();
    } else {
        especie = args[4];
        filtrovto = args[5];
    }
});


casper.then(function(){
    var fs = require('fs')
    var data = fs.read("cookies.txt")
    phantom.cookies = JSON.parse(data)
});

casper.then(function(){
    var intradiario = function(){
        casper.thenOpen('http://www.bolsar.com/VistasTR/PaginaIntradiarioEspecies.aspx/GetDataPack',
            {
                method: "POST",
                headers: {
                    'Content-Type': 'application/json; charset=utf-8',
                },
                data: {
                        "objEstadoIntradiarioEspecie": {
                            "FiltroEspecie": especie,
                            "FiltroVto": filtrovto,
                            "MensajeNro": 0
                        }
                }

            });
        casper.then(function(){
            // actualizamos las cookies
            var fs = require('fs')
            var cookies = JSON.stringify(phantom.cookies)
            fs.write("cookies.txt", cookies, 644)
            // retornamos la salida
            casper.echo( this.getPageContent());
        });
    }

    casper.then(function(){
        intradiario();
    });
});

casper.run();
