var casper = require('casper').create({
         pageSettings: {
            userAgent: "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/30.0.1599.101 Safari/537.36"
        }
    });

casper.start();

casper.then(function(){
    var fs = require('fs')
    var data = fs.read("cookies.txt")
    phantom.cookies = JSON.parse(data)
});

casper.then(function(){
    var lideres = function(){
        casper.thenOpen('http://www.bolsar.com/VistasTR/PaginaLideres.aspx/GetFechaYMercado',
            {
                method: "POST",
                headers: {
                    'Content-Type': 'application/json; charset=utf-8',
                },
                data: {"bDemora":"False"}
            });
        casper.then(function(){
            // actualizamos las cookies
            var fs = require('fs')
            var cookies = JSON.stringify(phantom.cookies)
            fs.write("cookies.txt", cookies, 644)
            // retornamos la salida
            casper.echo( this.getPageContent());
            // almacenamos la fechaymercado
            var fs = require('fs');
            var fechaymercado = this.getPageContent();
            fs.write("fechaymercado.json", fechaymercado, 644);
        });
    }

    casper.then(function(){
        lideres();
    });
});

casper.run();
