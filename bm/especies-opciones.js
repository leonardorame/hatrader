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
    var especies = function(){
        casper.thenOpen('http://www.bolsar.com/VistasTR/PaginaFuturosOpciones.aspx/GetOpciones',
            {
                method: "POST",
                headers: {
                    'Content-Type': 'application/json; charset=utf-8',
                },
                data: {
                        "bDemora":"False"
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
        especies();
    });
});

casper.run();
