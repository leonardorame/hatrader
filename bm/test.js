var casper = require('casper').create({
         pageSettings: {
            userAgent: "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.97 Safari/537.11"
        }
    });

var x = require('casper').selectXPath;
var firstUrl;
casper.start('http://www.bolsar.com/', function() {
    this.echo(this.getTitle());
});

casper.then(function() {
    firstUrl = this.getCurrentUrl()
    this.echo(firstUrl);
    this.evaluate(function(){
        document.getElementById('txtUsuario').value = 'martinrame';
        document.getElementById('txtPassword').value = 'leonardo13';
        var a = document.getElementById("btnOk");
        var e = document.createEvent('MouseEvents');
        e.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
        a.dispatchEvent(e);
        waitforload = true;
    });
});

casper.waitFor(function check() {
    return this.getCurrentUrl() != firstUrl;
}, function then() {
    casper.thenOpen(this.getCurrentUrl());
    this.echo(this.getTitle());
    console.log('<---- Antes de abrir ------> ' +this.getCurrentUrl());
    casper.thenOpen('http://www.bolsar.com/VistasTR/PaginaPrincipal.aspx');
    casper.thenOpen('http://www.bolsar.com/WebServices/AcreditadorWS.asmx/Refresh',
        {
            method: "post",
            data: {
                bDemora: "False"
            }
        });
    casper.then(function(){
        require('utils').dump(this.getPageContent());
    });
});


casper.run();
