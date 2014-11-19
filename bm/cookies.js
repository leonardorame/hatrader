var casper = require('casper').create({
         pageSettings: {
            userAgent: "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/30.0.1599.101 Safari/537.36"
        }
    });

//var x = require('casper').selectXPath;
var firstUrl;
casper.start('http://www.bolsar.com/');

casper.then(function() {
    firstUrl = this.getCurrentUrl()
    this.evaluate(function(){
        document.getElementById('txtUsuario').value = 'martinrame';
        document.getElementById('txtPassword').value = 'leonardo13';
        //document.getElementById('txtUsuario').value = 'csarachu';
        //document.getElementById('txtPassword').value = 'bE4bkzn6';
        var a = document.getElementById("btnOk");
        var e = document.createEvent('MouseEvents');
        e.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
        a.dispatchEvent(e);
        waitforload = true;
    });
});

casper.waitFor(function check(){
    return this.getCurrentUrl() != firstUrl;
}, function then() {
    var fs = require('fs')
    var cookies = JSON.stringify(phantom.cookies)
    fs.write("cookies.txt", cookies, 644)
});

casper.run();
