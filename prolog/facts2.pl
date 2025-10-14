
module(118, 'nocodb:create', 'Insert order', [table('orders')]).
module(119, 'nocodb:create', 'Insert invoice', [table('invoices')]).
module(122, 'regexp:Replace', '', [pattern('^x'), replace('y')]).

% router raíz
router(125, 'router:Switch', 'BasicRouter', []).
path(125, 'Ruta A', '116.statusCode == 200', [117,124, rid(200)]).
path(125, 'Ruta B', otherwise, [126]).


module(117, 'google-drive:uploadAFile', 'Crear archivo PDF', [fileName('{{pedido}}.pdf'), mime('application/pdf')]).
module(124, 'nocodb:updateRecord', '', [table('invoices'), where('id = {{invoiceId}}')]).
module(126, 'nocodb:updateRecord', '(ejemplo)', [table('invoices')]). 


router(200, 'router:Switch', 'Post-Drive Branch', []).
path(200, 'Sub-ruta A1', 'pdf_hex != null', [201,202]).
path(200, 'Sub-ruta A2', none, [203]).

module(201, 'http:MakeRequest', 'Notificar', [method('POST'), url('https://api.example.com/hook')]).
module(202, 'tools:Logger', 'Log', [level('info')]).
module(203, 'tools:Cleanup', 'Cleanup', []).
