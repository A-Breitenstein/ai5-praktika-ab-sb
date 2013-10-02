Starten:
--------------------
(w)erl -(s)name wk -setcookie zummsel
1> demoRemote:start(<Name>).
1> demoMSGQ:start().


Runterfahren:
-------------
2> Ctrl/Strg Shift G
-->q

Informationen zu Prozessen:
-------------
2> pman:start().
2> process_info(PID).

Kompilieren:
-------------
1> c(<Dateiname>).

Entfernter Aufruf:
-------------
1> net_adm:ping('node@ProfDrKlauck').
2> PID = {host,'node@ProfDrKlauck'}.
3> demoRemote:rpc(PID,call).




{tipOfTheDaymessageServer,messageServer@PIGPEN} ! {eval_loop,{getMsgId,node()}}.



dQ: 1,2,3,4,5,6,7    << 8,9  >>>> 8 bis 9 fehlt
hQ:10,11,12,15

2. EINE fehlermeldung erstellt

hQ [8],9,10,11,12,14,15

3.
dQ:1,2,3,4,5,6,7[8],9,10,11,12
hQ 14,15




15,14,16

Wenn in der Holdbackqueue von der Anzahl her mehr als die Hälfte an Nachrichten enthalten sind, als durch die vorgegebene
maximale Anzahl an Nachrichten in der Deliveryqueue stehen können, dann wird, sofern eine Lücke besteht, diese Lücke zwischen Deliveryqueue und Holdbackqueue
mit genau einer Fehlernachricht geschlossen, etwa: ***Fehlernachricht fuer Nachrichtennummern 11 bis 17 um 16.05 18:01:30,580|.. Es werden keine weiteren Lücken
innerhalb der Holdbackqueue gefüllt, also Lücken die nach der kleinsten in der Holdbackqueue vorhandenen Textzeilennummer vorkommen! In dem Sinne wird die Holdbackqueue
in diesem Fall nicht zwingend geleert, sondern nur bis zur nächsten Lücke geleert.

