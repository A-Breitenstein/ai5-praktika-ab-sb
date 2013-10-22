Starten des Namensdienstes:
--------------------------
(w)erl -(s)name ns -setcookie zummsel
1>nameservice:start( ).

ns_send:msg(State,NameServiceNode)
- State: einen der Nachrichten help, reset, list oder kill
- NameServiceNode: Name des Nodes, auf dem der Namensdienst gestartet wurde

Starten des Koordinators:
--------------------------
(w)erl -(s)name ko -setcookie zummsel
2>koordinator:start( ).

% liest die koordinator.cfg ein:
% {arbeitszeit, 3}:		simulierte Arbeitszeit für die ggT-Berechnung
% {termzeit, 3}:			Wenn termzeit keine Berechnung dann wird Terminierungsabstimmung initiiert
% {ggtprozessnummer, 42}:	Anzahl ggT Prozesse je Starter (und default ggT)
% {nameservicenode, ns@Brummpa}:	node des Namensdienstes
% {koordinatorname, chef}:	Name des Koordinators
% {korrigieren, 0}:	Korigiert falsche Terminierungsmeldungen (== 1)

ko_send:msg(State)
- State: einen der Nachrichten help, vals, step, ask, ggt, reset oder kill
- und liest koordinator.cfg ein

Starten der ggT-Prozesse:
--------------------------
(w)erl -(s)name ggt -setcookie zummsel
3>start_starter:go(Anzahl,Start)

- Anzahl: Anzahl der Starter auf einer Node ab Start+1
- aufruf von ggt_starter(Starternummer)
- aufruf von ggt_process:start(ArbeitsZeit,TermZeit,Praktikumsgruppe,Teamnummer,GGTProzessnummer,Starternummer,NameS,Koordinator)

% ggt_starter liest die ggt.cfg ein:
% {praktikumsgruppe, 4}:	Nummer des Praktikums
% {teamnummer, 88}:		Nummer des Teams
% {nameservicenode, ns@Brummpa}:	node des Namensdienstes
% {koordinatorname, chef}:	Name des Koordinators

Runterfahren:
-------------
2> Ctrl/Strg Shift G
-->q
