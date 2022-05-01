# Beschreibung

Ein taktisches Kartenspiel, bei dem zwei Spieler komplexe Armeen aufbauen und gegeneinander antreten lassen. Sie bestehen aus verschiedenen Einheiten, deren Stärke und Position auf dem Spielfeld durch einen Ablauf logischer/mathematischer Operationen bestimmt und durch das Spielen von Karten generiert werden. Die Anzahl der zu spielenden Karten ist pro Runde begrenzt. Einheiten werden pro Runde in einem separierten Bereich (Bank) generiert und durch chronologisch sinnvolles Einsetzen der zufällig ausgegebenen Karten strategisch aufgewertet. Mit Abschluss der Runde werden die Einheiten auf das 12 x 12 flächige, schachbrettähnliche Spielfeld gesetzt und wandern jede Runde um eine Reihe in Richtung gegnerische Seite weiter. Bei Aufeinandertreffen gegnerischer Einheiten überlebt nur die stärkere Einheit. Kommen Einheiten am gegnerischen Ende des Spielfeldes an, wird dem Gegner entsprechend ihrer Stärke Leben abgezogen - das Spiel ist gewonnen, wenn der Gegner 20 Lebenspunkte verloren hat.

# Ausführung

Das Projekt ist mit stack als Package `ffpproject` gebaut und wird am einfachsten auch mit stack gebaut und ausgeführt. Für Windows-PCs stellen wir eine einfach ausführbare `runProject.cmd` bereit, die eine ggf. noch ausgeführte Instanz des Projekts schließt, es dann neu baut und ausführt und den Standard-Browser mit der lokalen URL öffnet, auf der der Threepenny-Server und das Spiel läuft.

Alternativ sind die folgenden Schritte manuell der Reihe nach auszuführen:

- `stack build` im ffpproject-Ordner ausführen, um den Code zu bauen
- `stack exec ffpproject-exe` im ffp-Project-Ordner ausführen, um die von stack automatisch generierte ausführbare Datei zum Starten der Anwendung zu öffnen
- Browser der Wahl öffnen und auf http://127.0.0.1:8023 navigieren, das Spiel wird dort starten.
