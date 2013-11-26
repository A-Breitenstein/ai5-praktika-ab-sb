package hawmps.komponenten.server;

import hawmps.fassade.ISystemFassade;
import hawmps.komponenten.auftragskomponente.IAuftragsKomponente;
import hawmps.komponenten.bauteilkomponente.IBauteileKomponente;
import hawmps.komponenten.kundenkomponente.IKundenKomponente;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Created with IntelliJ IDEA.
 * User: timey
 * Date: 24.11.13
 * Time: 14:31
 *
 * Dieses Interface beschreibt die Methoden die entfernt aufgerufen werden koennen.
 * Damit stellt dieses Interface praktisch die Fassade des Systems nach aussen dar,
 * deswegen werden alle Interfaces der einzelen Komponenten extendet.
 *
 * Das MarkerInterface Remote muss extendet werden um die entfernten Aufrufe zu ermoeglichen.
 * Alle Datentypen der Methoden dieses Interface muessen Serializable implementieren oder primitive Typen sein!
 *
 */
public interface IMpsServer extends Remote, ISystemFassade {
    boolean isDeaktiviert() throws RemoteException;
    void setisDeaktiviert(boolean b) throws RemoteException;
    String getName() throws  RemoteException;
}
