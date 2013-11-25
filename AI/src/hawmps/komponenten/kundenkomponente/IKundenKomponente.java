package hawmps.komponenten.kundenkomponente;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.kundenkomponente.data_access.Kunde;
import hawmps.komponenten.kundenkomponente.data_access.KundeDTO;

import java.rmi.RemoteException;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 13:38
 */
public interface IKundenKomponente {
    KundeDTO createKunde(Name Vorname,Name Nachname,Adresse adresse) throws RemoteException;
    List<KundeDTO> findByNachname(Name Nachname) throws RemoteException;
    void deleteKundeByNummer(int kundenNummer) throws RemoteException;
}
