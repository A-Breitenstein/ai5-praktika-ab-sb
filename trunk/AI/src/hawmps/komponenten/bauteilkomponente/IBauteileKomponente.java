package hawmps.komponenten.bauteilkomponente;

import hawmps.adts.fachliche.Name;
import hawmps.komponenten.bauteilkomponente.data_access.Bauteil;
import hawmps.komponenten.bauteilkomponente.data_access.BauteilDTO;
import hawmps.komponenten.bauteilkomponente.data_access.Stueckliste;

import java.rmi.RemoteException;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.11.13
 * Time: 17:15
 */
public interface IBauteileKomponente {
    public BauteilDTO createBauteil(Name name, int arbeitsplanNummer, Stueckliste stueckliste) throws RemoteException;

    public void updateBauteil(BauteilDTO bauteil) throws RemoteException;

    public void deleteBauteilByNummer(int BauteilNummer) throws RemoteException;

    public BauteilDTO findBauteilByNummer(int nummer) throws RemoteException;

    public List<BauteilDTO> findBauteilByName(Name name) throws RemoteException;

    public List<Integer> getAlleUnterBauteileVon(BauteilDTO bauteilDTO) throws RemoteException;
    @Deprecated
    public void createTestBauteil(Name name) throws RemoteException;
}
