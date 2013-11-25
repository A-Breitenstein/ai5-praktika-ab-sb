package hawmps.fassade;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Betrag;
import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.auftragskomponente.IAuftragsKomponente;
import hawmps.komponenten.auftragskomponente.access.AuftragsKomponente;
import hawmps.komponenten.auftragskomponente.data_access.AngebotDTO;
import hawmps.komponenten.auftragskomponente.data_access.AuftragDTO;
import hawmps.komponenten.auftragskomponente.data_access.FertigungsAuftrag;
import hawmps.komponenten.bauteilkomponente.IBauteileKomponente;
import hawmps.komponenten.bauteilkomponente.access.BauteileKomponente;
import hawmps.komponenten.bauteilkomponente.data_access.BauteilDTO;
import hawmps.komponenten.bauteilkomponente.data_access.Stueckliste;
import hawmps.komponenten.kundenkomponente.IKundenKomponente;
import hawmps.komponenten.kundenkomponente.access.KundenKomponente;
import hawmps.komponenten.kundenkomponente.data_access.KundeDTO;
import hawmps.persistenceUtils.TransactionManagement;

import javax.persistence.EntityManager;
import java.rmi.RemoteException;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 24.11.13
 * Time: 17:55
 */
public class HawMps implements ISystemFassade{
    IAuftragsKomponente auftragsKomponente;
    IBauteileKomponente bauteileKomponente;
    IKundenKomponente kundenKomponente;
    EntityManager entityManager;
    TransactionManagement transaktionsVerwaltung;

    public HawMps(EntityManager entityManager) {
        this.entityManager = entityManager;
        transaktionsVerwaltung = new TransactionManagement(entityManager);
        kundenKomponente = KundenKomponente.create(entityManager);
        bauteileKomponente = BauteileKomponente.create(entityManager);
        auftragsKomponente = AuftragsKomponente.create(entityManager, bauteileKomponente, kundenKomponente);
    }


    @Override
    public AuftragDTO createAuftrag(boolean istAbgeschlossen, Datum beauftragtAm, List<FertigungsAuftrag> zugehoerigeFertigungsAuftrage, int angebotsNummer, int rechnungsNummer, int lieferNummer) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        AuftragDTO auftragDTO =  auftragsKomponente.createAuftrag(istAbgeschlossen, beauftragtAm, zugehoerigeFertigungsAuftrage, angebotsNummer, rechnungsNummer, lieferNummer);
        transaktionsVerwaltung.commitTransaction();
        return auftragDTO;
    }

    @Override
    public void updateAuftrag(AuftragDTO auftrag) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        auftragsKomponente.updateAuftrag(auftrag);
        transaktionsVerwaltung.commitTransaction();
    }

    @Override
    public void deleteAuftragByNummer(int auftragsNummer) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        auftragsKomponente.deleteAuftragByNummer(auftragsNummer);
        transaktionsVerwaltung.commitTransaction();
    }

    @Override
    public AuftragDTO findAuftragByNummer(int auftragsNummer) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
         AuftragDTO auftragDTO  = auftragsKomponente.findAuftragByNummer(auftragsNummer);
        transaktionsVerwaltung.commitTransaction();
        return auftragDTO;
    }

    @Override
    public AuftragDTO ueberfuehreAngebotInAuftrag(AngebotDTO angebotDTO) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        AuftragDTO auftragDTO =  auftragsKomponente.ueberfuehreAngebotInAuftrag(angebotDTO);
        transaktionsVerwaltung.commitTransaction();
        return auftragDTO;
    }

    @Override
    public AngebotDTO createAngebot(Datum gueltigAb, Datum gueltigBis, Betrag preis, int kundenNummer, int bauteilNummer) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        AngebotDTO angebotDTO = auftragsKomponente.createAngebot(gueltigAb, gueltigBis, preis, kundenNummer, bauteilNummer);
        transaktionsVerwaltung.commitTransaction();
        return angebotDTO;
    }

    @Override
    public AngebotDTO findAngebotByNummer(int angebotNummer) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        AngebotDTO angebotDTO =  auftragsKomponente.findAngebotByNummer(angebotNummer);
        transaktionsVerwaltung.commitTransaction();
        return angebotDTO;
    }

    @Override
    public List<AngebotDTO> findAngeboteByKundenNummer(int kundenNummer) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        List<AngebotDTO> angebotDTOs =  auftragsKomponente.findAngeboteByKundenNummer(kundenNummer);
        transaktionsVerwaltung.commitTransaction();
        return angebotDTOs;
    }

    @Override
    public BauteilDTO createBauteil(Name name, int arbeitsplanNummer, Stueckliste stueckliste) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        BauteilDTO bauteilDTO =  bauteileKomponente.createBauteil(name, arbeitsplanNummer, stueckliste);
        transaktionsVerwaltung.commitTransaction();
        return bauteilDTO;
    }

    @Override
    public void updateBauteil(BauteilDTO bauteil) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        bauteileKomponente.updateBauteil(bauteil);
        transaktionsVerwaltung.commitTransaction();
    }

    @Override
    public void deleteBauteilByNummer(int BauteilNummer) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        bauteileKomponente.deleteBauteilByNummer(BauteilNummer);
        transaktionsVerwaltung.commitTransaction();
    }

    @Override
    public BauteilDTO findBauteilByNummer(int nummer) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        BauteilDTO bauteilDTO =  bauteileKomponente.findBauteilByNummer(nummer);
        transaktionsVerwaltung.commitTransaction();
        return bauteilDTO;
    }

    @Override
    public List<BauteilDTO> findBauteilByName(Name name) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        List<BauteilDTO> bauteilDTOs =  bauteileKomponente.findBauteilByName(name);
        transaktionsVerwaltung.commitTransaction();
        return bauteilDTOs;
    }

    @Override
    public List<Integer> getAlleUnterBauteileVon(BauteilDTO bauteilDTO) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        List<Integer> nummern = bauteileKomponente.getAlleUnterBauteileVon(bauteilDTO);
        transaktionsVerwaltung.commitTransaction();
        return nummern;
    }

    @Override
    @Deprecated
    public void createTestBauteil(Name name) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        bauteileKomponente.createTestBauteil(name);
        transaktionsVerwaltung.commitTransaction();
    }

    @Override
    public KundeDTO createKunde(Name Vorname, Name Nachname, Adresse adresse) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        KundeDTO kundeDTO = kundenKomponente.createKunde(Vorname, Nachname, adresse);
        transaktionsVerwaltung.commitTransaction();
        return kundeDTO;
    }

    @Override
    public List<KundeDTO> findByNachname(Name Nachname) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        List<KundeDTO> kundeDTOs = kundenKomponente.findByNachname(Nachname);
        transaktionsVerwaltung.commitTransaction();
        return kundeDTOs;
    }

    @Override
    public void deleteKundeByNummer(int kundenNummer) throws RemoteException {
        transaktionsVerwaltung.beginTransaction();
        kundenKomponente.deleteKundeByNummer(kundenNummer);
        transaktionsVerwaltung.commitTransaction();
    }
}
