package hawmps.komponenten.auftraege.data_access;

import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Nummer;

import javax.persistence.EntityManager;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 15:06
 */
public class Repository {
    private EntityManager entityManager;

    public Repository(EntityManager entityManager) {
        this.entityManager = entityManager;
    }

    public Auftrag createAuftrag(Nummer nummer, boolean istAbgeschlossen, Datum beauftragtAm, Nummer fertigungsAuftragsNummer, Nummer angebotsNummer, Nummer rechnungsNummer, Nummer lieferNummer){
        Auftrag auftrag = new Auftrag(nummer,istAbgeschlossen,beauftragtAm,fertigungsAuftragsNummer,angebotsNummer,rechnungsNummer,lieferNummer);
        entityManager.persist(auftrag);
        return  auftrag;
    }
    public void updateAuftrag(Auftrag auftrag){
        entityManager.merge(auftrag);
    }

    public Auftrag findAuftragByNummer(Nummer auftragsNummer){

    }
}
