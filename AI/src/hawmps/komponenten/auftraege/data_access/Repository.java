package hawmps.komponenten.auftraege.data_access;

import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Nummer;

import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.ArrayList;
import java.util.List;

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

    public Auftrag createAuftrag( boolean istAbgeschlossen, Datum beauftragtAm, Nummer fertigungsAuftragsNummer, Nummer angebotsNummer, Nummer rechnungsNummer, Nummer lieferNummer){
        Auftrag auftrag = Auftrag.create(istAbgeschlossen, beauftragtAm, fertigungsAuftragsNummer, angebotsNummer, rechnungsNummer, lieferNummer);
        entityManager.persist(auftrag);
        return  auftrag;
    }
    public void updateAuftrag(Auftrag auftrag){
        entityManager.merge(auftrag);
    }

    public void deleteAuftragByNummer(Nummer auftragsNummer) {
        Auftrag auftrag = findAuftragByNummer(auftragsNummer);
        entityManager.remove(auftrag);
    }
    public Auftrag findAuftragByNummer(Nummer auftragsNummer){
        CriteriaBuilder builder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Auftrag> query = builder.createQuery(Auftrag.class);
        Root<Auftrag> root = query.from(Auftrag.class);

        query.select(root).where(builder.equal(root.get("nummer"), auftragsNummer));
        List<Auftrag> auftrag = new ArrayList<Auftrag>(entityManager.createQuery(query).getResultList());
        return auftrag.get(0);
    }
}
