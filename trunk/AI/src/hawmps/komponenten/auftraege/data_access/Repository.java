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

    private Repository(EntityManager entityManager) {
        this.entityManager = entityManager;
    }

    public static Repository create(EntityManager entityManager) {
        return new Repository(entityManager);
    }

    public Auftrag createAuftrag(boolean istAbgeschlossen, Datum beauftragtAm, List<FertigungsAuftrag> zugehoerigeFertigungsAuftrage, int angebotsNummer, int rechnungsNummer, int lieferNummer){
        Auftrag auftrag = Auftrag.create(istAbgeschlossen, beauftragtAm, zugehoerigeFertigungsAuftrage, angebotsNummer, rechnungsNummer, lieferNummer);
        entityManager.persist(auftrag);
        return  auftrag;
    }
    public void updateAuftrag(Auftrag auftrag){
        entityManager.merge(auftrag);
    }

    public void deleteAuftragByNummer(int auftragsNummer) {
        Auftrag auftrag = findAuftragByNummer(auftragsNummer);
        entityManager.remove(auftrag);
    }
    public Auftrag findAuftragByNummer(int auftragsNummer){
        CriteriaBuilder builder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Auftrag> query = builder.createQuery(Auftrag.class);
        Root<Auftrag> root = query.from(Auftrag.class);

        query.select(root).where(builder.equal(root.get("nummer"), auftragsNummer));
        List<Auftrag> auftrag = new ArrayList<Auftrag>(entityManager.createQuery(query).getResultList());
        return auftrag.get(0);
    }


}
