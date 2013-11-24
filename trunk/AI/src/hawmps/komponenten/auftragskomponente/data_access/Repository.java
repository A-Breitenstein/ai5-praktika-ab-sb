package hawmps.komponenten.auftragskomponente.data_access;

import hawmps.adts.fachliche.Betrag;
import hawmps.adts.fachliche.Datum;

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
        if(auftrag.isEmpty())
            return null;
        return auftrag.get(0);
    }

    public Angebot createAngebot(Datum gueltigAb, Datum gueltigBis, Betrag preis, int kundenNummer, int bauteilNummer) {
        Angebot angebot = Angebot.create(gueltigAb, gueltigBis, preis, kundenNummer, bauteilNummer);
        entityManager.persist(angebot);
        return angebot;
    }

    public Angebot findAngebotByNummer(int angebotNummer){
        CriteriaBuilder builder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Angebot> query = builder.createQuery(Angebot.class);
        Root<Angebot> root = query.from(Angebot.class);

        query.select(root).where(builder.equal(root.get("nummer"), angebotNummer));
        List<Angebot> angebotList = new ArrayList<Angebot>(entityManager.createQuery(query).getResultList());
        if(angebotList.isEmpty())
            return null;
        return angebotList.get(0);
    }

    public List<Angebot> findAngeboteByKundenNummer(int kundenNummer){
        CriteriaBuilder builder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Angebot> query = builder.createQuery(Angebot.class);
        Root<Angebot> root = query.from(Angebot.class);

        query.select(root).where(builder.equal(root.get("kundenNummer"), kundenNummer));
        List<Angebot> angebotList = new ArrayList<Angebot>(entityManager.createQuery(query).getResultList());
        if(angebotList.isEmpty())
            return null;
        return angebotList;
    }

    public void updateAngebot(Angebot angebot){
        entityManager.merge(angebot);
    }

    public void deleteAngebotByNummer(int angebotsNummer) {
        Angebot angebot = findAngebotByNummer(angebotsNummer);
        entityManager.remove(angebot);
    }



}
