package hawmps.komponenten.kunden.data_access;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.adts.fachliche.Nummer;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import javax.persistence.EntityManager;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 03.11.13
 * Time: 13:49
 */
public class Repository {
    private EntityManager entityManager;

    public Repository(EntityManager entityManager) {
        this.entityManager = entityManager;
    }

    public Kunde createKunde(Nummer nummer, Name vorname, Name nachname, Adresse adresse){
        throw new NotImplementedException();
    }
    public void updateKunde(Kunde kunde) {

    }
    public void deleteKunde(Kunde kunde){

    }

    public List<Kunde> findKundeByNachname(Name Nachname){

    }
    public List<Kunde> findKundeByNummer(Nummer KundenNummer){

    }
}
