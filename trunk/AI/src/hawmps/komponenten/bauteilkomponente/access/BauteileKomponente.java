package hawmps.komponenten.bauteilkomponente.access;

import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.bauteilkomponente.IBauteileKomponente;
import hawmps.komponenten.bauteilkomponente.business.BauteilVerwaltung;
import hawmps.komponenten.bauteilkomponente.data_access.*;

import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaBuilder;
import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.11.13
 * Time: 17:15
 */
public class BauteileKomponente implements IBauteileKomponente {
    private Repository repository;
    private EntityManager entityManager;
    private BauteilVerwaltung bauteilVerwaltung;

    private BauteileKomponente(EntityManager entityManager) {
        this.repository = Repository.create(entityManager);
        this.entityManager = entityManager;
        this.bauteilVerwaltung = new BauteilVerwaltung(repository);
    }

    public static BauteileKomponente create(EntityManager entityManager) {
        return new BauteileKomponente(entityManager);
    }

    @Override
    public BauteilDTO createBauteil(Name name, int arbeitsplanNummer, Stueckliste stueckliste) {
        return repository.createBauteil(name, arbeitsplanNummer, stueckliste).toDTO();
    }

    @Override
    public void updateBauteil(BauteilDTO bauteilDTO) {
        Bauteil bauteil1 = new Bauteil();
        bauteil1.FromDTO(bauteilDTO);
        repository.updateBauteil(bauteil1);
    }

    @Override
    public void deleteBauteilByNummer(int BauteilNummer) {
        repository.deleteBauteilByNummer(BauteilNummer);
    }

    @Override
    public BauteilDTO findBauteilByNummer(int nummer) {
        return repository.findBauteilByNummer(nummer).toDTO();
    }

    @Override
    public List<BauteilDTO> findBauteilByName(Name name) {
        List<BauteilDTO> result = new ArrayList<BauteilDTO>();
        for (Bauteil bauteil : repository.findBauteilByName(name)) {
            result.add(bauteil.toDTO());
        }
        return result;
    }

    @Override
    public List<Integer> getAlleUnterBauteileVon(BauteilDTO bauteilDTO) {
        return bauteilVerwaltung.getAlleUnterBauteileVon(repository.findBauteilByNummer(bauteilDTO.getNummer()));
    }

    @Override
    public void createTestBauteil(Name name) {
        Bauteil schraube = repository.createBauteil(Name.create("Schraube AB3"),-1,null);
        Bauteil kantholz = repository.createBauteil(Name.create("Kantholz XYZ3"),-1,null);
        Bauteil mutter = repository.createBauteil(Name.create("Mutter AB3"),-1,null);
        Bauteil holzplatte = repository.createBauteil(Name.create("Holzplatte"),-1,null);



        List<StuecklistenPosition> stuecklistenPositionen = new ArrayList<StuecklistenPosition>();
        stuecklistenPositionen.add(StuecklistenPosition.create(25, schraube));
        stuecklistenPositionen.add(StuecklistenPosition.create(4, kantholz));
        stuecklistenPositionen.add(StuecklistenPosition.create(25, mutter));
        stuecklistenPositionen.add(StuecklistenPosition.create(1, holzplatte));

        Stueckliste stueckliste = Stueckliste.create(Datum.create("20.11.13"), Datum.create("25.11.14"), stuecklistenPositionen);
        createBauteil(name,-1,stueckliste);
    }
}
