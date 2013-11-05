package hawmps.komponenten.bauteile;

import hawmps.adts.fachliche.Name;
import hawmps.adts.fachliche.Nummer;
import hawmps.komponenten.bauteile.data_access.Bauteil;
import hawmps.komponenten.bauteile.data_access.Stueckliste;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.11.13
 * Time: 17:15
 */
public interface IBauteileKomponente {
    public Bauteil createBauteil(Name name, Nummer arbeitsplanNummer, Stueckliste stueckliste);

    public void updateBauteil(Bauteil bauteil);

    public void deleteBauteilByNummer(Nummer BauteilNummer);

    public Bauteil findBauteilByNummer(Nummer nummer);

    public List<Bauteil> findBauteilByName(Name name);
}
