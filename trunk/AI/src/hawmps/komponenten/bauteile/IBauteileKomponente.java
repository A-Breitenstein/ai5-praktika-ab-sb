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
    public Bauteil createBauteil(Name name, int arbeitsplanNummer, Stueckliste stueckliste);

    public void updateBauteil(Bauteil bauteil);

    public void deleteBauteilByNummer(int BauteilNummer);

    public Bauteil findBauteilByNummer(int nummer);

    public List<Bauteil> findBauteilByName(Name name);
}
