package hawmps.komponenten.bauteilkomponente;

import hawmps.adts.fachliche.Name;
import hawmps.komponenten.bauteilkomponente.data_access.Bauteil;
import hawmps.komponenten.bauteilkomponente.data_access.BauteilDTO;
import hawmps.komponenten.bauteilkomponente.data_access.Stueckliste;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.11.13
 * Time: 17:15
 */
public interface IBauteileKomponente {
    public BauteilDTO createBauteil(Name name, int arbeitsplanNummer, Stueckliste stueckliste);

    public void updateBauteil(BauteilDTO bauteil);

    public void deleteBauteilByNummer(int BauteilNummer);

    public BauteilDTO findBauteilByNummer(int nummer);

    public List<BauteilDTO> findBauteilByName(Name name);

    public List<Integer> getAlleUnterBauteileVon(BauteilDTO bauteilDTO);
    @Deprecated
    public void createTestBauteil(Name name);
}
