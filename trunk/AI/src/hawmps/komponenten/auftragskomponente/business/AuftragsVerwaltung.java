package hawmps.komponenten.auftragskomponente.business;

import hawmps.adts.fachliche.Datum;
import hawmps.komponenten.auftragskomponente.data_access.Angebot;
import hawmps.komponenten.auftragskomponente.data_access.Auftrag;
import hawmps.komponenten.auftragskomponente.data_access.FertigungsAuftrag;
import hawmps.komponenten.auftragskomponente.data_access.Repository;
import hawmps.komponenten.bauteilkomponente.IBauteileKomponente;
import hawmps.komponenten.bauteilkomponente.data_access.Bauteil;
import hawmps.komponenten.bauteilkomponente.data_access.BauteilDTO;
import hawmps.komponenten.bauteilkomponente.data_access.StuecklistenPosition;
import hawmps.komponenten.bauteilkomponente.data_access.StuecklistenPositionDTO;
import hawmps.komponenten.kundenkomponente.IKundenKomponente;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.11.13
 * Time: 21:22
 */
public class AuftragsVerwaltung {
    Repository repository;
    IBauteileKomponente bauteileKomponente;
    IKundenKomponente kundenKomponente;

    private AuftragsVerwaltung(Repository repository, IBauteileKomponente bauteileKomponente, IKundenKomponente kundenKomponente) {
        this.repository = repository;
        this.bauteileKomponente = bauteileKomponente;
        this.kundenKomponente = kundenKomponente;
    }

    public static AuftragsVerwaltung create(Repository repository, IBauteileKomponente bauteileKomponente, IKundenKomponente kundenKomponente) {
        return new AuftragsVerwaltung(repository, bauteileKomponente, kundenKomponente);
    }

    public Auftrag ueberfuehreAngebotInAuftrag(Angebot angebot) {
        BauteilDTO bauteil = bauteileKomponente.findBauteilByNummer(angebot.getBauteilNummer());
        List<FertigungsAuftrag> fertigungsAuftraege = erstelleFertigungsAuftraege(bauteil);
        return repository.createAuftrag(false, Datum.create("11.11.11"),fertigungsAuftraege,angebot.getNummer(),-1,-1);

    }
    private List<FertigungsAuftrag> erstelleFertigungsAuftraege(BauteilDTO bauteil) {
        if (bauteil.getStueckliste() != null) {
            // bauteil ist ein komplexesbautel
            List<Integer> bauteilNummern = bauteileKomponente.getAlleUnterBauteileVon(bauteil);
            List<FertigungsAuftrag> fertigungsAuftragList = new ArrayList<FertigungsAuftrag>();
            fertigungsAuftragList.add(FertigungsAuftrag.create(null, bauteil.getNummer()));
            for (Integer nummer : bauteilNummern) {
                fertigungsAuftragList.add(FertigungsAuftrag.create(null, nummer));
            }

            return fertigungsAuftragList;
        }else
            return new ArrayList<FertigungsAuftrag>();


    }
}
