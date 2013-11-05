package hawmps.komponenten.auftraege.business;

import hawmps.adts.fachliche.Datum;
import hawmps.adts.fachliche.Nummer;
import hawmps.komponenten.auftraege.data_access.Auftrag;
import hawmps.komponenten.auftraege.data_access.FertigungsAuftrag;
import hawmps.komponenten.auftraege.data_access.Repository;
import hawmps.komponenten.bauteile.IBauteileKomponente;
import hawmps.komponenten.bauteile.data_access.Bauteil;
import hawmps.komponenten.bauteile.data_access.StuecklistenPosition;
import hawmps.komponenten.kunden.IKundenKomponente;

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

    public Auftrag ueberfuehreAngebotInAuftrag(Nummer bauteilNummer) {
        Bauteil bauteil = bauteileKomponente.findBauteilByNummer(bauteilNummer);
        List<FertigungsAuftrag> fertigungsAuftraege = erstelleFertigungsAuftraege(bauteil);
        Auftrag neuerAuftrag = Auftrag.create(false, Datum.create("11.11.11"),fertigungsAuftraege,null,null,null);
        return neuerAuftrag;

    }
    private List<FertigungsAuftrag> erstelleFertigungsAuftraege(Bauteil bauteil) {
        if (bauteil.getStueckliste() != null) {
            // bauteil ist ein komplexesbautel
            List<Nummer> bauteilNummern = new ArrayList<Nummer>();
            sammelAllesEin(bauteil,bauteilNummern);
            List<FertigungsAuftrag> fertigungsAuftragList = new ArrayList<FertigungsAuftrag>();
            fertigungsAuftragList.add(FertigungsAuftrag.create(null, bauteil.getNummer()));
            for (Nummer nummer : bauteilNummern) {
                fertigungsAuftragList.add(FertigungsAuftrag.create(null, nummer));
            }

            return fertigungsAuftragList;
        }else
            return new ArrayList<FertigungsAuftrag>();


    }
    private void sammelAllesEin(Bauteil bauteil,List<Nummer> bauteilNummern) {
        for (StuecklistenPosition o : bauteil.getStueckliste().getStuecklistenPositionen()) {
            if(o.getBauteil().getStueckliste() != null){
                bauteilNummern.add(o.getBauteil().getNummer());
                sammelAllesEin(o.getBauteil(),bauteilNummern);
            }
        }
    }
}
