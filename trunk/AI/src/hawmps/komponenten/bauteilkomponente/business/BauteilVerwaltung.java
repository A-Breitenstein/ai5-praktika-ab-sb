package hawmps.komponenten.bauteilkomponente.business;


import hawmps.komponenten.bauteilkomponente.data_access.Bauteil;
import hawmps.komponenten.bauteilkomponente.data_access.Repository;
import hawmps.komponenten.bauteilkomponente.data_access.StuecklistenPosition;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.11.13
 * Time: 21:21
 */
public class BauteilVerwaltung {
    Repository repository;

    public BauteilVerwaltung(Repository repository) {
        this.repository = repository;
    }
    public List<Integer> getAlleUnterBauteileVon(Bauteil bauteil) {
        List<Integer> bauteilNummer = new ArrayList<Integer>();
        sammelAllesEin(bauteil,bauteilNummer);
        return bauteilNummer;
    }
    private void sammelAllesEin(Bauteil bauteil, List<Integer> bauteilNummern) {
        for (StuecklistenPosition o : bauteil.getStueckliste().getStuecklistenPositionen()) {
            if(o.getBauteil().getStueckliste() != null){
                bauteilNummern.add(o.getBauteil().getNummer());
                sammelAllesEin(o.getBauteil(),bauteilNummern);
            }
        }
    }

}
