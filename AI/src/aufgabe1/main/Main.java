package aufgabe1.main;

import aufgabe1.entities.Bankkonto;
import aufgabe1.entities.Kinokarte;
import aufgabe1.entities.Kinosaal;
import aufgabe1.entities.Kunde;
import aufgabe1.manager.KinosaalManager;
import aufgabe1.manager.KundeCriteria;
import aufgabe1.manager.KundeManager;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Date: 04.10.13
 * Time: 16:53
 */
public class Main {

    public static void main(String[] args) {


        //CREATE KUNDE + KINOSAAL + sowie Karten und Bankkonto
//        createKundeTest();

        //KUNDE - KONTAINER
        Kunde kunde;


        // RETRIEVE KUNDE
//        kunde = retreiveKunde(String.valueOf("Kartenmann"));

        //UPDATE KUNDE-NACHNAME
//        updateKundeToHahaMann(kunde);

        //RETRIEVE KUNDE-HahaMann
//        kunde = retreiveKunde(String.valueOf("HahaMann"));

        //DELETE KUNDE
        deleteKunde(String.valueOf("HahaMann"));

    }

    private static void deleteKunde(String nachname) {
        KundeCriteria kundeCriteria = KundeCriteria.create(nachname);
        KundeManager.delete(kundeCriteria);
    }

    private static void updateKundeToHahaMann(Kunde kunde) {
        kunde.setNachname(String.valueOf("HahaMann"));
        KundeManager.update(kunde);
    }

    private static Kunde retreiveKunde(String nachname) {
        KundeCriteria kundeCriteria = KundeCriteria.create(nachname);

        List<Kunde> kundeList = KundeManager.find(kundeCriteria);
//
        for (Kunde kunde : kundeList) {
            System.out.println(kunde);
        }

        return kundeList.get(0);
    }

    private static void createKundeTest() {
        Kinosaal kinosaal = Kinosaal.create(1, 22);
        Kinokarte kinokarte1 = Kinokarte.create(kinosaal);
        Kinokarte kinokarte2 = Kinokarte.create(kinosaal);
        Kinokarte kinokarte3 = Kinokarte.create(kinosaal);
        Set<Kinokarte> kinokarten = new HashSet<Kinokarte>(
                Arrays.asList(
                        new Kinokarte[]{
                                kinokarte1,
                                kinokarte2,
                                kinokarte3
                        }
                )
        );

        Bankkonto bankkonto = Bankkonto.create(String.valueOf("GB29 NWBK 6016 1331 9268 19"));

        Kunde kunde = Kunde.create(1, String.valueOf("Kartenmann"), bankkonto, kinokarten);

        KundeManager.create(kunde);
    }

}
