package aufgabe1.main;

import aufgabe1.entities.Bankkonto;
import aufgabe1.entities.Kinokarte;
import aufgabe1.entities.Kinosaal;
import aufgabe1.entities.Kunde;
import aufgabe1.manager.kunde.KundeManager;
import aufgabe1.manager.kunde.KundeQuery;
import aufgabe1.manager.kunde.KundeQueryBuilder;

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
//        crud = retreiveKunde(String.valueOf("Kartenmann"));

        //UPDATE KUNDE-NACHNAME
//        updateKundeToHahaMann(crud);

        //RETRIEVE KUNDE-HahaMann
        kunde = retreiveKunde(String.valueOf("HahaMann"));

        //DELETE KUNDE
        deleteKunde(String.valueOf("HahaMann"));

    }

    private static void deleteKunde(String nachname) {

        KundeQueryBuilder kundeQueryBuilder = KundeManager.getQueryBuilder();
        KundeQuery kundeQuery = kundeQueryBuilder.withLastname(nachname).createQuery();

        KundeManager.delete(kundeQuery);
    }

    private static void updateKundeToHahaMann(Kunde kunde) {
        kunde.setNachname(String.valueOf("HahaMann"));
        KundeManager.update(kunde);
    }

    private static Kunde retreiveKunde(String nachname) {
        KundeQueryBuilder kundeQueryBuilder = KundeManager.getQueryBuilder();
        KundeQuery kundeQuery = kundeQueryBuilder.withLastname(nachname).createQuery();

        List<Kunde> kundeList = KundeManager.find(kundeQuery);
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

        Kunde kunde = Kunde.create(String.valueOf("Kartenmann"), bankkonto, kinokarten);

        KundeManager.create(kunde);
    }

}
