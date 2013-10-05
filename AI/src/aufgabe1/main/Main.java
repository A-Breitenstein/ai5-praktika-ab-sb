package aufgabe1.main;

import aufgabe1.entities.Bankkonto;
import aufgabe1.entities.Kinokarte;
import aufgabe1.entities.Kinosaal;
import aufgabe1.entities.Kunde;
import aufgabe1.manager.KinosaalManager;
import aufgabe1.manager.KundeManager;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * Date: 04.10.13
 * Time: 16:53
 */
public class Main {

    public static void main(String[] args) {

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

        KinosaalManager.create(kinosaal);

        KundeManager.create(kunde);

        Kunde findKunde = Kunde.create(1, String.valueOf("Kartenmann"), null, null);

//        Kunde kundeKartenmann = KundeManager.find(findKunde);
//
//        System.out.println(kundeKartenmann);
    }

}
