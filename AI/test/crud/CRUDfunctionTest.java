package crud;

import aufgabe1.entities.Bankkonto;
import aufgabe1.entities.Kinokarte;
import aufgabe1.entities.Kinosaal;
import aufgabe1.entities.Kunde;
import aufgabe1.manager.kinosaal.KinosaalManager;
import aufgabe1.manager.kinosaal.KinosaalQuery;
import aufgabe1.manager.kinosaal.KinosaalQueryBuilder;
import aufgabe1.manager.kunde.KundeManager;
import aufgabe1.manager.kunde.KundeQuery;
import aufgabe1.manager.kunde.KundeQueryBuilder;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.testng.Assert;


import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * WHHYYY : http://spitballer.blogspot.de/2010/04/jpa-persisting-vs-merging-entites.html
 *
 * Created with IntelliJ IDEA.
 * Date: 12.10.13
 * Time: 01:20
 * To change this template use File | Settings | File Templates.
 */
public class CRUDfunctionTest {

    final static String const_Kartenmann = "kartenmann";
    final static String const_Hahamann = "Hahamann";

    @Test
    public void createKundeTest() {

        //Erstelle Kinosaal Nr. 1
        KinosaalQueryBuilder kinosaalQueryBuilder = KinosaalManager.createQueryBuilder();
        KinosaalQuery kinosaalQuery = kinosaalQueryBuilder.withLastname(1).createQuery();

        List<Kinosaal> kinosaalList = KinosaalManager.find(kinosaalQuery);

        Kinosaal kinosaal;

        //Eigentlich ein exists() hier hin......
        if (kinosaalList.isEmpty()) {
            kinosaal = Kinosaal.create(1, 22);
        } else {
            kinosaal = kinosaalList.get(0);
        }


        //Kinokarten für Kinosaal Nr.1 erstellen
        Kinokarte kinokarte1 = Kinokarte.create(kinosaal);
        Kinokarte kinokarte2 = Kinokarte.create(kinosaal);
        Kinokarte kinokarte3 = Kinokarte.create(kinosaal);
        //Set von Kinokarten für den Kunden "kartenmann"
        Set<Kinokarte> kinokarten = new HashSet<Kinokarte>(
                Arrays.asList(
                        new Kinokarte[]{
                                kinokarte1,
                                kinokarte2,
                                kinokarte3
                        }
                )
        );

        //Bankkonto für den Kunden "kartenmann"
        Bankkonto bankkonto = Bankkonto.create(String.valueOf("GB29 NWBK 6016 1331 9268 19"));

        Kunde kunde = Kunde.create(String.valueOf(const_Kartenmann), bankkonto, kinokarten);

        if (kinosaalList.isEmpty()) {
            KundeManager.create(kunde);
        }else{
            KundeManager.update(kunde);
        }
    }

    @Test
    public void retreiveKunde() {

        //Hole Kunde "kartenmann" aus der Datenbank
        KundeQueryBuilder kundeQueryBuilder = KundeManager.getQueryBuilder();
        KundeQuery kundeQuery = kundeQueryBuilder.withLastname(const_Kartenmann).createQuery();

        List<Kunde> kundeList = KundeManager.find(kundeQuery);

        Kunde kunde = kundeList.get(0);

        //Gebe Kunden aus
        System.out.println(kunde);

        Assert.assertEquals(kunde.getNachname(), const_Kartenmann);
    }

    @Test
    public void updateKundeToHahaMann() {

        //Hole Kunde "kartenmann" aus der Datenbank
        KundeQueryBuilder kundeQueryBuilder = KundeManager.getQueryBuilder();
        KundeQuery kundeQuery = kundeQueryBuilder.withLastname(const_Kartenmann).createQuery();

        List<Kunde> kundeList = KundeManager.find(kundeQuery);

        Kunde kunde_kartenmann = kundeList.get(0);

        Assert.assertEquals(kunde_kartenmann.getNachname(),const_Kartenmann);

        //Ändere Kunden
        kunde_kartenmann.setNachname(const_Hahamann);
        KundeManager.update(kunde_kartenmann);

        //Suche nach geändertem Kunden "Hahamann"
        kundeQueryBuilder = KundeManager.getQueryBuilder();
        kundeQuery = kundeQueryBuilder.withLastname(const_Hahamann).createQuery();

        kundeList = KundeManager.find(kundeQuery);

        Kunde kunde_hahamann = kundeList.get(0);

        Assert.assertEquals(kunde_hahamann.getNachname(), const_Hahamann);
        Assert.assertTrue(kunde_kartenmann.getKdnr() == kunde_hahamann.getKdnr());

    }

    @Test
    public void deleteKunde() {
        KundeQueryBuilder kundeQueryBuilder = KundeManager.getQueryBuilder();
        KundeQuery kundeQuery = kundeQueryBuilder.withLastname(const_Hahamann).createQuery();

        KundeManager.delete(kundeQuery);

        //Suchen nach dem Kunden
        kundeQueryBuilder = KundeManager.getQueryBuilder();
        kundeQuery = kundeQueryBuilder.withLastname(const_Hahamann).createQuery();

        List<Kunde> kundeList = KundeManager.find(kundeQuery);

        Assert.assertTrue(kundeList.isEmpty());
    }
}
