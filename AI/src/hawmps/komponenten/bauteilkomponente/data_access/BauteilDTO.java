package hawmps.komponenten.bauteilkomponente.data_access;

import hawmps.adts.fachliche.Name;

import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.11.13
 * Time: 17:21
 */
public class BauteilDTO implements Serializable {
    private int nummer;

    private Name name;

    private int arbeitsplanNummer;

    private StuecklisteDTO stueckliste;

    public BauteilDTO(int nummer, Name name, int arbeitsplanNummer, StuecklisteDTO stueckliste) {
        this.nummer = nummer;
        this.name = name;
        this.arbeitsplanNummer = arbeitsplanNummer;
        this.stueckliste = stueckliste;
    }

    public int getNummer() {
        return nummer;
    }

    public void setNummer(int nummer) {
        this.nummer = nummer;
    }

    public Name getName() {
        return name;
    }

    public void setName(Name name) {
        this.name = name;
    }

    public int getArbeitsplanNummer() {
        return arbeitsplanNummer;
    }

    public void setArbeitsplanNummer(int arbeitsplanNummer) {
        this.arbeitsplanNummer = arbeitsplanNummer;
    }

    public StuecklisteDTO getStueckliste() {
        return stueckliste;
    }

    public void setStueckliste(StuecklisteDTO stueckliste) {
        this.stueckliste = stueckliste;
    }

    @Override
    public String toString() {
        return "BauteilDTO{" +
                "nummer=" + nummer +
                ", name=" + name +
                ", arbeitsplanNummer=" + arbeitsplanNummer +
                ", stueckliste=" + stueckliste +
                '}';
    }
}
