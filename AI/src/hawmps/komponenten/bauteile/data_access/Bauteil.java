package hawmps.komponenten.bauteile.data_access;

import hawmps.adts.fachliche.Name;
import hawmps.adts.fachliche.Nummer;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import javax.persistence.*;
import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 04.11.13
 * Time: 20:17
 */
@Entity
public class Bauteil implements Serializable{
    @Id
    @OneToOne(cascade = CascadeType.ALL, mappedBy = "Bauteil")
    private Nummer nummer;
    @OneToOne
    private Name name;
    @OneToOne
    private Nummer arbeitsplanNummer;
    @OneToOne
    private Stueckliste stueckliste;

    public Bauteil() {
    }

    private Bauteil(Name name, Nummer arbeitsplanNummer, Stueckliste stueckliste) {
        this.name = name;
        this.arbeitsplanNummer = arbeitsplanNummer;
        this.stueckliste = stueckliste;
    }

    public static Bauteil create(Name name, Nummer arbeitsplanNummer, Stueckliste stueckliste) {
        return new Bauteil(name, arbeitsplanNummer, stueckliste);
    }

    public Nummer getNummer() {
        return nummer;
    }

    public void setNummer(Nummer nummer) {
        this.nummer = nummer;
    }

    public Name getName() {
        return name;
    }

    public void setName(Name name) {
        this.name = name;
    }

    public Nummer getArbeitsplanNummer() {
        return arbeitsplanNummer;
    }

    public void setArbeitsplanNummer(Nummer arbeitsplanNummer) {
        this.arbeitsplanNummer = arbeitsplanNummer;
    }

    public Stueckliste getStueckliste() {
        return stueckliste;
    }

    public void setStueckliste(Stueckliste stueckliste) {
        this.stueckliste = stueckliste;
    }

    @Override
    public String toString() {
        return "Bauteil{" +
                "nummer=" + nummer +
                ", name=" + name +
                ", arbeitsplanNummer=" + arbeitsplanNummer +
                ", stueckliste=" + stueckliste +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Bauteil bauteil = (Bauteil) o;

        if (!nummer.equals(bauteil.nummer)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return nummer.hashCode();
    }
    public BauteilDTO toDTO(){
        throw new NotImplementedException();
    }

    public void FromDTO(BauteilDTO bauteilDTO) {
        throw new NotImplementedException();
    }
}
