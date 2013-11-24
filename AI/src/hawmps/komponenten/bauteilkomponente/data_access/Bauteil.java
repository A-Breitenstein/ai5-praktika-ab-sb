package hawmps.komponenten.bauteilkomponente.data_access;

import hawmps.adts.fachliche.Name;
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
    @GeneratedValue
    private int nummer;

    private Name name;

    private int arbeitsplanNummer;

    @OneToOne(cascade = CascadeType.ALL)
    private Stueckliste stueckliste;

    public Bauteil() {
    }

    private Bauteil(Name name, int arbeitsplanNummer, Stueckliste stueckliste) {
        this.name = name;
        this.arbeitsplanNummer = arbeitsplanNummer;
        this.stueckliste = stueckliste;
    }

    public static Bauteil create(Name name, int arbeitsplanNummer, Stueckliste stueckliste) {
        return new Bauteil(name, arbeitsplanNummer, stueckliste);
    }

    public int getNummer() {
        return nummer;
    }

    public void setNummer(int nummer) {
        this.nummer = nummer;
    }

    public int getArbeitsplanNummer() {
        return arbeitsplanNummer;
    }

    public void setArbeitsplanNummer(int arbeitsplanNummer) {
        this.arbeitsplanNummer = arbeitsplanNummer;
    }

    public Name getName() {
        return name;
    }

    public void setName(Name name) {
        this.name = name;
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

        if (nummer != bauteil.nummer) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return nummer;
    }

    public BauteilDTO toDTO(){
        BauteilDTO bauteilDTO = new BauteilDTO(nummer,name,arbeitsplanNummer,null);
        if(stueckliste != null)
            bauteilDTO.setStueckliste(stueckliste.toDTO(bauteilDTO));
        return bauteilDTO;
    }

    public void FromDTO(BauteilDTO bauteilDTO) {
        nummer = bauteilDTO.getNummer();
        name = bauteilDTO.getName();
        arbeitsplanNummer = bauteilDTO.getArbeitsplanNummer();

        if (bauteilDTO.getStueckliste() != null) {
            stueckliste = new Stueckliste();
            stueckliste.fromDTO(bauteilDTO.getStueckliste(),this);
        }
    }
}
