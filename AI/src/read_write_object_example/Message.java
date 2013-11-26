package read_write_object_example;

import hawmps.adts.fachliche.Adresse;
import hawmps.adts.fachliche.Name;
import hawmps.komponenten.kundenkomponente.IKundenKomponente;
import hawmps.komponenten.kundenkomponente.data_access.KundeDTO;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 24.11.13
 * Time: 20:06
 */
public class Message implements Serializable{
    public Funktion funktion;
    public Object[] parameter;
    public Object rueckgabewert;

    public Message() {
    }

    public Message(Funktion funktion, Object[] parameter) {
        this.funktion = funktion;
        this.parameter = parameter;
    }

    @Override
    public String toString() {
        return "Message{" +
                "funktion=" + funktion +
                ", parameter=" + Arrays.toString(parameter) +
                ", rueckgabewert=" + rueckgabewert +
                '}';
    }

    enum Funktion implements Serializable{
        createKunde,
        findByNachname,
        deleteKundeByNummer;
    }
}
