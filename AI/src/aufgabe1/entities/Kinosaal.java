package aufgabe1.entities;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import java.util.Set;

/**
 * Created with IntelliJ IDEA.
 * User: Akatsuki
 * Date: 04.10.13
 * Time: 16:54
 * To change this template use File | Settings | File Templates.
 */
@Entity
public class Kinosaal {

    @Id
    private int SaalNr;

    private int size;

    @OneToMany
    private Set<Kinokarte> kinokarten;

    public Kinosaal() {
    }


}
