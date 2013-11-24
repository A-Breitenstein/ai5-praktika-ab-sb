package hawmps.fassade;

import hawmps.komponenten.auftragskomponente.IAuftragsKomponente;
import hawmps.komponenten.bauteilkomponente.IBauteileKomponente;
import hawmps.komponenten.kundenkomponente.IKundenKomponente;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 24.11.13
 * Time: 17:56
 */
public interface ISystemFassade extends IAuftragsKomponente,IBauteileKomponente,IKundenKomponente {
}
