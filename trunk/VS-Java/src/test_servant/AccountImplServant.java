package test_servant;

import bank_access.AccountImplBase;
import bank_access.OverdraftException;
import mware_lib.servant.Servant;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 27.11.13
 * Time: 20:47
 */
public class AccountImplServant extends AccountImplBase implements Servant {
    double account;


    public AccountImplServant(double account) {
        this.account = account;
    }

    @Override
    public void transfer(double amount) throws OverdraftException {
        account += amount;
    }

    @Override
    public double getBalance() {
        return account;
    }

    @Override
    public String toString() {
        return "AccountImplServant{" +
                "account=" + account +
                '}';
    }

}
