package bank_access;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 18.11.13
 * Time: 12:59
 * To change this template use File | Settings | File Templates.
 */
public abstract class AccountImplBase {
    public abstract void transfer(double amount) throws OverdraftException;
    public abstract double getBalance();
    public static AccountImplBase narrow_cast(Object o) {...}
}