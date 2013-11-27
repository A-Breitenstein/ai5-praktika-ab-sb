package cash_access;

import bank_access.OverdraftException;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 18.11.13
 * Time: 13:00
 * To change this template use File | Settings | File Templates.
 */
public abstract class TransactionImplBase {
    public abstract void deposit(String accountId,double amount)
            throws InvalidParamException;
    public abstract void withdraw(String accountId,double amount)
            throws InvalidParamException,OverdraftException;
    public abstract double getBalance(String accountId)
            throws InvalidParamException;
    public static TransactionImplBase narrow_cast(Object o) {return null;}
}