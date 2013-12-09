package bank_access;


import mware_lib.NameServiceMessage;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 18.11.13
 * Time: 12:59
 * To change this template use File | Settings | File Templates.
 */
public abstract class ManagerImplBase{
    public abstract String createAccount(String owner,String branch);
    public static ManagerImplBase narrowCast(Object gor) {
        return new ManagerImplStub((NameServiceMessage) gor);
    }

}