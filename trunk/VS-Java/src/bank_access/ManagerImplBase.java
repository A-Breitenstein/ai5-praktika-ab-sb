package bank_access;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 18.11.13
 * Time: 12:59
 * To change this template use File | Settings | File Templates.
 */
public abstract class ManagerImplBase{
    public abstract String createAccount(String owner,String branch);
    public static ManagerImplBase narrow_cast(Object gor) {return null;}

    //TEST
    public static SkeletonFactoryBase skeletonFactory(){ return new ManagerSkeletonFactory();}

}