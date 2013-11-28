package bank_access;

import mware_lib.ObjectServerMessage;
import mware_lib.Skeleton;
import mware_lib.SkeletonFactory;

import java.net.Socket;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 28.11.13
 * Time: 23:11
 */
public class ManagerSkeletonFactory extends SkeletonFactoryBase implements SkeletonFactory {
    ManagerImplBase servant;
    private final static ManagerImplBase tester = new ManagerImplServant();
    @Override
    public Skeleton createSkeleton(Socket clientSocket, ObjectServerMessage serviceMessage) {
        Skeleton tmp = new ManagerImplSkeleton(this, clientSocket, serviceMessage, getReferences());
        addToSkeletonList(tmp);
        return tmp;
    }

    @Override
    public void setServant(Object servant) {
        this.servant = (ManagerImplBase) servant;
    }
    @Override
    public Class getServantClass() {
        return tester.getClass().getSuperclass();
    }

}
