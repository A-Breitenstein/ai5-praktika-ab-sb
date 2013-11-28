package bank_access;

import mware_lib.ObjectServerMessage;
import mware_lib.Skeleton;

import java.net.Socket;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 28.11.13
 * Time: 23:35
 */
public class ManagerImplSkeleton extends ManagerImplBase implements Skeleton{
    public ManagerImplSkeleton(ManagerSkeletonFactory managerSkeletonFactory, Socket clientSocket, ObjectServerMessage serviceMessage, int references) {
        throw new UnsupportedOperationException("ManagerImplSkeleton :: constructor not implemented yet");
    }

    @Override
    public String createAccount(String owner, String branch) {
        throw new UnsupportedOperationException("ManagerImplSkeleton :: createAccount not implemented yet");
    }

    @Override
    public Object callFunction(ObjectServerMessage serviceMessage) {
        throw new UnsupportedOperationException("ManagerImplSkeleton :: callFunction not implemented yet");
    }
}
