package mware_lib;

import java.net.Socket;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 27.11.13
 * Time: 20:49
 */
public interface Servant {
    Skeleton createSkeleton(Socket clientSocket,ObjectServerMessage serviceMessage);
    void removeSkeleton(Skeleton skeleton);

    int getReferences();


}
