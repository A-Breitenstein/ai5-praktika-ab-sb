package mware_lib.skeleton;

import mware_lib.object_server.ObjectServerMessage;

import java.net.Socket;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 28.11.13
 * Time: 22:39
 */
public interface SkeletonFactory {
    Skeleton createSkeleton(Socket clientSocket, ObjectServerMessage serviceMessage);

    void removeSkeleton(Skeleton skeleton);
    int getReferences();
}
