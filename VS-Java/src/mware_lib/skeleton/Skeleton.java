package mware_lib.skeleton;

import mware_lib.object_server.ObjectServerMessage;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 27.11.13
 * Time: 20:44
 */
public interface Skeleton {

    Object callFunction(ObjectServerMessage serviceMessage);
}
