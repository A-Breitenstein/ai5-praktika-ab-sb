package mware_lib.stub;

import bank_access.OverdraftException;
import mware_lib.object_server.ObjectServerMessage;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 27.11.13
 * Time: 20:46
 */
public interface Stub {
    ObjectServerMessage sendObjectServerMessage(ObjectServerMessage message) throws OverdraftException;
}

